/*$Id$*/
/* Tcl ViaVoiceOutloud Interface program
   (c) Copyright 1999 by Paige Phault

   The author hereby grants permission to use, copy, modify, distribute, and 
   license this software for any purpose, provided that existing copyright notices
   are retained in all copies and that this notice is included verbatim in any 
   distributions. No written agreement, license, or royalty fee is required for 
   any of the authorized uses.  Modifications to this software may be copyrighted 
   by their authors and need not follow the licensing terms described here, 
   provided that the new terms are clearly indicated on the first page of each 
   file where they apply.

   IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR 
   DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT 
   OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY DERIVATIVES THEREOF, 
   EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING, 
   BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
   PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE IS PROVIDED ON AN 
   "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO OBLIGATION TO PROVIDE 
   MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

   dynamic loading of eci library contributed by Jeffrey Sorensen
   --this allows a compiled version  of this 
   speech server to be distributed without violating the IBM
   Viavoice license.
   This means that end-users only need install the Viavoice RTK
   (Runtime toolkit)
   to use Emacspeak with the ViaVoice TTS engine.
*/
/* TCL usage
        package require tts

        proc index x {
                puts "I just played index $x"
        }

        synth "Hello world"
        synth -index 0 "This is some" -index 1 "really wierd"
        say -index 2 "text"
        say -reset

The only difference bewtween say and synth is that synth calls
eciSynthesize and say doesn't.  You can put as many text blocks as
you like after a command.
*/
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <linux/soundcard.h>
#include <sys/ioctl.h>
#include <tcl.h>
#include <dlfcn.h>

#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"

#ifdef WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

#define ECILIBRARYNAME "libibmeci.so"

#define DSP "/dev/dsp"
int dsp = -1;
//.04  second using 11025k samples.
//note that in the tcl server we select for 0.02 seconds so
//that we dont queue up too many speech samples,
//This is important for stopping speech immediately.
#define BUFSIZE 512
short waveBuffer[BUFSIZE];

/* The following declarations are derived from the publically available
   documentation for ViaVoice TTS outloud.  
--they are placed here to obviate the need for having the
ViaVoice SDK installed.
*/

typedef enum
{
  eciDataNotProcessed, eciDataProcessed
}
ECICallbackReturn;

typedef enum
{
  eciWaveformBuffer, eciPhonemeBuffer, eciIndexReply, eciPhonemeIndexReply
}
ECIMessage;

typedef enum
{
  eciSynthMode,
  eciInputType,
  eciTextMode,
  eciDictionary,
  eciSampleRate = 5,
  eciWantPhonemeIndices = 7,
  eciRealWorldUnits,
  eciLanguageDialect,
  eciNumberMode,
  eciPhrasePrediction,
  eciNumParams
}
ECIParam;
static void (*_eciVersion) (char *);
static void *(*_eciNew) ();
static void (*_eciDelete) (void *);
static int (*_eciReset) (void *);
static int (*_eciStop) (void *);
static int (*_eciClearInput) (void *);
static int (*_eciPause) (void *, int);
static int (*_eciSynthesize) (void *);
static int (*_eciSynchronize) (void *);
static int (*_eciSpeaking) (void *);
static int (*_eciAddText) (void *, char *);
static int (*_eciInsertIndex) (void *, int);
static int (*_eciSetParam) (void *, int, int);
static int (*_eciGetVoiceParam) (void *, int, int);
static int (*_eciSetVoiceParam) (void *, int, int, int);
static int (*_eciSetOutputBuffer) (void *, int, short *);
static int (*_eciSetOutputDevice) (void *, int);
static void (*_eciRegisterCallback) (void *,
                                     int (*)(void *, int, long, void *),
                                     void *);
static int openDSP ();
extern "C" EXPORT int Tcleci_Init (Tcl_Interp * interp);
int SetRate (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int GetRate (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int getTTSVersion (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Say (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Stop (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int SpeakingP (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Synchronize (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Pause (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Resume (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int setOutput (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int closeDSP (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int eciCallback (void *, int, long, void *);

void
TclEciFree (ClientData eciHandle)
{
  _eciDelete (eciHandle);
  close (dsp);
  dsp = -1;
}

int
Tcleci_Init (Tcl_Interp * interp)
{
  int rc;
  void *eciHandle;
  void *eciLib;
  //< configure shared library symbols

  eciLib = dlopen (ECILIBRARYNAME, RTLD_LAZY);
  if (eciLib == NULL)
    {
      Tcl_AppendResult (interp, "Could not load ",
                        ECILIBRARYNAME,
                        "Error was: ",
                        dlerror (),
                        "\nPlease install the IBM ViaVoice Outloud RTK",
                        NULL);
      return TCL_ERROR;
    }

  _eciVersion = (void (*)(char *)) dlsym (eciLib, "eciVersion");
  _eciNew = (void *(*)()) dlsym (eciLib, "eciNew");
  _eciDelete = (void (*)(void *)) dlsym (eciLib, "eciDelete");
  _eciReset = (int (*)(void *)) dlsym (eciLib, "eciReset");
  _eciStop = (int (*)(void *)) dlsym (eciLib, "eciStop");
  _eciClearInput = (int (*)(void *)) dlsym (eciLib, "eciClearInput");
  _eciPause = (int (*)(void *, int)) dlsym (eciLib, "eciPause");
  _eciSynthesize = (int (*)(void *)) dlsym (eciLib, "eciSynthesize");
  _eciSynchronize = (int (*)(void *)) dlsym (eciLib, "eciSynchronize");
  _eciSpeaking = (int (*)(void *)) dlsym (eciLib, "eciSpeaking");
  _eciInsertIndex = (int (*)(void *, int)) dlsym (eciLib, "eciInsertIndex");
  _eciAddText = (int (*)(void *, char *)) dlsym (eciLib, "eciAddText");
  _eciSetParam = (int (*)(void *, int, int)) dlsym (eciLib, "eciSetParam");
  _eciGetVoiceParam = (int (*)(void *, int, int))
    dlsym (eciLib, "eciGetVoiceParam");
  _eciSetVoiceParam = (int (*)(void *, int, int, int))
    dlsym (eciLib, "eciSetVoiceParam");
  _eciRegisterCallback = (void
                          (*)(void *, int (*)(void *, int, long, void *),
                              void *)) dlsym (eciLib, "eciRegisterCallback");
  _eciSetOutputBuffer =
    (int (*)(void *, int, short *)) dlsym (eciLib, "eciSetOutputBuffer");
  _eciSetOutputDevice =
    (int (*)(void *, int)) dlsym (eciLib, "eciSetOutputDevice");

  //>
  //< check for needed symbols 

  int okay = 1;
  if (!_eciNew)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciNew undef\n", NULL);
    }
  if (!_eciDelete)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciDelete undef\n", NULL);
    }
  if (!_eciReset)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciReset undef\n", NULL);
    }
  if (!_eciStop)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciStop undef\n", NULL);
    }
  if (!_eciClearInput)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciClearInput undef\n", NULL);
    }
  if (!_eciPause)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciPause undef\n", NULL);
    }
  if (!_eciSynthesize)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSynthesize undef\n", NULL);
    }
  if (!_eciSpeaking)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSpeaking undef\n", NULL);
    }
  if (!_eciInsertIndex)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciInsertIndex undef\n", NULL);
    }
  if (!_eciAddText)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciAddText undef\n", NULL);
    }
  if (!_eciSetParam)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSetParam undef\n", NULL);
    }
  if (!_eciSetParam)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSetParam undef\n", NULL);
    }
  if (!_eciGetVoiceParam)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciGetVoiceParam undef\n", NULL);
    }
  if (!_eciSetVoiceParam)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSetVoiceParam undef\n", NULL);
    }
  if (!_eciRegisterCallback)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciRegisterCallback undef\n", NULL);
    }
  if (!_eciSetOutputBuffer)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSetOutputBuffer undef\n", NULL);
    }
  if (!_eciSetOutputDevice)
    {
      okay = 0;
      Tcl_AppendResult (interp, "eciSetOutputDevice undef\n", NULL);
    }
  if (!okay)
    {
      Tcl_AppendResult (interp, "Missing symbols from ", ECILIBRARYNAME,
                        NULL);
      return TCL_ERROR;
    }

  //>

  if (Tcl_PkgProvide (interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK)
    {
      Tcl_AppendResult (interp, "Error loading ", PACKAGENAME, NULL);
      return TCL_ERROR;
    }

  eciHandle = _eciNew ();
  if (eciHandle == 0)
    {
      Tcl_AppendResult (interp, "Could not open text-to-speech engine", NULL);
      return TCL_ERROR;
    }
  //<initialize TTS 

  if ((_eciSetParam (eciHandle, eciInputType, 1) == -1)
      || (_eciSetParam (eciHandle, eciSynthMode, 1) == -1)
      || (_eciSetParam (eciHandle, eciSampleRate, 1) == -1)
      //|| (_eciSetParam(eciHandle,8/*eciRealWorldUnits*/, 1) == -1)
    )
    {
      Tcl_AppendResult (interp, "Could not initialized tts", NULL);
      _eciDelete (eciHandle);
      return TCL_ERROR;
    }
  _eciRegisterCallback (eciHandle, eciCallback, interp);

  //>
  //<register tcl commands 

  Tcl_CreateObjCommand (interp, "setRate", SetRate,
                        (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand (interp, "getRate", GetRate, (ClientData) eciHandle,
                        TclEciFree);
  Tcl_CreateObjCommand (interp, "ttsVersion", getTTSVersion,
                        (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand (interp, "say", Say, (ClientData) eciHandle,
                        TclEciFree);
  Tcl_CreateObjCommand (interp, "synth", Say, (ClientData) eciHandle, NULL);
  Tcl_CreateObjCommand (interp, "synchronize", Synchronize,
                        (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand (interp, "stop", Stop, (ClientData) eciHandle,
                        TclEciFree);
  Tcl_CreateObjCommand (interp, "speakingP", SpeakingP,
                        (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand (interp, "pause", Pause, (ClientData) eciHandle,
                        TclEciFree);
  Tcl_CreateObjCommand (interp, "resume", Resume, (ClientData) eciHandle,
                        TclEciFree);
  Tcl_CreateObjCommand (interp, "setOutput", setOutput,
                        (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand (interp, "closeDSP", closeDSP, (ClientData) eciHandle,
                        TclEciFree);
  //>
  rc = Tcl_Eval (interp, "proc index x {global tts; \
set tts(last_index) $x}");
  return TCL_OK;
}

int
playTTS (int samples)
{
  int i;
  //Use BUFSIZE here since we dont want a variable sized array
  short stereo[2 * BUFSIZE];//we will get BUFSIZE or less samples
  /* mono to stereo */
  for (i = 0; i < samples; i++)
    {
      stereo[2 * i] = waveBuffer[i];
      stereo[2 * i + 1] = waveBuffer[i];
    }
  if (dsp == -1)
    openDSP ();
  write (dsp, stereo, 4 * samples);
  return eciDataProcessed;
}

int
eciCallback (void *eciHandle, int msg, long lparam, void *data)
{
  int rc;
  Tcl_Interp *interp = (Tcl_Interp *) data;
  if (msg == eciIndexReply /* eciIndexReply */ )
    {
      char buffer[128];
      snprintf (buffer,
                128,
                "index %ld", lparam);
      rc = Tcl_Eval (interp, buffer);
      if (rc != TCL_OK)
        Tcl_BackgroundError (interp);
    }
  else if ((msg == eciWaveformBuffer) && (lparam > 0))
    {
      playTTS (lparam);
    }
  return 1;
}

int
GetRate (ClientData eciHandle, Tcl_Interp * interp, int objc,
         Tcl_Obj * CONST objv[])
{
  int rc, rate, voice;
  if (objc != 2)
    {
      Tcl_AppendResult (interp, "Usage: getRate voiceCode  ", TCL_STATIC);
      return TCL_ERROR;
    }
  rc = Tcl_GetIntFromObj (interp, objv[1], &voice);
  if (rc != TCL_OK)
    return rc;
  rate = _eciGetVoiceParam (eciHandle, voice, 6 /*eciSpeed */ );
  Tcl_SetObjResult (interp, Tcl_NewIntObj (rate));
  return TCL_OK;
}

int
SetRate (ClientData eciHandle, Tcl_Interp * interp, int objc,
         Tcl_Obj * CONST objv[])
{
  int rc, rate, voice;
  if (objc != 3)
    {
      Tcl_AppendResult (interp, "Usage: setRate voiceCode speechRate ",
                        TCL_STATIC);
      return TCL_ERROR;
    }
  rc = Tcl_GetIntFromObj (interp, objv[1], &voice);
  if (rc != TCL_OK)
    return rc;
  rc = Tcl_GetIntFromObj (interp, objv[2], &rate);
  if (rc != TCL_OK)
    return rc;
  rc = _eciSetVoiceParam (eciHandle, voice, 6 /*eciSpeed */ , rate);
  if (rc == -1)
    {
      Tcl_AppendResult (interp, "Could not set rate", TCL_STATIC);
      return TCL_ERROR;
    }
  //fprintf(stderr, "setRate returned %d\n", rc);
  rate = _eciGetVoiceParam (eciHandle, voice, 6 /*eciSpeed */ );
  fprintf (stderr, "eciGetVoiceParam returned %d for voice %d \n",
           rate, voice);
  return TCL_OK;
}


int
Say (ClientData eciHandle, Tcl_Interp * interp, int objc,
     Tcl_Obj * CONST objv[])
{
  int i, rc, index, length;
  for (i = 1; i < objc; i++)
    {
      // if string begins with -, assume it is an index value
      char *txt = Tcl_GetStringFromObj (objv[i], &length);
      if (Tcl_StringMatch (txt, "-reset"))
        {
          _eciReset (eciHandle);
          if ((_eciSetParam (eciHandle, 1 /*eciInputType */ , 1) == -1)
              || (_eciSetParam (eciHandle, 0 /*eciSynthMode */ , 1) == -1))
            {
              Tcl_AppendResult (interp, "Could not initialized tts", NULL);
              return TCL_ERROR;
            }
        }
      else if (Tcl_StringMatch (txt, "-index"))
        {
          i++;
          if (i == objc)
            {
              Tcl_AppendResult (interp, "missing index parameter",
                                TCL_STATIC);
              return TCL_ERROR;
            }
          rc = Tcl_GetIntFromObj (interp, objv[i], &index);
          if (rc != TCL_OK)
            return rc;
          rc = _eciInsertIndex (eciHandle, index);
          if (!rc)
            {
              Tcl_AppendResult (interp, "Could not insert index", TCL_STATIC);
              return TCL_ERROR;
            }
        }
      else
        {
          // assume objv[i] is text to synthesize...
          rc = _eciAddText (eciHandle, Tcl_GetStringFromObj (objv[i], NULL));
          if (!rc)
            {
              Tcl_SetResult (interp, "Internal tts error", TCL_STATIC);
              return TCL_ERROR;
            }
        }
    }
  if (Tcl_StringMatch (Tcl_GetStringFromObj (objv[0], NULL), "synth"))
    {
      rc = _eciSynthesize (eciHandle);
      if (!rc)
        {
          Tcl_SetResult (interp, "Internal tts synth error", TCL_STATIC);
          return TCL_ERROR;
        }
    }
  return TCL_OK;
}

int
Synchronize (ClientData eciHandle, Tcl_Interp * interp,
             int objc, Tcl_Obj * CONST objv[])
{
  int rc = _eciSynchronize (eciHandle);
  if (!rc)
    {
      Tcl_SetResult (interp, "Internal tts synth error", TCL_STATIC);
      return TCL_ERROR;
    }
  if (dsp != -1)
    {
      close (dsp);
      dsp = -1;
    }
  return TCL_OK;
}

int
Stop (ClientData eciHandle, Tcl_Interp * interp, int objc,
      Tcl_Obj * CONST objv[])
{
  if (_eciStop (eciHandle))
    {
      if (dsp != -1)
        {
          close (dsp);
          dsp = -1;
        }
      return TCL_OK;
    }
  Tcl_SetResult (interp, "Could not stop synthesis", TCL_STATIC);
  return TCL_ERROR;
}

int
SpeakingP (ClientData eciHandle, Tcl_Interp * interp, int
           objc, Tcl_Obj * CONST objv[])
{
  if (_eciSpeaking (eciHandle))
    {
      Tcl_SetObjResult (interp, Tcl_NewIntObj (1));
    }
  else
    {
      Tcl_SetObjResult (interp, Tcl_NewIntObj (0));
    }
  return TCL_OK;
}

int
Pause (ClientData eciHandle, Tcl_Interp * interp, int objc,
       Tcl_Obj * CONST objv[])
{
  if (_eciPause (eciHandle, 1))
    return TCL_OK;
  Tcl_SetResult (interp, "Could not pause synthesis", TCL_STATIC);
  return TCL_ERROR;
}

int
Resume (ClientData eciHandle, Tcl_Interp * interp, int objc,
        Tcl_Obj * CONST objv[])
{
  if (_eciPause (eciHandle, 0))
    return TCL_OK;
  Tcl_SetResult (interp, "Could not resume synthesis", TCL_STATIC);
  return TCL_ERROR;
}

int
openDSP ()
{
  int tmp, _dsp;
  _dsp = open (DSP, O_WRONLY);
  if (_dsp == -1)
    {
      return -1;
    }
  dsp = _dsp;
  ioctl (dsp, SNDCTL_DSP_RESET, 0);
  tmp = 11025;
  ioctl (dsp, SNDCTL_DSP_SPEED, &tmp);
  tmp = 1;
  ioctl (dsp, SNDCTL_DSP_STEREO, &tmp);
  tmp = 16;
  ioctl (dsp, SNDCTL_DSP_SAMPLESIZE, &tmp);
  tmp = 11025;
  ioctl (dsp, SNDCTL_DSP_GETBLKSIZE, &tmp);
  return dsp;
}

int
closeDSP (ClientData eciHandle, Tcl_Interp * interp, int objc,
          Tcl_Obj * CONST objv[])
{
  close (dsp);
  dsp = -1;
  return TCL_OK;
}

int
setOutput (ClientData eciHandle, Tcl_Interp * interp, int objc,
           Tcl_Obj * CONST objv[])
{
  int rc, length;
  char *output;
  if (objc != 2)
    {
      Tcl_AppendResult (interp, "Usage: setOutput [buffer | default] ",
                        TCL_STATIC);
      return TCL_ERROR;
    }
  output = Tcl_GetStringFromObj (objv[1], &length);
  if (Tcl_StringMatch (output, "buffer"))
    {
      fprintf (stderr, "setting output to buffer\n");
      //<set output wave buffer 
      rc = _eciSynchronize (eciHandle);
      if (!rc)
        {
          Tcl_AppendResult (interp, "Error  resetting TTS engine.\n", NULL);
          return TCL_ERROR;
        }
      rc = _eciSetOutputBuffer (eciHandle, BUFSIZE, waveBuffer);
      if (!rc)
        {
          Tcl_AppendResult (interp, "Error setting output buffer.\n", NULL);
          return TCL_ERROR;
        }
      openDSP ();
      if (dsp == -1)
        {
          Tcl_AppendResult (interp, "Could not open output device ",
                            DSP, NULL);
          return (TCL_ERROR);
        }
      //>
    }
  else if (Tcl_StringMatch (output, "default"))
    {
      //stop using wave buffers
      fprintf (stderr, "switching to default device.\n");
      rc = _eciSetOutputDevice (eciHandle, -1);
      if (!rc)
        {
          Tcl_AppendResult (interp,
                            "Failed to set output to default device. \n",
                            NULL);
          return TCL_ERROR;
        }
      rc = _eciSetOutputBuffer (eciHandle, 0, NULL);
      if (!rc)
        {
          Tcl_AppendResult (interp, "Error unsetting output buffer.\n", NULL);
          return TCL_OK;
        }
      close (dsp);
    }
  else
    {
      Tcl_AppendResult (interp, "Usage: setOutput [buffer | default] ",
                        TCL_STATIC);
      return TCL_ERROR;
    }
  fprintf (stderr, "Set output to %s\n", output);
  return TCL_OK;
}

int
getTTSVersion (ClientData eciHandle, Tcl_Interp * interp, int objc,
               Tcl_Obj * CONST objv[])
{
  char *version = (char *) malloc (16);
  if (objc != 1)
    {
      Tcl_AppendResult (interp, "Usage: ttsVersion   ", TCL_STATIC);
      return TCL_ERROR;
    }
  _eciVersion (version);
  Tcl_SetResult (interp, version, TCL_STATIC);
  return TCL_OK;
}

//<end of file 
//local variables:
//folded-file: t
//end:
//>
