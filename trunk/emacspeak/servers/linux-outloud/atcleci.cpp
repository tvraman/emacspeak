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

February 2005 TVR: Updating to use alsalib output routines
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
#include <alsa/asoundlib.h>


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

snd_pcm_t *AHandle;
char *device = "default";                    /* playback device */
//.04  second using 11025k samples.
//note that in the tcl server we select for 0.02 seconds so
//that we dont queue up too many speech samples,
//This is important for stopping speech immediately.
#define BUFSIZE 512
short *waveBuffer;
//< alsa: define  format and rate defaults

snd_pcm_format_t format = SND_PCM_FORMAT_S16;   /* sample format */
unsigned int rate = 11025;                      /* stream rate */
unsigned int channels = 1;                      /* count of channels */
unsigned int buffer_time = 500000;              /* ring buffer in us */
unsigned int period_time = 100000;		/* period time in us */

snd_pcm_uframes_t buffer_size;
snd_pcm_uframes_t period_size;
snd_output_t *output = NULL;

//>
//<decls and function prototypes 

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
static int alsa_init ();
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
int alsa_close (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int eciCallback (void *, int, long, void *);
int playWaveFile (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);

//>
//<alsa: set hw and sw params


static int set_hwparams(snd_pcm_t *handle,
			snd_pcm_hw_params_t *params,
			snd_pcm_access_t access)
{
	int err, dir;
        unsigned int rrate;

	
	/* set the interleaved read/write format */
	err = snd_pcm_hw_params_set_access(handle, params, access);
	if (err < 0) {
		printf("Access type not available for playback: %s\n", snd_strerror(err));
		return err;
	}
	/* set the sample format */
	err = snd_pcm_hw_params_set_format(handle, params, format);
	if (err < 0) {
		printf("Sample format not available for playback: %s\n", snd_strerror(err));
		return err;
	}
	/* set the count of channels */
	err = snd_pcm_hw_params_set_channels(handle, params, channels);
	if (err < 0) {
		printf("Channels count (%i) not available for playbacks: %s\n", channels, snd_strerror(err));
		return err;
	}
	/* set the stream rate */
	rrate = rate;
	err = snd_pcm_hw_params_set_rate_near(handle, params, &rrate, 0);
	if (err < 0) {
		printf("Rate %iHz not available for playback: %s\n", rate, snd_strerror(err));
		return err;
	}
	if (rrate != rate) {
		printf("Rate doesn't match (requested %iHz, get %iHz)\n", rate, err);
		return -EINVAL;
	}
	/* set the buffer time */
	err = snd_pcm_hw_params_set_buffer_time_near(handle, params, &buffer_time, &dir);
	if (err < 0) {
		printf("Unable to set buffer time %i for playback: %s\n", buffer_time, snd_strerror(err));
		return err;
	}
	err = snd_pcm_hw_params_get_buffer_size(params, &buffer_size);
	if (err < 0) {
		printf("Unable to get buffer size for playback: %s\n", snd_strerror(err));
		return err;
	}
	/* set the period time */
	err = snd_pcm_hw_params_set_period_time_near(handle, params, &period_time, &dir);
	if (err < 0) {
		printf("Unable to set period time %i for playback: %s\n", period_time, snd_strerror(err));
		return err;
	}
	err = snd_pcm_hw_params_get_period_size(params, &period_size, &dir);
	if (err < 0) {
		printf("Unable to get period size for playback: %s\n", snd_strerror(err));
		return err;
	}
	/* write the parameters to device */
	err = snd_pcm_hw_params(handle, params);
	if (err < 0) {
		printf("Unable to set hw params for playback: %s\n", snd_strerror(err));
		return err;
	}
	return 0;
}
                        


static int set_swparams(snd_pcm_t *handle, snd_pcm_sw_params_t *swparams)
{
        int err;

        /* get the current swparams */
        err = snd_pcm_sw_params_current(handle, swparams);
        if (err < 0) {
                printf("Unable to determine current swparams for playback: %s\n", snd_strerror(err));
                return err;
        }
        /* start the transfer when the buffer is almost full: */
        /* (buffer_size / avail_min) * avail_min */
        err = snd_pcm_sw_params_set_start_threshold(handle, swparams, (buffer_size / period_size) * period_size);
        if (err < 0) {
                printf("Unable to set start threshold mode for playback: %s\n", snd_strerror(err));
                return err;
        }
        /* allow the transfer when at least period_size samples can be processed */
        err = snd_pcm_sw_params_set_avail_min(handle, swparams, period_size);
        if (err < 0) {
                printf("Unable to set avail min for playback: %s\n", snd_strerror(err));
                return err;
        }
        /* align all transfers to 1 sample */
        err = snd_pcm_sw_params_set_xfer_align(handle, swparams, 1);
        if (err < 0) {
                printf("Unable to set transfer align for playback: %s\n", snd_strerror(err));
                return err;
        }
        /* write the parameters to the playback device */
        err = snd_pcm_sw_params(handle, swparams);
        if (err < 0) {
                printf("Unable to set sw params for playback: %s\n", snd_strerror(err));
                return err;
        }
        return 0;
}

//>
//<alsa: xrun recovery, write samples 

static int xrun_recovery(snd_pcm_t *handle, int err)
{
        if (err == -EPIPE) {    /* under-run */
                err = snd_pcm_prepare(handle);
                if (err < 0)
                        printf("Can't recovery from underrun, prepare failed: %s\n", snd_strerror(err));
                return 0;
        } else if (err == -ESTRPIPE) {
                while ((err = snd_pcm_resume(handle)) == -EAGAIN)
                        sleep(1);       /* wait until the suspend flag is released */
                if (err < 0) {
                        err = snd_pcm_prepare(handle);
                        if (err < 0)
                                printf("Can't recovery from suspend, prepare failed: %s\n", snd_strerror(err));
                }
                return 0;
        }
        return err;
}

/*
 *   Transfer method - write only
 */

static int write_loop(snd_pcm_t *handle,
                      signed short *samples,
                      snd_pcm_channel_area_t *areas)
{
        signed short *ptr;
        int err, cptr;

        while (1) {
                ptr = samples;
                cptr = period_size;
                while (cptr > 0) {
                        err = snd_pcm_writei(handle, ptr, cptr);
                        if (err == -EAGAIN)
                                continue;
                        if (err < 0) {
                                if (xrun_recovery(handle, err) < 0) {
                                        printf("Write error: %s\n", snd_strerror(err));
                                        exit(EXIT_FAILURE);
                                }
                                break;  /* skip one period */
                        }
                        ptr += err ;/* channels=1*/
                        cptr -= err;
                }
        }
}

//>
void
TclEciFree (ClientData eciHandle)
{
  _eciDelete (eciHandle);
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
      Tcl_AppendResult (interp,
                        "eciSetOutputBuffer undef\n", NULL);
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
  Tcl_CreateObjCommand (interp, "playWave", playWaveFile, (ClientData) NULL,
			NULL);
  Tcl_CreateObjCommand (interp, "alsa_close", alsa_close, (ClientData) eciHandle,
			TclEciFree);
  //>
  rc = Tcl_Eval (interp, "proc index x {global tts; \
set tts(last_index) $x}");
  return TCL_OK;
}

/* will play wave files 16 bit stereo and sample rate 11025k */
int
playWaveFile (ClientData unused, Tcl_Interp * interp, int objc,
	      Tcl_Obj * CONST objv[])
{
  size_t count;
  int length;
  char *filename;
  FILE *fp;
  short samples[2 * BUFSIZE];
  if (objc != 2)
    {
      Tcl_AppendResult (interp, "Usage: playWave filename", NULL);
      return TCL_ERROR;
    }
  filename = Tcl_GetStringFromObj (objv[1], &length);
  fp = fopen (filename, "r");
  if (fp == NULL)
    {
      Tcl_AppendResult (interp, "Error opening wave file.", NULL);
      return TCL_ERROR;
    }
  while ((count = fread (samples, 2, 2 * BUFSIZE, fp)) > 0)
    {
      //write (dsp, samples, count);
    }
  fclose (fp);
  fprintf (stderr, "Played %s\n", filename);
  return TCL_OK;
}

int
playTTS (int samples)
{/*converted to alsa*/
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
      sprintf (buffer, "index %ld", lparam);
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
  return TCL_OK;
}

int
Stop (ClientData eciHandle, Tcl_Interp * interp, int objc,
      Tcl_Obj * CONST objv[])
{
  if (_eciStop (eciHandle))
    {
      //possibly call alsa stop here 
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
/* initialize and open alsa device */
int
alsa_init () {
  int err;
  snd_pcm_hw_params_t *hwparams;
  snd_pcm_sw_params_t *swparams;
  snd_pcm_channel_area_t *areas;
  snd_pcm_hw_params_alloca(&hwparams);
  snd_pcm_sw_params_alloca(&swparams);
  err = snd_output_stdio_attach(&output, stdout, 0);
  if (err < 0) {
    fprintf(stderr,
            "Output failed: %s\n",
            snd_strerror(err));
    return TCL_ERROR;
  }
  if ((err = snd_pcm_open(&AHandle, device, SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
    fprintf(stderr,
            "Playback open error: %s\n",
            snd_strerror(err));
    return TCL_ERROR;
  }
/* choose all parameters */
	err = snd_pcm_hw_params_any(AHandle, hwparams);
	if (err < 0) {
		printf("Broken configuration for playback: no configurations available: %s\n", snd_strerror(err));
		return TCL_ERROR;
	}
        if ((err = set_hwparams(AHandle, hwparams, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
    fprintf(stderr,
            "Setting of hwparams failed: %s\n",
            snd_strerror(err));
    return TCL_ERROR;
  }
  if ((err = set_swparams(AHandle, swparams)) < 0) {
    fprintf(stderr,
            "Setting of swparams failed: %s\n",
            snd_strerror(err));
    return TCL_ERROR;
  }

  waveBuffer= (short *)malloc((period_size * channels * snd_pcm_format_width(format)) / 8);
  if (waveBuffer == NULL) {
    fprintf(stderr,
            "Not enough memory\n");
    return TCL_ERROR;
  }
  areas = (snd_pcm_channel_area_t*)calloc(channels, sizeof(snd_pcm_channel_area_t));
  if (areas == NULL) {
    fprintf(stderr,
            "Not enough memory\n" );
    return TCL_ERROR;
  }
  /*set up area -- simplified for single channel */
  areas[0].addr = waveBuffer;
  areas[0].first = 16;
  areas[0].step =  16;
  return TCL_OK;
}

int
alsa_close (ClientData eciHandle, Tcl_Interp * interp, int objc,
	  Tcl_Obj * CONST objv[])
{
  
  //shut down alsa
  snd_pcm_close(AHandle);
  free(waveBuffer);
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
      fprintf (stderr, "setting output to buffer using alsa\n");
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
      rc = alsa_init ();
      if (!rc) {
	  Tcl_AppendResult (interp, "Could not open output device ",
			    device, NULL);
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
