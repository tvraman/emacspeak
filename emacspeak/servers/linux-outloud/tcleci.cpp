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

#include <eci.h>
#include <tcl.h>

#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"

#ifdef WIN32
#define EXPORT __declspec(dllexport) 
#else
#define EXPORT
#endif

extern "C" EXPORT int Tcleci_Init(Tcl_Interp *interp);
int SetRate(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int GetRate(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Say(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Stop(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int SpeakingP(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Synchronize(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Pause(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Resume(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
ECICallbackReturn eciCallback(ECIHand, ECIMessage, long, void *);

void TclEciFree(ClientData eciHandle) {
  eciDelete(eciHandle);
}

int Tcleci_Init(Tcl_Interp *interp) {
  int rc;
  ECIHand eciHandle;

  if (Tcl_PkgProvide(interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK) {
    Tcl_AppendResult(interp, "Error loading ", PACKAGENAME, NULL);
    return TCL_ERROR;
  }

  eciHandle = eciNew();
  if (eciHandle == NULL_ECI_HAND) { 
    Tcl_AppendResult(interp, "Could not open text-to-speech engine", NULL);
    return TCL_ERROR;
  }

  if (   (eciSetParam(eciHandle, eciInputType, 1) == -1) 
         || (eciSetParam(eciHandle, eciSynthMode, 1) == -1)
         //|| (eciSetParam(eciHandle,eciRealWorldUnits, 1) == -1)
         ) {
    Tcl_AppendResult(interp, "Could not initialized tts", NULL);
    eciDelete(eciHandle); 
    return TCL_ERROR;
  }

  Tcl_CreateObjCommand(interp, "setRate", SetRate,
                       (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "getRate", GetRate, (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "say", Say, (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "synth", Say, (ClientData)
                       eciHandle, NULL);
  Tcl_CreateObjCommand(interp, "synchronize", Synchronize, (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "stop", Stop, (ClientData)
                       eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "speakingP", SpeakingP, (ClientData) eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "pause", Pause, (ClientData)
                       eciHandle, TclEciFree);
  Tcl_CreateObjCommand(interp, "resume", Resume, (ClientData) eciHandle, TclEciFree);
  rc = Tcl_Eval(interp,
                "proc index x {global tts; 
set tts(last_index) $x}");
  eciRegisterCallback( eciHandle, eciCallback, interp );
  return TCL_OK;
}

ECICallbackReturn eciCallback(ECIHand eciHandle, ECIMessage msg, long lparam, void *data) {
  int rc;
  Tcl_Interp *interp = (Tcl_Interp *) data;
  if (msg == eciIndexReply) {
    char buffer[128];
    sprintf(buffer, "index %d", lparam);
    rc = Tcl_Eval(interp, buffer);
    if (rc != TCL_OK) Tcl_BackgroundError(interp);
  }
  return eciDataProcessed;
}

int GetRate(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  int  rc, rate, voice;
  if (objc!=2) {
    Tcl_AppendResult(interp, "Usage: getRate voiceCode  ", TCL_STATIC);
    return TCL_ERROR;
  }
  rc = Tcl_GetIntFromObj(interp, objv[1], &voice);
  if (rc != TCL_OK) return rc;
  rate = eciGetVoiceParam(eciHandle, voice, eciSpeed);
Tcl_SetObjResult( interp, Tcl_NewIntObj( rate ));
  return TCL_OK;
}


int SetRate(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  int  rc, rate, voice ;
  if (objc!=3) {
    Tcl_AppendResult(interp, "Usage: setRate voiceCode speechRate ", TCL_STATIC);
    return TCL_ERROR;
  }
  rc = Tcl_GetIntFromObj(interp, objv[1], &voice);
  if (rc != TCL_OK) return rc;
  rc = Tcl_GetIntFromObj(interp, objv[2], &rate);
  if (rc != TCL_OK) return rc;
  fprintf(stderr, "Setting rate to %d for voice %d\n",
          rate, voice);
  rc = eciSetVoiceParam (eciHandle, voice,  eciSpeed, rate);
  if (rc == -1) {
    Tcl_AppendResult(interp, "Could not set rate", TCL_STATIC);
    return TCL_ERROR;
  }
  fprintf(stderr, "setRate returned %d\n", rc);
  rate = eciGetVoiceParam(eciHandle, voice, eciSpeed);
  fprintf(stderr, "eciGetVoiceParam returned %d for voice %d \n",
          rate, voice );
  return TCL_OK;
}
      

int Say(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  int i, rc, index, length;
  for (i=1; i<objc; i++) {
    // if string begins with -, assume it is an index value
    char *txt = Tcl_GetStringFromObj(objv[i], &length);
    if (Tcl_StringMatch(txt, "-reset")) {
      eciReset(eciHandle);
if (   (eciSetParam(eciHandle, eciInputType, 1) == -1) 
         || (eciSetParam(eciHandle, eciSynthMode, 1) == -1)) {
    Tcl_AppendResult(interp, "Could not initialized tts", NULL);
    return TCL_ERROR;
  }
    } else if (Tcl_StringMatch(txt, "-index")) {
      i++;
      if (i==objc) {
        Tcl_AppendResult(interp, "missing index parameter", TCL_STATIC);
        return TCL_ERROR;
      }
      rc = Tcl_GetIntFromObj(interp, objv[i], &index);
      if (rc != TCL_OK) return rc;
      rc = eciInsertIndex(eciHandle, index);
      if (!rc) {
        Tcl_AppendResult(interp, "Could not insert index", TCL_STATIC);
        return TCL_ERROR;
      }
    } else {
      // assume objv[i] is text to synthesize...
      rc = eciAddText(eciHandle, Tcl_GetStringFromObj(objv[i],NULL));
      if (!rc) {
        Tcl_SetResult(interp, "Internal tts error", TCL_STATIC);
        return TCL_ERROR;
      }
    }
  }
  if (Tcl_StringMatch(Tcl_GetStringFromObj(objv[0],NULL), "synth")) {
    rc = eciSynthesize(eciHandle);
    if (!rc) {
      Tcl_SetResult(interp, "Internal tts synth error", TCL_STATIC);
      return TCL_ERROR;
    }
    eciSpeaking(eciHandle);
  }
  return TCL_OK;
}

int Synchronize(ClientData eciHandle, Tcl_Interp *interp,
                int objc, Tcl_Obj *CONST objv[]) {
  int rc = eciSynchronize(eciHandle);
  if (!rc) {
    Tcl_SetResult(interp, "Internal tts synth error", TCL_STATIC);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int Stop(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  if (eciStop(eciHandle)) return TCL_OK;
  Tcl_SetResult(interp, "Could not stop synthesis", TCL_STATIC);
  return TCL_ERROR;
}

int SpeakingP(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  if ( eciSpeaking(eciHandle)) {
    return 1;
  } else {
    return 0;
  }
}  

int Pause(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  if (eciPause(eciHandle, 1)) return TCL_OK;
  Tcl_SetResult(interp, "Could not pause synthesis", TCL_STATIC);
  return TCL_ERROR;
}


int Resume(ClientData eciHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  if (eciPause(eciHandle, 0)) return TCL_OK;
  Tcl_SetResult(interp, "Could not resume synthesis", TCL_STATIC);
  return TCL_ERROR;
}
