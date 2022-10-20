/* {{{copyright*/
/**
 *Copyright (C) 1995 -- 2022, T. V. Raman
 *All Rights Reserved
 *
 * This file is not part of GNU Emacs, but the same permissions apply.
 *
 * GNU Emacs is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * GNU Emacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */
/* }}} */
/* {{{headers*/

#define __VXWORKS__
#include <tcl.h>
#include <dtk/ttsapi.h>
#include <stdio.h>

/* }}} */
/* {{{defines*/

#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"

extern int Tcldtk_Init(Tcl_Interp *interp);

#define DEBUG_LEVEL 0

/* }}} */
/* {{{prototypes*/

char *getErrorMsg(MMRESULT);

void TclDtkFree(ClientData);
int Say(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Stop(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);

int Synchronize(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Pause(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Resume(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);

/* }}} */
/* {{{global variables*/

char *error_msg;
char error_buff[80];
/* }}} */
/* {{{getErrorMsg*/

char *getErrorMsg(MMRESULT errno) {
  switch (errno) {
  case MMSYSERR_NOERROR:
    return "Success - No Error";
  case MMSYSERR_ERROR:
    return "Error - Unspecified error";
  case MMSYSERR_BADDEVICEID:
    return "Error - Device ID out of range";
  case MMSYSERR_NOTENABLED:
    return "Error - Driver failed to be enabled";
  case MMSYSERR_ALLOCATED:
    return "Error - Device already allocated";
  case MMSYSERR_INVALHANDLE:
    return "Error - Device handle is invalid";
  case MMSYSERR_NODRIVER:
    return "Error - No device driver present";
  case MMSYSERR_NOMEM:
    return "Error - Memory allocation error";
  case MMSYSERR_NOTSUPPORTED:
    return "Error - Function is not supported";
  case MMSYSERR_BADERRNUM:
    return "Error - Error value out of range";
  case MMSYSERR_INVALFLAG:
    return "Error - Invalid flag passed";
  case MMSYSERR_INVALPARAM:
    return "Error - Invalid parameter passed";
  case MMSYSERR_HANDLEBUSY:
    return "Error - Handle being used in another thread";
  case MMSYSERR_LASTERROR:
    return "Error - Last error in range";
  default:
    sprintf(error_buff, "Error - Unrecognized error number: %d", errno);
    return error_msg;
  }
  return "Opps - shouldn't have got to here!\n";
}

/* }}} */
/* {{{closing down*/

void TclDtkFree(ClientData dtkHandle) {
  MMRESULT status;
  status = TextToSpeechShutdown( dtkHandle );
  if (status != MMSYSERR_NOERROR) {
  }
}

/* }}} */
/* {{{init*/

int Tcldtk_Init(Tcl_Interp *interp) {
  MMRESULT status;
  LPTTS_HANDLE_T dtkHandle;
  unsigned int devNo = 0;
  DWORD devOptions = 0;

  devNo = WAVE_MAPPER;
  if (Tcl_PkgProvide(interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK) {
    sprintf(error_buff, "Error loading %s\n", PACKAGENAME);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }

  status = TextToSpeechStartup(&dtkHandle, devNo, devOptions, NULL, 0);
  sprintf(error_buff, "TTS startup returned %d", status);

  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg(status);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }

  if (dtkHandle == NULL) {
    sprintf(error_buff, "Could not open text-to-speech engine");

    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }

  Tcl_CreateObjCommand(interp, "say", Say, (ClientData) dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand(interp, "synth", Say, (ClientData) dtkHandle, NULL);
  Tcl_CreateObjCommand(interp, "synchronize", Synchronize,
                       (ClientData) dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand(interp,"stop", Stop, (ClientData) dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand(interp, "pause", Pause,
                       (ClientData) dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand(interp, "resume", Resume,
                       (ClientData) dtkHandle, TclDtkFree);

  return TCL_OK;
}

/* }}} */
/* {{{say*/

int Say(ClientData dtkHandle, Tcl_Interp *interp, int objc,
        Tcl_Obj *CONST objv[]) {
  int i, length;
  MMRESULT status;
  DWORD dwFlags = TTS_NORMAL;
  char *txt = NULL;

  for (i=1; i<objc; i++) {
    sprintf(error_buff, "For loop - %d. objc = %d", i, objc);
    txt = Tcl_GetStringFromObj(objv[i], &length);
    sprintf(error_buff, "String length is %d", length);
    sprintf(error_buff, "Tcl obj %d. String = %s\n", i, txt);
    if (Tcl_StringMatch(txt, "-reset")) {
      status = TextToSpeechReset(dtkHandle, FALSE);
      if (status != MMSYSERR_NOERROR) {
        error_msg = getErrorMsg(status);
        Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
        return TCL_ERROR;
      }
    }else {
      status = TextToSpeechSpeak(dtkHandle, txt, dwFlags);
      if (status != MMSYSERR_NOERROR) {
        error_msg = getErrorMsg(status);
        Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
        return TCL_ERROR;
      }
    }
  }
  if (Tcl_StringMatch(Tcl_GetStringFromObj(objv[0],NULL), "synth")) {

    status = TextToSpeechSpeak(dtkHandle, "", TTS_FORCE);
    if (status != MMSYSERR_NOERROR) {
      error_msg = getErrorMsg(status);
      Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}

/* }}} */
/* {{{ sync*/

int Synchronize(ClientData dtkHandle, Tcl_Interp *interp,
                int objc, Tcl_Obj *CONST objv[]) {
  MMRESULT status;

  status = TextToSpeechSync(dtkHandle);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg(status);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }
  return TCL_OK;
}

/* }}} */
/* {{{stop*/

int Stop(ClientData dtkHandle, Tcl_Interp *interp,
         int objc, Tcl_Obj *CONST objv[]) {
  MMRESULT status;
  status = TextToSpeechReset (dtkHandle, FALSE);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg(status);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }
  status = TextToSpeechSpeak(dtkHandle,
                             "[:phoneme arpabet speak on :say clause]", TTS_NORMAL);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg(status);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }
  return TCL_OK;
}

/* }}} */
/* {{{ pause and resume */

int Pause(ClientData dtkHandle, Tcl_Interp *interp,
          int objc, Tcl_Obj *CONST objv[]) {
  MMRESULT status;

  status = TextToSpeechPause(dtkHandle);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg(status);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }
  return TCL_OK;
}

int Resume(ClientData dtkHandle, Tcl_Interp *interp,
           int objc, Tcl_Obj *CONST objv[]) {
  MMRESULT status;

  status = TextToSpeechResume(dtkHandle);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg(status);
    Tcl_SetObjResult(interp, Tcl_NewStringObj(error_msg, -1));
    return TCL_ERROR;
  }
  return TCL_OK;
}

/* }}} */
/* {{{end of file*/
/* local variables: */
/* folded-file: t */
/* end: */
/* }}} */
