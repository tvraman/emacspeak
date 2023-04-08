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
#include <stdlib.h>
#include<string.h>
#include <langinfo.h>
#include <locale.h>
#include <iconv.h>

/* }}} */
/* {{{defines*/

#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"

/* }}} */
/* {{{prototypes*/

void TclDtkFree (ClientData);
int Say (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Stop (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Synchronize (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);

/* }}} */
/* {{{ iso-latin1 cleanup and speak: */

/* We assume emacs never sends us a malformed utf-8 string
   *The Dectalk  may silently fail on some chars, e.g., þ 
   * (latin small *letter thorn). 
 */

int
speak_latin1 (LPTTS_HANDLE_T dtkHandle, char *in, size_t inLen) {
  char *out, *outP;
  int status;
  iconv_t conv_d =
    iconv_open ("ISO-8859-1//TRANSLIT//IGNORE", nl_langinfo (CODESET));
  size_t outsize = 2 * inLen;
  size_t r;
  out = malloc (outsize + 1);
  if (out == NULL) {
    perror ("malloc");
    exit (EXIT_FAILURE);
  }
  outP = out;
  memset (outP, 0, outsize + 1);
  r = iconv (conv_d, &in, &inLen, &outP, &outsize);

  if (r == -1) {		/* conversion failed:  speak orig input */
    status = TextToSpeechSpeak (dtkHandle, in, TTS_FORCE);
  } else {
    status = TextToSpeechSpeak (dtkHandle, out, TTS_FORCE);
  }
  free (out);
  return status;
}

/* }}} */
/* {{{getErrorMsg*/

char *
getErrorMsg (int errCode) {
  switch (errCode) {
  case MMSYSERR_NOERROR:
    return "Success";
  default:
    return "Error";
  }
}

/* }}} */
/* {{{closing down*/

void
TclDtkFree (ClientData dtkHandle) {
  TextToSpeechShutdown (dtkHandle);
}

/* }}} */
/* {{{init*/

int
Tcldtk_Init (Tcl_Interp * interp) {
  int status;
  char *error_msg = NULL;
  LPTTS_HANDLE_T dtkHandle;
  if (Tcl_PkgProvide (interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK) {
    Tcl_AppendResult (interp, "Error loading ", PACKAGENAME, NULL);
    return TCL_ERROR;
  }

  status = TextToSpeechStartup (&dtkHandle, WAVE_MAPPER, 0, NULL, 0);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg (status);
    Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
    return TCL_ERROR;
  }
  setlocale (LC_CTYPE, "ISO-latin-1");
  if (dtkHandle == NULL) {
    Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
    return TCL_ERROR;
  }

  Tcl_CreateObjCommand (interp, "say", Say, (ClientData) dtkHandle,
			TclDtkFree);
  Tcl_CreateObjCommand (interp, "synth", Say, (ClientData) dtkHandle, NULL);
  Tcl_CreateObjCommand (interp, "synchronize", Synchronize,
			(ClientData) dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand (interp, "stop", Stop, (ClientData) dtkHandle,
			TclDtkFree);
  return TCL_OK;
}

/* }}} */
/* {{{say*/

int
Say (ClientData dtkHandle, Tcl_Interp * interp, int objc,
     Tcl_Obj * CONST objv[]) {
  int length;
  int status;
  char *txt = Tcl_GetStringFromObj (objv[1], &length);
  status = speak_latin1 (dtkHandle, txt, length);
  if (status != TCL_OK) {
    Tcl_SetObjResult (interp, Tcl_NewStringObj ("TTS Error", -1));
    return TCL_ERROR;
  }
  return TCL_OK;
}

/* }}} */
/* {{{ sync*/

int
Synchronize (ClientData dtkHandle, Tcl_Interp * interp,
	     int objc, Tcl_Obj * CONST objv[]) {
  char *error_msg = NULL;
  int status = TextToSpeechSync (dtkHandle);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg (status);
    Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
    return TCL_ERROR;
  }
  return TCL_OK;
}

/* }}} */
/* {{{stop*/

int
Stop (ClientData dtkHandle, Tcl_Interp * interp,
      int objc, Tcl_Obj * CONST objv[]) {
  char *error_msg = NULL;
  int status = TextToSpeechReset (dtkHandle, FALSE);
  if (status != MMSYSERR_NOERROR) {
    error_msg = getErrorMsg (status);
    Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
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
