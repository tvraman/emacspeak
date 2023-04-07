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
/* {{{ iso-latin1 cleanup: */

/* Cloned from say.c   */

char *
string_to_latin1 (char *in, size_t inLen)
{
  char *out, *outP;
  iconv_t conv_d =
    iconv_open ("ISO-8859-1//TRANSLIT//IGNORE", nl_langinfo (CODESET));
  size_t outsize = 4 * inLen;
  size_t outLeft = 0;
  size_t inLeft = inLen;
  size_t r;
  size_t offset;

  out = malloc (outsize + 1);
  if (out == NULL)
    {
      perror ("malloc");
      exit (EXIT_FAILURE);
    }
  outLeft = outsize;
  outP = out;

  do
    {
      memset (outP, 0, outLeft + 1);

      r = iconv (conv_d, &in, &inLeft, &outP, &outLeft);
      if (r == -1)
	{
	  offset = outP - out;
	  outsize = 2 * outsize;
	  out = realloc (out, outsize + 1);
	  if (out == NULL)
	    {
	      perror ("realloc");
	      exit (EXIT_FAILURE);
	    }
	  outLeft += outsize;
	  outP = out + offset;
	}
    }
  while (inLeft > 0);

  iconv (conv_d, NULL, NULL, NULL, NULL);

  return out;
}

/* }}} */
/* {{{getErrorMsg*/

char *
getErrorMsg (MMRESULT errno)
{
  switch (errno)
    {
    case MMSYSERR_NOERROR:
      return "Success";
    case MMSYSERR_ERROR:
      return "Error - Unspecified error";
    default:
      return "Error - Unrecognized error:";
    }
}

/* }}} */
/* {{{closing down*/

void
TclDtkFree (ClientData dtkHandle)
{
  MMRESULT status;
  status = TextToSpeechShutdown (dtkHandle);
  if (status != MMSYSERR_NOERROR)
    {
    }
}

/* }}} */
/* {{{init*/

int
Tcldtk_Init (Tcl_Interp * interp)
{
  MMRESULT status;
  char *error_msg = NULL;
  LPTTS_HANDLE_T dtkHandle;
  unsigned int devNo = WAVE_MAPPER;
  DWORD devOptions = 0;

  if (Tcl_PkgProvide (interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK)
    {
      Tcl_AppendResult (interp, "Error loading ", PACKAGENAME, NULL);
      return TCL_ERROR;
    }

  status = TextToSpeechStartup (&dtkHandle, devNo, devOptions, NULL, 0);
  if (status != MMSYSERR_NOERROR)
    {
      error_msg = getErrorMsg (status);
      Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
      return TCL_ERROR;
    }
  setlocale (LC_CTYPE, "ISO-latin-1");
  if (dtkHandle == NULL)
    {
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
     Tcl_Obj * CONST objv[])
{
  int i, length;
  char *error_msg = NULL;
  MMRESULT status;
  char *txt = NULL;

  for (i = 1; i < objc; i++)
    {
      txt = Tcl_GetStringFromObj (objv[i], &length);
      txt = string_to_latin1 (txt, strlen (txt));
      status = TextToSpeechSpeak (dtkHandle, txt, TTS_FORCE);
      if (status != MMSYSERR_NOERROR)
	{
	  error_msg = getErrorMsg (status);
	  Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
	  return TCL_ERROR;
	}
    }
  return TCL_OK;
}

/* }}} */
/* {{{ sync*/

int
Synchronize (ClientData dtkHandle, Tcl_Interp * interp,
	     int objc, Tcl_Obj * CONST objv[])
{
  char *error_msg = NULL;
  MMRESULT status;

  status = TextToSpeechSync (dtkHandle);
  if (status != MMSYSERR_NOERROR)
    {
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
      int objc, Tcl_Obj * CONST objv[])
{
  MMRESULT status;
  char *error_msg = NULL;
  status = TextToSpeechReset (dtkHandle, FALSE);
  if (status != MMSYSERR_NOERROR)
    {
      error_msg = getErrorMsg (status);
      Tcl_SetObjResult (interp, Tcl_NewStringObj (error_msg, -1));
      return TCL_ERROR;
    }
  status = TextToSpeechSpeak (dtkHandle,
			      "[:phoneme arpabet speak on :say clause]",
			      TTS_NORMAL);
  if (status != MMSYSERR_NOERROR)
    {
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
