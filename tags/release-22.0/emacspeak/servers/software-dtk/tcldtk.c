/*$Id$*/
/* <copyright*/
/**
 *Copyright (C) 1995 -- 2003, T. V. Raman 
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
/* > */
/* <headers*/
#include <tcl.h>
#include <dtk/ttsapi.h>
#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"

/* > */
/* <prototypes*/

#define EXPORT
extern EXPORT int Tcldtk_Init (Tcl_Interp * interp);

int Say (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Stop (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);

int Synchronize (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Pause (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Resume (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);

/* > */
/* <closing down*/

void
TclDtkFree (ClientData dtkHandle)
{
  if (TextToSpeechShutdown (dtkHandle) != MMSYSERR_NOERROR)
    fprintf (stderr, "TextToSpeechShutdown failed.\n");
}

/* > */
/* <init*/

int
Tcldtk_Init (Tcl_Interp * interp)
{
  int status;
  LPTTS_HANDLE_T dtkHandle;
  unsigned int devNo = 0;
  DWORD devOptions = 0;
  devOptions |= WAVE_OPEN_SHAREABLE;
  devOptions |= WAVE_FORMAT_1S16;
  if (Tcl_PkgProvide (interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK)
    {
      Tcl_AppendResult (interp, "Error loading ", PACKAGENAME, NULL);
      return TCL_ERROR;
    }
  fprintf (stderr, "Calling tts startup.\n");
  status = TextToSpeechStartup (&dtkHandle, devNo, devOptions, NULL, 0);
  fprintf (stderr, "tts startup returned %d\n", status);
  switch (status)
    {
    case MMSYSERR_NODRIVER:
      Tcl_AppendResult (interp,
			"TTS: Could not find any wave devices\n", NULL);
      return TCL_ERROR;
      break;
    case MMSYSERR_NOTENABLED:
      Tcl_AppendResult (interp, "TTS: DECtalk license not found.\n", NULL);
      return TCL_ERROR;
      break;
    case MMSYSERR_ALLOCATED:
      Tcl_AppendResult (interp,
			"TTS: DECtalk has exceeded license quota.\n", NULL);
      return TCL_ERROR;
      break;
    case MMSYSERR_NOERROR:
      break;

    default:
      Tcl_AppendResult (interp, "\n%s: TextToSpeechStartup failed, \n", NULL);
      return TCL_ERROR;
    }
  if (dtkHandle == NULL)
    {
      Tcl_AppendResult (interp, "Could not open text-to-speech engine", NULL);
      return TCL_ERROR;
    }

  Tcl_CreateObjCommand (interp, "say", Say, (ClientData) dtkHandle,
			TclDtkFree);
  Tcl_CreateObjCommand (interp, "synth", Say, (ClientData) dtkHandle, NULL);
  Tcl_CreateObjCommand (interp, "synchronize", Synchronize,
			(ClientData) dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand (interp, "stop", Stop, (ClientData) dtkHandle,
			TclDtkFree);

  Tcl_CreateObjCommand (interp, "pause", Pause, (ClientData)
			dtkHandle, TclDtkFree);
  Tcl_CreateObjCommand (interp, "resume", Resume, (ClientData) dtkHandle,
			TclDtkFree);
  return TCL_OK;
}

/* > */
/* <say*/

int
Say (ClientData dtkHandle, Tcl_Interp * interp, int objc,
     Tcl_Obj * CONST objv[])
{
  int i, rc, length;
  DWORD dwFlags = 0;
  for (i = 1; i < objc; i++)
    {
      char *txt = Tcl_GetStringFromObj (objv[i], &length);
      if (Tcl_StringMatch (txt, "-reset"))
	{
	  TextToSpeechReset (dtkHandle, 0);
	}
      else
	{
	  if (TextToSpeechSpeak (dtkHandle,
				 Tcl_GetStringFromObj (objv[i], NULL),
				 dwFlags) != MMSYSERR_NOERROR)
	    {
	      Tcl_SetResult (interp, "Internal tts error", TCL_STATIC);
	      return TCL_ERROR;
	    }
	}
    }
  if (Tcl_StringMatch (Tcl_GetStringFromObj (objv[0], NULL), "synth"))
    {
      rc = TextToSpeechSpeak (dtkHandle, "", TTS_FORCE);
      if (rc != MMSYSERR_NOERROR)
	{
	  Tcl_SetResult (interp, "Internal tts synth error", TCL_STATIC);
	  return TCL_ERROR;
	}
    }
  return TCL_OK;
}

/* > */
/* < sync*/

int
Synchronize (ClientData dtkHandle, Tcl_Interp * interp,
	     int objc, Tcl_Obj * CONST objv[])
{
  int rc = TextToSpeechSync (dtkHandle);
  if (!rc)
    {
      Tcl_SetResult (interp, "Internal tts synth error", TCL_STATIC);
      return TCL_ERROR;
    }
  return TCL_OK;
}

/* > */
/* <stop*/

int
Stop (ClientData dtkHandle, Tcl_Interp * interp, int objc,
      Tcl_Obj * CONST objv[])
{
  TextToSpeechReset (dtkHandle, FALSE);
  TextToSpeechSpeak (dtkHandle,
		     "[:phoneme arpabet speak on :say clause]", TTS_NORMAL);
  return TCL_OK;
}

/* > */
/* < pause and resume */

int
Pause (ClientData dtkHandle, Tcl_Interp * interp, int objc,
       Tcl_Obj * CONST objv[])
{
  if (TextToSpeechPause (dtkHandle) == 0)
    {				/*paused successfully */
      return TCL_OK;
    }
  else
    {
      Tcl_SetResult (interp, "Could not pause synthesis", TCL_STATIC);
      return TCL_ERROR;
    }
}

int
Resume (ClientData dtkHandle, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
  if (TextToSpeechResume (dtkHandle) == 0)
    return TCL_OK;
  Tcl_SetResult (interp, "Could not resume synthesis", TCL_STATIC);
  return TCL_ERROR;
}

/* > */
/* <end of file*/
/* local variables: */
/* folded-file: t */
/* end: */
/* > */
