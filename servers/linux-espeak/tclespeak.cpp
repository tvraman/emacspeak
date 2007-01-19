/*Id: This code comes from atcleci.cpp 4231 2006-10-13 02:43:46Z tv.raman.tv */

// Jan 2007 Gilles Casse gcasse@oralux.org>
// * eSpeak driver for emacspeak
// 

//<copyright info

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
//>
//<includes

#include <sys/time.h>
#include <dlfcn.h>
#include <tcl.h>
#include <string.h>
#include <stdlib.h>
#include <string>
#include <assert.h>
#include "speak_lib.h"

#include <string>
using std::string;

#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"
#define EXPORT

//using namespace std;

//>
//<decls and function prototypes

extern "C" EXPORT int Tclespeak_Init (Tcl_Interp * interp);

int SetRate (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int GetRate (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int getTTSVersion (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Punct (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Caps (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Say (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Stop (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int SpeakingP (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Synchronize (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Pause (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
int Resume (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);

//>
//<TclEspeakFree

void
TclEspeakFree (ClientData handle)
{
  espeak_Terminate();
}

//>
//<Tclespeak_init

int
Tclespeak_Init (Tcl_Interp * interp)
{
  int rc;
  void* handle=NULL;


  //<setup package, create tts handle

  if (Tcl_PkgProvide (interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK)
    {
      Tcl_AppendResult (interp, "Error loading ", PACKAGENAME, NULL);
      return TCL_ERROR;
    }

  espeak_Initialize(AUDIO_OUTPUT_PLAYBACK, 200, NULL);

  //>
  //<register tcl commands

  Tcl_CreateObjCommand (interp, "setRate", SetRate,
			(ClientData) handle, TclEspeakFree);
  Tcl_CreateObjCommand (interp, "getRate", GetRate, (ClientData) handle,
			TclEspeakFree);
  Tcl_CreateObjCommand (interp, "ttsVersion", getTTSVersion,
			(ClientData) handle, TclEspeakFree);
  Tcl_CreateObjCommand (interp, "punct", Punct, (ClientData) handle, NULL);
  Tcl_CreateObjCommand (interp, "caps", Caps, (ClientData) handle, NULL);
  Tcl_CreateObjCommand (interp, "say", Say, (ClientData) handle,
			TclEspeakFree);
  Tcl_CreateObjCommand (interp, "synth", Say, (ClientData) handle, NULL);
  Tcl_CreateObjCommand (interp, "synchronize", Synchronize,
			(ClientData) handle, TclEspeakFree);
  Tcl_CreateObjCommand (interp, "stop", Stop, (ClientData) handle,
			TclEspeakFree);
  Tcl_CreateObjCommand (interp, "speakingP", SpeakingP,
			(ClientData) handle, TclEspeakFree);
  Tcl_CreateObjCommand (interp, "pause", Pause, (ClientData) handle,
			TclEspeakFree);
  Tcl_CreateObjCommand (interp, "resume", Resume, (ClientData) handle,
			TclEspeakFree);
  //>
  //<set up index processing

  rc = Tcl_Eval (interp, "proc index x {global tts; \
set tts(last_index) $x}");

  //>
  return TCL_OK;
}

int
GetRate (ClientData handle, Tcl_Interp * interp,
	 int objc, Tcl_Obj * CONST objv[])
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

  rate = espeak_GetParameter(espeakRATE, 1);

  Tcl_SetObjResult (interp, Tcl_NewIntObj (rate));
  return TCL_OK;
}

int
SetRate (ClientData handle, Tcl_Interp * interp,
	 int objc, Tcl_Obj * CONST objv[])
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

  return (espeak_SetParameter(espeakRATE, rate, 0) == EE_OK) ? TCL_OK : TCL_ERROR;
}

//>
//<say

static bool closeTags(string input, string& output)
{
  char* tag_orig = strdup(input.c_str());
  output = "";

  // check that a text (non whitespace) is present
  char* tag = tag_orig;
  int a_tag_count=0;
  bool a_text_is_present = false;

  while (*tag)
    {
      if (*tag == '<')
	{
	  a_tag_count++;
	}
      if((a_tag_count == 0) 
	 && (*tag != ' ')
	 && (*tag != '\n')
	 && (*tag != '\r')
	 && (*tag != '\t'))
	{
	  a_text_is_present = true;
	  break;
	}
      if ((*tag == '>') && a_tag_count)
	{
	  a_tag_count--;
	}
      tag++;
    }

  if (a_text_is_present)
    {
      tag = tag_orig;
      while (tag)
	{
	  // look for a '<'
	  tag = strrchr(tag_orig, '<');

	  if (tag)
	    {
	      char* end = strchr(tag, ' ');
	      if (!end && (NULL == strchr(tag, '/')))
		{
		  end = strchr(tag, '>');
		}
	      if (end && (tag+1<end))
		{
		  *end=0;
		  output += "</" + string(tag+1) + ">";
		}
	      *tag = 0;
	    }
	}
    }

  free(tag_orig);

  return a_text_is_present;
}


int
Say (ClientData handle, Tcl_Interp * interp,
     int objc, Tcl_Obj * CONST objv[])
{
  int i, rc, index, length;
  for (i = 1; i < objc; i++)
    {
      // if string begins with -, assume it is an index value
      char *txt = Tcl_GetStringFromObj (objv[i], &length);
      if (Tcl_StringMatch (txt, "-reset"))
	{
	  //TBD
// 	  espeakReset (handle);
// 	  if ((espeakSetParam (handle, espeakInputType, 1) == -1)
// 	      || (espeakSetParam (handle, espeakSynthMode, 1) == -1)
// 	      || (espeakSetParam (handle, espeakSampleRate, 1) == -1))
// 	    {
// 	      Tcl_AppendResult (interp, "Could not re-initialized tts", NULL);
// 	      return TCL_ERROR;
// 	    }
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
	  // TBD
// 	  rc = espeakInsertIndex (handle, index);
// 	  if (!rc)
// 	    {
// 	      Tcl_AppendResult (interp, "Could not insert index", TCL_STATIC);
// 	      return TCL_ERROR;
// 	    }
	}
      else
	{
	  // TBD: need a forthcoming eSpeak service (a deffered espeak_Synth).
	  
	  char* a_text = (char*)Tcl_GetStringFromObj (objv[i], NULL);
	  if (a_text)
	    {
	      string a_begin_ssml = a_text;
	      string a_end_ssml;
	      if (closeTags(a_begin_ssml, a_end_ssml))
		{
		  string a_ssml = a_begin_ssml + a_end_ssml;

		  unsigned int unique_identifier=0;
		  espeak_Synth(a_ssml.c_str(), a_ssml.length()+1, 0, POS_CHARACTER, 0, espeakCHARS_UTF8|espeakSSML, &unique_identifier, NULL);
		}
	      // TBD:: EE_BUFFER_FULL?
	    }
	}
    }
  // TBD
  if (Tcl_StringMatch (Tcl_GetStringFromObj (objv[0], NULL), "synth"))
    {
      // TBD: need a forthcoming eSpeak service.
//       rc = espeakSynthesize (handle);
//       if (!rc)
// 	{
// 	  Tcl_SetResult (interp, "Internal tts synth error", TCL_STATIC);
// 	  return TCL_ERROR;
// 	}
    }
  return TCL_OK;
}

//>
//<stop, pause, resume

//<synchronize, stop

int
Synchronize (ClientData handle,
	     Tcl_Interp * interp, int objc, Tcl_Obj * CONST objv[])
{

  espeak_Synchronize();

  return TCL_OK;
}

int
Stop (ClientData handle,
      Tcl_Interp * interp, int objc, Tcl_Obj * CONST objv[])
{
  espeak_Cancel();
  return TCL_OK;
}

//>

int
SpeakingP (ClientData handle, Tcl_Interp * interp, int objc,
	   Tcl_Obj * CONST objv[])
{
  if (espeak_IsPlaying())
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
Pause (ClientData handle, Tcl_Interp * interp, int objc,
       Tcl_Obj * CONST objv[])
{
  // TBD: need a forthcoming eSpeak service.
  return TCL_OK;
}

int
Resume (ClientData handle, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
  // TBD: need a forthcoming eSpeak service.
  return TCL_OK;
}

//>
//<setOutput:NoOp

int
setOutput (ClientData handle, Tcl_Interp * interp, int objc,
	   Tcl_Obj * CONST objv[])
{
  return TCL_OK;
}

//>
//< Caps

int
Caps (ClientData handle,
      Tcl_Interp * interp, int objc, Tcl_Obj * CONST objv[])
{
  char* a_mode = (char*)Tcl_GetStringFromObj (objv[1], NULL);
  if (a_mode)
    {
      int a_type = 0; // none
  
      if (strcmp(a_mode,"tone") == 0)
	{
	  a_type = 1;
	}
      else if (strcmp(a_mode,"spelling") == 0)
	{
	  a_type = 2;
	}
      else if (strcmp(a_mode,"pitch") == 0)
	{
	  a_type = 30;
	}

      espeak_SetParameter(espeakCAPITALS, a_type, 0);
    }
  return TCL_OK;
}

//>
//< Punct

int
Punct (ClientData handle,
      Tcl_Interp * interp, int objc, Tcl_Obj * CONST objv[])
{
  char* a_mode = (char*)Tcl_GetStringFromObj (objv[1], NULL);
  if (a_mode)
    {
      espeak_PUNCT_TYPE a_type = espeakPUNCT_NONE;
  
      if (strcmp(a_mode,"none")==0)
	{
	  a_type = espeakPUNCT_NONE;
	}
      else if (strcmp(a_mode,"all")==0)
	{
	  a_type = espeakPUNCT_ALL;
	}
      else if (strcmp(a_mode,"some")==0)
	{
	  a_type = espeakPUNCT_SOME;
	}

      espeak_SetParameter(espeakPUNCTUATION, a_type, 0);

    }
  return TCL_OK;
}

//>
//<getVersion

int
getTTSVersion (ClientData handle, Tcl_Interp * interp,
	       int objc, Tcl_Obj * CONST objv[])
{
  if (objc != 1)
    {
      Tcl_AppendResult (interp, "Usage: ttsVersion   ", TCL_STATIC);
      return TCL_ERROR;
    }

  // TBD: need a forthcoming eSpeak service.

  char *version = (char *) malloc (16);
  strcpy(version, "????");
  Tcl_SetResult (interp, version, TCL_STATIC);
  return TCL_OK;
}

//>
//<end of file
//local variables:
//folded-file: t
//end:
//>
