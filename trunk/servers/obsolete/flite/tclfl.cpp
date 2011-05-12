/*$Id$*/
/* Embed FLite (Festival Lite) in TCL*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include "flite.h"
#include "flite_version.h"
#include "voxdefs.h"

#include <linux/soundcard.h>
#include <sys/ioctl.h>
#include <tcl.h>

cst_voice *REGISTER_VOX(const char *voxdir);
cst_voice *UNREGISTER_VOX(cst_voice *vox);

#define PACKAGENAME "tts"
#define PACKAGEVERSION "1.0"

#define DSP "/dev/dsp"
int  dsp = -1;
//.1 second using 11025k samples.
//note that in the tcl server we select for 0.09 seconds so
//that we dont queue up too many speech samples,
//This is important for stopping speech immediately.
#define BUFSIZE 512
short waveBuffer[BUFSIZE];

/* function prototypes */
static int openDSP();
extern  int Tclfl_Init(Tcl_Interp *interp);
int SetRate(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int GetRate(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int getTTSVersion(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Say(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int Stop(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);
int closeDSP(ClientData, Tcl_Interp *, int, Tcl_Obj * CONST []);

void TclFlFree(ClientData flHandle) {
  close (dsp);
  dsp = -1;
}


int Tclfl_Init(Tcl_Interp *interp) {
  int rc;
  cst_voice *v;
  v = REGISTER_VOX(NULL);
  flite_init();
  if (Tcl_PkgProvide(interp, PACKAGENAME, PACKAGEVERSION) != TCL_OK) {
    Tcl_AppendResult(interp, "Error loading ", PACKAGENAME, NULL);
    return TCL_ERROR;
  }
  
  //<register tcl commands 

  Tcl_CreateObjCommand(interp, "setRate", SetRate,
                       (ClientData) NULL, TclFlFree);
  Tcl_CreateObjCommand(interp, "getRate", GetRate, (ClientData) NULL, TclFlFree);
  Tcl_CreateObjCommand(interp, "ttsVersion", getTTSVersion, (ClientData) NULL, TclFlFree);
  Tcl_CreateObjCommand(interp, "say", Say, (ClientData) NULL, TclFlFree);
  Tcl_CreateObjCommand(interp, "synth", Say, (ClientData)
                       NULL, NULL);
  Tcl_CreateObjCommand(interp, "synchronize", Synchronize, (ClientData) NULL, TclFlFree);
  Tcl_CreateObjCommand(interp, "stop", Stop, (ClientData)
                       NULL, TclFlFree);
  Tcl_CreateObjCommand(interp, "closeDSP", closeDSP, (ClientData)
                       NULL, TclFlFree);

  //>
  return TCL_OK;
}





int playTTS (int samples) {
  int i;
  short stereo[2*samples];
  /* mono to stereo */
  for (i=0; i<samples; i++) {
    stereo[2*i] =waveBuffer[i];
    stereo[2*i+1] = waveBuffer[i];
  }
  if (dsp == -1) openDSP();
  write (dsp, stereo,  4*samples);
  return TCL_OK;
}



int GetRate(ClientData flHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  int  rc, rate, voice;
  if (objc!=2) {
    Tcl_AppendResult(interp, "Usage: getRate voiceCode  ", TCL_STATIC);
    return TCL_ERROR;
  }
  rc = Tcl_GetIntFromObj(interp, objv[1], &voice);
  if (rc != TCL_OK) return rc;
  rate =100; /* to be implemented */
  Tcl_SetObjResult( interp, Tcl_NewIntObj( rate ));
  return TCL_OK;
}

int SetRate(ClientData flHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  int  rc, rate, voice ;
  if (objc!=3) {
    Tcl_AppendResult(interp, "Usage: setRate voiceCode speechRate ", TCL_STATIC);
    return TCL_ERROR;
  }
  rc = Tcl_GetIntFromObj(interp, objv[1], &voice);
  if (rc != TCL_OK) return rc;
  rc = Tcl_GetIntFromObj(interp, objv[2], &rate);
  if (rc != TCL_OK) return rc;
  /* to be implemented */
  rc=TCL_OK;
  if (rc == -1) {
    Tcl_AppendResult(interp, "Could not set rate", TCL_STATIC);
    return TCL_ERROR;
  }
  return TCL_OK;
}
      

int Say(ClientData flHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
  /* tbd */
}


int Stop(ClientData flHandle, Tcl_Interp *interp, int objc,
         Tcl_Obj *CONST objv[]) {
  if (dsp != -1) {
    close (dsp);
    dsp = -1;
  }

  return TCL_OK;}
}







int openDSP() {
int tmp, _dsp;
_dsp = open(DSP, O_WRONLY);
if (_dsp == -1) {
return -1;
}
dsp = _dsp;
ioctl(dsp, SNDCTL_DSP_RESET, 0);
tmp=11025;
ioctl(dsp, SNDCTL_DSP_SPEED,&tmp);
tmp=1;
ioctl(dsp, SNDCTL_DSP_STEREO, &tmp);
tmp=16;
ioctl(dsp, SNDCTL_DSP_SAMPLESIZE, &tmp);
tmp=11025;
ioctl(dsp, SNDCTL_DSP_GETBLKSIZE, &tmp);
return dsp;
}

int closeDSP(ClientData flHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
close(dsp);
dsp = -1;
return TCL_OK;
}



int getTTSVersion(ClientData flHandle, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]) {
char* version= (char*)malloc(16);
if (objc!=1) {
Tcl_AppendResult(interp, "Usage: ttsVersion   ", TCL_STATIC);
return TCL_ERROR;
}
/* tbd*/
Tcl_SetResult( interp,  version, TCL_STATIC);
return TCL_OK;
}

//<end of file 
//local variables:
//folded-file: t
//end:
//>
