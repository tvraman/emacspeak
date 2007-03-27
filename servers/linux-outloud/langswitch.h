#ifndef LANGSWITCH_H
#define LANGSWITCH_H

#include <tcl.h>


/* The declarations in langswitch (.h, .cpp) are derived from the publically 
   available eci.h (APIs for IBM Text To Speech) at http://ibmtts-sdk.sf.net

   Its license is copied below

 * Copyright (c) 2005, 2006, IBM Corp. All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.  *
 *     Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *     * Neither the name of IBM Corp. nor the names of its
 *     contributors may be used to endorse or promote products derived from
 *     this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


enum ECILanguageDialect {
    NODEFINEDCODESET                = 0x00000000,
    eciGeneralAmericanEnglish       = 0x00010000,
    eciBritishEnglish               = 0x00010001,
    eciCastilianSpanish             = 0x00020000,
    eciMexicanSpanish               = 0x00020001,
    eciStandardFrench               = 0x00030000,
    eciCanadianFrench               = 0x00030001,
    eciStandardGerman               = 0x00040000,
    eciStandardItalian              = 0x00050000,
    eciMandarinChinese              = 0x00060000,
    eciMandarinChineseGB            = eciMandarinChinese,
    eciMandarinChinesePinYin        = 0x00060100,
    eciMandarinChineseUCS           = 0x00060800,
    eciTaiwaneseMandarin            = 0x00060001,
    eciTaiwaneseMandarinBig5        = eciTaiwaneseMandarin,
    eciTaiwaneseMandarinZhuYin      = 0x00060101,
    eciTaiwaneseMandarinPinYin      = 0x00060201,
    eciTaiwaneseMandarinUCS         = 0x00060801,
    eciBrazilianPortuguese          = 0x00070000,
    eciStandardJapanese             = 0x00080000,
    eciStandardJapaneseSJIS         = eciStandardJapanese,
    eciStandardJapaneseUCS          = 0x00080800,
    eciStandardFinnish              = 0x00090000,
    eciStandardKorean               = 0x000A0000,
    eciStandardKoreanUHC            = eciStandardKorean,
    eciStandardKoreanUCS            = 0x000A0800,
    eciStandardCantonese            = 0x000B0000,
    eciStandardCantoneseGB          = eciStandardCantonese,
    eciStandardCantoneseUCS         = 0x000B0800,
    eciHongKongCantonese            = 0x000B0001,
    eciHongKongCantoneseBig5        = eciHongKongCantonese,
    eciHongKongCantoneseUCS         = 0x000B0801,
    eciStandardDutch                = 0x000C0000,
    eciStandardNorwegian            = 0x000D0000,
    eciStandardSwedish              = 0x000E0000,
    eciStandardDanish               = 0x000F0000,
    eciStandardReserved             = 0x00100000,
    eciStandardThai                 = 0x00110000,
    eciStandardThaiTIS              = eciStandardThai

};


// 
enum {ANNOTATION_MAX_SIZE=10, LANG_INFO_MAX=22};
int SetLanguage (ClientData, Tcl_Interp *, int, Tcl_Obj * CONST[]);
enum ECILanguageDialect initLanguage (Tcl_Interp * interp, enum ECILanguageDialect* aLanguages, int nLanguages);
const char* getAnnotation (Tcl_Interp *interp, int* theIndex);
char* convertFromUTF8 (Tcl_Interp * interp, const char* theString);


#endif

