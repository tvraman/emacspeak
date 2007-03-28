//<Changelog:
// * Language switching, 
// March 2007: 
// Initial public release, Gilles Casse <gcasse@oralux.org>
//
//>
//<includes
#include <stdlib.h>
#include <string.h>
#include "langswitch.h"
//>
//<decls and function prototypes

struct langInfo {
  enum ECILanguageDialect lang;
  const char* code;
  const char* encoding;
  const char* id;
  const char* label;
};

enum {LANGUAGE_MAX_LABEL=30}; // max size of the label field


static struct langInfo TheLanguages[]={
  {NODEFINEDCODESET,NULL,NULL,NULL,NULL}, 
  {eciGeneralAmericanEnglish,"en_US","iso8859-1","1.0","American"}, 
  {eciBritishEnglish,"en_GB","iso8859-1","1.1","British"}, 
  {eciCastilianSpanish,"es_ES","iso8859-1","2.0","Español"}, 
  {eciMexicanSpanish,"es_MX","iso8859-1","2.1","Mexicano"}, 
  {eciStandardFrench,"fr_FR","iso8859-1","3.0","Français"}, 
  {eciCanadianFrench,"fr_CA","iso8859-1","3.1","Français Canadien"}, 
  {eciStandardGerman,"de_DE","iso8859-1","4.0","Deutsch"}, 
  {eciStandardItalian,"it_IT","iso8859-1","5.0","Italiano"}, 
  {eciMandarinChineseGB,"zh_CN","gb2312","6.0","Chinese Simplified"}, 
  {eciMandarinChinesePinYin,"zh_CN","gb2312","6.0.1","Chinese Simplified"}, 
  {eciMandarinChineseUCS,"zh_CN","UCS2","6.0.8","Chinese Simplified"}, 
  {eciTaiwaneseMandarinBig5,"zh_HK","big5","6.1","Chinese Traditional"}, 
  {eciTaiwaneseMandarinZhuYin,"zh_HK","big5","6.1.1","Chinese Traditional"}, 
  {eciTaiwaneseMandarinPinYin,"zh_HK","big5","6.1.2","Chinese Traditional"}, 
  {eciTaiwaneseMandarinUCS,"zh_HK","UCS2","6.1.8","Chinese Traditional"}, 
  {eciBrazilianPortuguese,"pt_BR","iso8859-1","7.0","Brazilian Portuguese"}, 
  {eciStandardJapaneseSJIS,"ja_JP","shiftjis","8.0","Japanese"}, 
  {eciStandardJapaneseUCS,"ja_JP","UCS2","8.0.8","Japanese"}, 
  {eciStandardFinnish,"fi_FI","iso8859-1","9.0","Finnish"}, 
  {eciStandardKoreanUHC,NULL,NULL,NULL}, 
  {eciStandardKoreanUCS,NULL,NULL,NULL,NULL}, 
  {eciStandardCantoneseGB,NULL,NULL,NULL,NULL}, 
  {eciStandardCantoneseUCS,NULL,NULL,NULL,NULL}, 
  {eciHongKongCantoneseBig5,NULL,NULL,NULL,NULL}, 
  {eciHongKongCantoneseUCS,NULL,NULL,NULL,NULL}, 
  {eciStandardDutch,NULL,NULL,NULL,NULL}, 
  {eciStandardNorwegian,NULL,NULL,NULL,NULL}, 
  {eciStandardSwedish,NULL,NULL,NULL,NULL}, 
  {eciStandardDanish,NULL,NULL,NULL,NULL}, 
  {eciStandardReserved,NULL,NULL,NULL,NULL}, 
  {eciStandardThai,NULL,NULL,NULL,NULL}, 
  {eciStandardThaiTIS,NULL,NULL,NULL,NULL}, 
};

//>
//<initLanguage

enum ECILanguageDialect 
initLanguage (Tcl_Interp * interp, enum ECILanguageDialect* aLanguages, int nLanguages)
{
  // List the available languages
  int i = 0;
  int j = 0;
  enum ECILanguageDialect aCurrentLanguage, aEnglishLanguage, aFirstLanguage;
  aCurrentLanguage = aEnglishLanguage = aFirstLanguage = NODEFINEDCODESET;
  char* aDefaultLang = (char*)getenv("LANGUAGE");

  if (aDefaultLang == NULL)
    {
      aDefaultLang = (char*)getenv("LANG");
      if (aDefaultLang == NULL)
	{
	  aDefaultLang = "en";
	}
    }
  if (strlen(aDefaultLang) < 2)
  {
    aDefaultLang = "en";
  }

  Tcl_SetVar2(interp, "langsynth", "current", "0", 0);

  for (i = 0; i < LANG_INFO_MAX; i++)
    {
      char buffer_i[3];
      snprintf(buffer_i, 3, "%d", i); 
      Tcl_SetVar2(interp, "langalias", (char*)(TheLanguages[i].code), buffer_i, 0);
    }

  int aCurrentLangIndex=0;
  int aEnglishLangIndex=0;
  int aFirstLangIndex=0;

  for (i = 0; i < nLanguages; i++)
    {
      int aLang=0;
      char buffer_i[3];
      char buffer_j[3];
	    
      for (aLang = 0; aLang < LANG_INFO_MAX; aLang++)
	{
	  if (TheLanguages[aLang].lang == aLanguages[i])
	    break;
	}

      if ((aLang == LANG_INFO_MAX) || (TheLanguages[aLang].code == NULL))
	{
	  continue;
	}

      snprintf(buffer_i, 3, "%d", aLang); 
      snprintf(buffer_j, 3, "%d", j++); 
      Tcl_SetVar2(interp, "langsynth", buffer_j, buffer_i, 0);

      if (aCurrentLanguage == NODEFINEDCODESET)
	{
	  if (strncmp(aDefaultLang, TheLanguages[aLang].code, 2) == 0)
	    {
	      aCurrentLanguage = TheLanguages[aLang].lang;
	      aCurrentLangIndex = aLang; 
	    }
	  else if (strncmp("en", TheLanguages[aLang].code, 2) == 0)
	    {
	      aEnglishLanguage = TheLanguages[aLang].lang;
	      aEnglishLangIndex = aLang; 
	    }
	  else if (j == 1)
	    {
	      aFirstLanguage = TheLanguages[aLang].lang;
	      aFirstLangIndex = aLang; 
	    }
	}
      Tcl_SetVar2(interp, "langlabel", buffer_j, (char*)(TheLanguages[aLang].label), 0);
      Tcl_SetVar2(interp, "langsynth", "top", buffer_j, 0);
    }

  if (aCurrentLanguage == NODEFINEDCODESET)
    {
      if (aEnglishLanguage != NODEFINEDCODESET)
      {
	aCurrentLangIndex = aEnglishLangIndex;
	aCurrentLanguage = aEnglishLanguage;
      }
      else if (aFirstLanguage != NODEFINEDCODESET)
	{
	  aCurrentLangIndex = aFirstLangIndex;
	  aCurrentLanguage = aFirstLanguage;
	}
    }

  if (aCurrentLanguage != NODEFINEDCODESET)
    {
      char buffer_i[3];
      snprintf(buffer_i, 3, "%d", aCurrentLangIndex);
      Tcl_SetVar2(interp, "langsynth", "current", buffer_i, 0);
    }

  return aCurrentLanguage;
}
//>
//<getAnnotation

const char* getAnnotation (Tcl_Interp *interp, int* theIndex) 
{
  const char* code = NULL;
  char* aInfo = Tcl_GetVar2(interp, "langsynth", "current", 0);

  if (aInfo)
    {
      *theIndex = atoi (aInfo);
      if ((*theIndex > 0) && (*theIndex < LANG_INFO_MAX))
	{
	  code = TheLanguages[*theIndex].id;
	}
    }
  return code;
}

//>
//<convertFromUTF8

char* 
convertFromUTF8 (Tcl_Interp * interp, const char* src)
{
  char* dest = NULL;

  if (interp && src)
    {
      int srcLen = -1;
      int aLength = 2*strlen(src) + 1;
      dest = new char[ aLength ];
      int srcReadPtr = 0;
      int destWrotePtr = 0;
      int destCharsPtr = 0;
      int aIndex = 0;
      
      const char* aEncoding = "iso8859-1";
      if (getAnnotation(interp, &aIndex))
	{
	  aEncoding = TheLanguages[aIndex].encoding;
	}
  
      // Characters are converted from utf-8 to the expected encoding
      Tcl_Encoding encoding = Tcl_GetEncoding(interp, aEncoding);
      Tcl_UtfToExternal (interp, encoding, 
			 src, srcLen, 
			 0, NULL, 
			 dest, aLength,
			 &srcReadPtr, &destWrotePtr, &destCharsPtr);      
    }
  return dest;
}


//>

//<end of file
//local variables:
//folded-file: t
//end:
//>
