;;; emacspeak-tdtd.el --- Speech enable  DTD authoring 
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable tdtd 
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)


;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; tdtd is an emacs package for authoring and maintaining
;;; XML and SGML DTDs
;;; tdtd is at http://www.mulberrytech.com/tdtd/index.html
;;; this module speech-enables tdtd

;;; Code:

;;}}}
;;{{{  speech-enable interactive commands

(defadvice dtd-mode (after emacspeak pre act comp)
  "set up for voice locking."
(emacspeak-tdtd-voice-lock-setup)
(voice-lock-mode 1)
(dtk-set-punctuations "all"))

(defun emacspeak-tdtd-voice-lock-setup()
  "Setup voice locking for tdtd mode."
  (declare (special voice-lock-defaults
            dtd-xml-flag 
        dtd-decl-flag dtd-sys-decl-flag ))
    (make-local-variable 'voice-lock-defaults)
    (cond
     (dtd-xml-flag
      (setq voice-lock-defaults '(dtd-xml-voice-lock-keywords t)))
     (dtd-decl-flag
      (setq voice-lock-defaults '(dtd-decl-voice-lock-keywords t)))
     (dtd-sys-decl-flag
      (setq voice-lock-defaults '(dtd-sys-decl-voice-lock-keywords t)))
     (t
      (setq voice-lock-defaults '(dtd-sgml-voice-lock-keywords t)))))

;;}}}
;;{{{ voice locking 

(defvar dtd-sgml-voice-lock-keywords
  '(("<!--[^-]*\\(-[^-]+\\)*-->"
     . voice-lock-comment-personality)
    ("--[^-]*\\(-[^-]+\\)*--"
     . voice-lock-comment-personality)
    ("<!--[^-]*\\(-[^-]+\\)*-->"
     . voice-lock-comment-personality)
    ("[%&][^; \t]+;" . voice-lock-reference-personality)
    ("\\(<!\\(ATTLIST\\|ENTITY\\(\\s-+%\\|\\)\\|NOTATION\\)\\)\\s-+\\(\\S-+\\)[^>]+\\(>\\)" (1 voice-lock-keyword-personality) (4 voice-lock-variable-name-personality) (5 voice-lock-keyword-personality))
    ("\\(<!DOCTYPE\\)\\s-+\\([^[]+\\)\\s-+\\(\\[\\)" (1
                                                      voice-lock-keyword-personality) (2 voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(<!ELEMENT\\)\\s-+\\([^ \t()|]+\\)\\s-+[^>]+\\(>\\)"
     (1 voice-lock-keyword-personality) (2
                                         voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(<!\\[\\)[^[]*\\(\\[\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\(<!\\(SHORTREF\\|USEMAP\\)\\)\\s-+\\(\\S-+\\)[^>]+\\(>\\)" (1 voice-lock-keyword-personality) (3 voice-lock-variable-name-personality) (4 voice-lock-keyword-personality))
    ("\\(<!ELEMENT\\)\\s-+\\([^
\t()|]+\\)\\s-+\\(o\\s-+o\\|O\\s-+O\\|-\\s-+[oO]\\|[oO]\\s-+-\\)\\s-+[^>]+\\(>\\)"
     (1 voice-lock-keyword-personality) (2
                                         voice-lock-variable-name-personality) (3 voice-lock-keyword-personality) (4 voice-lock-keyword-personality))
    ("\\(\\(-\\|+\\)(\\)[^)]*\\()\\)" (1
                                       voice-lock-variable-name-personality) (3 voice-lock-variable-name-personality))
    ("\\(<!LINKTYPE\\)\\s-+\\([^ \t]+\\)\\s-+\\([^
\t]+\\)\\s-+\\(\\#IMPLIED\\)[ \t\n]*\\(\\[\\)" (1
                                                voice-lock-keyword-personality) (2 voice-lock-variable-name-personality) (3 voice-lock-variable-name-personality) (4 voice-lock-keyword-personality) (5 voice-lock-keyword-personality))
    ("\\(<!LINKTYPE\\)\\s-+\\([^ \t]+\\)\\s-+\\([^
\t]+\\)\\s-+\\([^ \t]+\\)\\s-*\\(\\[\\)" (1
                                          voice-lock-keyword-personality) (2 voice-lock-variable-name-personality) (3 voice-lock-variable-name-personality) (4 voice-lock-type-personality) (5 voice-lock-keyword-personality))
    ("\\(<!IDLINK\\)\\b[^>]+\\(>\\)" (1
                                      voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\(<\\?\\)\\([^ \t>]+\\)\\s-*[^>]*\\(>\\)" (1
                                                  voice-lock-keyword-personality) (2 voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(#\\(ALL\\|C\\(ONREF\\|URRENT\\)\\|DEFAULT\\|FIXED\\|I\\(MPLI\\(CIT\\|ED\\)\\|NITIAL\\)\\|NOTATION\\|P\\(CDATA\\|OSTLINK\\)\\|RE\\(QUIRED\\|STORE\\)\\|SIMPLE\\|USELINK\\)\\|+//\\(\\|IDN\\)\\|-//\\|//\\)\\b" (1 voice-lock-type-personality))
    ("\\b\\(A\\(NY\\|TTLIST\\)\\|CDATA\\|D\\(ATA\\|OCTYPE\\|TD\\)\\|E\\(LEMENT\\|MPTY\\|NTIT\\(IES\\|Y\\)\\|XPLICIT\\)\\|I\\(D\\(\\|LINK\\|REFS?\\)\\|MPLICIT\\)\\|LINK\\(\\|TYPE\\)\\|N\\(AME\\(\\|CHAR\\|S\\)\\|DATA\\|MTOKENS?\\|OTATION\\|U\\(MBERS?\\|TOKENS?\\)\\)\\|P\\(OSTLINK\\|UBLIC\\)\\|RCDATA\\|S\\(DATA\\|HORTREF\\|IMPLE\\|UBDOC\\|YSTEM\\)\\|TEMP\\|USE\\(LINK\\|MAP\\)\\)\\b" (1 voice-lock-type-personality))
    ("\\(\\]?\\]\\)\\s-*\\(>\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\('\\)[^']*\\('\\)" (1 voice-lock-string-personality)
     (2 voice-lock-string-personality))
    ("\\(\"\\)[^\"]*\\(\"\\)" (1
                               voice-lock-string-personality) (2 voice-lock-string-personality))
    ("[,()|&]" . voice-lock-function-name-personality)
    ("[+*?]" . voice-lock-string-personality)
    ))

(defvar dtd-xml-voice-lock-keywords
  '(("<!--[^-]*\\(-[^-]+\\)*-->"
     . voice-lock-comment-personality)
    ("[%&][^; \t]+;" . voice-lock-reference-personality)
    ("\\(<!\\(ATTLIST\\|ENTITY\\(\\s-+%\\|\\)\\|NOTATION\\)\\)\\s-+\\(\\S-+\\)[^>]+\\(>\\)" (1 voice-lock-keyword-personality) (4 voice-lock-variable-name-personality) (5 voice-lock-keyword-personality))
    ("\\(<!DOCTYPE\\)\\s-+\\([^[]+\\)\\s-+\\(\\[\\)" (1
                                                      voice-lock-keyword-personality) (2 voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(<!ELEMENT\\)\\s-+\\([^ \t()|]+\\)\\s-+[^>]+\\(>\\)"
     (1 voice-lock-keyword-personality) (2
                                         voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(<!\\[\\)[^[]*\\(\\[\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\(<\\?\\)\\(xml\\)\\(\\s-+version\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+encoding\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+standalone\\s-*=\\s-*\\('\\(yes\\|no\\)'\\|\"\\(yes\\|no\\)\"\\)\\)?\\s-*\\(\\?>\\)" (1 voice-lock-keyword-personality) (2 voice-lock-type-personality nil) (3 voice-lock-type-personality nil t) (5 voice-lock-type-personality nil t) (7 voice-lock-type-personality nil t) (11 voice-lock-keyword-personality))
    ("\\(<\\?\\)\\([^
\t?>]+\\)\\s-*\\([^?>]\\|\\?[^>]\\|>[^\n\r]\\)*\\(\\?>\\)"
     (1 voice-lock-keyword-personality) (2
                                         voice-lock-variable-name-personality) (4 voice-lock-keyword-personality))
    ("\\(#\\(DEFAULT\\|FIXED\\|IMPLIED\\|NOTATION\\|PCDATA\\|REQUIRED\\)\\|+//\\(\\|IDN\\)\\|-//\\|//\\)\\b" (1 voice-lock-type-personality))
    ("\\b\\(xml:\\(l\\(ang\\|ink\\)\\|space\\)\\)\\b" (1
                                                       voice-lock-type-personality))
    ("\\b\\(A\\(NY\\|TTLIST\\)\\|C\\(APACITY\\|DATA\\|HARSET\\)\\|D\\(OC\\(TYPE\\|UMENT\\)\\|TD\\)\\|E\\(LEMENTS?\\|MPTY\\|NTIT\\(IES\\|Y\\)\\)\\|ID\\(\\|REFS?\\)\\|LPD\\|N\\(DATA\\|MTOKENS?\\|O\\(NSGML\\|TATION\\)\\|UTOKENS?\\)\\|PUBLIC\\|S\\(HORTREF\\|UBDOC\\|Y\\(NTAX\\|STEM\\)\\)\\|TEXT\\|XML\\|xml\\)\\b" (1 voice-lock-type-personality))
    ("\\(\\]?\\]\\)\\s-*\\(>\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\('\\)[^']*\\('\\)" (1 voice-lock-string-personality)
     (2 voice-lock-string-personality))
    ("\\(\"\\)[^\"]*\\(\"\\)" (1
                               voice-lock-string-personality) (2 voice-lock-string-personality))
    ("[,()|&]" . voice-lock-function-name-personality)
    ("[+*?]" . voice-lock-string-personality)
    ))

(defvar dtd-decl-voice-lock-keywords
  '(("--[^-]*\\(-[^-]+\\)*--"
     . voice-lock-comment-personality)
    ("<!--[^-]*\\(-[^-]+\\)*-->"
     . voice-lock-comment-personality)
    ("[%&][^; \t]+;" . voice-lock-reference-personality)
    ("\\(<!\\(ATTLIST\\|ENTITY\\(\\s-+%\\|\\)\\|NOTATION\\)\\)\\s-+\\(\\S-+\\)[^>]+\\(>\\)" (1 voice-lock-keyword-personality) (4 voice-lock-variable-name-personality) (5 voice-lock-keyword-personality))
    ("\\(<!DOCTYPE\\)\\s-+\\([^[]+\\)\\s-+\\(\\[\\)" (1
                                                      voice-lock-keyword-personality) (2 voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(<!ELEMENT\\)\\s-+\\([^ \t()|]+\\)\\s-+[^>]+\\(>\\)"
     (1 voice-lock-keyword-personality) (2
                                         voice-lock-variable-name-personality) (3 voice-lock-keyword-personality))
    ("\\(<!\\[\\)[^[]*\\(\\[\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\(<!SGML\\)\\s-+\\([^>]\\|>[^$]\\)+\\(>$\\)" (1
                                                     voice-lock-keyword-personality) (3 voice-lock-keyword-personality))
    ("\\b\\(A\\(LL\\|N[DY]\\|PPINFO\\|TT\\(C\\(AP\\|HCAP\\|NT\\)\\|LIST\\|RIB\\|SPLEN\\)\\|VGRPCAP\\)\\|B\\(ASESET\\|B\\|SEQLEN\\)\\|C\\(APACITY\\|DATA\\|HARSET\\|O\\(M\\|N\\(CUR\\|REF\\|TROLS\\)\\)\\|RO\\|URRENT\\)\\|D\\(ATATAG\\|E\\(FAULT\\|LIM\\|SCSET\\)\\|OC\\(TYPE\\|UMENT\\)\\|S[CO]\\|T\\(AGLEN\\|D\\|EMPLEN\\|G[CO]\\)\\)\\|E\\(LEM\\(CAP\\|ENTS?\\)\\|MPTY\\(\\|NRM\\)\\|N\\(DTAG\\|T\\(C\\(AP\\|HCAP\\)\\|IT\\(IES\\|Y\\)\\|LVL\\)\\)\\|RO\\|TAGO\\|X\\(GRPCAP\\|NMCAP\\|PLICIT\\)\\)\\|F\\(EATURES\\|IXED\\|ORMAL\\|UNC\\(HAR\\|TION\\)\\)\\|G\\(ENERAL\\|RP\\([CO]\\|C\\(AP\\|NT\\)\\|GTCNT\\|LVL\\)\\)\\|HCRO\\|I\\(D\\(\\|CAP\\|LINK\\|REF\\(\\|CAP\\|S\\)\\)\\|GNORE\\|M\\(MEDNET\\|PL\\(I\\(CIT\\|ED\\)\\|YDEF\\)\\)\\|N\\(CLUDE\\|ITIAL\\|STANCE\\|TE\\(GRAL\\|RNAL\\)\\)\\)\\|KEEPRSRE\\|L\\(CNM\\(CHAR\\|STRT\\)\\|I\\(NK\\(\\|TYPE\\)\\|T\\(\\|A\\|LEN\\)\\)\\|K\\(NMCAP\\|SETCAP\\)\\|PD\\)\\|M\\([DS]\\|APCAP\\|D[CO]\\|IN\\(IMIZE\\|US\\)\\|S\\(C\\|ICHAR\\|OCHAR\\|SCHAR\\)\\)\\|N\\(AM\\(E\\(\\|C\\(ASE\\|HAR\\)\\|LEN\\|S\\(\\|TRT\\)\\)\\|ING\\)\\|DATA\\|E\\(STC\\|T\\(\\|ENABL\\)\\)\\|MTOKENS?\\|O\\(\\|ASSERT\\|N\\(E\\|SGML\\)\\|RMSEP\\|T\\(ATION\\|C\\(AP\\|HCAP\\)\\)\\)\\|U\\(MBERS?\\|TOKENS?\\)\\)\\|O\\(\\|MIT\\(NAME\\|TAG\\)\\|PT\\|R\\|THER\\)\\|P\\(CDATA\\|ERO\\|I\\(\\|[CO]\\|LEN\\)\\|LUS\\|OSTLINK\\|UBLIC\\)\\|QUANTITY\\|R\\([ES]\\|ANK\\|CDATA\\|E\\([FP]\\|FC\\|QUIRED\\|STORE\\)\\|NI\\)\\|S\\(COPE\\|DATA\\|E\\(EALSO\\|PCHAR\\|Q\\)\\|GMLREF\\|H\\(ORT\\(REF\\|TAG\\)\\|UNCHAR\\)\\|IMPLE\\|PACE\\|TA\\(GO\\|RTTAG\\)\\|UBDOC\\|WITCHES\\|Y\\(NTAX\\|STEM\\)\\)\\|T\\(A\\(B\\|G\\(C\\|L\\(EN\\|VL\\)\\)\\)\\|E\\(MP\\|XT\\)\\|OTALCAP\\|YPE\\)\\|U\\(CNM\\(CHAR\\|STRT\\)\\|N\\(CLOSED\\|USED\\)\\|RN\\|SE\\(LINK\\|MAP\\)\\)\\|V\\(AL\\(IDITY\\|UE\\)\\|I\\)\\|YES\\)\\b" (1 voice-lock-type-personality))
    ("\\(ISO
8879\\(-1986\\|:1986\\(\\|(\\(ENR)\\|WWW)\\)\\)\\)\\)" (1
                                                        voice-lock-type-personality))
    ("\\(-//\\|//\\)\\b" (1 voice-lock-type-personality))
    ("\\(\\]?\\]\\)\\s-*\\(>\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\('\\)[^']*\\('\\)" (1 voice-lock-string-personality)
     (2 voice-lock-string-personality))
    ("\\(\"\\)[^\"]*\\(\"\\)" (1
                               voice-lock-string-personality) (2 voice-lock-string-personality))
    ("[,()|&]" . voice-lock-function-name-personality)
    ("[+*?]" . voice-lock-string-personality)
    ))

(defvar dtd-sys-decl-voice-lock-keywords
  '(("--[^-]*\\(-[^-]+\\)*--"
     . voice-lock-comment-personality)
    ("[%&][^; \t]+;" . voice-lock-reference-personality)
    ("\\(<!SYSTEM\\)\\([^>]\\|>[^$]\\)+\\(>$\\)" (1
                                                  voice-lock-keyword-personality) (3 voice-lock-keyword-personality))
    ("\\(+//\\(\\|IDN\\)\\|-//\\|//\\)\\b" (1
                                            voice-lock-type-personality))
    ("\\b\\(A\\(LL\\|N[DY]\\|SN1\\|TT\\(C\\(AP\\|HCAP\\|NT\\)\\|LIST\\|RIB\\|SPLEN\\)\\|VGRPCAP\\)\\|B\\(ASESET\\|B\\|SEQLEN\\)\\|C\\(APACITY\\|DATA\\|HA\\(NGES\\|RSET\\)\\|O\\(M\\|N\\(CUR\\|REF\\|TROLS\\)\\)\\|RO\\|URRENT\\)\\|D\\(ATATAG\\|E\\(FAULT\\|LIM\\(\\|LEN\\)\\|SCSET\\)\\|OC\\(TYPE\\|UMENT\\)\\|S[CO]\\|T\\(AGLEN\\|D\\|EMPLEN\\|G[CO]\\)\\)\\|E\\(LEM\\(CAP\\|ENTS?\\)\\|MPTY\\(\\|NRM\\)\\|N\\(DTAG\\|T\\(C\\(AP\\|HCAP\\)\\|IT\\(IES\\|Y\\)\\|LVL\\)\\)\\|RO\\|TAGO\\|X\\(CLUDE\\|GRPCAP\\|NMCAP\\|PLICIT\\)\\)\\|F\\(EATURES\\|IXED\\|ORMAL\\|UNC\\(HAR\\|TION\\)\\)\\|G\\(ENERAL\\|RP\\([CO]\\|C\\(AP\\|NT\\)\\|GTCNT\\|LVL\\)\\)\\|HCRO\\|I\\(D\\(\\|CAP\\|LINK\\|REF\\(\\|CAP\\|S\\)\\)\\|GNORE\\|M\\(MEDNET\\|PL\\(I\\(CIT\\|ED\\)\\|YDEF\\)\\)\\|N\\(CLUDE\\|ITIAL\\|STANCE\\|TE\\(GRAL\\|RNAL\\)\\)\\)\\|KEEPRSRE\\|L\\(CNM\\(CHAR\\|STRT\\)\\|I\\(NK\\(\\|TYPE\\)\\|T\\(\\|A\\|LEN\\)\\)\\|K\\(NMCAP\\|SETCAP\\)\\|PD\\)\\|M\\([DS]\\|APCAP\\|D[CO]\\|IN\\(IMIZE\\|US\\)\\|ODEL\\|S\\(C\\|ICHAR\\|OCHAR\\|SCHAR\\)\\)\\|N\\(AM\\(E\\(\\|C\\(ASE\\|HAR\\)\\|LEN\\|S\\(\\|TRT\\)\\)\\|ING\\)\\|DATA\\|E\\(STC\\|T\\(\\|ENABL\\)\\)\\|MTOKENS?\\|O\\(\\|ASSERT\\|N\\(E\\|SGML\\)\\|RMSEP\\|T\\(ATION\\|C\\(AP\\|HCAP\\)\\)\\)\\|U\\(MBERS?\\|TOKENS?\\)\\)\\|O\\(\\|MIT\\(NAME\\|TAG\\)\\|PT\\|R\\|THER\\)\\|P\\(ACK\\|CDATA\\|ERO\\|I\\(\\|[CO]\\|LEN\\)\\|LUS\\|OSTLINK\\|UBLIC\\)\\|QUANTITY\\|R\\([ES]\\|ANK\\|CDATA\\|E\\([FP]\\|FC\\|QUIRED\\|STORE\\)\\|NI\\)\\|S\\(COPE\\|D\\(ATA\\|IF\\)\\|E\\(EALSO\\|PCHAR\\|Q\\(\\|UENCE\\)\\)\\|GML\\(\\|REF\\)\\|H\\(ORT\\(REF\\|TAG\\)\\|UNCHAR\\)\\|IMPLE\\|PACE\\|R\\(CNT\\|LEN\\)\\|TA\\(GO\\|RTTAG\\)\\|UBDOC\\|WITC\\(ES\\|HES\\)\\|Y\\(NTAX\\|STEM\\)\\)\\|T\\(A\\(B\\|G\\(C\\|L\\(EN\\|VL\\)\\)\\)\\|E\\(MP\\|XT\\)\\|OTALCAP\\|YPE\\)\\|U\\(CNM\\(CHAR\\|STRT\\)\\|N\\(CLOSED\\|PACK\\|USED\\)\\|RN\\|SE\\(LINK\\|MAP\\)\\)\\|V\\(AL\\(ID\\(ATE\\|ITY\\)\\|UE\\)\\|I\\)\\|YES\\)\\b" (1 voice-lock-type-personality))
    ("\\(ISO 8879:1986\\(\\|(\\(ENR)\\|WWW)\\)\\)\\)" (1
                                                       voice-lock-type-personality))
    ("\\(\\]?\\]\\)\\s-*\\(>\\)" (1
                                  voice-lock-keyword-personality) (2 voice-lock-keyword-personality))
    ("\\('\\)[^']*\\('\\)" (1 voice-lock-string-personality)
     (2 voice-lock-string-personality))
    ("\\(\"\\)[^\"]*\\(\"\\)" (1
                               voice-lock-string-personality) (2 voice-lock-string-personality))
    ("[,()|&]" . voice-lock-function-name-personality)
    ("[+*?]" . voice-lock-string-personality)
    ))

;;}}}
(provide 'emacspeak-tdtd)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
