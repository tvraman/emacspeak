# $Id$
# $Author$ 
# Description:  Makefile for Emacspeak 
# Keywords: Emacspeak, Dectalk, Makefile 
# {{{ LCD Entry: 

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu 
# A speech interface to Emacs |
# $Date$ |
#  $Revision$ | 
# Location undetermined
#

# }}}
# {{{ Copyright:  

#Copyright (C) 1995 -- 2000, T. V. Raman 

# Copyright (c) 1994, 1995 by Digital Equipment Corporation.
# All Rights Reserved. 
#
# This file is not part of GNU Emacs, but the same permissions apply.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# }}}
# {{{make rules 

# what emacs is called on your system
EMACS = emacs
SRC=".."
# How to run in batch mode
BATCH = -batch -q -no-site-file
# How to compile 
COMPILE = -eval "(setq byte-compile-dynamic t)" -f batch-byte-compile
%.elc:  %.el
	$(EMACS) $(BATCH)  -l $(SRC)/lisp/emacspeak-load-path.el $(COMPILE) $<

# }}}
# {{{  grouping files into dependent modules

#The various modules:
DTK_OBJECTS = dtk-css-speech.elc	dtk-speak.elc		dtk-voices.elc\
outloud-voices.elc outloud-css-speech.elc \
dtk-interp.elc		dtk-tcl.elc

SPEAK_OBJECTS =   \
	emacspeak-speak.elc  \
        emacspeak-actions.elc \
	voice-lock.elc \
	lazy-voice-lock.elc \
	jit-voice-lock.elc \
	fast-voice-lock.elc \
	voice-setup.elc \
	voice-settings.elc \
        html-voice.elc \
	html-outline.elc \
        emacspeak-pronounce.elc \
	emacspeak-sounds.elc 

CORE_OBJECTS =  \
	emacspeak.elc \
	emacspeak-advice.elc\
	emacspeak-keymap.elc\
	emacspeak-fix-interactive.elc\
emacspeak-freeamp.elc \
	emacspeak-redefine.elc\
	emacspeak-replace.elc \
        emacspeak-table.elc \
        emacspeak-table-ui.elc \
	emacspeak-tabulate.elc 

AUX_OBJECTS =  cd-tool.elc \
emacspeak-ansi-color.elc \
emacspeak-arc.elc \
emacspeak-auctex.elc \
emacspeak-aumix.elc \
emacspeak-babel.elc \
emacspeak-bbdb.elc \
emacspeak-bibtex.elc \
emacspeak-bookmark.elc \
emacspeak-browse-kill-ring.elc \
emacspeak-bs.elc \
emacspeak-buff-menu.elc \
emacspeak-buff-sel.elc \
emacspeak-c.elc \
emacspeak-calc.elc \
emacspeak-calculator.elc \
emacspeak-calendar.elc \
emacspeak-checkdoc.elc \
emacspeak-cmuscheme.elc \
emacspeak-compile.elc \
emacspeak-cperl.elc \
emacspeak-custom.elc \
emacspeak-dictation.elc \
emacspeak-dictionary.elc \
emacspeak-dired.elc \
emacspeak-dismal.elc \
emacspeak-dmacro.elc \
emacspeak-ecb.elc \
emacspeak-ediff.elc \
emacspeak-enriched.elc \
emacspeak-entertain.elc \
emacspeak-erc.elc \
emacspeak-eshell.elc \
emacspeak-eterm.elc \
emacspeak-eudc.elc \
emacspeak-facemenu.elc \
emacspeak-filtertext.elc \
emacspeak-find-dired.elc \
emacspeak-find-func.elc \
emacspeak-finder.elc \
emacspeak-flyspell.elc \
emacspeak-folding.elc \
emacspeak-forms.elc \
emacspeak-generic.elc \
emacspeak-gnuplot.elc \
emacspeak-gnus.elc\
emacspeak-gomoku.elc \
emacspeak-gridtext.elc \
emacspeak-gud.elc \
emacspeak-hide.elc \
emacspeak-hideshow.elc \
emacspeak-hyperbole.elc \
emacspeak-ibuffer.elc \
emacspeak-imenu.elc \
emacspeak-info.elc\
emacspeak-ispell.elc \
emacspeak-jde.elc \
emacspeak-kotl.elc \
emacspeak-make-mode.elc \
emacspeak-man.elc \
emacspeak-message.elc \
emacspeak-metapost.elc \
emacspeak-midge.elc \
emacspeak-mpg123.elc \
emacspeak-mspools.elc \
emacspeak-net-utils.elc \
emacspeak-ocr.elc \
emacspeak-oo-browser.elc \
emacspeak-outline.elc \
emacspeak-pcl-cvs.elc \
emacspeak-perl.elc \
emacspeak-psgml.elc \
emacspeak-python.elc \
emacspeak-realaudio.elc \
emacspeak-reftex.elc \
emacspeak-remote.elc \
emacspeak-rmail.elc \
emacspeak-sawfish.elc \
emacspeak-setup.el \
emacspeak-sgml-mode.elc \
emacspeak-sh-script.elc \
emacspeak-solitaire.elc \
emacspeak-speedbar.elc \
emacspeak-sql.elc \
emacspeak-supercite.elc \
emacspeak-tapestry.elc \
emacspeak-tar.elc \
emacspeak-tcl.elc \
emacspeak-tdtd.elc \
emacspeak-tempo.elc \
emacspeak-tetris.elc \
emacspeak-texinfo.elc \
emacspeak-tnt.elc \
emacspeak-url-template.elc \
emacspeak-view-process.elc \
emacspeak-view.elc \
emacspeak-vm.elc \
emacspeak-w3.elc \
emacspeak-websearch.elc \
emacspeak-widget.elc \
emacspeak-windmove.elc \
emacspeak-winring.elc \
emacspeak-wizards.elc  \
emacspeak-wrolo.elc \
emacspeak-xslide.elc  \
emacspeak-xslt-process.elc 

ADDON_OBJECTS= tapestry.elc  

OBJECTS = $(DTK_OBJECTS) $(SPEAK_OBJECTS) $(CORE_OBJECTS)  \
	$(AUX_OBJECTS) $(ADDON_OBJECTS)

# }}}
# {{{  User level targets all  

all: $(OBJECTS)

# }}}
# {{{ User level target--  config  junked
#This ugliness not needed anymore --Thanks to 
# Koichi INOUE <inoue@argv.org>
#config:
#	@rm -f emacspeak-setup.el emacspeak-load-path.el  
#	#@sed "s@<emacspeak-dir>@$(SRC)@g" \
#		emacspeak-load-path.def > emacspeak-load-path.el
#	@sed "s@<emacspeak-dir>@$(SRC)@g" \
#		emacspeak-setup.def  > emacspeak-setup.el
#	@chmod 444 emacspeak-setup.el emacspeak-load-path.el
#	@echo "Configured emacspeak in directory $(SRC). Now type make emacspeak"

# }}}
# {{{  Make rules for the various modules

# Do this in this brain-dead way because different makes do pattern
# rules differently. grumble grumble...
#dtk module:
#dtk-interp contains the interface to the interpreter
#these are inline functions that dtk-tcl.el needs.
#dtk-tcl contains inline functions.
#Compile dtk-tcl first, and make dtk-speak depend on it
#Modules using functions from the dtk module should depend on dtk-speak

dtk-css-speech.elc: dtk-css-speech.el dtk-voices.elc
dtk-interp.elc: dtk-interp.el
dtk-speak.elc: dtk-speak.el dtk-tcl.el dtk-interp.elc
dtk-tcl.elc: dtk-tcl.el dtk-interp.elc dtk-voices.elc
dtk-voices.elc: dtk-voices.el
outloud-css-speech.elc: outloud-css-speech.el outloud-voices.elc
outloud-voices.elc: outloud-voices.el
voice-lock.elc: voice-lock.el voice-settings.elc voice-setup.elc
voice-settings.elc: voice-settings.el
voice-setup.elc: voice-setup.el
#voice lock helpers 

fast-voice-lock.elc: fast-voice-lock.el voice-lock.elc
jit-voice-lock.elc: jit-voice-lock.el voice-lock.elc
lazy-voice-lock.elc: lazy-voice-lock.el voice-lock.elc

html-voice.elc: html-voice.el voice-lock.elc
html-outline.elc: html-outline.el

cd-tool.elc: cd-tool.el
emacspeak-actions.elc: emacspeak-actions.el  voice-lock.elc
emacspeak-advice.elc: emacspeak-advice.el dtk-speak.elc
emacspeak-ansi-color.elc: emacspeak-ansi-color.el dtk-speak.elc
emacspeak-arc.elc: emacspeak-arc.el dtk-speak.elc
emacspeak-auctex.elc: emacspeak-auctex.el dtk-speak.elc
emacspeak-aumix.elc: emacspeak-aumix.el dtk-speak.elc
emacspeak-babel.elc: emacspeak-babel.el dtk-speak.elc
emacspeak-bbdb.elc: emacspeak-bbdb.el dtk-speak.elc
emacspeak-bibtex.elc: emacspeak-bibtex.el dtk-speak.elc
emacspeak-bookmark.elc: emacspeak-bookmark.el dtk-speak.elc
emacspeak-browse-kill-ring.elc: emacspeak-browse-kill-ring.el dtk-speak.elc
emacspeak-bs.elc: emacspeak-bs.el dtk-speak.elc
emacspeak-buff-menu.elc: emacspeak-buff-menu.el dtk-speak.elc
emacspeak-buff-sel.elc: emacspeak-buff-sel.el dtk-speak.elc
emacspeak-c.elc: emacspeak-c.el dtk-speak.elc
emacspeak-cmuscheme.elc: emacspeak-cmuscheme.el dtk-speak.elc
emacspeak-calc.elc: emacspeak-calc.el 
emacspeak-calculator.elc: emacspeak-calculator.el  dtk-speak.elc
emacspeak-calendar.elc: emacspeak-calendar.el dtk-speak.el
emacspeak-checkdoc.elc: emacspeak-checkdoc.el dtk-speak.elc
emacspeak-cperl.elc: emacspeak-cperl.el dtk-speak.elc
emacspeak-custom.elc: emacspeak-custom.el dtk-speak.elc
emacspeak-dictation.elc: emacspeak-dictation.el dtk-speak.elc
emacspeak-dictionary.elc: emacspeak-dictionary.el dtk-speak.elc
emacspeak-dired.elc: emacspeak-dired.el dtk-speak.elc
emacspeak-dismal.elc: emacspeak-dismal.el dtk-speak.elc
emacspeak-dmacro.elc: emacspeak-dmacro.el dtk-speak.elc
emacspeak-ecb.elc: emacspeak-ecb.el dtk-speak.elc
emacspeak-ediff.elc: emacspeak-ediff.el dtk-speak.elc
emacspeak-enriched.elc: emacspeak-enriched.el dtk-speak.elc
emacspeak-entertain.elc: emacspeak-entertain.el dtk-speak.elc
emacspeak-erc.elc: emacspeak-erc.el dtk-speak.elc
emacspeak-eshell.elc: emacspeak-eshell.el dtk-speak.elc
emacspeak-eterm.elc: emacspeak-eterm.el dtk-speak.elc
emacspeak-eudc.elc: emacspeak-eudc.el dtk-speak.elc
emacspeak-facemenu.elc: emacspeak-facemenu.el dtk-speak.elc
emacspeak-filtertext.elc: emacspeak-filtertext.el dtk-speak.elc
emacspeak-find-dired.elc: emacspeak-find-dired.el dtk-speak.elc
emacspeak-find-func.elc: emacspeak-find-func.el dtk-speak.elc
emacspeak-finder.elc: emacspeak-finder.el
emacspeak-fix-interactive.elc: emacspeak-fix-interactive.el dtk-speak.elc
emacspeak-flyspell.elc: emacspeak-flyspell.el dtk-speak.elc
emacspeak-folding.elc: emacspeak-folding.el dtk-speak.elc
emacspeak-forms.elc: emacspeak-forms.el dtk-speak.elc
emacspeak-freeamp.elc: emacspeak-freeamp.el  dtk-speak.elc
emacspeak-generic.elc: emacspeak-generic.el dtk-speak.elc
emacspeak-gnuplot.elc: emacspeak-gnuplot.el dtk-speak.elc
emacspeak-gnus.elc: emacspeak-gnus.el dtk-speak.elc
emacspeak-gomoku.elc: emacspeak-gomoku.el dtk-speak.elc
emacspeak-gridtext.elc: emacspeak-gridtext.el dtk-speak.elc
emacspeak-gud.elc: emacspeak-gud.el dtk-speak.elc
emacspeak-hide.elc: emacspeak-hide.el emacspeak-speak.elc
emacspeak-hideshow.elc: emacspeak-hideshow.el dtk-speak.elc
emacspeak-hyperbole.elc: emacspeak-hyperbole.el dtk-speak.elc
emacspeak-ibuffer.elc: emacspeak-ibuffer.el dtk-speak.elc
emacspeak-imenu.elc: emacspeak-imenu.el dtk-speak.elc
emacspeak-info.elc: emacspeak-info.el dtk-speak.elc 
emacspeak-ispell.elc: emacspeak-ispell.el dtk-speak.elc
emacspeak-jde.elc: emacspeak-jde.el dtk-speak.elc
emacspeak-keymap.elc: emacspeak-keymap.el 
emacspeak-kotl.elc: emacspeak-kotl.el dtk-speak.elc
emacspeak-load-path.elc: emacspeak-load-path.elc
emacspeak-make-mode.elc: emacspeak-make-mode.el dtk-speak.elc
emacspeak-man.elc: emacspeak-man.el dtk-speak.elc
emacspeak-message.elc: emacspeak-message.el dtk-speak.elc
emacspeak-metapost.elc: emacspeak-metapost.el dtk-speak.elc
emacspeak-midge.elc: emacspeak-midge.el dtk-speak.elc
emacspeak-mpg123.elc: emacspeak-mpg123.el dtk-speak.elc
emacspeak-mspools.elc: emacspeak-mspools.el dtk-speak.elc
emacspeak-net-utils.elc: emacspeak-net-utils.el dtk-speak.elc
emacspeak-oo-browser.elc: emacspeak-oo-browser.el dtk-speak.elc 
emacspeak-outline.elc: emacspeak-outline.el dtk-speak.elc
emacspeak-ocr.elc: emacspeak-ocr.el dtk-speak.elc
emacspeak-pcl-cvs.elc: emacspeak-pcl-cvs.el dtk-speak.elc
emacspeak-perl.elc: emacspeak-perl.el dtk-speak.elc
emacspeak-pronounce.elc: emacspeak-pronounce.el
emacspeak-psgml.elc: emacspeak-psgml.el dtk-speak.elc
emacspeak-python.elc: emacspeak-python.el dtk-speak.elc
emacspeak-realaudio.elc: emacspeak-realaudio.el dtk-speak.elc
emacspeak-redefine.elc: emacspeak-redefine.el  dtk-speak.elc
emacspeak-reftex.elc: emacspeak-reftex.el dtk-speak.elc
emacspeak-remote.elc: emacspeak-remote.el
emacspeak-replace.elc: emacspeak-replace.el dtk-speak.elc
emacspeak-rmail.elc: emacspeak-rmail.el dtk-speak.elc
emacspeak-sawfish.elc: emacspeak-sawfish.el dtk-speak.elc
emacspeak-setup.elc: emacspeak-setup.el
emacspeak-sgml-mode.elc: emacspeak-sgml-mode.el dtk-speak.elc
emacspeak-sh-script.elc: emacspeak-sh-script.el dtk-speak.elc
emacspeak-solitaire.elc: emacspeak-solitaire.el dtk-speak.elc
emacspeak-sounds.elc: emacspeak-sounds.el
emacspeak-speak.elc: emacspeak-speak.el dtk-speak.elc voice-lock.elc
emacspeak-speedbar.elc: emacspeak-speedbar.el dtk-speak.elc
emacspeak-sql.elc: emacspeak-sql.el dtk-speak.elc
emacspeak-supercite.elc: emacspeak-supercite.el dtk-speak.elc
emacspeak-table-ui.elc: emacspeak-table-ui.el emacspeak-tabulate.elc  \
emacspeak-table.elc
emacspeak-table.elc: emacspeak-table.el
emacspeak-tabulate.elc: emacspeak-tabulate.el dtk-speak.elc
emacspeak-tapestry.elc: emacspeak-tapestry.el dtk-speak.elc
emacspeak-tar.elc: emacspeak-tar.el dtk-speak.elc
emacspeak-tcl.elc: emacspeak-tcl.el dtk-speak.elc
emacspeak-tdtd.elc: emacspeak-tdtd.el dtk-speak.elc
emacspeak-tempo.elc: emacspeak-tempo.el dtk-speak.elc
emacspeak-tetris.elc: emacspeak-tetris.el dtk-speak.elc
emacspeak-texinfo.elc: emacspeak-texinfo.el dtk-speak.elc
emacspeak-tnt.elc: emacspeak-tnt.el dtk-speak.elc
emacspeak-url-template.elc: emacspeak-url-template.el dtk-speak.elc
emacspeak-view-process.elc: emacspeak-view-process.el dtk-speak.elc
emacspeak-view.elc: emacspeak-view.el dtk-speak.elc
emacspeak-vm.elc: emacspeak-vm.el dtk-speak.elc
emacspeak-w3.elc: emacspeak-w3.el dtk-speak.elc
emacspeak-websearch.elc: emacspeak-websearch.el dtk-speak.elc
emacspeak-widget.elc: emacspeak-widget.el dtk-speak.elc
emacspeak-winring.elc: emacspeak-winring.el dtk-speak.elc
emacspeak-windmove.elc: emacspeak-windmove.el dtk-speak.elc
emacspeak-wizards.elc: emacspeak-wizards.el dtk-speak.elc
emacspeak-wrolo.elc: emacspeak-wrolo.el dtk-speak.elc 
emacspeak-xslide.elc: emacspeak-xslide.el dtk-speak.elc
emacspeak-xslt-process.elc: emacspeak-xslt-process.el dtk-speak.elc
emacspeak.elc: emacspeak.el

# }}}
# {{{  user level target-- clean

clean:
	rm -f *.elc 

# }}}
# {{{ end of file 

#local variables: 
#major-mode: makefile-mode
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
