# $Author: tv.raman.tv $
# Description:  Makefile for Emacspeak
# Keywords: Emacspeak, TTS, Makefile
# {{{ LCD Entry:

# LCD Archive Entry:
# emacspeak| T. V. Raman |raman@cs.cornell.edu
# A speech interface to Emacs |
# $Date: 2007-06-24 15:56:04 -0700 (Sun, 24 Jun 2007) $ |
#  $Revision: 4673 $ |
# Location undetermined
#

# }}}
# {{{ Copyright:

#Copyright (C) 1995 -- 2017, T. V. Raman

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
DEPS=-l $(SRC)/lisp/emacspeak-load-path.el \
-l $(SRC)/lisp/emacspeak-loaddefs.el \
-l $(SRC)/lisp/emacspeak-cus-load.el \
-l $(SRC)/lisp/g-client/g-load-path.el

COMPILE =  -f batch-byte-compile
# How to compile
%.elc:  %.el
	$(EMACS) $(BATCH)  $(DEPS)  $(COMPILE) $<
# }}}
# {{{  grouping files into dependent modules

#The various modules:
TTS_OBJECTS = acss-structure.elc \
dtk-speak.elc		dectalk-voices.elc\
tts.elc tts-cmds.elc  plain-voices.elc espeak-voices.elc \
outloud-voices.elc  mac-voices.elc\
dtk-interp.elc dtk-unicode.elc

SPEAK_OBJECTS =   \
emacspeak-speak.elc  \
emacspeak-actions.elc \
voice-setup.elc \
emacspeak-personality.elc \
emacspeak-popup.elc \
emacspeak-pronounce.elc \
emacspeak-sounds.elc

CORE_OBJECTS =  \
emacspeak-load-path.elc \
emacspeak.elc \
emacspeak-setup.elc \
emacspeak-advice.elc\
emacspeak-keymap.elc\
emacspeak-fix-interactive.elc\
emacspeak-re-builder.elc \
emacspeak-redefine.elc\
emacspeak-replace.elc \
emacspeak-table.elc \
emacspeak-table-ui.elc \
emacspeak-tabulate.elc \
amixer.elc \
emacspeak-add-log.elc \
emacspeak-amark.elc \
emacspeak-analog.elc \
emacspeak-ansi-color.elc \
emacspeak-apt-sources.elc \
emacspeak-arc.elc \
emacspeak-auctex.elc \
emacspeak-autoload.el \
emacspeak-aumix.elc \
emacspeak-bbc.elc \
emacspeak-bbdb.elc \
emacspeak-bibtex.elc \
emacspeak-bookmark.elc \
emacspeak-bookshare.elc \
emacspeak-browse-kill-ring.elc \
emacspeak-bs.elc \
emacspeak-buff-menu.elc \
emacspeak-c.elc \
emacspeak-calc.elc \
emacspeak-calculator.elc \
emacspeak-calendar.elc \
emacspeak-cedet.elc \
emacspeak-checkdoc.elc \
emacspeak-ciel.elc \
emacspeak-cider.elc \
emacspeak-clojure.elc \
emacspeak-cmuscheme.elc \
emacspeak-compile.elc \
emacspeak-company.elc \
congrats.elc \
emacspeak-cperl.elc \
emacspeak-custom.elc \
emacspeak-dbus.elc \
emacspeak-dictionary.elc \
emacspeak-desktop.elc \
emacspeak-diff-mode.elc \
emacspeak-dired.elc \
emacspeak-dismal.elc \
emacspeak-dmacro.elc \
emacspeak-epub.elc \
emacspeak-ecb.elc \
emacspeak-ein.elc \
emacspeak-ediary.elc \
emacspeak-ediff.elc \
emacspeak-elisp-refs.elc \
emacspeak-elscreen.elc \
emacspeak-eww.elc \
emacspeak-eclim.elc \
emacspeak-elfeed.elc \
emacspeak-enriched.elc \
emacspeak-xkcd.elc \
emacspeak-emms.elc \
emacspeak-entertain.elc \
emacspeak-epa.elc \
emacspeak-eperiodic.elc \
emacspeak-erc.elc \
emacspeak-eshell.elc \
emacspeak-ess.elc \
emacspeak-evil.elc \
emacspeak-eterm.elc \
emacspeak-eudc.elc \
emacspeak-facemenu.elc \
emacspeak-feeds.elc \
emacspeak-filtertext.elc \
emacspeak-find-dired.elc \
emacspeak-ftf.elc \
emacspeak-find-func.elc \
emacspeak-flycheck.elc \
emacspeak-flyspell.elc \
emacspeak-folding.elc \
emacspeak-forms.elc \
emacspeak-generic.elc \
emacspeak-geiser.elc \
emacspeak-gnuplot.elc \
emacspeak-gnus.elc\
emacspeak-go-mode.elc \
emacspeak-gomoku.elc \
emacspeak-google.elc \
emacspeak-gridtext.elc \
emacspeak-gtags.elc \
emacspeak-gud.elc \
emacspeak-helm.elc \
emacspeak-hide.elc \
emacspeak-hideshow.elc \
emacspeak-hydra.elc \
emacspeak-ibuffer.elc \
emacspeak-ido.elc \
emacspeak-ivy.elc \
emacspeak-imenu.elc \
emacspeak-info.elc\
emacspeak-ispell.elc \
emacspeak-jabber.elc \
emacspeak-indium.elc \
emacspeak-jde.elc \
emacspeak-js2.elc \
emacspeak-jss.elc \
emacspeak-kite.elc \
emacspeak-kmacro.elc  \
emacspeak-librivox.elc \
emacspeak-lua.elc \
emacspeak-magit.elc \
emacspeak-m-player.elc \
emacspeak-make-mode.elc \
emacspeak-markdown.elc \
emacspeak-man.elc \
emacspeak-message.elc \
emacspeak-metapost.elc \
emacspeak-midge.elc \
emacspeak-mpg123.elc \
emacspeak-muse.elc \
emacspeak-mspools.elc \
emacspeak-net-utils.elc \
emacspeak-newsticker.elc \
emacspeak-ocr.elc \
emacspeak-org.elc \
emacspeak-npr.elc \
emacspeak-nxml.elc \
emacspeak-outline.elc \
emacspeak-package.elc \
emacspeak-paradox.elc \
emacspeak-perl.elc \
emacspeak-php-mode.elc \
emacspeak-pianobar.elc \
emacspeak-proced.elc \
emacspeak-projectile.elc \
emacspeak-py.elc \
emacspeak-elpy.elc \
emacspeak-pydoc.elc \
emacspeak-python.elc \
emacspeak-racket.elc \
emacspeak-re-builder.elc \
emacspeak-reftex.elc \
emacspeak-rst.elc \
emacspeak-related.elc \
emacspeak-rpm-spec.elc \
emacspeak-remote.elc \
emacspeak-rmail.elc \
emacspeak-rpm.elc  \
emacspeak-ruby.elc \
emacspeak-slime.elc \
emacspeak-ses.elc \
emacspeak-setup.el \
emacspeak-sgml-mode.elc \
emacspeak-sh-script.elc \
emacspeak-sigbegone.elc \
emacspeak-smart-window.elc \
emacspeak-solitaire.elc \
emacspeak-speedbar.elc \
ladspa.elc \
sox.elc \
soundscape.elc \
sox-gen.elc \
emacspeak-sql.elc \
emacspeak-supercite.elc \
emacspeak-sudoku.elc \
emacspeak-etable.elc \
emacspeak-tapestry.elc \
emacspeak-tar.elc \
emacspeak-tcl.elc \
emacspeak-tdtd.elc \
emacspeak-tempo.elc \
emacspeak-tetris.elc \
emacspeak-threes.elc \
emacspeak-tide.elc \
emacspeak-2048.elc \
emacspeak-texinfo.elc \
emacspeak-todo-mode.elc \
emacspeak-twittering.elc \
emacspeak-url-template.elc \
emacspeak-vdiff.elc \
emacspeak-view.elc \
emacspeak-vm.elc \
emacspeak-w3.elc \
emacspeak-w3m.elc \
emacspeak-wdired.elc \
emacspeak-we.elc \
emacspeak-webspace.elc \
emacspeak-webutils.elc \
emacspeak-websearch.elc \
emacspeak-widget.elc \
emacspeak-windmove.elc \
emacspeak-winring.elc \
emacspeak-woman.elc \
emacspeak-wizards.elc  \
emacspeak-wrolo.elc \
emacspeak-xref.elc \
emacspeak-xml-shell.elc \
emacspeak-xslt.elc \
emacspeak-yasnippet.elc 

ADDON_OBJECTS = \
nm.elc \
tapestry.elc   stack-f.elc  \
dom-addons.elc \
xbacklight.elc  toy-braille.elc tetris.elc cd-tool.elc 

OBJECTS = $(TTS_OBJECTS) $(SPEAK_OBJECTS) $(CORE_OBJECTS)  $(ADDON_OBJECTS)

# }}}
# {{{  User level targets all

all: $(OBJECTS)
	cd g-client; $(MAKE)

muggles: emacspeak-muggles.el
	make emacspeak-muggles.elc
maths: emacspeak-maths.el
	make emacspeak-maths.elc
force:

config: emacspeak-loaddefs.el emacspeak-cus-load.el 
lint:
	echo "." |\
emacs -batch -q \
-l advice.el -l emacspeak-load-path.el  -l emacspeak-loaddefs.el \
-f elint-directory 

# }}}
# {{{build time target --loaddefs

emacspeak-loaddefs.el: force
	@echo ";;;Auto generated" > emacspeak-loaddefs.el
	$(EMACS) $(BATCH)  -l ./emacspeak-load-path.el  \
	-l ./emacspeak-autoload.el  \
	-f emacspeak-auto-generate-autoloads

emacspeak-cus-load.el: force
	$(EMACS) $(BATCH) -l ./emacspeak-load-path.el  -l cus-dep -l ./emacspeak-autoload.el \
-f emacspeak-auto-custom-make-dependencies "."
	mv cus-load.el emacspeak-cus-load.el

# }}}
# {{{  Make rules for the various modules

#dtk module:
#dtk-interp contains the interface to the interpreter
#Modules using functions from the dtk module should depend on dtk-speak

dtk-interp.elc: dtk-interp.el
dtk-unicode.elc: dtk-unicode.el
dtk-speak.elc: dtk-speak.el  dtk-interp.elc dtk-unicode.elc tts.elc
acss-structure.elc: acss-structure.el tts.elc 
tts.elc: tts.el
tts-cmds.elc: tts-cmds.el tts.elc
dectalk-voices.elc: dectalk-voices.el voice-setup.elc 
outloud-voices.elc: outloud-voices.el voice-setup.elc 
plain-voices.elc: plain-voices.el voice-setup.elc 
mac-voices.elc: mac-voices.el voice-setup.elc 
espeak-voices.elc: espeak-voices.el voice-setup.elc 
voice-setup.elc: voice-setup.el acss-structure.elc 
emacspeak-amark.elc: emacspeak-amark.el
cd-tool.elc: cd-tool.el
nm.elc: nm.el
amixer.elc: amixer.el
emacspeak-actions.elc: emacspeak-actions.el
emacspeak-add-log.elc: emacspeak-add-log.el dtk-speak.elc voice-setup.elc
emacspeak-advice.elc: emacspeak-advice.el dtk-speak.elc voice-setup.elc
emacspeak-analog.elc: emacspeak-analog.el dtk-speak.elc voice-setup.elc
emacspeak-ansi-color.elc: emacspeak-ansi-color.el dtk-speak.elc voice-setup.elc
emacspeak-apt-sources.elc: emacspeak-apt-sources.el dtk-speak.elc voice-setup.elc
emacspeak-arc.elc: emacspeak-arc.el dtk-speak.elc voice-setup.elc
emacspeak-auctex.elc: emacspeak-auctex.el dtk-speak.elc voice-setup.elc
emacspeak-aumix.elc: emacspeak-aumix.el dtk-speak.elc voice-setup.elc
emacspeak-autoload.elc: emacspeak-autoload.el
emacspeak-bbc.elc: emacspeak-bbc.el dtk-speak.elc voice-setup.elc
emacspeak-bbdb.elc: emacspeak-bbdb.el dtk-speak.elc voice-setup.elc
emacspeak-bibtex.elc: emacspeak-bibtex.el dtk-speak.elc voice-setup.elc
emacspeak-bookmark.elc: emacspeak-bookmark.el dtk-speak.elc voice-setup.elc
emacspeak-bookshare.elc: emacspeak-bookshare.el dtk-speak.elc voice-setup.elc emacspeak-webutils.elc
emacspeak-browse-kill-ring.elc: emacspeak-browse-kill-ring.el dtk-speak.elc voice-setup.elc
emacspeak-bs.elc: emacspeak-bs.el dtk-speak.elc voice-setup.elc
emacspeak-buff-menu.elc: emacspeak-buff-menu.el dtk-speak.elc voice-setup.elc
emacspeak-c.elc: emacspeak-c.el dtk-speak.elc voice-setup.elc
emacspeak-cider.elc: emacspeak-cider.el dtk-speak.elc voice-setup.elc
emacspeak-clojure.elc: emacspeak-clojure.el dtk-speak.elc voice-setup.elc
emacspeak-cmuscheme.elc: emacspeak-cmuscheme.el dtk-speak.elc voice-setup.elc
emacspeak-ciel.elc: emacspeak-ciel.el dtk-speak.elc
emacspeak-calc.elc: emacspeak-calc.el
emacspeak-calculator.elc: emacspeak-calculator.el  dtk-speak.elc voice-setup.elc
emacspeak-calendar.elc: emacspeak-calendar.el dtk-speak.el
emacspeak-cedet.elc: emacspeak-cedet.el dtk-speak.elc voice-setup.elc
emacspeak-checkdoc.elc: emacspeak-checkdoc.el dtk-speak.elc voice-setup.elc
emacspeak-compile.elc: emacspeak-compile.el  dtk-speak.elc
emacspeak-company.elc: emacspeak-company.el dtk-speak.elc
congrats.elc: congrats.el dtk-speak.elc
emacspeak-cperl.elc: emacspeak-cperl.el dtk-speak.elc voice-setup.elc
emacspeak-custom.elc: emacspeak-custom.el dtk-speak.elc voice-setup.elc
emacspeak-cyclebuffer.elc: emacspeak-cyclebuffer.el dtk-speak.elc
emacspeak-epub.elc: emacspeak-epub.el dtk-speak.elc voice-setup.elc
emacspeak-dbus.elc: emacspeak-dbus.el dtk-speak.elc nm.elc 
emacspeak-desktop.elc: emacspeak-desktop.el dtk-speak.elc voice-setup.elc
emacspeak-dictionary.elc: emacspeak-dictionary.el dtk-speak.elc voice-setup.elc
emacspeak-diff-mode.elc: emacspeak-diff-mode.el dtk-speak.elc voice-setup.elc
emacspeak-dired.elc: emacspeak-dired.el dtk-speak.elc voice-setup.elc
emacspeak-dismal.elc: emacspeak-dismal.el dtk-speak.elc voice-setup.elc
emacspeak-dmacro.elc: emacspeak-dmacro.el dtk-speak.elc voice-setup.elc
emacspeak-ecb.elc: emacspeak-ecb.el dtk-speak.elc voice-setup.elc
emacspeak-ein.elc: emacspeak-ein.el dtk-speak.elc voice-setup.elc
emacspeak-ediary.elc: emacspeak-ediary.el dtk-speak.elc voice-setup.elc
emacspeak-ediff.elc: emacspeak-ediff.el dtk-speak.elc voice-setup.elc
emacspeak-elisp-refs.elc: emacspeak-elisp-refs.el dtk-speak.elc voice-setup.elc
emacspeak-elscreen.elc: emacspeak-elscreen.el dtk-speak.elc voice-setup.elc
emacspeak-xkcd.elc: emacspeak-xkcd.el dtk-speak.elc
emacspeak-eclim.elc: emacspeak-eclim.el dtk-speak.elc voice-setup.elc
emacspeak-elfeed.elc: emacspeak-elfeed.el dtk-speak.elc
emacspeak-enriched.elc: emacspeak-enriched.el dtk-speak.elc voice-setup.elc
emacspeak-emms.elc: emacspeak-emms.el dtk-speak.elc
emacspeak-entertain.elc: emacspeak-entertain.el dtk-speak.elc voice-setup.elc
emacspeak-epa.elc: emacspeak-epa.el dtk-speak.elc voice-setup.elc
emacspeak-eperiodic.elc: emacspeak-eperiodic.el dtk-speak.elc voice-setup.elc
emacspeak-erc.elc: emacspeak-erc.el dtk-speak.elc voice-setup.elc
emacspeak-eshell.elc: emacspeak-eshell.el dtk-speak.elc voice-setup.elc
emacspeak-ess.elc: emacspeak-ess.el dtk-speak.elc voice-setup.elc
emacspeak-evil.elc: emacspeak-evil.el dtk-speak.elc voice-setup.elc
emacspeak-eterm.elc: emacspeak-eterm.el dtk-speak.elc voice-setup.elc
emacspeak-eudc.elc: emacspeak-eudc.el dtk-speak.elc voice-setup.elc
dom-addons.elc: dom-addons.el 
emacspeak-eww.elc: emacspeak-eww.el  dom-addons.elc dtk-speak.elc voice-setup.elc
emacspeak-facemenu.elc: emacspeak-facemenu.el dtk-speak.elc voice-setup.elc
emacspeak-feeds.elc: emacspeak-feeds.el emacspeak-webutils.elc dtk-speak.elc
emacspeak-filtertext.elc: emacspeak-filtertext.el dtk-speak.elc voice-setup.elc
emacspeak-find-dired.elc: emacspeak-find-dired.el dtk-speak.elc voice-setup.elc
emacspeak-ftf.elc: emacspeak-ftf.el dtk-speak.elc
emacspeak-find-func.elc: emacspeak-find-func.el dtk-speak.elc voice-setup.elc
emacspeak-fix-interactive.elc: emacspeak-fix-interactive.el dtk-speak.elc voice-setup.elc
emacspeak-flycheck.elc: emacspeak-flycheck.el dtk-speak.elc voice-setup.elc
emacspeak-flyspell.elc: emacspeak-flyspell.el dtk-speak.elc voice-setup.elc
emacspeak-folding.elc: emacspeak-folding.el dtk-speak.elc voice-setup.elc
emacspeak-forms.elc: emacspeak-forms.el dtk-speak.elc voice-setup.elc
emacspeak-generic.elc: emacspeak-generic.el dtk-speak.elc voice-setup.elc
emacspeak-geiser.elc: emacspeak-geiser.el dtk-speak.elc voice-setup.elc
emacspeak-gnuplot.elc: emacspeak-gnuplot.el dtk-speak.elc voice-setup.elc
emacspeak-gnus.elc: emacspeak-gnus.el dtk-speak.elc voice-setup.elc
emacspeak-google.elc: emacspeak-google.el dtk-speak.elc
emacspeak-go-mode.elc: emacspeak-go-mode.el dtk-speak.elc
emacspeak-gomoku.elc: emacspeak-gomoku.el dtk-speak.elc voice-setup.elc
emacspeak-gridtext.elc: emacspeak-gridtext.el dtk-speak.elc voice-setup.elc
emacspeak-gtags.elc: emacspeak-gtags.el dtk-speak.elc voice-setup.elc
emacspeak-gud.elc: emacspeak-gud.el dtk-speak.elc voice-setup.elc
emacspeak-helm.elc: emacspeak-helm.el dtk-speak.elc
emacspeak-hide.elc: emacspeak-hide.el emacspeak-speak.elc
emacspeak-hideshow.elc: emacspeak-hideshow.el dtk-speak.elc voice-setup.elc
emacspeak-hydra.elc: emacspeak-hydra.el dtk-speak.elc
emacspeak-ibuffer.elc: emacspeak-ibuffer.el dtk-speak.elc voice-setup.elc
emacspeak-ido.elc: emacspeak-ido.el dtk-speak.elc voice-setup.elc
emacspeak-ivy.elc: emacspeak-ivy.el dtk-speak.elc voice-setup.elc
emacspeak-imenu.elc: emacspeak-imenu.el dtk-speak.elc voice-setup.elc
emacspeak-info.elc: emacspeak-info.el dtk-speak.elc voice-setup.elc
emacspeak-ispell.elc: emacspeak-ispell.el dtk-speak.elc voice-setup.elc
emacspeak-jabber.elc: emacspeak-jabber.el dtk-speak.elc voice-setup.elc
emacspeak-indium.elc: emacspeak-indium.el dtk-speak.elc voice-setup.elc
emacspeak-jde.elc: emacspeak-jde.el dtk-speak.elc voice-setup.elc
emacspeak-js2.elc: emacspeak-js2.el dtk-speak.elc voice-setup.elc
emacspeak-keymap.elc: emacspeak-keymap.el
emacspeak-jss.elc: emacspeak-jss.el dtk-speak.elc voice-setup.elc
emacspeak-kite.elc: emacspeak-kite.el dtk-speak.elc voice-setup.elc
emacspeak-kmacro.elc: emacspeak-kmacro.el dtk-speak.elc voice-setup.elc
emacspeak-librivox.elc: emacspeak-librivox.el emacspeak-m-player.elc dtk-speak.elc voice-setup.elc
emacspeak-lua.elc: emacspeak-lua.el dtk-speak.elc
emacspeak-loaddefs.elc: emacspeak-loaddefs.el
emacspeak-load-path.elc: emacspeak-load-path.el
emacspeak-magit.elc: emacspeak-magit.el dtk-speak.elc voice-setup.elc
emacspeak-m-player.elc: emacspeak-m-player.el dtk-speak.elc ladspa.elc
emacspeak-maths.elc: emacspeak-maths.el emacspeak-muggles.elc dtk-speak.elc
emacspeak-make-mode.elc: emacspeak-make-mode.el dtk-speak.elc voice-setup.elc
emacspeak-markdown.elc: emacspeak-markdown.el dtk-speak.elc
emacspeak-man.elc: emacspeak-man.el dtk-speak.elc voice-setup.elc
emacspeak-message.elc: emacspeak-message.el dtk-speak.elc voice-setup.elc
emacspeak-metapost.elc: emacspeak-metapost.el dtk-speak.elc voice-setup.elc
emacspeak-midge.elc: emacspeak-midge.el dtk-speak.elc voice-setup.elc
emacspeak-mpg123.elc: emacspeak-mpg123.el dtk-speak.elc voice-setup.elc
emacspeak-muse.elc: emacspeak-muse.el dtk-speak.elc
emacspeak-mspools.elc: emacspeak-mspools.el dtk-speak.elc voice-setup.elc
emacspeak-muggles.elc: emacspeak-muggles.el dtk-speak.elc
emacspeak-net-utils.elc: emacspeak-net-utils.el dtk-speak.elc voice-setup.elc
emacspeak-newsticker.elc: emacspeak-newsticker.el dtk-speak.elc voice-setup.elc
emacspeak-npr.elc: emacspeak-npr.el dtk-speak.elc voice-setup.elc
emacspeak-nxml.elc: emacspeak-nxml.el dtk-speak.elc voice-setup.elc
emacspeak-org.elc: emacspeak-org.el dtk-speak.elc
emacspeak-outline.elc: emacspeak-outline.el dtk-speak.elc voice-setup.elc
emacspeak-ocr.elc: emacspeak-ocr.el dtk-speak.elc voice-setup.elc
emacspeak-package.elc: emacspeak-package.el dtk-speak.elc
emacspeak-paradox.elc: emacspeak-paradox.el dtk-speak.elc
emacspeak-perl.elc: emacspeak-perl.el dtk-speak.elc voice-setup.elc
emacspeak-php-mode.elc: emacspeak-php-mode.el dtk-speak.elc voice-setup.elc
emacspeak-pianobar.elc: emacspeak-pianobar.el dtk-speak.elc voice-setup.elc
emacspeak-pronounce.elc: emacspeak-pronounce.el
emacspeak-personality.elc: emacspeak-personality.el
emacspeak-popup.elc: emacspeak-popup.el dtk-speak.elc
emacspeak-proced.elc: emacspeak-proced.el dtk-speak.elc voice-setup.elc
emacspeak-projectile.elc: emacspeak-projectile.el dtk-speak.elc
emacspeak-py.elc: emacspeak-py.el dtk-speak.elc voice-setup.elc
emacspeak-python.elc: emacspeak-python.el dtk-speak.elc voice-setup.elc
emacspeak-racket.elc: emacspeak-racket.el dtk-speak.elc voice-setup.elc
emacspeak-elpy.elc: emacspeak-elpy.el dtk-speak.elc
emacspeak-pydoc.elc: emacspeak-pydoc.el dtk-speak.elc
emacspeak-re-builder.elc: emacspeak-re-builder.el dtk-speak.elc voice-setup.elc
emacspeak-redefine.elc: emacspeak-redefine.el  dtk-speak.elc voice-setup.elc
emacspeak-reftex.elc: emacspeak-reftex.el dtk-speak.elc voice-setup.elc
emacspeak-rst.elc: emacspeak-rst.el dtk-speak.elc voice-setup.elc
emacspeak-related.elc: emacspeak-related.el dtk-speak.elc voice-setup.elc
emacspeak-rpm-spec.elc: emacspeak-rpm-spec.el dtk-speak.elc voice-setup.elc
emacspeak-remote.elc: emacspeak-remote.el
emacspeak-replace.elc: emacspeak-replace.el dtk-speak.elc voice-setup.elc
emacspeak-ruby.elc: emacspeak-ruby.el dtk-speak.elc
emacspeak-rmail.elc: emacspeak-rmail.el dtk-speak.elc voice-setup.elc
emacspeak-slime.elc: emacspeak-slime.el dtk-speak.elc
emacspeak-ses.elc: emacspeak-ses.el dtk-speak.elc voice-setup.elc
emacspeak-setup.elc: emacspeak-setup.el
emacspeak-sgml-mode.elc: emacspeak-sgml-mode.el dtk-speak.elc voice-setup.elc
emacspeak-sh-script.elc: emacspeak-sh-script.el dtk-speak.elc voice-setup.elc
emacspeak-sigbegone.elc: emacspeak-sigbegone.el voice-setup.elc
emacspeak-smart-window.elc: emacspeak-smart-window.el dtk-speak.elc
emacspeak-solitaire.elc: emacspeak-solitaire.el dtk-speak.elc voice-setup.elc
emacspeak-sounds.elc: emacspeak-sounds.el
emacspeak-speak.elc: emacspeak-speak.el dtk-speak.elc voice-setup.elc
emacspeak-speedbar.elc: emacspeak-speedbar.el dtk-speak.elc voice-setup.elc
emacspeak-sql.elc: emacspeak-sql.el dtk-speak.elc voice-setup.elc
ladspa.elc: ladspa.el 
sox.elc: sox.el ladspa.elc
soundscape.elc: soundscape.el
sox-gen.elc: sox-gen.el sox.elc
emacspeak-supercite.elc: emacspeak-supercite.el dtk-speak.elc voice-setup.elc
emacspeak-sudoku.elc: emacspeak-sudoku.el dtk-speak.elc
emacspeak-table-ui.elc: emacspeak-table-ui.el emacspeak-tabulate.elc  \
emacspeak-table.elc
emacspeak-table.elc: emacspeak-table.el
emacspeak-tabulate.elc: emacspeak-tabulate.el dtk-speak.elc voice-setup.elc
emacspeak-etable.elc: emacspeak-etable.el dtk-speak.elc
emacspeak-tapestry.elc: emacspeak-tapestry.el dtk-speak.elc voice-setup.elc
emacspeak-tar.elc: emacspeak-tar.el dtk-speak.elc voice-setup.elc
emacspeak-tcl.elc: emacspeak-tcl.el dtk-speak.elc voice-setup.elc
emacspeak-tdtd.elc: emacspeak-tdtd.el dtk-speak.elc voice-setup.elc
emacspeak-tempo.elc: emacspeak-tempo.el dtk-speak.elc voice-setup.elc
emacspeak-tetris.elc: emacspeak-tetris.el dtk-speak.elc voice-setup.elc
emacspeak-threes.elc: emacspeak-threes.el dtk-speak.elc
emacspeak-tide.elc: emacspeak-tide.el dtk-speak.elc
emacspeak-2048.elc: emacspeak-2048.el dtk-speak.elc
emacspeak-texinfo.elc: emacspeak-texinfo.el dtk-speak.elc voice-setup.elc
emacspeak-todo-mode.elc: emacspeak-todo-mode.el dtk-speak.elc voice-setup.elc
emacspeak-twittering.elc: emacspeak-twittering.el dtk-speak.elc voice-setup.elc
emacspeak-url-template.elc: emacspeak-url-template.el emacspeak-webutils.elc emacspeak-we.elc
emacspeak-vdiff.elc: emacspeak-vdiff.el dtk-speak.elc voice-setup.elc
emacspeak-view.elc: emacspeak-view.el dtk-speak.elc voice-setup.elc
emacspeak-vm.elc: emacspeak-vm.el dtk-speak.elc voice-setup.elc
emacspeak-w3.elc: emacspeak-w3.el emacspeak-webutils.elc emacspeak-we.elc
emacspeak-w3m.elc: emacspeak-w3m.el emacspeak-webutils.elc emacspeak-we.elc
emacspeak-wdired.elc: emacspeak-wdired.el dtk-speak.elc voice-setup.elc
emacspeak-we.elc: emacspeak-we.el emacspeak-webutils.elc
emacspeak-webutils.elc: emacspeak-webutils.el dtk-speak.elc voice-setup.elc
emacspeak-webspace.elc: emacspeak-webspace.el emacspeak-webutils.elc
emacspeak-websearch.elc: emacspeak-websearch.el emacspeak-we.elc
emacspeak-webmarks.elc: emacspeak-webmarks.el emacspeak-we.elc
emacspeak-widget.elc: emacspeak-widget.el dtk-speak.elc voice-setup.elc
emacspeak-winring.elc: emacspeak-winring.el dtk-speak.elc voice-setup.elc
emacspeak-woman.elc: emacspeak-woman.el dtk-speak.elc
emacspeak-windmove.elc: emacspeak-windmove.el dtk-speak.elc voice-setup.elc
emacspeak-wizards.elc: emacspeak-wizards.el dtk-speak.elc voice-setup.elc
emacspeak-wrolo.elc: emacspeak-wrolo.el dtk-speak.elc
emacspeak-xref.elc: emacspeak-xref.el dtk-speak.elc
emacspeak-xml-shell.elc: emacspeak-xml-shell.el dtk-speak.elc voice-setup.elc
emacspeak-xslt.elc: emacspeak-xslt.el
emacspeak-yasnippet.elc: emacspeak-yasnippet.el dtk-speak.elc

emacspeak.elc: emacspeak.el
emacspeak-setup.elc: emacspeak-setup.el
loaddefs: emacspeak-loaddefs.elc

# }}}
# {{{Development  target: indent, tags
indent:
	$(EMACS) -batch -q -no-site-file -l ../utils/indent-files.el

elint:
	$(EMACS) -batch -q -no-site-file -l ../utils/elint-files.el

tags:
	etags *.el g-client/*.el

# }}}
# {{{  user level target-- clean

clean:
	rm -f *.elc  emacspeak-loaddefs.el emacspeak-cus-load.el
	cd g-client && make clean 

# }}}
# {{{ end of file

#local variables:
#mode: makefile
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
