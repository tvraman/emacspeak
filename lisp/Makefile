# $Author: tv.raman.tv $
# Description:  Makefile for Emacspeak
# Keywords: Emacspeak, Dectalk, Makefile
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

#Copyright (C) 1995 -- 2015, T. V. Raman

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
BATCH = -batch -q -no-site-file   -eval '(setq vc-handled-backends nil\
byte-compile-warnings t)'
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
DTK_OBJECTS = acss-structure.elc \
dtk-speak.elc		dectalk-voices.elc\
plain-voices.elc espeak-voices.elc \
outloud-voices.elc multispeech-voices.elc mac-voices.elc\
dtk-interp.elc dtk-unicode.elc

SPEAK_OBJECTS =   \
emacspeak-speak.elc  \
emacspeak-actions.elc \
voice-setup.elc \
emacspeak-personality.elc \
emacspeak-pronounce.elc \
emacspeak-sounds.elc

CORE_OBJECTS =  \
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
emacspeak-alsaplayer.elc \
emacspeak-amark.elc \
emacspeak-analog.elc \
emacspeak-ansi-color.elc \
emacspeak-apt-sources.elc \
emacspeak-apt-utils.elc \
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
emacspeak-eww.elc \
flite-voices.elc \
emacspeak-eclim.elc \
emacspeak-elfeed.elc \
emacspeak-enriched.elc \
emacspeak-xkcd.elc \
emacspeak-emms.elc \
emacspeak-entertain.elc \
emacspeak-eperiodic.elc \
emacspeak-espn.elc \
emacspeak-erc.elc \
emacspeak-eshell.elc \
emacspeak-ess.elc \
emacspeak-eterm.elc \
emacspeak-eudc.elc \
emacspeak-facemenu.elc \
emacspeak-feeds.elc \
emacspeak-filtertext.elc \
emacspeak-find-dired.elc \
emacspeak-ftf.elc \
emacspeak-find-func.elc \
emacspeak-finder.elc \
emacspeak-flycheck.elc \
emacspeak-flyspell.elc \
emacspeak-folding.elc \
emacspeak-forms.elc \
emacspeak-generic.elc \
emacspeak-gnuplot.elc \
emacspeak-gnus.elc\
emacspeak-go-mode.elc \
emacspeak-gomoku.elc \
emacspeak-google.elc \
emacspeak-gridtext.elc \
emacspeak-gtags.elc \
emacspeak-gud.elc \
emacspeak-hide.elc \
emacspeak-hideshow.elc \
emacspeak-ibuffer.elc \
emacspeak-ido.elc \
emacspeak-imenu.elc \
emacspeak-info.elc\
emacspeak-ispell.elc \
emacspeak-jabber.elc \
emacspeak-jde.elc \
emacspeak-js2.elc \
emacspeak-jss.elc \
emacspeak-kite.elc \
emacspeak-kmacro.elc  \
emacspeak-librivox.elc \
emacspeak-madplay.elc \
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
emacspeak-perl.elc \
emacspeak-php-mode.elc \
emacspeak-pianobar.elc \
emacspeak-proced.elc \
emacspeak-py.elc \
emacspeak-python.elc \
emacspeak-re-builder.elc \
emacspeak-reftex.elc \
emacspeak-rpm-spec.elc \
emacspeak-remote.elc \
emacspeak-rmail.elc \
emacspeak-rpm.elc  \
emacspeak-ruby.elc \
emacspeak-sawfish.elc \
emacspeak-ses.elc \
emacspeak-setup.el \
emacspeak-sgml-mode.elc \
emacspeak-sh-script.elc \
emacspeak-sigbegone.elc \
emacspeak-solitaire.elc \
emacspeak-speedbar.elc \
sox.elc \
emacspeak-sql.elc \
emacspeak-supercite.elc \
emacspeak-sudoku.elc \
emacspeak-swbuff.elc \
emacspeak-etable.elc \
emacspeak-tapestry.elc \
emacspeak-tar.elc \
emacspeak-tcl.elc \
emacspeak-tdtd.elc \
emacspeak-tempo.elc \
emacspeak-tetris.elc \
emacspeak-2048.elc \
emacspeak-texinfo.elc \
emacspeak-todo-mode.elc \
emacspeak-twittering.elc \
emacspeak-url-template.elc \
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
emacspeak-xref.elc \
emacspeak-xml-shell.elc \
emacspeak-xslt.elc

ADDON_OBJECTS = \
tapestry.elc   stack-f.elc  dom.elc \
toy-braille.elc tetris.elc cd-tool.elc nm.elc

OBJECTS = $(DTK_OBJECTS) $(SPEAK_OBJECTS) $(CORE_OBJECTS)  $(ADDON_OBJECTS)
# }}}
# {{{  User level targets all

all: $(OBJECTS)
	cd g-client; $(MAKE)

force:

config: emacspeak-loaddefs.el emacspeak-cus-load.el emacspeak-finder-inf.el
lint:
	echo "." |\
emacs -batch -q \
-l advice.el -l emacspeak-load-path.el  -l emacspeak-loaddefs.el \
-f elint-directory 
# }}}
# {{{build time target --loaddefs

emacspeak-finder-inf.el: force
	$(EMACS) $(BATCH) -l emacspeak-load-path.el  -l emacspeak-finder.el  -f emacspeak-finder-compile-keywords

emacspeak-loaddefs.el: force
	echo ";;;Auto generated" > emacspeak-loaddefs.el
	$(EMACS) $(BATCH)  -l ./emacspeak-load-path.el  \
	-l ./emacspeak-autoload.el  \
	-f emacspeak-auto-generate-autoloads

emacspeak-cus-load.el: force
	$(EMACS) $(BATCH) -l ./emacspeak-load-path.el  -l cus-dep -l ./emacspeak-autoload.el \
-f emacspeak-auto-custom-make-dependencies "."
	mv cus-load.el emacspeak-cus-load.el

# }}}
# {{{  Make rules for the various modules

# Do this in this brain-dead way because different makes do pattern
# rules differently. grumble grumble...
#dtk module:
#dtk-interp contains the interface to the interpreter
#Modules using functions from the dtk module should depend on dtk-speak

dtk-interp.elc: dtk-interp.el
dtk-unicode.elc: dtk-unicode.el
dtk-speak.elc: dtk-speak.el  dtk-interp.elc dtk-unicode.elc
acss-structure.elc: acss-structure.el
dectalk-voices.elc: dectalk-voices.el voice-setup.elc
outloud-voices.elc: outloud-voices.el voice-setup.elc
plain-voices.elc: plain-voices.el voice-setup.elc
multispeech-voices.elc: multispeech-voices.el voice-setup.elc
mac-voices.elc: mac-voices.el voice-setup.elc
espeak-voices.elc: espeak-voices.el voice-setup.elc
voice-setup.elc: voice-setup.el acss-structure.elc
emacspeak-alsaplayer.elc: emacspeak-alsaplayer.el
ommacspeak-amark.elc: emacspeak-amark.el
cd-tool.elc: cd-tool.el
nm.elc: nm.el
amixer.elc: amixer.el
emacspeak-actions.elc: emacspeak-actions.el
emacspeak-add-log.elc: emacspeak-add-log.el dtk-speak.elc voice-setup.elc
emacspeak-advice.elc: emacspeak-advice.el dtk-speak.elc voice-setup.elc
emacspeak-analog.elc: emacspeak-analog.el dtk-speak.elc voice-setup.elc
emacspeak-ansi-color.elc: emacspeak-ansi-color.el dtk-speak.elc voice-setup.elc
emacspeak-apt-sources.elc: emacspeak-apt-sources.el dtk-speak.elc voice-setup.elc
emacspeak-apt-utils.elc: emacspeak-apt-utils.el dtk-speak.elc voice-setup.elc
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
emacspeak-cmuscheme.elc: emacspeak-cmuscheme.el dtk-speak.elc voice-setup.elc
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
emacspeak-xkcd.elc: emacspeak-xkcd.el dtk-speak.elc
flite-voices.elc: flite-voices.el
emacspeak-eclim.elc: emacspeak-eclim.el dtk-speak.elc voice-setup.elc
emacspeak-elfeed.elc: emacspeak-elfeed.el dtk-speak.elc
emacspeak-enriched.elc: emacspeak-enriched.el dtk-speak.elc voice-setup.elc
emacspeak-emms.elc: emacspeak-emms.el dtk-speak.elc
emacspeak-entertain.elc: emacspeak-entertain.el dtk-speak.elc voice-setup.elc
emacspeak-eperiodic.elc: emacspeak-eperiodic.el dtk-speak.elc voice-setup.elc
emacspeak-espn.elc: emacspeak-espn.el dtk-speak.elc
emacspeak-erc.elc: emacspeak-erc.el dtk-speak.elc voice-setup.elc
emacspeak-eshell.elc: emacspeak-eshell.el dtk-speak.elc voice-setup.elc
emacspeak-ess.elc: emacspeak-ess.el dtk-speak.elc voice-setup.elc
emacspeak-eterm.elc: emacspeak-eterm.el dtk-speak.elc voice-setup.elc
emacspeak-eudc.elc: emacspeak-eudc.el dtk-speak.elc voice-setup.elc
emacspeak-eww.elc: emacspeak-eww.el dtk-speak.elc voice-setup.elc
emacspeak-facemenu.elc: emacspeak-facemenu.el dtk-speak.elc voice-setup.elc
emacspeak-feeds.elc: emacspeak-feeds.el emacspeak-webutils.elc dtk-speak.elc
emacspeak-filtertext.elc: emacspeak-filtertext.el dtk-speak.elc voice-setup.elc
emacspeak-find-dired.elc: emacspeak-find-dired.el dtk-speak.elc voice-setup.elc
emacspeak-ftf.elc: emacspeak-ftf.el dtk-speak.elc
emacspeak-find-func.elc: emacspeak-find-func.el dtk-speak.elc voice-setup.elc
emacspeak-finder.elc: emacspeak-finder.el
emacspeak-fix-interactive.elc: emacspeak-fix-interactive.el dtk-speak.elc voice-setup.elc
emacspeak-flycheck.elc: emacspeak-flycheck.el dtk-speak.elc voice-setup.elc
emacspeak-flyspell.elc: emacspeak-flyspell.el dtk-speak.elc voice-setup.elc
emacspeak-folding.elc: emacspeak-folding.el dtk-speak.elc voice-setup.elc
emacspeak-forms.elc: emacspeak-forms.el dtk-speak.elc voice-setup.elc
emacspeak-generic.elc: emacspeak-generic.el dtk-speak.elc voice-setup.elc
emacspeak-gnuplot.elc: emacspeak-gnuplot.el dtk-speak.elc voice-setup.elc
emacspeak-gnus.elc: emacspeak-gnus.el dtk-speak.elc voice-setup.elc
emacspeak-google.elc: emacspeak-google.el dtk-speak.elc
emacspeak-go-mode.elc: emacspeak-go-mode.el dtk-speak.elc
emacspeak-gomoku.elc: emacspeak-gomoku.el dtk-speak.elc voice-setup.elc
emacspeak-gridtext.elc: emacspeak-gridtext.el dtk-speak.elc voice-setup.elc
emacspeak-gtags.elc: emacspeak-gtags.el dtk-speak.elc voice-setup.elc
emacspeak-gud.elc: emacspeak-gud.el dtk-speak.elc voice-setup.elc
emacspeak-hide.elc: emacspeak-hide.el emacspeak-speak.elc
emacspeak-hideshow.elc: emacspeak-hideshow.el dtk-speak.elc voice-setup.elc
emacspeak-ibuffer.elc: emacspeak-ibuffer.el dtk-speak.elc voice-setup.elc
emacspeak-ido.elc: emacspeak-ido.el dtk-speak.elc voice-setup.elc
emacspeak-imenu.elc: emacspeak-imenu.el dtk-speak.elc voice-setup.elc
emacspeak-info.elc: emacspeak-info.el dtk-speak.elc voice-setup.elc
emacspeak-ispell.elc: emacspeak-ispell.el dtk-speak.elc voice-setup.elc
emacspeak-jabber.elc: emacspeak-jabber.el dtk-speak.elc
emacspeak-jde.elc: emacspeak-jde.el dtk-speak.elc voice-setup.elc
emacspeak-js2.elc: emacspeak-js2.el dtk-speak.elc voice-setup.elc
emacspeak-keymap.elc: emacspeak-keymap.el
emacspeak-jss.elc: emacspeak-jss.el dtk-speak.elc voice-setup.elc
emacspeak-kite.elc: emacspeak-kite.el dtk-speak.elc voice-setup.elc
emacspeak-kmacro.elc: emacspeak-kmacro.el dtk-speak.elc voice-setup.elc
emacspeak-librivox.elc: emacspeak-librivox.el dtk-speak.elc voice-setup.elc
emacspeak-loaddefs.elc: emacspeak-loaddefs.el
emacspeak-load-path.elc: emacspeak-load-path.elc
emacspeak-madplay.elc: emacspeak-madplay.el dtk-speak.elc voice-setup.elc
emacspeak-magit.elc: emacspeak-magit.el dtk-speak.elc voice-setup.elc
emacspeak-m-player.elc: emacspeak-m-player.el dtk-speak.elc voice-setup.elc
emacspeak-make-mode.elc: emacspeak-make-mode.el dtk-speak.elc voice-setup.elc
emacspeak-markdown.elc: emacspeak-markdown.el dtk-speak.elc
emacspeak-man.elc: emacspeak-man.el dtk-speak.elc voice-setup.elc
emacspeak-message.elc: emacspeak-message.el dtk-speak.elc voice-setup.elc
emacspeak-metapost.elc: emacspeak-metapost.el dtk-speak.elc voice-setup.elc
emacspeak-midge.elc: emacspeak-midge.el dtk-speak.elc voice-setup.elc
emacspeak-mpg123.elc: emacspeak-mpg123.el dtk-speak.elc voice-setup.elc
emacspeak-muse.elc: emacspeak-muse.el dtk-speak.elc
emacspeak-mspools.elc: emacspeak-mspools.el dtk-speak.elc voice-setup.elc
emacspeak-net-utils.elc: emacspeak-net-utils.el dtk-speak.elc voice-setup.elc
emacspeak-newsticker.elc: emacspeak-newsticker.el dtk-speak.elc voice-setup.elc
emacspeak-npr.elc: emacspeak-npr.el dtk-speak.elc voice-setup.elc
emacspeak-nxml.elc: emacspeak-nxml.el dtk-speak.elc voice-setup.elc
emacspeak-org.elc: emacspeak-org.el dtk-speak.elc
emacspeak-outline.elc: emacspeak-outline.el dtk-speak.elc voice-setup.elc
emacspeak-ocr.elc: emacspeak-ocr.el dtk-speak.elc voice-setup.elc
emacspeak-package.elc: emacspeak-package.el dtk-speak.elc
emacspeak-perl.elc: emacspeak-perl.el dtk-speak.elc voice-setup.elc
emacspeak-php-mode.elc: emacspeak-php-mode.el dtk-speak.elc voice-setup.elc
emacspeak-pianobar.elc: emacspeak-pianobar.el dtk-speak.elc voice-setup.elc
emacspeak-pronounce.elc: emacspeak-pronounce.el
emacspeak-personality.elc: emacspeak-personality.el
emacspeak-proced.elc: emacspeak-proced.el dtk-speak.elc voice-setup.elc
emacspeak-py.elc: emacspeak-py.el dtk-speak.elc voice-setup.elc
emacspeak-python.elc: emacspeak-python.el dtk-speak.elc voice-setup.elc
emacspeak-re-builder.elc: emacspeak-re-builder.el dtk-speak.elc voice-setup.elc
emacspeak-redefine.elc: emacspeak-redefine.el  dtk-speak.elc voice-setup.elc
emacspeak-reftex.elc: emacspeak-reftex.el dtk-speak.elc voice-setup.elc
emacspeak-rpm-spec.elc: emacspeak-rpm-spec.el dtk-speak.elc voice-setup.elc
emacspeak-remote.elc: emacspeak-remote.el
emacspeak-replace.elc: emacspeak-replace.el dtk-speak.elc voice-setup.elc
emacspeak-ruby.elc: emacspeak-ruby.el dtk-speak.elc
emacspeak-rmail.elc: emacspeak-rmail.el dtk-speak.elc voice-setup.elc
emacspeak-sawfish.elc: emacspeak-sawfish.el dtk-speak.elc
emacspeak-ses.elc: emacspeak-ses.el dtk-speak.elc voice-setup.elc
emacspeak-setup.elc: emacspeak-setup.el
emacspeak-sgml-mode.elc: emacspeak-sgml-mode.el dtk-speak.elc voice-setup.elc
emacspeak-sh-script.elc: emacspeak-sh-script.el dtk-speak.elc voice-setup.elc
emacspeak-sigbegone.elc: emacspeak-sigbegone.el voice-setup.elc
emacspeak-solitaire.elc: emacspeak-solitaire.el dtk-speak.elc voice-setup.elc
emacspeak-sounds.elc: emacspeak-sounds.el
emacspeak-speak.elc: emacspeak-speak.el dtk-speak.elc voice-setup.elc
emacspeak-speedbar.elc: emacspeak-speedbar.el dtk-speak.elc voice-setup.elc
emacspeak-sql.elc: emacspeak-sql.el dtk-speak.elc voice-setup.elc
sox.elc: sox.el 
emacspeak-supercite.elc: emacspeak-supercite.el dtk-speak.elc voice-setup.elc
emacspeak-sudoku.elc: emacspeak-sudoku.el dtk-speak.elc
emacspeak-swbuff.elc: emacspeak-swbuff.el dtk-speak.elc voice-setup.elc
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
emacspeak-2048.elc: emacspeak-2048.el dtk-speak.elc
emacspeak-texinfo.elc: emacspeak-texinfo.el dtk-speak.elc voice-setup.elc
emacspeak-todo-mode.elc: emacspeak-todo-mode.el dtk-speak.elc voice-setup.elc
emacspeak-twittering.elc: emacspeak-twittering.el dtk-speak.elc voice-setup.elc
emacspeak-url-template.elc: emacspeak-url-template.el emacspeak-webutils.elc emacspeak-we.elc
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
emacspeak-xref.elc: emacspeak-xref.el dtk-speak.elc
emacspeak-xml-shell.elc: emacspeak-xml-shell.el dtk-speak.elc voice-setup.elc
emacspeak-xslt.elc: emacspeak-xslt.el

emacspeak.elc: emacspeak.el
emacspeak-setup.elc: emacspeak-setup.el
loaddefs: emacspeak-loaddefs.elc
# }}}
# {{{Development  target: indent
indent:
	$(EMACS) -batch -q -no-site-file -l ../utils/indent-files.el

elint:
	$(EMACS) -batch -q -no-site-file -l ../utils/elint-files.el

# }}}
# {{{  user level target-- clean

clean:
	rm -f *.elc  emacspeak-loaddefs.el emacspeak-cus-load.el

# }}}
# {{{ end of file

#local variables:
#major-mode: makefile-mode
#eval:  (fold-set-marks "# {{{" "# }}}")
#fill-column: 90
#folded-file: t
#end:

# }}}
