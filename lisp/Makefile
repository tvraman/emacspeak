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
# the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

# }}}
# {{{make rules

.POSIX:
# what emacs is called on your system
EMACS = emacs
SRC="../lisp"
# How to run in batch mode
BATCH = -batch -q -no-site-file     \
-f package-initialize \
--eval '(setq file-name-handler-alist nil gc-cons-threshold 64000000 \
load-source-file-function  nil)' 
DEPS= -l $(SRC)/emacspeak-preamble.el -l $(SRC)/emacspeak-loaddefs.el 

COMPILE =  -f batch-byte-compile
# How to compile
%.elc:  %.el 
	$(EMACS) $(BATCH)  $(DEPS)  $(COMPILE) $<

# }}}
# {{{  grouping files into dependent modules

#The various modules:
TTS_OBJECTS =  \
emacspeak-preamble.elc \
emacspeak-loaddefs.elc \
dtk-speak.elc		dectalk-voices.elc\
plain-voices.elc espeak-voices.elc \
outloud-voices.elc  mac-voices.elc

SPEAK_OBJECTS =   \
voice-setup.elc \
voice-defs.elc \
emacspeak-pronounce.elc \
emacspeak-speak.elc  \
emacspeak-advice.elc\
emacspeak-keymap.elc\
emacspeak-sounds.elc \
emacspeak.elc \
emacspeak-setup.elc 

CORE_OBJECTS =  \
amixer.elc \
emacspeak-2048.elc \
emacspeak-abc-mode.elc \
emacspeak-actions.elc \
emacspeak-add-log.elc \
emacspeak-amark.elc \
emacspeak-analog.elc \
emacspeak-annotate.elc \
emacspeak-apt-sources.elc \
emacspeak-arc.elc \
emacspeak-auctex.elc \
emacspeak-autoload.el \
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
emacspeak-calibredb.elc \
emacspeak-crossword.elc \
emacspeak-chess.elc \
emacspeak-cider.elc \
emacspeak-ciel.elc \
emacspeak-clojure.elc \
emacspeak-cmuscheme.elc \
emacspeak-comint.elc \
emacspeak-company.elc \
emacspeak-compile.elc \
emacspeak-cperl.elc \
emacspeak-custom.elc \
emacspeak-dbus.elc \
emacspeak-deadgrep.elc \
emacspeak-debugger.elc \
emacspeak-desktop.elc \
emacspeak-dictionary.elc \
emacspeak-diff-mode.elc \
emacspeak-dired.elc \
emacspeak-dismal.elc \
emacspeak-dumb-jump.elc \
emacspeak-eaf.elc \
emacspeak-ecb.elc \
emacspeak-eclim.elc \
emacspeak-ediff.elc \
emacspeak-eglot.elc \
emacspeak-ein.elc \
emacspeak-elfeed.elc \
emacspeak-elisp-refs.elc \
emacspeak-elpher.elc \
emacspeak-elpy.elc \
emacspeak-elscreen.elc \
emacspeak-emms.elc \
emacspeak-enriched.elc \
emacspeak-entertain.elc \
emacspeak-epa.elc \
emacspeak-eperiodic.elc \
emacspeak-epub.elc \
emacspeak-erc.elc \
emacspeak-eshell.elc \
emacspeak-ess.elc \
emacspeak-etable.elc \
emacspeak-eterm.elc \
emacspeak-eudc.elc \
emacspeak-evil.elc \
emacspeak-eww.elc \
emacspeak-extras.elc \
emacspeak-feeds.elc \
emacspeak-filtertext.elc \
emacspeak-flycheck.elc \
emacspeak-flymake.elc \
emacspeak-flyspell.elc \
emacspeak-folding.elc \
emacspeak-forge.elc \
emacspeak-forms.elc \
emacspeak-geiser.elc \
emacspeak-gh-explorer.elc \
emacspeak-gnuplot.elc \
emacspeak-gnus.elc\
emacspeak-go-mode.elc \
emacspeak-gomoku.elc \
emacspeak-google.elc \
emacspeak-gridtext.elc \
emacspeak-gtags.elc \
emacspeak-gud.elc \
emacspeak-haskell.elc \
emacspeak-helm.elc \
emacspeak-hide-lines.elc  \
emacspeak-hide.elc \
emacspeak-hideshow.elc \
emacspeak-hydra.elc \
emacspeak-ibuffer.elc \
emacspeak-ido.elc \
emacspeak-iedit.elc \
emacspeak-indium.elc \
emacspeak-info.elc\
emacspeak-ispell.elc \
emacspeak-ivy.elc \
emacspeak-jabber.elc \
emacspeak-jdee.elc \
emacspeak-js2.elc \
emacspeak-kmacro.elc  \
emacspeak-librivox.elc \
emacspeak-lispy.elc \
emacspeak-lua.elc \
emacspeak-m-player.elc \
emacspeak-magit.elc \
emacspeak-make-mode.elc \
emacspeak-man.elc \
emacspeak-markdown.elc \
emacspeak-maths.elc \
emacspeak-message.elc \
emacspeak-metapost.elc \
emacspeak-midge.elc \
emacspeak-mines.elc \
emacspeak-mspools.elc \
emacspeak-muse.elc \
emacspeak-navi-mode.elc \
emacspeak-net-utils.elc \
emacspeak-newsticker.elc \
emacspeak-nov.elc \
emacspeak-nxml.elc \
emacspeak-ocr.elc \
emacspeak-org.elc \
emacspeak-orgalist.elc \
emacspeak-origami.elc \
emacspeak-outline.elc \
emacspeak-package.elc \
emacspeak-paradox.elc \
emacspeak-perl.elc \
emacspeak-pianobar.elc \
emacspeak-popup.elc \
emacspeak-proced.elc \
emacspeak-project.elc \
emacspeak-projectile.elc \
emacspeak-py.elc \
emacspeak-pydoc.elc \
emacspeak-python.elc \
emacspeak-racer.elc \
emacspeak-racket.elc \
emacspeak-re-builder.elc \
emacspeak-re-builder.elc \
emacspeak-reftex.elc \
emacspeak-related.elc \
emacspeak-rg.elc \
emacspeak-rmail.elc \
emacspeak-rpm-spec.elc \
emacspeak-rst.elc \
emacspeak-ruby.elc \
emacspeak-rust-mode.elc \
emacspeak-sage.elc \
emacspeak-sdcv.elc \
emacspeak-selectrum.elc \
emacspeak-ses.elc \
emacspeak-setup.el \
emacspeak-sgml-mode.elc \
emacspeak-sh-script.elc \
emacspeak-shx.elc \
emacspeak-slime.elc \
emacspeak-smart-window.elc \
emacspeak-smartparens.elc \
emacspeak-solitaire.elc \
emacspeak-speedbar.elc \
emacspeak-sql.elc \
emacspeak-sudoku.elc \
emacspeak-supercite.elc \
emacspeak-syslog.elc \
emacspeak-tab-bar.elc \
emacspeak-table-ui.elc \
emacspeak-table.elc \
emacspeak-tabulate.elc \
emacspeak-tar.elc \
emacspeak-tcl.elc \
emacspeak-tempo.elc \
emacspeak-tetris.elc \
emacspeak-texinfo.elc \
emacspeak-threes.elc \
emacspeak-tide.elc \
emacspeak-todo-mode.elc \
emacspeak-transient.elc \
emacspeak-twittering.elc \
emacspeak-typo.elc \
emacspeak-url-template.elc \
emacspeak-vdiff.elc \
emacspeak-vertico.elc \
emacspeak-view.elc \
emacspeak-vm.elc \
emacspeak-vterm.elc \
emacspeak-vuiet.elc \
emacspeak-wdired.elc \
emacspeak-we.elc \
emacspeak-websearch.elc \
emacspeak-webspace.elc \
emacspeak-widget.elc \
emacspeak-windmove.elc \
emacspeak-winring.elc \
emacspeak-wizards.elc  \
emacspeak-woman.elc \
emacspeak-xkcd.elc \
emacspeak-xref.elc \
emacspeak-xslt.elc \
emacspeak-yaml.elc \
emacspeak-yasnippet.elc \
ladspa.elc \
soundscape.elc \
sox-gen.elc \
sox.elc 

ADDON_OBJECTS = \
emacspeak-nm.elc  dom-addons.elc  \
xbacklight.elc  toy-braille.elc tetris.elc cd-tool.elc  \
g-utils.elc gweb.elc gmaps.elc gm-nnir.elc 


OBJECTS = $(TTS_OBJECTS) $(SPEAK_OBJECTS) $(CORE_OBJECTS)  $(ADDON_OBJECTS)

# }}}
# {{{  User level targets all

all: $(OBJECTS)

muggles: emacspeak-muggles.elc 
extra-muggles: extra-muggles.elc
maths: emacspeak-maths.elc

config: emacspeak-loaddefs.el   
lint:
	echo "." |\
emacs -batch -q \
-f package-initialize \
-l advice.el -l emacspeak-preamble.el  -l emacspeak-loaddefs.el \
-l g-utils.el \
-f elint-directory 

# }}}
# {{{build time target --loaddefs

emacspeak-loaddefs.el: 
	@echo ";;;Auto generated" > emacspeak-loaddefs.el
	$(EMACS) $(BATCH)  -l ./emacspeak-preamble.el  \
	-l ./emacspeak-autoload.el  \
	-f emacspeak-auto-generate-autoloads

# }}}
# {{{Development  target: indent, tags, relint 
indent:
	$(EMACS) -batch -q -no-site-file -l ../utils/indent-files.el

elint:
	$(EMACS) -batch -q -no-site-file -l ../utils/elint-files.el

relint:
	$(EMACS) -batch -f package-initialize -l relint.el -f relint-batch ./*.el 

tags:
	etags *.el 

# }}}
# {{{  user level target-- clean

clean:
	rm -f *.elc  emacspeak-loaddefs.el 


# }}}
# {{{ end of file

#local variables:
#mode: makefile
#fill-column: 90
#folded-file: t
#end:

# }}}
