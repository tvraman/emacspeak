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

.POSIX:
# what emacs is called on your system
EMACS = emacs
SRC="../lisp"
# How to run in batch mode
BATCH = -batch -q -no-site-file     \
-f package-initialize \
--eval '(setq file-name-handler-alist nil gc-cons-threshold 64000000 \
load-source-file-function  nil)' 
DEPS=-l $(SRC)/emacspeak-preamble.el -l $(SRC)/emacspeak-loaddefs.el 

COMPILE =  -f batch-byte-compile
# How to compile
%.elc:  %.el
	$(EMACS) $(BATCH)  $(DEPS)  $(COMPILE) $<

# }}}
# {{{  grouping files into dependent modules

#The various modules:
TTS_OBJECTS =  \
dectalk-voices.elc plain-voices.elc espeak-voices.elc \
outloud-voices.elc  mac-voices.elc

CORE_OBJECTS =   \
emacspeak-preamble.elc emacspeak-loaddefs.elc \
dtk-speak.elc dtk-unicode.elc dtk-interp.elc \
emacspeak-speak.elc   emacspeak-keymap.elc  emacspeak-pronounce.elc \
emacspeak-sounds.elc emacspeak.elc emacspeak-setup.elc \
emacspeak-advice.elc voice-setup.elc voice-defs.elc

MOD_OBJECTS =  \
amixer.elc \
emacspeak-2048.elc \
emacspeak-abc-mode.elc \
emacspeak-add-log.elc \
emacspeak-amark.elc \
emacspeak-analog.elc \
emacspeak-apt-sources.elc \
emacspeak-arc.elc \
emacspeak-auctex.elc \
emacspeak-autoload.el  \
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
emacspeak-ecb.elc \
emacspeak-eclim.elc \
emacspeak-ediff.elc \
emacspeak-eglot.elc \
emacspeak-ein.elc \
emacspeak-elfeed.elc \
emacspeak-elisp-refs.elc \
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
emacspeak-gnus.elc \
emacspeak-go-mode.elc \
emacspeak-gomoku.elc \
emacspeak-google.elc \
emacspeak-gridtext.elc \
emacspeak-gtags.elc \
emacspeak-gud.elc \
emacspeak-haskell.elc \
emacspeak-helm.elc \
emacspeak-hide-lines.elc \
emacspeak-hide.elc \
emacspeak-hideshow.elc \
emacspeak-hydra.elc \
emacspeak-ibuffer.elc \
emacspeak-ido.elc \
emacspeak-iedit.elc \
emacspeak-indium.elc \
emacspeak-info.elc \
emacspeak-ispell.elc \
emacspeak-ivy.elc \
emacspeak-jabber.elc \
emacspeak-jdee.elc \
emacspeak-js2.elc \
emacspeak-kmacro.elc \
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
emacspeak-setup.elc \
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
emacspeak-tapestry.elc \
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
emacspeak-wizards.elc \
emacspeak-woman.elc \
emacspeak-xkcd.elc \
emacspeak-xref.elc \
emacspeak-xslt.elc \
emacspeak-yaml.elc \
emacspeak-yasnippet.elc \
ladspa.elc \
soundscape.elc \
sox-gen.elc \
sox.elc \
nm.elc tapestry.elc dom-addons.elc  \
xbacklight.elc  toy-braille.elc tetris.elc cd-tool.elc  \
g-utils.elc gweb.elc gmaps.elc gm-nnir.elc \
emacspeak-muggles.elc emacspeak-maths.elc \
emacspeak-extras.elc  extra-muggles.elc 

OBJECTS = $(TTS_OBJECTS) $(CORE_OBJECTS) $(MOD_OBJECTS)  

# }}}
# {{{  User level targets all

all: $(OBJECTS)

muggles: emacspeak-muggles.elc  
extra-muggles: extra-muggles.elc
maths: emacspeak-maths.elc

force:

config: emacspeak-loaddefs.el    custom
lint:
	echo "." |\
emacs -batch -q \
-f package-initialize \
-l advice.el -l emacspeak-preamble.el  -l emacspeak-loaddefs.el \
-f elint-directory 

# }}}
# {{{build time target --loaddefs

emacspeak-loaddefs.el: force
	@echo ";;;Auto generated" > emacspeak-loaddefs.el
	$(EMACS) $(BATCH)  -l ./emacspeak-preamble.el  \
	-l ./emacspeak-autoload.el  \
	-f emacspeak-auto-generate-autoloads

custom: force
	$(EMACS) $(BATCH) -l ./emacspeak-preamble.el  -l cus-dep -l ./emacspeak-autoload.el \
-f emacspeak-auto-custom-make-dependencies "."
	cat cus-load.el  >> emacspeak-loaddefs.el
	rm -f cus-load.el

# }}}
# {{{  Make rules for the various modules

dtk-interp.elc: dtk-interp.el
dtk-unicode.elc: dtk-unicode.el
dtk-speak.elc: dtk-speak.el  dtk-interp.elc dtk-unicode.elc 
dectalk-voices.elc: dectalk-voices.el 
outloud-voices.elc: outloud-voices.el 
plain-voices.elc: plain-voices.el 
mac-voices.elc: mac-voices.el 
espeak-voices.elc: espeak-voices.el 
voice-setup.elc: voice-setup.el 
voice-defs.elc: voice-defs.el  voice-setup.elc
emacspeak-amark.elc: emacspeak-amark.el
cd-tool.elc: cd-tool.el
nm.elc: nm.el
amixer.elc: amixer.el

# Core Modules 
emacspeak-actions.elc: emacspeak-actions.el dtk-speak.elc
emacspeak-abc-mode.elc: emacspeak-abc-mode.el dtk-speak.elc 
emacspeak-add-log.elc: emacspeak-add-log.el dtk-speak.elc voice-setup.elc
emacspeak-preamble.elc: emacspeak-preamble.el
emacspeak-loaddefs.elc: emacspeak-loaddefs.el
emacspeak-advice.elc: emacspeak-advice.el dtk-speak.elc voice-setup.elc
emacspeak-analog.elc: emacspeak-analog.el dtk-speak.elc voice-setup.elc
emacspeak-apt-sources.elc: emacspeak-apt-sources.el dtk-speak.elc voice-setup.elc
emacspeak-arc.elc: emacspeak-arc.el dtk-speak.elc voice-setup.elc
emacspeak-auctex.elc: emacspeak-auctex.el dtk-speak.elc voice-setup.elc
emacspeak-autoload.elc: emacspeak-autoload.el
emacspeak-bbc.elc: emacspeak-bbc.el dtk-speak.elc voice-setup.elc
emacspeak-bbdb.elc: emacspeak-bbdb.el dtk-speak.elc voice-setup.elc
emacspeak-bibtex.elc: emacspeak-bibtex.el dtk-speak.elc voice-setup.elc
emacspeak-bookmark.elc: emacspeak-bookmark.el dtk-speak.elc voice-setup.elc
emacspeak-bookshare.elc: emacspeak-bookshare.el dtk-speak.elc voice-setup.elc 
emacspeak-browse-kill-ring.elc: emacspeak-browse-kill-ring.el dtk-speak.elc voice-setup.elc
emacspeak-bs.elc: emacspeak-bs.el dtk-speak.elc voice-setup.elc
emacspeak-buff-menu.elc: emacspeak-buff-menu.el dtk-speak.elc voice-setup.elc
emacspeak-c.elc: emacspeak-c.el dtk-speak.elc voice-setup.elc
emacspeak-comint.elc: emacspeak-comint.el dtk-speak.elc voice-setup.elc
emacspeak-cider.elc: emacspeak-cider.el dtk-speak.elc voice-setup.elc
emacspeak-clojure.elc: emacspeak-clojure.el dtk-speak.elc voice-setup.elc
emacspeak-cmuscheme.elc: emacspeak-cmuscheme.el dtk-speak.elc voice-setup.elc
emacspeak-ciel.elc: emacspeak-ciel.el dtk-speak.elc
emacspeak-calc.elc: emacspeak-calc.el
emacspeak-calculator.elc: emacspeak-calculator.el  dtk-speak.elc voice-setup.elc
emacspeak-calendar.elc: emacspeak-calendar.el dtk-speak.el
emacspeak-chess.elc: emacspeak-chess.el dtk-speak.elc voice-setup.elc
emacspeak-compile.elc: emacspeak-compile.el  dtk-speak.elc
emacspeak-company.elc: emacspeak-company.el dtk-speak.elc
emacspeak-cperl.elc: emacspeak-cperl.el dtk-speak.elc voice-setup.elc
emacspeak-custom.elc: emacspeak-custom.el dtk-speak.elc voice-setup.elc
emacspeak-cyclebuffer.elc: emacspeak-cyclebuffer.el dtk-speak.elc
emacspeak-epub.elc: emacspeak-epub.el dtk-speak.elc voice-setup.elc
emacspeak-dbus.elc: emacspeak-dbus.el dtk-speak.elc nm.elc
emacspeak-deadgrep.elc: emacspeak-deadgrep.el dtk-speak.elc
emacspeak-debugger.elc: emacspeak-debugger.el dtk-speak.elc
emacspeak-desktop.elc: emacspeak-desktop.el dtk-speak.elc voice-setup.elc
emacspeak-dictionary.elc: emacspeak-dictionary.el dtk-speak.elc voice-setup.elc
emacspeak-diff-mode.elc: emacspeak-diff-mode.el dtk-speak.elc voice-setup.elc
emacspeak-dired.elc: emacspeak-dired.el dtk-speak.elc voice-setup.elc
emacspeak-dismal.elc: emacspeak-dismal.el dtk-speak.elc voice-setup.elc
emacspeak-ecb.elc: emacspeak-ecb.el dtk-speak.elc voice-setup.elc
emacspeak-ein.elc: emacspeak-ein.el dtk-speak.elc voice-setup.elc
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
emacspeak-feeds.elc: emacspeak-feeds.el  dtk-speak.elc
emacspeak-filtertext.elc: emacspeak-filtertext.el dtk-speak.elc voice-setup.elc
emacspeak-fix-interactive.elc: emacspeak-fix-interactive.el dtk-speak.elc voice-setup.elc
emacspeak-flycheck.elc: emacspeak-flycheck.el dtk-speak.elc voice-setup.elc
emacspeak-flymake.elc: emacspeak-flymake.el dtk-speak.elc voice-setup.elc
emacspeak-flyspell.elc: emacspeak-flyspell.el dtk-speak.elc voice-setup.elc
emacspeak-folding.elc: emacspeak-folding.el dtk-speak.elc voice-setup.elc
emacspeak-forge.elc: emacspeak-forge.el dtk-speak.elc voice-setup.elc
emacspeak-forms.elc: emacspeak-forms.el dtk-speak.elc voice-setup.elc
emacspeak-geiser.elc: emacspeak-geiser.el dtk-speak.elc voice-setup.elc
emacspeak-gh-explorer.elc: emacspeak-gh-explorer.el dtk-speak.elc voice-setup.elc
emacspeak-gnuplot.elc: emacspeak-gnuplot.el dtk-speak.elc voice-setup.elc
emacspeak-gnus.elc: emacspeak-gnus.el dtk-speak.elc voice-setup.elc
emacspeak-google.elc: emacspeak-google.el dtk-speak.elc
emacspeak-go-mode.elc: emacspeak-go-mode.el dtk-speak.elc
emacspeak-gomoku.elc: emacspeak-gomoku.el dtk-speak.elc voice-setup.elc
emacspeak-gridtext.elc: emacspeak-gridtext.el dtk-speak.elc voice-setup.elc
emacspeak-gtags.elc: emacspeak-gtags.el dtk-speak.elc voice-setup.elc
emacspeak-gud.elc: emacspeak-gud.el dtk-speak.elc voice-setup.elc
emacspeak-haskell.elc: emacspeak-haskell.el dtk-speak.elc
emacspeak-helm.elc: emacspeak-helm.el dtk-speak.elc
emacspeak-hide.elc: emacspeak-hide.el emacspeak-speak.elc
emacspeak-hideshow.elc: emacspeak-hideshow.el dtk-speak.elc voice-setup.elc
emacspeak-hide-lines.elc: emacspeak-hide-lines.el dtk-speak.elc voice-setup.elc
emacspeak-hydra.elc: emacspeak-hydra.el dtk-speak.elc
emacspeak-ibuffer.elc: emacspeak-ibuffer.el dtk-speak.elc voice-setup.elc
emacspeak-ido.elc: emacspeak-ido.el dtk-speak.elc voice-setup.elc
emacspeak-iedit.elc: emacspeak-iedit.el dtk-speak.elc voice-setup.elc
emacspeak-ivy.elc: emacspeak-ivy.el dtk-speak.elc voice-setup.elc
emacspeak-info.elc: emacspeak-info.el dtk-speak.elc voice-setup.elc
emacspeak-ispell.elc: emacspeak-ispell.el dtk-speak.elc voice-setup.elc
emacspeak-jabber.elc: emacspeak-jabber.el dtk-speak.elc voice-setup.elc
emacspeak-indium.elc: emacspeak-indium.el dtk-speak.elc voice-setup.elc
emacspeak-jdee.elc: emacspeak-jdee.el dtk-speak.elc voice-setup.elc
emacspeak-js2.elc: emacspeak-js2.el dtk-speak.elc voice-setup.elc
emacspeak-keymap.elc: emacspeak-keymap.el
emacspeak-kmacro.elc: emacspeak-kmacro.el dtk-speak.elc voice-setup.elc
emacspeak-librivox.elc: emacspeak-librivox.el emacspeak-m-player.elc dtk-speak.elc voice-setup.elc
emacspeak-lispy.elc: emacspeak-lispy.el  dtk-speak.elc voice-setup.elc
emacspeak-lua.elc: emacspeak-lua.el dtk-speak.elc
emacspeak-magit.elc: emacspeak-magit.el dtk-speak.elc voice-setup.elc
emacspeak-m-player.elc: emacspeak-m-player.el dtk-speak.elc ladspa.elc
emacspeak-maths.elc: emacspeak-maths.el  dtk-speak.elc
emacspeak-make-mode.elc: emacspeak-make-mode.el dtk-speak.elc voice-setup.elc
emacspeak-markdown.elc: emacspeak-markdown.el dtk-speak.elc
emacspeak-man.elc: emacspeak-man.el dtk-speak.elc voice-setup.elc
emacspeak-message.elc: emacspeak-message.el dtk-speak.elc voice-setup.elc
emacspeak-metapost.elc: emacspeak-metapost.el dtk-speak.elc voice-setup.elc
emacspeak-midge.elc: emacspeak-midge.el dtk-speak.elc voice-setup.elc
emacspeak-mines.elc: emacspeak-mines.el dtk-speak.elc
emacspeak-muse.elc: emacspeak-muse.el dtk-speak.elc
emacspeak-mspools.elc: emacspeak-mspools.el dtk-speak.elc voice-setup.elc
emacspeak-muggles.elc: emacspeak-muggles.el dtk-speak.elc
extra-muggles.elc: extra-muggles.el dtk-speak.elc
emacspeak-navi-mode.elc: emacspeak-navi-mode.el dtk-speak.elc voice-setup.elc
emacspeak-net-utils.elc: emacspeak-net-utils.el dtk-speak.elc voice-setup.elc
emacspeak-newsticker.elc: emacspeak-newsticker.el dtk-speak.elc
emacspeak-nov.elc: emacspeak-nov.el dtk-speak.elc voice-setup.elc
emacspeak-nxml.elc: emacspeak-nxml.el dtk-speak.elc voice-setup.elc
emacspeak-org.elc: emacspeak-org.el dtk-speak.elc voice-setup.elc
emacspeak-orgalist.elc: emacspeak-orgalist.el dtk-speak.elc voice-setup.elc
emacspeak-origami.elc: emacspeak-origami.el dtk-speak.elc voice-setup.elc
emacspeak-outline.elc: emacspeak-outline.el dtk-speak.elc voice-setup.elc
emacspeak-ocr.elc: emacspeak-ocr.el dtk-speak.elc voice-setup.elc
emacspeak-package.elc: emacspeak-package.el dtk-speak.elc voice-setup.elc
emacspeak-paradox.elc: emacspeak-paradox.el dtk-speak.elc voice-setup.elc
emacspeak-perl.elc: emacspeak-perl.el dtk-speak.elc voice-setup.elc
emacspeak-pianobar.elc: emacspeak-pianobar.el dtk-speak.elc voice-setup.elc
emacspeak-pronounce.elc: emacspeak-pronounce.el
emacspeak-popup.elc: emacspeak-popup.el dtk-speak.elc
emacspeak-proced.elc: emacspeak-proced.el dtk-speak.elc voice-setup.elc
emacspeak-project.elc: emacspeak-project.el dtk-speak.elc
emacspeak-projectile.elc: emacspeak-projectile.el dtk-speak.elc
emacspeak-py.elc: emacspeak-py.el dtk-speak.elc voice-setup.elc
emacspeak-python.elc: emacspeak-python.el dtk-speak.elc voice-setup.elc
emacspeak-racket.elc: emacspeak-racket.el dtk-speak.elc voice-setup.elc
emacspeak-elpy.elc: emacspeak-elpy.el dtk-speak.elc
emacspeak-pydoc.elc: emacspeak-pydoc.el dtk-speak.elc
emacspeak-re-builder.elc: emacspeak-re-builder.el dtk-speak.elc voice-setup.elc
emacspeak-rg.elc: emacspeak-rg.el dtk-speak.elc voice-setup.elc
emacspeak-racer.elc: emacspeak-racer.el dtk-speak.elc voice-setup.elc
emacspeak-rust-mode.elc: emacspeak-rust-mode.el dtk-speak.elc voice-setup.elc
emacspeak-reftex.elc: emacspeak-reftex.el dtk-speak.elc voice-setup.elc
emacspeak-rst.elc: emacspeak-rst.el dtk-speak.elc voice-setup.elc
emacspeak-related.elc: emacspeak-related.el dtk-speak.elc voice-setup.elc
emacspeak-rpm-spec.elc: emacspeak-rpm-spec.el dtk-speak.elc voice-setup.elc
emacspeak-replace.elc: emacspeak-replace.el dtk-speak.elc voice-setup.elc
emacspeak-ruby.elc: emacspeak-ruby.el dtk-speak.elc
emacspeak-rmail.elc: emacspeak-rmail.el dtk-speak.elc voice-setup.elc
emacspeak-sage.elc: emacspeak-sage.el dtk-speak.elc
emacspeak-slime.elc: emacspeak-slime.el dtk-speak.elc
emacspeak-ses.elc: emacspeak-ses.el dtk-speak.elc voice-setup.elc
emacspeak-setup.elc: emacspeak-setup.el
emacspeak-sgml-mode.elc: emacspeak-sgml-mode.el dtk-speak.elc voice-setup.elc
emacspeak-sh-script.elc: emacspeak-sh-script.el dtk-speak.elc voice-setup.elc
emacspeak-shx.elc: emacspeak-shx.el dtk-speak.elc
emacspeak-smartparens.elc: emacspeak-smartparens.el dtk-speak.elc voice-setup.elc
emacspeak-smart-window.elc: emacspeak-smart-window.el dtk-speak.elc
emacspeak-solitaire.elc: emacspeak-solitaire.el dtk-speak.elc voice-setup.elc
emacspeak-sounds.elc: emacspeak-sounds.el
emacspeak-speak.elc: emacspeak-speak.el dtk-speak.elc voice-setup.elc
emacspeak-speedbar.elc: emacspeak-speedbar.el dtk-speak.elc voice-setup.elc
emacspeak-syslog.elc: emacspeak-syslog.el dtk-speak.elc voice-setup.elc
emacspeak-sql.elc: emacspeak-sql.el dtk-speak.elc voice-setup.elc
ladspa.elc: ladspa.el 
sox.elc: sox.el ladspa.elc
soundscape.elc: soundscape.el
sox-gen.elc: sox-gen.el sox.elc
emacspeak-sdcv.elc: emacspeak-sdcv.el dtk-speak.elc voice-setup.elc
emacspeak-selectrum.elc: emacspeak-selectrum.el dtk-speak.elc voice-setup.elc
emacspeak-supercite.elc: emacspeak-supercite.el dtk-speak.elc voice-setup.elc
emacspeak-sudoku.elc: emacspeak-sudoku.el dtk-speak.elc
emacspeak-table-ui.elc: emacspeak-table-ui.el emacspeak-tabulate.elc  \
emacspeak-table.elc
emacspeak-table.elc: emacspeak-table.el
emacspeak-tabulate.elc: emacspeak-tabulate.el dtk-speak.elc voice-setup.elc
emacspeak-etable.elc: emacspeak-etable.el dtk-speak.elc
emacspeak-tab-bar.elc: emacspeak-tab-bar.el dtk-speak.elc voice-setup.elc
emacspeak-tapestry.elc: emacspeak-tapestry.el dtk-speak.elc voice-setup.elc
emacspeak-tar.elc: emacspeak-tar.el dtk-speak.elc voice-setup.elc
emacspeak-tcl.elc: emacspeak-tcl.el dtk-speak.elc voice-setup.elc
emacspeak-tempo.elc: emacspeak-tempo.el dtk-speak.elc voice-setup.elc
emacspeak-tetris.elc: emacspeak-tetris.el dtk-speak.elc voice-setup.elc
emacspeak-threes.elc: emacspeak-threes.el dtk-speak.elc
emacspeak-tide.elc: emacspeak-tide.el dtk-speak.elc
emacspeak-2048.elc: emacspeak-2048.el dtk-speak.elc
emacspeak-texinfo.elc: emacspeak-texinfo.el dtk-speak.elc voice-setup.elc
emacspeak-transient.elc: emacspeak-transient.el dtk-speak.elc voice-setup.elc
emacspeak-todo-mode.elc: emacspeak-todo-mode.el dtk-speak.elc voice-setup.elc
emacspeak-twittering.elc: emacspeak-twittering.el dtk-speak.elc voice-setup.elc
emacspeak-typo.elc: emacspeak-typo.el dtk-speak.elc 
emacspeak-url-template.elc: emacspeak-url-template.el  emacspeak-we.elc
emacspeak-vdiff.elc: emacspeak-vdiff.el dtk-speak.elc voice-setup.elc
emacspeak-vuiet.elc: emacspeak-vuiet.el dtk-speak.elc voice-setup.elc
emacspeak-view.elc: emacspeak-view.el dtk-speak.elc voice-setup.elc
emacspeak-vterm.elc: emacspeak-vterm.el dtk-speak.elc voice-setup.elc
emacspeak-vm.elc: emacspeak-vm.el dtk-speak.elc voice-setup.elc
emacspeak-wdired.elc: emacspeak-wdired.el dtk-speak.elc voice-setup.elc
emacspeak-we.elc: emacspeak-we.el 
emacspeak-webspace.elc: emacspeak-webspace.el 
emacspeak-websearch.elc: emacspeak-websearch.el emacspeak-we.elc
emacspeak-webmarks.elc: emacspeak-webmarks.el emacspeak-we.elc
emacspeak-widget.elc: emacspeak-widget.el dtk-speak.elc voice-setup.elc
emacspeak-winring.elc: emacspeak-winring.el dtk-speak.elc voice-setup.elc
emacspeak-woman.elc: emacspeak-woman.el dtk-speak.elc
emacspeak-windmove.elc: emacspeak-windmove.el dtk-speak.elc voice-setup.elc
emacspeak-wizards.elc: emacspeak-wizards.el dtk-speak.elc voice-setup.elc
emacspeak-extras.elc: emacspeak-extras.el dtk-speak.elc voice-setup.elc
emacspeak-xref.elc: emacspeak-xref.el dtk-speak.elc
emacspeak-xslt.elc: emacspeak-xslt.el
emacspeak-yaml.elc: emacspeak-yaml.el dtk-speak.elc
emacspeak-yasnippet.elc: emacspeak-yasnippet.el dtk-speak.elc

emacspeak.elc: emacspeak.el
emacspeak-setup.elc: emacspeak-setup.el
loaddefs: emacspeak-loaddefs.el

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
