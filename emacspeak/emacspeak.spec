#$Id$cho
Summary: emacspeak -- The Complete Audio Desktop
Name: emacspeak
Version: 16.7600
Release: 1
Copyright: GPL
Group: Applications/Editors
Source: http://emacspeak.sf.net/emacspeak.tar.gz
URL: http://emacspeak.sf.net
Vendor: Emacspeak Inc 
Packager: T. V. Raman <raman@cs.cornell.edu>
Requires: emacs tcl tclx

%description 
Emacspeak is a speech interface that allows visually impaired users to
interact independently and efficiently with the computer. Available free of
cost on the Internet, Emacspeak has dramatically changed how the author and
hundreds of blind and visually impaired users around the world interact with
the personal computer and the Internet. A rich suite of task-oriented
speech-enabled tools provides efficient speech-enabled access to the evolving
semantic WWW. When combined with Linux running on low-cost PC hardware,
Emacspeak/Linux provides a reliable, stable speech-friendly solution that
opens up the Internet to visually impaired users around the world. 

%prep
%setup

%build
make  config SRC=`pwd`
make

%install
make install

%post 
cd /usr/share/emacs/site-lisp/emacspeak
find . -type d -print |xargs chmod 755
echo "Emacspeak is now installed on your system.
Note that this has installed the  Emacspeak speech server for
ViaVoice Outloud  --a software speech synthesis engine.
However, you need to obtain and install the ViaVoice Outloud
RPMs  before you can use software TTS with Emacspeak 
--See  file  docs/VIAVOICE for details.

To use Software Dectalk for Linux, see  docs/DTK.

NEWS file for a  summary of new features        --control e cap n in Emacs
FAQ for Frequently Asked Questions              -- control e cap F in Emacs
Custom for customizations              -- control e cap C in Emacs
Tips Tips for productivity tips            -- control e cap T in Emacs

Make sure you read the Emacs info pages
"

%files 
 /usr/share/emacs/site-lisp/emacspeak/

%doc servers/linux-outloud/VIAVOICE \
 servers/software-dtk/DTK \
 etc/NEWS \
 etc/NEWS-16.0 \
 etc/NEWS-15.0 \
 etc/NEWS-14.0 \
 etc/NEWS-13.0 \
 etc/NEWS-12.0 \
 etc/NEWS-11.0 \
 etc/NEWS-10.0 \
 etc/NEWS-9.0 \
 etc/NEWS-8.0 \
 etc/FAQ \
 etc/HELP \
 etc/COPYRIGHT \
 etc/tips.html \
 etc/applications.html \
 info/acknowledge.texi \
 info/announce.texi \
 info/audio-desktop.texi \
 info/copyright.texi \
 info/documents.texi \
 info/emacspeak.texi \
 info/eterm.texi \
 info/install.texi \
 info/introduction.texi \
 info/online-help.texi \
 info/packages.texi \
 info/preamble.texi \
 info/structure.texi \
 info/tts.texi \
 info/using.texi \
 info/commands.texi \
 user-guide/acknowledgments.html \
 user-guide/before-you-begin.html \
 user-guide/entertainment.html \
 user-guide/index.html \
 user-guide/introduction.html \
 user-guide/productivity.html \
 user-guide/system-administration.html \
 user-guide/working-online.html \
 user-guide/working-with-files.html \
 user-guide/espk-article.sgml
/usr/bin/emacspeak
emacspeak.info/usr/share/info/
/usr/share/info/emacspeak.info-1
/usr/share/info/emacspeak.info-2
/usr/share/info/emacspeak.info-3
/usr/share/info/emacspeak.info-4
/usr/share/info/emacspeak.info-5

# local variables:
# mode: rpm-spec
# end:
