#$Id$cho
Summary: software-dtk --Dectalk Software TTS Server for Emacspeak
Name: software-dtk
Version: 1.0
Release: 1
Copyright: GPL
Group: Applications/TTS
Source: http://emacspeak.sf.net/software-dtk.tar.gz
URL: http://emacspeak.sf.net
Vendor: Emacspeak Inc 
Packager: T. V. Raman <raman@cs.cornell.edu>
Requires:  emacs emacspeak tcl tclx

%description 
Software Dectalk TTS is a commercial product. This package provides a
speech server needed for using Software Dectalk with Emacspeak.
Emacspeak is a speech interface that allows visually impaired users to
interact independently and efficiently with the computer. Available
free of cost on the Internet, Emacspeak has dramatically changed how
the author and hundreds of blind and visually impaired users around
the world interact with the personal computer and the Internet. A rich
suite of task-oriented speech-enabled tools provides efficient
speech-enabled access to the evolving semantic WWW. When combined with
Linux running on low-cost PC hardware, Emacspeak/Linux provides a
reliable, stable speech-friendly solution that opens up the Internet
to visually impaired users around the world.

%prep
%setup

%build
make

%install
make install

%files 
 /usr/share/emacs/site-lisp/emacspeak/servers/software-dtk/

# local variables:
# mode: rpm-spec
# end:
