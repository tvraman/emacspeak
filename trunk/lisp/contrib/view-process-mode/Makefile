# $Id: Makefile,v 1.1 1996/10/15 22:51:07 raman Exp raman $
#
# Copyright (C) 1994, 1995, 1996 Heiko Muenkel
# email: muenkel@tnt.uni-hannover.de
#
# Makefile for the package HM--VIEW-PROCESS
#
# Type: 'make help' for informations about the main targets.
#
MAJOR_VERSION	= 2
MINOR_VERSION	= 4

TAR 		= tar
TAROPTIONS 	= --verbose --gzip --portability --create --file
CI		= ci
CO		= co
MAKE		= make
DELETE		= rm
ETAGS		= etags
EMACS		= emacs-19.34
BATCHFLAGS 	= -batch -q

TARFILE 	= hm--view-process-${MAJOR_VERSION}.${MINOR_VERSION}.tar.gz
PACKAGE-VERSION = V-${MAJOR_VERSION}-${MINOR_VERSION}
DIRECTORY-NAME 	= view-process-mode

PACKAGE-FILES	= ${DIRECTORY-NAME}/Makefile \
		  ${DIRECTORY-NAME}/ANNOUNCEMENT \
		  ${DIRECTORY-NAME}/INSTALL \
		  ${DIRECTORY-NAME}/LSM \
		  ${DIRECTORY-NAME}/README \
		  ${DIRECTORY-NAME}/view-process-adapt.el \
		  ${DIRECTORY-NAME}/view-process-emacs-19.el \
		  ${DIRECTORY-NAME}/view-process-mode.el \
		  ${DIRECTORY-NAME}/view-process-system-specific.el \
		  ${DIRECTORY-NAME}/view-process-xemacs.el

#		  ${DIRECTORY-NAME}/LCD-entry \


EL-FILES 	= $(filter %.el,$(notdir ${PACKAGE-FILES}))

ELC-FILES 	= $(EL-FILES:.el=.elc)

PRELOADS 	= -l view-process-mode.el

LOADDIR		= -eval '(setq load-path (cons "." load-path))'

RCS-FILES 	= $(notdir ${PACKAGE-FILES})

%.elc : %.el
	${EMACS} ${BATCHFLAGS} ${LOADDIR} ${PRELOADS} -f batch-byte-compile $<

# Targets

all: compile

compile: ${ELC-FILES}

distribution:
	${TAR} --directory .. ${TAROPTIONS} ${TARFILE} ${PACKAGE-FILES}

clean: clean-elc clean-backup clean-tar

clean-elc:
	${DELETE} ${ELC-FILES}

clean-backup:
	${DELETE} *~

clean-tar:
	${DELETE} *.tar.gz

ci-package:
	${CO} -l ${RCS-FILES}; \
	${CI}  -u -n${PACKAGE-VERSION} ${RCS-FILES}

ci:;	${CO} -l ${RCS-FILES}; \
	${CI} -u2 ${RCS-FILES}

co:;	${CO} -l ${RCS-FILES}

list:;	@echo "RCS-FILES:"; \
	@echo ${RCS-FILES}; \
	@echo "EL-FILES:"; \
	@echo ${EL-FILES}; \
	@echo "ELC-FILES:"; \
	@echo ${ELC-FILES}

tags:;	${ETAGS} *.el

help:;	@echo Package Release ${MAJOR_VERSION}.${MINOR_VERSION}
	@echo "Main Targets:"
	@echo "============="
	@echo "make all          to compile all lisp files"
	@echo "make ci-package   to check in all files to make a new RCS"
	@echo "                  package version"
	@echo "make clean        to delete all *.tar.gz, *~ and *.elc files"
	@echo "make distribution to make a tar file"
	@echo "make tags         to build a tags file"

