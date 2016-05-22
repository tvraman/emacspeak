#!/bin/sh

set -e

PACKAGE_NAME=emacspeak
TMP_DIR=`/bin/mktemp -d -t emacspeak.XXXXXX` || exit 1
ORIG_PATH=$(pwd)

while test $# -gt 0
do
    case $1 in
	--upstream-version)
	    shift
	    VERSION=$1
	    ;;
	*)
	    ORIG_SRC_TAR=$(readlink -m $1)
	    ;;
    esac
    shift
done

ORIG_SRC_DIR=${PACKAGE_NAME}-${VERSION}
DEB_SRC_DIR=${PACKAGE_NAME}-${VERSION}+dfsg
DEB_SRC_TAR=${PACKAGE_NAME}_${VERSION}+dfsg.orig.tar.bz2

cd ${TMP_DIR}
tar -axf ${ORIG_SRC_TAR}
mv ${ORIG_SRC_DIR} ${DEB_SRC_DIR}
cd ${DEB_SRC_DIR}

# This is the real reason for repack: the license is non-dfsg
rm -rf info
# Remove accidental backup files
find . -name "*~" -delete
# patches are already applied and don't need to go in
rm -rf lisp/g-client/patches

cd ..
tar -acf ${DEB_SRC_TAR} ${DEB_SRC_DIR}
cd ${ORIG_PATH}
mv ${TMP_DIR}/${DEB_SRC_TAR} ./
rm -rf ${TMP_DIR}
