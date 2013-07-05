#!/bin/bash

if (( $# != 1 ))
then
  echo "USAGE: $0 <path to vips>"
  exit 0
fi

BASE=`dirname "$0"`
BASE=`cd "$BASE" ; pwd`
BASE=`dirname "$BASE"`

cd $1
if (( $? != 0 ))
then
  echo "Cant change to the directory given: $1. Aborting."
  exit 1
fi

VIPS_D=`pwd`

if [ -d src/libvips ];then
    cd src/libvips
else
    echo "Something is wrong with the vips directory structure or $1 is not the path to vips! Aborting."
    exit 2
fi

PARSEC_D=$VIPS_D/../../../
LIBS_D=$PARSEC_D/pkgs/libs/

mkdir tmp 2>/dev/null

cp colour/im_LabQ2Lab.c        tmp
cp colour/im_Lab2LabQ.c        tmp
cp colour/im_Lab2XYZ.c         tmp
cp colour/im_XYZ2Lab.c         tmp
cp conversion/im_extract.c     tmp
cp conversion/im_black.c       tmp
cp convolution/im_sharpen.c    tmp
cp relational/relational.c     tmp
cp relational/im_ifthenelse.c  tmp
cp resample/im_affine.c        tmp
cp arithmetic/im_lintra.c      tmp
cp arithmetic/im_recomb.c      tmp

cd tmp
cat * > ../../tools/iofuncs/some_libvips.c && rm * && cd ../../tools/iofuncs/
# make the assembly file:
printf "\n\nmaking some_libvips.s in: `pwd` \n\n"
gcc -std=gnu99 -DHAVE_CONFIG_H -I . -I ../../ -I ../../libvips/include -DG_DISABLE_CAST_CHECKS -pthread -I $LIBS_D/libxml2/inst/amd64-linux.gcc/include/libxml2 -I $LIBS_D/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -I $LIBS_D/glib/inst/amd64-linux.gcc/include/glib-2.0/  -I ../../../obj/amd64-linux.gcc/  -O3 -g -funroll-loops -fprefetch-loop-arrays -static-libgcc -Wl,--hash-style=both,--as-needed -DPARSEC_VERSION=3.0-beta-20120904 -L ../../../inst/amd64-linux.gcc/lib/  -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/zlib/inst/amd64-linux.gcc/lib -L /usr/lib64 -L /usr/lib  -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/zlib/inst/amd64-linux.gcc/lib -DG_DISABLE_CAST_CHECKS -I $LIBS_D/libxml2/inst/amd64-linux.gcc/include/libxml2 -I $LIBS_D/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/libxml2/inst/amd64-linux.gcc/lib -S some_libvips.c  -lgobject-2.0 -lgthread-2.0 -pthread   -lm  -lstdc++ -lxml2 -lgmodule-2.0 -ldl -lglib-2.0
case "$?" in
  0) : ;;
  *) exit $? ;;
esac

$BASE/bin/link-vips -o vips some_libvips.s

