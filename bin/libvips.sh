#!/bin/bash

if (( $# != 1 ))
then
    printf "USAGE: $0 <path to vips>"
    exit 0
fi

cd $1
if (( $? != 0 ))
then
  echo "Cant change to the directory given: $1. Aborting."
  exit 1
fi

cd src/libvips

if (( $? != 0 ))
then
  echo "Something is wrong with the vips directory structure or $1 is not the path to vips! Aborting."
  exit 2
fi

mkdir tmp 2>/dev/null

mv colour/im_LabQ2Lab.c        tmp
mv colour/im_Lab2LabQ.c        tmp
mv colour/im_Lab2XYZ.c         tmp
mv colour/im_XYZ2Lab.c         tmp
mv conversion/im_extract.c     tmp
mv conversion/im_black.c       tmp
mv convolution/im_sharpen.c    tmp
mv relational/relational.c     tmp
mv relational/im_ifthenelse.c  tmp
mv resample/im_affine.c        tmp
mv arithmetic/im_lintra.c      tmp
mv arithmetic/im_recomb.c      tmp

cd tmp
cat * > some_libvips.c

# make the assembly file:
gcc -std=gnu99 -DHAVE_CONFIG_H -I. -I../../ -I../../libvips/include -DG_DISABLE_CAST_CHECKS -pthread -I ../../../../../libs/libxml2/inst/amd64-linux.gcc/include/libxml2 -I../../../../../libs/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -I../../../../../libs/glib/inst/amd64-linux.gcc/include/glib-2.0/  -I../../../obj/amd64-linux.gcc/  -O3 -g -funroll-loops -fprefetch-loop-arrays -static-libgcc -Wl,--hash-style=both,--as-needed -DPARSEC_VERSION=3.0-beta-20120904 -L../../../inst/amd64-linux.gcc/lib/  -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/zlib/inst/amd64-linux.gcc/lib -L/usr/lib64 -L/usr/lib  -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/zlib/inst/amd64-linux.gcc/lib -DG_DISABLE_CAST_CHECKS -I../../../../../libs/libxml2/inst/amd64-linux.gcc/include/libxml2 -I../../../../../libs/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/libxml2/inst/amd64-linux.gcc/lib -S ../../some_libvips.c  -lgobject-2.0 -lgthread-2.0 -pthread   -lm  -lstdc++ -lxml2 -lgmodule-2.0 -ldl -lglib-2.0

# make the object file:
