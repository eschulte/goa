#!/bin/bash

if (( $# != 1 ))
then
  echo "USAGE: $0 <path to vips>"
  exit 0
fi

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

#cd src/libvips
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

# make the object file:
printf "\n\nmaking some_libvips.o in: `pwd` \n\n"
gcc -L ../../../inst/amd64-linux.gcc/lib/  -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/zlib/inst/amd64-linux.gcc/lib -L /usr/lib64 -L /usr/lib  -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/zlib/inst/amd64-linux.gcc/lib -DG_DISABLE_CAST_CHECKS -I $LIBS_D/libxml2/inst/amd64-linux.gcc/include/libxml2 -I $LIBS_D/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/libxml2/inst/amd64-linux.gcc/lib -c some_libvips.s  -lgobject-2.0 -lgthread-2.0 -pthread   -lm  -lstdc++ -lxml2 -lgmodule-2.0 -ldl -lglib-2.0
case "$?" in
  0) : ;;
  *) exit $? ;;
esac

printf "\nleaving `pwd`\n"
cd ../../libvips/

printf "\n\nmaking libvips.o in: `pwd` \n\n"
gcc-4.4 -std=gnu99 -fno-inline -DHAVE_CONFIG_H -I ./include -DG_DISABLE_CAST_CHECKS -pthread -I $LIBS_D/libxml2/inst/amd64-linux.gcc/include/libxml2 -I $LIBS_D/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -I $LIBS_D/glib/inst/amd64-linux.gcc/include/glib-2.0/ -I ./iofuncs/  -I ../../obj/amd64-linux.gcc/  -O3 -g -funroll-loops -fprefetch-loop-arrays -static-libgcc -Wl,--hash-style=both,--as-needed -DPARSEC_VERSION=3.0-beta-20120904 -L ../../inst/amd64-linux.gcc/lib/  -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/zlib/inst/amd64-linux.gcc/lib -L /usr/lib64 -L /usr/lib  -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/zlib/inst/amd64-linux.gcc/lib -DG_DISABLE_CAST_CHECKS -I $LIBS_D/libxml2/inst/amd64-linux.gcc/include/libxml2 -I $LIBS_D/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -L $LIBS_D/glib/inst/amd64-linux.gcc/lib -L $LIBS_D/libxml2/inst/amd64-linux.gcc/lib -combine -c acquire/im_clamp.c resample/interpolate.c resample/im_stretch3.c resample/im_shrink.c resample/resample_dispatch.c resample/transform.c resample/im_rightshift_size.c arithmetic/im_stats.c arithmetic/im_linreg.c arithmetic/im_maxpos.c arithmetic/im_divide.c arithmetic/im_sign.c arithmetic/im_bandmean.c arithmetic/im_avg.c arithmetic/im_maxpos_avg.c arithmetic/im_remainder.c arithmetic/im_add.c arithmetic/arith_dispatch.c arithmetic/im_minpos.c arithmetic/im_abs.c arithmetic/im_cross_phase.c arithmetic/im_multiply.c arithmetic/im_deviate.c arithmetic/im_point_bilinear.c arithmetic/im_maxpos_vec.c arithmetic/round.c arithmetic/im_invert.c arithmetic/math.c arithmetic/im_measure.c arithmetic/im_subtract.c arithmetic/power.c boolean/bool_dispatch.c boolean/boolean.c colour/im_LabQ2disp.c colour/im_Yxy2XYZ.c colour/im_Lab2LabS.c colour/im_dE_fromLab.c colour/im_LCh2Lab.c colour/disp.c colour/im_disp2XYZ.c colour/colour_dispatch.c colour/im_icc_transform.c colour/im_LCh2UCS.c colour/im_LabQ2LabS.c colour/im_lab_morph.c colour/im_dE00_fromLab.c colour/im_LabS2Lab.c colour/im_dECMC_fromLab.c colour/im_XYZ2Yxy.c colour/derived.c colour/im_UCS2LCh.c colour/im_Lab2LCh.c colour/im_rad2float.c colour/im_LabS2LabQ.c colour/im_float2rad.c colour/colour.c colour/im_XYZ2disp.c conversion/im_scale.c conversion/im_flipver.c conversion/im_subsample.c conversion/im_lrjoin.c conversion/im_copy_file.c conversion/im_system.c conversion/im_ri2c.c conversion/im_c2imag.c conversion/im_embed.c conversion/im_clip2fmt.c conversion/im_insert.c conversion/im_c2amph.c conversion/im_system_image.c conversion/conver_dispatch.c conversion/im_c2real.c conversion/im_mask2vips.c conversion/im_copy.c conversion/im_msb.c conversion/im_tbjoin.c conversion/im_c2rect.c conversion/im_replicate.c conversion/im_grid.c conversion/im_rot90.c conversion/im_scaleps.c conversion/im_text.c conversion/im_rot180.c conversion/im_rot270.c conversion/im_vips2mask.c conversion/im_zoom.c conversion/im_wrap.c conversion/im_fliphor.c conversion/im_gbandjoin.c conversion/im_falsecolour.c conversion/im_gaussnoise.c convolution/im_conv_f.c convolution/im_conv.c convolution/im_convsep.c convolution/convol_dispatch.c convolution/im_gradcor.c convolution/im_contrast_surface.c convolution/im_addgnoise.c convolution/im_fastcor.c convolution/im_spcor.c convolution/im_compass.c convolution/im_convsep_f.c deprecated/im_thresh.c deprecated/im_fav4.c deprecated/im_setbox.c deprecated/im_cmulnorm.c deprecated/im_bernd.c deprecated/im_litecor.c deprecated/deprecated_dispatch.c deprecated/im_gaddim.c deprecated/im_print.c deprecated/im_line.c deprecated/im_printlines.c deprecated/im_gadd.c deprecated/im_resize_linear.c deprecated/im_debugim.c deprecated/im_slice.c deprecated/im_convsub.c deprecated/im_gfadd.c deprecated/rename.c format/im_png2vips.c format/format_dispatch.c format/im_vips2jpeg.c format/matlab.c format/im_jpeg2vips.c format/im_magick2vips.c format/radiance.c format/im_vips2raw.c format/im_exr2vips.c format/im_raw2vips.c format/im_tiff2vips.c format/im_csv2vips.c format/im_vips2png.c format/format.c format/im_vips2tiff.c format/im_tile_cache.c format/im_analyze2vips.c format/im_vips2ppm.c format/im_ppm2vips.c format/im_vips2csv.c freq_filt/im_phasecor_fft.c freq_filt/fft_sp.c freq_filt/im_freqflt.c freq_filt/im_freq_mask.c freq_filt/im_invfftr.c freq_filt/im_rotquad.c freq_filt/im_disp_ps.c freq_filt/im_fractsurf.c freq_filt/fmaskcir.c freq_filt/freq_dispatch.c freq_filt/im_invfft.c freq_filt/im_fwfft.c freq_filt/fmask4th.c histograms_lut/im_maplut.c histograms_lut/im_invertlut.c histograms_lut/im_gammacorrect.c histograms_lut/im_histgr.c histograms_lut/im_histplot.c histograms_lut/im_histindexed.c histograms_lut/im_mpercent.c histograms_lut/im_hsp.c histograms_lut/hist_dispatch.c histograms_lut/im_identity.c histograms_lut/im_heq.c histograms_lut/im_hist.c histograms_lut/im_histnD.c histograms_lut/im_buildlut.c histograms_lut/tone.c histograms_lut/im_histeq.c histograms_lut/im_project.c histograms_lut/im_lhisteq.c histograms_lut/im_stdif.c histograms_lut/im_histspec.c inplace/im_plotmask.c inplace/plot_point.c inplace/im_circle.c inplace/im_paintrect.c inplace/im_insertplace.c inplace/smudge_area.c inplace/line_draw.c inplace/flood.c inplace/inplace_dispatch.c iofuncs/time.c iofuncs/buf.c iofuncs/im_guess_prefix.c iofuncs/init.c iofuncs/sinkdisc.c iofuncs/sink.c iofuncs/base64.c iofuncs/im_prepare.c iofuncs/debug.c iofuncs/im_wrapmany.c iofuncs/im_unmapfile.c iofuncs/package.c iofuncs/im_init_world.c iofuncs/rect.c iofuncs/im_cp_desc.c iofuncs/sinkscreen.c iofuncs/check.c iofuncs/window.c iofuncs/threadpool.c iofuncs/buffer.c iofuncs/im_mapfile.c iofuncs/im_demand_hint.c iofuncs/im_histlin.c iofuncs/semaphore.c iofuncs/im_setbuf.c iofuncs/im_open_vips.c iofuncs/im_open.c iofuncs/region.c iofuncs/object.c iofuncs/im_generate.c iofuncs/sinkmemory.c iofuncs/im_close.c iofuncs/im_setupout.c iofuncs/memory.c iofuncs/dispatch_types.c iofuncs/im_image.c iofuncs/callback.c iofuncs/meta.c iofuncs/header.c iofuncs/util.c iofuncs/im_partial.c iofuncs/im_binfile.c iofuncs/im_writeline.c iofuncs/error.c mask/im_mattrn.c mask/im_matmul.c mask/rw_mask.c mask/im_gaussmasks.c mask/im_logmasks.c mask/im_matcat.c mask/mask_dispatch.c mask/matalloc.c mask/im_matinv.c mask/rotmask.c morphology/im_rank.c morphology/im_erode.c morphology/im_profile.c morphology/im_label_regions.c morphology/im_dilate.c morphology/im_zerox.c morphology/morph_dispatch.c morphology/im_rank_image.c morphology/im_cntlines.c mosaicing/im_initialize.c mosaicing/im_improve.c mosaicing/mosaic1.c mosaicing/im_tbmosaic.c mosaicing/im_avgdxdy.c mosaicing/im_lrcalcon.c mosaicing/match.c mosaicing/im_tbcalcon.c mosaicing/global_balance.c mosaicing/im_maxpos_subpel.c mosaicing/im_lrmosaic.c mosaicing/im_align_bands.c mosaicing/im_lrmerge.c mosaicing/im_clinear.c mosaicing/im_chkpair.c mosaicing/mosaicing_dispatch.c mosaicing/im_remosaic.c mosaicing/im_tbmerge.c other/glds_funcs.c other/im_benchmark.c other/im_eye.c other/im_spatres.c other/im_make_xy.c other/im_grey.c other/im_zone.c other/im_dif_std.c other/other_dispatch.c other/im_simcontr.c other/cooc_funcs.c other/im_sines.c other/im_meanstd.c relational/relational_dispatch.c relational/im_blend.c video/video_dispatch.c video/im_video_v4l1.c video/im_video_test.c -o ../tools/iofuncs/libvips.o -lgobject-2.0 -lgthread-2.0 -pthread   -lm  -lstdc++ -lxml2 -lgmodule-2.0 -ldl -lglib-2.0
case "$?" in
  0) : ;;
  *) exit $? ;;
esac


echo "Leaving `pwd`"
cd ../tools/iofuncs/
echo "Entering `pwd` to build vips"
# make vips.o
gcc -std=gnu99 -DHAVE_CONFIG_H -I. -I../../ -I../../libvips/include -DG_DISABLE_CAST_CHECKS -pthread -I ../../../../../libs/libxml2/inst/amd64-linux.gcc/include/libxml2 -I../../../../../libs/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -I../../../../../libs/glib/inst/amd64-linux.gcc/include/glib-2.0/  -I../../../obj/amd64-linux.gcc/  -O3 -g -funroll-loops -fprefetch-loop-arrays -static-libgcc -Wl,--hash-style=both,--as-needed -DPARSEC_VERSION=3.0-beta-20120904 -L../../../inst/amd64-linux.gcc/lib/  -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/zlib/inst/amd64-linux.gcc/lib -L/usr/lib64 -L/usr/lib  -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/zlib/inst/amd64-linux.gcc/lib -DG_DISABLE_CAST_CHECKS -I../../../../../libs/libxml2/inst/amd64-linux.gcc/include/libxml2 -I../../../../../libs/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/libxml2/inst/amd64-linux.gcc/lib -c vips.c  -lgobject-2.0 -lgthread-2.0 -pthread -lm  -lstdc++ -lxml2 -lgmodule-2.0 -ldl -lglib-2.0 -lrt -lz
case "$?" in
  0) : ;;
  *) exit $? ;;
esac

# make vips
gcc -std=gnu99 -DHAVE_CONFIG_H -I. -I../../ -I../../libvips/include -DG_DISABLE_CAST_CHECKS -pthread -I ../../../../../libs/libxml2/inst/amd64-linux.gcc/include/libxml2 -I../../../../../libs/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -I../../../../../libs/glib/inst/amd64-linux.gcc/include/glib-2.0/  -I../../../obj/amd64-linux.gcc/  -O3 -g -funroll-loops -fprefetch-loop-arrays -static-libgcc -Wl,--hash-style=both,--as-needed -DPARSEC_VERSION=3.0-beta-20120904 -L../../../inst/amd64-linux.gcc/lib/  -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/zlib/inst/amd64-linux.gcc/lib -L/usr/lib64 -L/usr/lib  -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/zlib/inst/amd64-linux.gcc/lib -DG_DISABLE_CAST_CHECKS -I../../../../../libs/libxml2/inst/amd64-linux.gcc/include/libxml2 -I../../../../../libs/glib/inst/amd64-linux.gcc/lib/glib-2.0/include -L../../../../../libs/glib/inst/amd64-linux.gcc/lib -L../../../../../libs/libxml2/inst/amd64-linux.gcc/lib -o vips vips.o some_libvips.o libvips.o -lgobject-2.0 -lgthread-2.0 -pthread -lm  -lstdc++ -lxml2 -lgmodule-2.0 -ldl -lglib-2.0 -lrt -lz
case "$?" in
  0) : ;;
  *) exit $? ;;
esac

rm -rf $VIPS_D/src/libvips/tmp

if (( $? == 0 )); then
  echo "Done!"
else
  echo "Looks like something didn't work out."
fi
