#!/bin/sh
#
# run the OCaml optimize with appropriate arguments
#
./optimize \
    --program ../data/fft.s \
    --compiler ecc \
    --test-script ../src/host-test.sh \
    --test-command __TEST_SCRIPT__ __SOURCE_NAME__ __FITNESS_FILE__ \
    --optimize-feature time-wo-init
    # --tournament-size 2 \
