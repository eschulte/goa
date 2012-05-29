#!/bin/sh
#
# run the OCaml optimize with appropriate arguments
#
./optimize \
    --program ../data/fft.s \
    --compiler ecc \
    --sanity no \
    --pos-tests 0 \
    --fault-scheme uniform \
    --fix-scheme uniform \
    --test-script ../src/host-test.sh \
    --test-command "__TEST_SCRIPT__ __SOURCE_NAME__ > __FITNESS_FILE__" \
    --optimize-feature time-wo-init
    # --test-script ../src/host-test.sh \
    # --tournament-size 2 \
