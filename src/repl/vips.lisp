(in-package :optimize)

(setf *orig* (from-file (make-instance 'asm-perf) "benchmarks/vips/vips.s")
      *script* "run vips ~a -p"
      (linker *orig*) "link-vips"
      *num-threads* 16
      *tournament-size* 4
      *evals* 15000
      *work-dir* "sh-runner/work"
      *fitness-function* (case (arch)
                           (:intel 'intel-sandybridge-energy-model)
                           (:amd   'amd-opteron-energy-model))
      (fitness *orig*) (or (test *orig*) (error "bad fitness"))
      *max-population-size* 512
      *period* 256
      *res-dir* '(:RELATIVE "results" "vips-energy-2"))

(setf *population* (loop :for n :below *max-population-size*
                      :collect (copy *orig*)))

;; kick off optimization threads
(loop :for n :below *num-threads* :do
   (push (make-thread #'do-optimize) *threads*))


;;; Memory usage at the beginning of the run
#|

OPTIMIZE> (sb-vm::memory-usage :print-spaces t :print-summary nil)

Breakdown for dynamic space:
  114,872,768 bytes for 7,179,548 cons objects.
  102,302,400 bytes for 1,160,451 instance objects.
  100,780,080 bytes for   710,029 simple-character-string objects.
  58,388,736 bytes for   503,074 simple-vector objects.
  19,244,416 bytes for    20,643 code objects.
  16,625,232 bytes for   512,126 simple-bit-vector objects.
  15,556,304 bytes for    32,521 simple-array-unsigned-fixnum objects.
  14,418,176 bytes for    48,512 simple-array-unsigned-byte-64 objects.
  10,956,896 bytes for   310,624 closure objects.
   4,502,560 bytes for    49,130 simple-array-unsigned-byte-8 objects.
   3,937,472 bytes for    61,523 symbol objects.
   2,228,656 bytes for    50,919 simple-base-string objects.
   1,673,904 bytes for    64,900 bignum objects.
   1,456,752 bytes for    91,047 value-cell objects.
     929,216 bytes for    58,076 sap objects.
     725,856 bytes for    22,683 fdefn objects.
     645,008 bytes for     8,063 array-header objects.
     342,656 bytes for     2,322 simple-array-unsigned-byte-16 objects.
     321,840 bytes for         5 simple-array-unsigned-byte-31 objects.
     249,584 bytes for     4,557 funcallable-instance objects.
      48,288 bytes for         3 simple-array-unsigned-byte-7 objects.
      36,256 bytes for        47 simple-array-unsigned-byte-32 objects.
      31,296 bytes for     1,780 simple-array-fixnum objects.
      28,896 bytes for       903 weak-pointer objects.
       3,392 bytes for       212 double-float objects.
       1,616 bytes for         5 simple-array-signed-byte-16 objects.
         768 bytes for        24 ratio objects.
          64 bytes for         2 complex-double-float objects.
          32 bytes for         1 complex object.
          32 bytes for         2 complex-single-float objects.
          32 bytes for         2 simple-array-nil objects.
          16 bytes for         1 simple-array-unsigned-byte-2 object.
          16 bytes for         1 simple-array-unsigned-byte-4 object.
          16 bytes for         1 simple-array-unsigned-byte-15 object.
          16 bytes for         1 simple-array-unsigned-byte-63 object.
          16 bytes for         1 simple-array-signed-byte-8 object.
          16 bytes for         1 simple-array-signed-byte-32 object.
          16 bytes for         1 simple-array-signed-byte-64 object.
          16 bytes for         1 simple-array-single-float object.
          16 bytes for         1 simple-array-double-float object.
          16 bytes for         1 simple-array-complex-single-float object.
          16 bytes for         1 simple-array-complex-double-float object.
  470,309,360 bytes for 10,893,745 dynamic objects (space total.)
; No value
OPTIMIZE> (sb-vm:instance-usage :dynamic :top-n 100)

Top 100 dynamic instance types:
  SB-C:TN: 9,990,640 bytes, 56,765 objects.
  SB-C:TN-REF: 7,383,680 bytes, 92,296 objects.
  SB-C::LAMBDA-VAR: 6,099,072 bytes, 31,766 objects.
  SB-C::COMPILED-DEBUG-FUN: 5,688,032 bytes, 50,786 objects.
  SB-C::CBLOCK: 5,021,280 bytes, 28,530 objects.
  SB-C::LVAR: 4,648,240 bytes, 58,103 objects.
  SB-C::REF: 4,617,424 bytes, 41,227 objects.
  SB-IMPL::STRING-OUTPUT-STREAM: 4,454,400 bytes, 27,840 objects.
  PATHNAME: 4,220,224 bytes, 65,941 objects.
  SB-C::CTRAN: 3,945,744 bytes, 82,203 objects.
  SB-C::VOP: 3,288,880 bytes, 29,365 objects.
  SB-C::GLOBAL-CONFLICTS: 3,009,280 bytes, 37,616 objects.
  SB-KERNEL:LEXENV: 2,973,264 bytes, 26,547 objects.
  HASH-TABLE: 2,680,640 bytes, 16,754 objects.
  SB-C::CLAMBDA: 2,257,920 bytes, 7,056 objects.
  SB-C::COMBINATION: 2,084,400 bytes, 14,475 objects.
  SB-ASSEM:SSET: 1,907,616 bytes, 39,742 objects.
  RESTART: 1,776,320 bytes, 27,755 objects.
  SB-C::CONSET: 1,736,160 bytes, 36,170 objects.
  SB-KERNEL:VALUES-TYPE: 1,444,320 bytes, 15,045 objects.
  SB-KERNEL:FUN-TYPE: 1,281,616 bytes, 11,443 objects.
  SB-C::IR2-BLOCK: 1,165,824 bytes, 6,624 objects.
  SB-C::COMPILED-DEBUG-INFO: 990,864 bytes, 20,643 objects.
  SB-ALIEN-INTERNALS:ALIEN-VALUE: 927,872 bytes, 28,996 objects.
  SB-C::IR2-LVAR: 917,376 bytes, 19,112 objects.
  SB-THREAD:MUTEX: 893,232 bytes, 18,609 objects.
  SB-VM::EA: 847,680 bytes, 13,245 objects.
  SB-INT:XSET: 803,360 bytes, 25,105 objects.
  SB-ALIEN:CAST: 783,488 bytes, 6,121 objects.
  SB-C::CONSTRAINT: 673,856 bytes, 10,529 objects.
  SB-C::CRETURN: 672,768 bytes, 7,008 objects.
  SB-C::GLOBAL-VAR: 664,832 bytes, 5,936 objects.
  SB-FORMAT::FORMAT-DIRECTIVE: 663,520 bytes, 8,294 objects.
  SB-ASSEM:LABEL: 641,696 bytes, 20,053 objects.
  SB-KERNEL:NUMERIC-TYPE: 565,440 bytes, 5,890 objects.
  SB-C:BIND: 562,720 bytes, 7,034 objects.
  SB-C:DEFINITION-SOURCE-LOCATION: 507,168 bytes, 10,566 objects.
  SB-KERNEL:CONSTANT: 486,528 bytes, 4,344 objects.
  SB-C::CIF: 421,344 bytes, 3,762 objects.
  SB-C::TAIL-SET: 401,088 bytes, 8,356 objects.
  SB-SYS:DEADLINE-TIMEOUT: 398,016 bytes, 12,438 objects.
  SB-PCL::FAST-METHOD-CALL: 396,240 bytes, 8,255 objects.
  SB-KERNEL:MEMBER-TYPE: 359,232 bytes, 5,613 objects.
  SB-ASSEM::BACK-PATCH: 338,448 bytes, 7,051 objects.
  SB-ASSEM::CHOOSER: 284,096 bytes, 4,439 objects.
  SB-SYS:FD-STREAM: 272,080 bytes, 895 objects.
  SB-ASSEM:SEGMENT: 254,592 bytes, 1,326 objects.
  SB-PCL::SLOT-INFO: 253,104 bytes, 5,273 objects.
  SB-PRETTY::NEWLINE: 221,952 bytes, 4,624 objects.
  SB-C::LOCATION-INFO: 221,760 bytes, 4,620 objects.
  SB-C::VOP-INFO: 215,552 bytes, 842 objects.
  SB-C::OPERAND-PARSE: 203,280 bytes, 1,815 objects.
  SB-C::VOP-PARSE: 202,080 bytes, 842 objects.
  SB-PCL::ARG-INFO: 174,832 bytes, 1,561 objects.
  SB-KERNEL:DEFSTRUCT-SLOT-DESCRIPTION: 173,680 bytes, 2,171 objects.
  SB-C:COMPONENT: 171,200 bytes, 1,070 objects.
  SB-PCL::INITIAL: 161,792 bytes, 5,056 objects.
  SB-PRETTY::BLOCK-START: 160,960 bytes, 2,515 objects.
  SB-ASSEM::FILLER: 123,216 bytes, 2,567 objects.
  SB-KERNEL::CONDITION-SLOT: 113,568 bytes, 1,183 objects.
  SB-C::FUN-INFO: 112,112 bytes, 1,001 objects.
  SB-KERNEL:UNION-TYPE: 109,504 bytes, 1,711 objects.
  SB-C::ENTRY: 108,192 bytes, 1,127 objects.
  SB-C::CLOOP: 107,904 bytes, 1,124 objects.
  SB-C::IR2-PHYSENV: 104,448 bytes, 1,088 objects.
  SB-C::DEBUG-SOURCE: 102,320 bytes, 1,279 objects.
  SB-C::INTERVAL: 102,304 bytes, 3,197 objects.
  SB-KERNEL:ARRAY-TYPE: 101,600 bytes, 1,270 objects.
  SB-KERNEL:LAYOUT: 95,760 bytes, 855 objects.
  SB-PCL::CACHE: 92,640 bytes, 1,158 objects.
  SB-PRETTY::LOGICAL-BLOCK: 91,968 bytes, 1,437 objects.
  SB-PRETTY::BLOCK-END: 80,352 bytes, 2,511 objects.
  SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION: 74,752 bytes, 2,336 objects.
  SB-PRETTY:PRETTY-STREAM: 70,304 bytes, 338 objects.
  SB-KERNEL:KEY-INFO: 67,808 bytes, 2,119 objects.
  SB-C::CSET: 67,200 bytes, 600 objects.
  SB-DISASSEM:INSTRUCTION: 67,088 bytes, 599 objects.
  STANDARD-METHOD: 65,696 bytes, 2,053 objects.
  SB-C::IR2-COMPONENT: 63,648 bytes, 442 objects.
  SB-PRETTY::INDENTATION: 63,552 bytes, 1,324 objects.
  SB-KERNEL::CONDITION-CLASSOID: 63,504 bytes, 441 objects.
  SB-IMPL::STRING-INPUT-STREAM: 62,720 bytes, 490 objects.
  SB-KERNEL::CLASSOID-CELL: 61,296 bytes, 1,277 objects.
  SB-KERNEL:DEFSTRUCT-DESCRIPTION: 57,904 bytes, 329 objects.
  SB-EXT:EXIT: 57,568 bytes, 514 objects.
  SB-C::DEFINED-FUN: 57,344 bytes, 448 objects.
  SB-KERNEL:CONS-TYPE: 56,000 bytes, 875 objects.
  SB-ASSEM::ALIGNMENT-NOTE: 56,000 bytes, 875 objects.
  SB-C::CLEANUP: 55,632 bytes, 1,159 objects.
  SB-C::PHYSENV: 54,336 bytes, 1,132 objects.
  SB-PCL::CONDITION-EFFECTIVE-SLOT-DEFINITION: 52,832 bytes, 1,651 objects.
  SB-C::OPTIONAL-DISPATCH: 51,456 bytes, 201 objects.
  SB-PCL::STRUCTURE-DIRECT-SLOT-DEFINITION: 50,016 bytes, 1,563 objects.
  SB-PCL::WRAPPER: 42,368 bytes, 331 objects.
  SB-C::ARG-INFO: 41,536 bytes, 649 objects.
  SB-C::FUNCTIONAL: 41,280 bytes, 215 objects.
  SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION: 41,024 bytes, 1,282 objects.
  SB-PCL::PV-TABLE: 36,192 bytes, 754 objects.
  SB-PCL::CLASS-EQ-SPECIALIZER: 35,712 bytes, 1,116 objects.
  SB-KERNEL:STRUCTURE-CLASSOID: 35,280 bytes, 315 objects.
  Other types: 847,136 bytes, 16,924 objects.
  Dynamic instance total: 102,405,776 bytes, 1,161,918 objects.
; No value
OPTIMIZE> 

|#
