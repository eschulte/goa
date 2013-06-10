(in-package :optimize)

(setf
 *orig* (from-file (make-instance 'asm-perf) "benchmarks/h264ref/h264ref.s")
 *benchmark* "h264ref"
 (flags *orig*) '("-lm" "-O3")
 *model* (case (arch)
           (:intel 'intel-sandybridge-energy-model)
           (:amd   'amd-opteron-energy-model))
 *model* (eval *model*)
 (fitness *orig*) (test *orig*)
 *max-population-size* (expt 2 4)
 *population* (loop :for n :below *max-population-size*
                 :collect (copy *orig*))
 *evals* (expt 2 12))

(sb-thread:make-thread (lambda () (evolve #'test :max-evals 256)))

(defvar begin-memory (top-memory-instances :dynamic :top-n 200))

(defun print-memory ()
  (sb-ext:gc :full t :force t)
  (format *standard-output* "~&#|~%")
  (sb-vm::memory-usage :print-spaces t :print-summary nil)
  (sb-vm:instance-usage :dynamic :top-n 200)
  (format *standard-output* "~&|#~%"))


;;; Memory Usage
#|

;; in the beginning
Breakdown for dynamic space:
  430,292,496 bytes for 26,893,281 cons objects.
  70,620,400 bytes for   244,673 simple-character-string objects.
  25,039,664 bytes for   281,666 instance objects.
  18,791,088 bytes for    19,002 code objects.
  14,332,816 bytes for   119,076 simple-vector objects.
   3,639,552 bytes for    56,868 symbol objects.
   3,160,016 bytes for     5,575 simple-array-unsigned-byte-64 objects.
   1,842,688 bytes for    39,792 simple-base-string objects.
   1,217,472 bytes for    33,620 closure objects.
   1,206,144 bytes for    36,414 simple-bit-vector objects.
   1,036,192 bytes for    32,105 bignum objects.
     726,496 bytes for    22,703 fdefn objects.
     373,856 bytes for     4,107 simple-array-unsigned-byte-8 objects.
     335,440 bytes for     1,880 simple-array-unsigned-byte-16 objects.
     321,840 bytes for         5 simple-array-unsigned-byte-31 objects.
     246,624 bytes for     4,527 funcallable-instance objects.
      53,008 bytes for       663 array-header objects.
      48,288 bytes for         3 simple-array-unsigned-byte-7 objects.
      45,696 bytes for     2,856 value-cell objects.
      38,352 bytes for       183 simple-array-unsigned-fixnum objects.
      36,224 bytes for        46 simple-array-unsigned-byte-32 objects.
       7,632 bytes for       477 sap objects.
       3,296 bytes for        38 simple-array-fixnum objects.
       2,320 bytes for       145 double-float objects.
       1,088 bytes for        34 weak-pointer objects.
         864 bytes for         4 simple-array-signed-byte-16 objects.
         448 bytes for        14 ratio objects.
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
  573,420,336 bytes for 27,799,775 dynamic objects (space total.)

Top 200 dynamic instance types:
  SB-C::COMPILED-DEBUG-FUN: 5,529,888 bytes, 49,374 objects.
  SB-C:TN: 2,530,704 bytes, 14,379 objects.
  SB-C:TN-REF: 1,813,680 bytes, 22,671 objects.
  SB-C::GLOBAL-CONFLICTS: 1,130,320 bytes, 14,129 objects.
  SB-KERNEL:FUN-TYPE: 953,344 bytes, 8,512 objects.
  SB-C::COMPILED-DEBUG-INFO: 913,968 bytes, 19,041 objects.
  SB-C::VOP: 774,368 bytes, 6,914 objects.
  SB-C::REF: 742,112 bytes, 6,626 objects.
  SB-C::LVAR: 692,640 bytes, 8,658 objects.
  SB-C::LAMBDA-VAR: 609,024 bytes, 3,172 objects.
  SB-C::CTRAN: 564,672 bytes, 11,764 objects.
  SB-C::CLAMBDA: 503,680 bytes, 1,574 objects.
  SB-KERNEL:VALUES-TYPE: 443,424 bytes, 4,619 objects.
  SB-C::CBLOCK: 424,160 bytes, 2,410 objects.
  SB-C::COMBINATION: 381,600 bytes, 2,650 objects.
  SB-KERNEL:LEXENV: 370,048 bytes, 3,304 objects.
  SB-C:DEFINITION-SOURCE-LOCATION: 347,616 bytes, 7,242 objects.
  SB-ASSEM:SSET: 334,608 bytes, 6,971 objects.
  HASH-TABLE: 314,080 bytes, 1,963 objects.
  SB-KERNEL:NUMERIC-TYPE: 269,280 bytes, 2,805 objects.
  SB-C::IR2-BLOCK: 250,976 bytes, 1,426 objects.
  SB-C::IR2-LVAR: 240,048 bytes, 5,001 objects.
  SB-PCL::SLOT-INFO: 235,728 bytes, 4,911 objects.
  SB-C::VOP-INFO: 215,552 bytes, 842 objects.
  SB-PCL::FAST-METHOD-CALL: 212,976 bytes, 4,437 objects.
  SB-C::CONSTRAINT: 207,104 bytes, 3,236 objects.
  SB-C::OPERAND-PARSE: 203,280 bytes, 1,815 objects.
  SB-C::VOP-PARSE: 202,080 bytes, 842 objects.
  SB-THREAD:MUTEX: 176,976 bytes, 3,687 objects.
  SB-PCL::ARG-INFO: 174,720 bytes, 1,560 objects.
  SB-KERNEL:DEFSTRUCT-SLOT-DESCRIPTION: 174,000 bytes, 2,175 objects.
  SB-C::FUN-INFO: 112,112 bytes, 1,001 objects.
  SB-KERNEL:MEMBER-TYPE: 107,392 bytes, 1,678 objects.
  SB-ASSEM:LABEL: 106,400 bytes, 3,325 objects.
  SB-C::DEBUG-SOURCE: 100,560 bytes, 1,257 objects.
  SB-KERNEL:CONSTANT: 98,784 bytes, 882 objects.
  SB-KERNEL::CONDITION-SLOT: 96,000 bytes, 1,000 objects.
  SB-C::GLOBAL-VAR: 94,416 bytes, 843 objects.
  SB-KERNEL:LAYOUT: 92,400 bytes, 825 objects.
  SB-C::CONSET: 87,792 bytes, 1,829 objects.
  SB-C::LOCATION-INFO: 82,464 bytes, 1,718 objects.
  SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION: 74,752 bytes, 2,336 objects.
  SB-IMPL::STRING-OUTPUT-STREAM: 68,480 bytes, 428 objects.
  SB-DISASSEM:INSTRUCTION: 67,088 bytes, 599 objects.
  SB-KERNEL:KEY-INFO: 65,056 bytes, 2,033 objects.
  SB-C::TAIL-SET: 64,896 bytes, 1,352 objects.
  SB-KERNEL::CONDITION-CLASSOID: 63,504 bytes, 441 objects.
  SB-KERNEL:ARRAY-TYPE: 63,120 bytes, 789 objects.
  SB-C::CIF: 62,048 bytes, 554 objects.
  STANDARD-METHOD: 61,888 bytes, 1,934 objects.
  SB-KERNEL::CLASSOID-CELL: 61,296 bytes, 1,277 objects.
  SB-ALIEN:CAST: 61,056 bytes, 477 objects.
  SB-KERNEL:DEFSTRUCT-DESCRIPTION: 58,080 bytes, 330 objects.
  SB-INT:XSET: 56,832 bytes, 1,776 objects.
  SB-C:BIND: 56,800 bytes, 710 objects.
  SB-FORMAT::FORMAT-DIRECTIVE: 55,840 bytes, 698 objects.
  RESTART: 52,544 bytes, 821 objects.
  SB-PCL::STRUCTURE-DIRECT-SLOT-DEFINITION: 50,016 bytes, 1,563 objects.
  SB-PCL::CACHE: 49,680 bytes, 621 objects.
  SB-KERNEL:UNION-TYPE: 48,768 bytes, 762 objects.
  SB-PCL::CONDITION-EFFECTIVE-SLOT-DEFINITION: 46,528 bytes, 1,454 objects.
  PATHNAME: 44,352 bytes, 693 objects.
  SB-PCL::PV-TABLE: 36,192 bytes, 754 objects.
  SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION: 35,744 bytes, 1,117 objects.
  SB-KERNEL:STRUCTURE-CLASSOID: 35,280 bytes, 315 objects.
  SB-PCL::WRAPPER: 33,664 bytes, 263 objects.
  SB-PCL::CLASS-EQ-SPECIALIZER: 33,472 bytes, 1,046 objects.
  SB-PCL::INITIAL: 32,480 bytes, 1,015 objects.
  SB-ALIEN-INTERNALS:ALIEN-POINTER-TYPE: 29,472 bytes, 614 objects.
  SB-C::TRANSFORM: 27,936 bytes, 582 objects.
  SB-C::OPTIONAL-DISPATCH: 27,392 bytes, 107 objects.
  SB-C::CRETURN: 27,072 bytes, 282 objects.
  SB-KERNEL:CONS-TYPE: 25,664 bytes, 401 objects.
  SB-C::ENTRY: 24,288 bytes, 253 objects.
  SB-C::ARG-INFO: 23,680 bytes, 370 objects.
  SB-SYS:FD-STREAM: 21,584 bytes, 71 objects.
  SB-KERNEL:STANDARD-CLASSOID: 21,408 bytes, 223 objects.
  SB-MOP:STANDARD-DIRECT-SLOT-DEFINITION: 15,904 bytes, 497 objects.
  SB-MOP:STANDARD-READER-METHOD: 15,040 bytes, 470 objects.
  SB-C::CSET: 14,672 bytes, 131 objects.
  PACKAGE: 14,336 bytes, 112 objects.
  SB-IMPL::PACKAGE-HASHTABLE: 14,336 bytes, 224 objects.
  SB-PCL::CONDITION-CLASS: 14,144 bytes, 442 objects.
  SB-C::CLEANUP: 13,392 bytes, 279 objects.
  SB-THREAD:THREAD: 13,056 bytes, 136 objects.
  SB-C::IR2-PHYSENV: 13,056 bytes, 136 objects.
  SB-ALIEN-INTERNALS:ALIEN-INTEGER-TYPE: 12,144 bytes, 253 objects.
  SB-ALIEN-INTERNALS:ALIEN-RECORD-FIELD: 11,856 bytes, 247 objects.
  SB-ALIEN::ALIEN-C-STRING-TYPE: 11,760 bytes, 147 objects.
  SB-SYS:DEADLINE-TIMEOUT: 11,680 bytes, 365 objects.
  SB-MOP:STANDARD-WRITER-METHOD: 10,624 bytes, 332 objects.
  STRUCTURE-CLASS: 10,240 bytes, 320 objects.
  SB-C::MV-COMBINATION: 9,072 bytes, 63 objects.
  SB-C:COMPONENT: 8,800 bytes, 55 objects.
  SB-PCL::ONE-CLASS: 8,592 bytes, 179 objects.
  SB-C::CLOOP: 8,448 bytes, 88 objects.
  SB-PCL::CONDITION-DIRECT-SLOT-DEFINITION: 8,160 bytes, 255 objects.
  SB-MOP:EQL-SPECIALIZER: 7,744 bytes, 242 objects.
  SB-C::PHYSENV: 7,584 bytes, 158 objects.
  SB-IMPL::EXTERNAL-FORMAT: 7,552 bytes, 59 objects.
  SB-KERNEL:BUILT-IN-CLASSOID: 7,168 bytes, 64 objects.
  SB-C::DEFINED-FUN: 7,168 bytes, 56 objects.
  SB-PCL::CONSTANT-FAST-METHOD-CALL: 7,104 bytes, 111 objects.
  STANDARD-CLASS: 6,976 bytes, 218 objects.
  SB-PCL::CACHING: 6,816 bytes, 213 objects.
  SB-ALIEN-INTERNALS:ALIEN-VALUE: 6,784 bytes, 212 objects.
  ASDF:CL-SOURCE-FILE: 6,592 bytes, 206 objects.
  SB-VM::PRIM-OBJECT-SLOT: 6,016 bytes, 94 objects.
  SB-PRETTY::PPRINT-DISPATCH-ENTRY: 5,888 bytes, 92 objects.
  SB-EXT:EXIT: 5,488 bytes, 49 objects.
  SB-C:SC: 4,896 bytes, 34 objects.
  SB-ALIEN-INTERNALS:ALIEN-ARRAY-TYPE: 4,800 bytes, 75 objects.
  SB-KERNEL:INTERSECTION-TYPE: 4,672 bytes, 73 objects.
  SB-PCL::GLOBAL-READER-METHOD: 4,480 bytes, 140 objects.
  SB-IMPL::CASE-FROB-STREAM: 4,480 bytes, 40 objects.
  SWANK-BACKEND::MAILBOX: 4,464 bytes, 93 objects.
  SB-C:PRIMITIVE-TYPE: 4,416 bytes, 92 objects.
  SB-PCL::GLOBAL-WRITER-METHOD: 4,416 bytes, 138 objects.
  SB-IMPL::LINKAGE-INFO: 4,288 bytes, 134 objects.
  SB-THREAD:WAITQUEUE: 4,256 bytes, 133 objects.
  SB-KERNEL:CONSTANT-TYPE: 4,032 bytes, 63 objects.
  SB-IMPL::STRING-INPUT-STREAM: 3,968 bytes, 31 objects.
  SB-PRETTY::NEWLINE: 3,840 bytes, 80 objects.
  SB-VM::EA: 3,648 bytes, 57 objects.
  SB-DI::COMPILED-DEBUG-FUN: 3,440 bytes, 43 objects.
  SB-PCL::ONE-INDEX: 3,360 bytes, 70 objects.
  SB-DI::COMPILED-DEBUG-VAR: 3,328 bytes, 52 objects.
  SB-C::ENTRY-INFO: 3,264 bytes, 51 objects.
  SB-PCL::FGEN: 3,216 bytes, 67 objects.
  BABEL-ENCODINGS::CONCRETE-MAPPING: 3,200 bytes, 100 objects.
  SB-C::TYPE-INFO: 3,072 bytes, 48 objects.
  SB-IMPL::PROCESS: 3,024 bytes, 27 objects.
  SB-ALIEN-INTERNALS:ALIEN-RECORD-TYPE: 3,008 bytes, 47 objects.
  SB-PRETTY::BLOCK-START: 3,008 bytes, 47 objects.
  SB-C::RETURN-INFO: 2,736 bytes, 57 objects.
  CFFI::SIMPLE-STRUCT-SLOT: 2,528 bytes, 79 objects.
  SB-ASSEM::BACK-PATCH: 2,496 bytes, 52 objects.
  SB-KERNEL:NEGATION-TYPE: 2,432 bytes, 38 objects.
  SB-PCL::TWO-CLASS: 2,432 bytes, 38 objects.
  SB-THREAD:SEMAPHORE: 2,432 bytes, 38 objects.
  SB-VM::SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES: 2,400 bytes, 25 objects.
  SB-ACLREPL::CMD-TABLE-ENTRY: 2,368 bytes, 37 objects.
  SB-VM::ROOM-INFO: 2,304 bytes, 48 objects.
  SB-ALIEN::ALIEN-TYPE-CLASS: 2,304 bytes, 16 objects.
  SB-C::APPROXIMATE-FUN-TYPE: 2,304 bytes, 48 objects.
  BUILT-IN-CLASS: 2,080 bytes, 65 objects.
  SB-C::COMPILER-ERROR-CONTEXT: 2,000 bytes, 25 objects.
  SB-KERNEL::TYPE-CLASS: 1,920 bytes, 15 objects.
  SB-VM:PRIMITIVE-OBJECT: 1,920 bytes, 24 objects.
  SB-PCL::METHOD-CALL: 1,856 bytes, 58 objects.
  CFFI::FOREIGN-STRING-TYPE: 1,600 bytes, 50 objects.
  SB-PRETTY::BLOCK-END: 1,568 bytes, 49 objects.
  SB-PRETTY::INDENTATION: 1,488 bytes, 31 objects.
  SB-PRETTY:PRETTY-STREAM: 1,456 bytes, 7 objects.
  SB-C::DEBUG-NAME-MARKER: 1,392 bytes, 87 objects.
  SB-PRETTY::LOGICAL-BLOCK: 1,344 bytes, 21 objects.
  ASDF:SYSTEM: 1,280 bytes, 40 objects.
  CFFI::FOREIGN-TYPEDEF: 1,216 bytes, 38 objects.
  ASM-PERF: 1,184 bytes, 37 objects.
  SB-KERNEL:CHARACTER-SET-TYPE: 1,152 bytes, 18 objects.
  SB-IMPL::BUFFER: 1,104 bytes, 23 objects.
  SB-PCL::CONSTANT-VALUE: 1,088 bytes, 34 objects.
  BABEL-ENCODINGS::ABSTRACT-MAPPING: 1,088 bytes, 34 objects.
  BABEL-ENCODINGS:CHARACTER-ENCODING: 1,088 bytes, 34 objects.
  SB-PCL::N-N: 1,056 bytes, 33 objects.
  SB-C::FUNCTIONAL: 960 bytes, 5 objects.
  SB-ASSEM::CHOOSER: 960 bytes, 15 objects.
  SB-KERNEL:ALIEN-TYPE-TYPE: 896 bytes, 14 objects.
  SB-C::FINITE-SB: 768 bytes, 8 objects.
  SB-C::UNDEFINED-WARNING: 768 bytes, 16 objects.
  CFFI::FOREIGN-BUILT-IN-TYPE: 768 bytes, 24 objects.
  SB-C::POLICY-DEPENDENT-QUALITY: 720 bytes, 15 objects.
  SB-ASSEM::FILLER: 720 bytes, 15 objects.
  SB-C::EVENT-INFO: 704 bytes, 11 objects.
  ASDF:MODULE: 672 bytes, 21 objects.
  SB-PCL::DISPATCH: 640 bytes, 20 objects.
  SWANK-BACKEND:ARGLIST: 640 bytes, 5 objects.
  SB-PCL::CHECKING: 608 bytes, 19 objects.
  SB-C::NLX-INFO: 576 bytes, 9 objects.
  SB-ASSEM:SEGMENT: 576 bytes, 3 objects.
  SYNONYM-STREAM: 560 bytes, 5 objects.
  SB-C::FILE-INFO: 560 bytes, 7 objects.
  BROADCAST-STREAM: 560 bytes, 5 objects.
  CFFI::FOREIGN-STRUCT-TYPE: 544 bytes, 17 objects.
  READTABLE: 512 bytes, 8 objects.
  SB-C::SOURCE-INFO: 448 bytes, 7 objects.
  SB-PCL::GLOBAL-BOUNDP-METHOD: 448 bytes, 14 objects.
  SB-C::MODULAR-FUN-INFO: 448 bytes, 7 objects.
  SB-ASSEM::ALIGNMENT-NOTE: 448 bytes, 7 objects.
  SB-ALIEN-INTERNALS:HEAP-ALIEN-INFO: 432 bytes, 9 objects.
  SB-C::IR2-NLX-INFO: 432 bytes, 9 objects.
  SB-KERNEL:NAMED-TYPE: 384 bytes, 6 objects.
  SB-LOOP::LOOP-PATH: 384 bytes, 6 objects.
  ASDF:STATIC-FILE: 384 bytes, 12 objects.
  SB-PCL::CLASS-PRECEDENCE-DESCRIPTION: 384 bytes, 8 objects.
  TWO-WAY-STREAM: 336 bytes, 3 objects.
  SB-DISASSEM::ARG-FORM-KIND: 336 bytes, 7 objects.
  SB-KERNEL::RAW-SLOT-DATA: 320 bytes, 5 objects.
  SB-KERNEL:HAIRY-TYPE: 320 bytes, 5 objects.
  CL-PPCRE::STR: 320 bytes, 10 objects.
  Other types: 9,760 bytes, 248 objects.
  Dynamic instance total: 25,088,608 bytes, 282,173 objects.
|#
