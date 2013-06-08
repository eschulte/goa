;;; debug.lisp --- functions for interactive debugging

;; Copyright (C) 2013  Eric Schulte

;;; Code:
(in-package :optimize)
(require :hu.dwim.debug)

(defun setup ()
  (setf *orig* (from-file (make-instance 'asm-perf)
                          "benchmarks/bzip2/bzip2.s"))
  (setf *benchmark* "bzip2")
  (run *orig*))


;;; Memory analysis
(defvar *bad* nil)

(let ((vector-cnt 0)
      (cons-cnt 0)
      (other-cnt 0))
  (sb-vm::map-allocated-objects
   (lambda (obj type size)
     (declare (ignorable type size))
     (typecase obj
       (asm-perf (format t "main object"))
       ((SIMPLE-VECTOR 256)
        (incf vector-cnt)
        (when (= vector-cnt 10)
          (setf *bad* obj)))
       (cons (incf cons-cnt))
       (t (incf other-cnt))))
   :dynamic)
  (list vector-cnt cons-cnt other-cnt))


;;; Debugging a large run of VIPS on real
#|

OPTIMIZE> (sb-vm::memory-usage :print-spaces t :print-summary nil)

Breakdown for dynamic space:
  6,241,039,616 bytes for 390,064,976 cons objects.
  3,463,317,824 bytes for   260,128 simple-character-string objects.
  30,547,568 bytes for   118,703 simple-vector objects.
  27,968,816 bytes for     3,891 simple-array-unsigned-byte-64 objects.
  22,069,616 bytes for   247,692 instance objects.
  18,686,656 bytes for    18,872 code objects.
   7,866,560 bytes for    15,579 simple-array-unsigned-fixnum objects.
   5,414,240 bytes for    19,634 simple-array-unsigned-byte-8 objects.
   3,457,216 bytes for    54,019 symbol objects.
   2,523,440 bytes for    58,181 simple-base-string objects.
   2,087,568 bytes for    59,808 closure objects.
   1,035,792 bytes for    31,976 bignum objects.
     725,888 bytes for    22,684 fdefn objects.
     610,816 bytes for    18,939 simple-bit-vector objects.
     332,736 bytes for     1,836 simple-array-unsigned-byte-16 objects.
     321,840 bytes for         5 simple-array-unsigned-byte-31 objects.
     246,576 bytes for     4,526 funcallable-instance objects.
      93,728 bytes for     1,172 array-header objects.
      86,704 bytes for     5,419 value-cell objects.
      69,760 bytes for     4,360 sap objects.
      48,288 bytes for         3 simple-array-unsigned-byte-7 objects.
      36,224 bytes for        46 simple-array-unsigned-byte-32 objects.
      24,768 bytes for       774 weak-pointer objects.
       5,920 bytes for       185 ratio objects.
       3,296 bytes for        38 simple-array-fixnum objects.
       1,664 bytes for         6 simple-array-signed-byte-16 objects.
       1,520 bytes for        95 double-float objects.
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
  9,828,624,976 bytes for 391,013,565 dynamic objects (space total.)
; No value
OPTIMIZE> (sb-vm:instance-usage :dynamic :top-n 100)

Top 100 dynamic instance types:
  SB-C::COMPILED-DEBUG-FUN: 5,471,648 bytes, 48,854 objects.
  SB-IMPL::STRING-OUTPUT-STREAM: 2,362,560 bytes, 14,766 objects.
  SB-FORMAT::FORMAT-DIRECTIVE: 1,147,280 bytes, 14,341 objects.
  PATHNAME: 941,312 bytes, 14,708 objects.
  SB-C::COMPILED-DEBUG-INFO: 905,856 bytes, 18,872 objects.
  SB-KERNEL:FUN-TYPE: 898,240 bytes, 8,020 objects.
  SB-C:TN: 711,040 bytes, 4,040 objects.
  SB-C:TN-REF: 470,000 bytes, 5,875 objects.
  SB-C:DEFINITION-SOURCE-LOCATION: 355,248 bytes, 7,401 objects.
  SB-C::GLOBAL-CONFLICTS: 291,200 bytes, 3,640 objects.
  SB-PCL::SLOT-INFO: 236,112 bytes, 4,919 objects.
  SB-PCL::FAST-METHOD-CALL: 215,712 bytes, 4,494 objects.
  SB-C::VOP-INFO: 215,552 bytes, 842 objects.
  SB-KERNEL:VALUES-TYPE: 211,968 bytes, 2,208 objects.
  SB-C::VOP: 209,440 bytes, 1,870 objects.
  SB-C::OPERAND-PARSE: 203,280 bytes, 1,815 objects.
  SB-C::VOP-PARSE: 202,080 bytes, 842 objects.
  SB-SYS:FD-STREAM: 199,120 bytes, 655 objects.
  HASH-TABLE: 196,960 bytes, 1,231 objects.
  SB-C::REF: 189,392 bytes, 1,691 objects.
  SB-C::LVAR: 186,480 bytes, 2,331 objects.
  SB-PCL::ARG-INFO: 174,720 bytes, 1,560 objects.
  SB-KERNEL:DEFSTRUCT-SLOT-DESCRIPTION: 173,680 bytes, 2,171 objects.
  SB-C::CTRAN: 157,152 bytes, 3,274 objects.
  SB-C::LAMBDA-VAR: 148,992 bytes, 776 objects.
  SB-C::CBLOCK: 146,608 bytes, 833 objects.
  SB-THREAD:MUTEX: 141,936 bytes, 2,957 objects.
  SB-C::CLAMBDA: 138,240 bytes, 432 objects.
  RESTART: 129,792 bytes, 2,028 objects.
  SB-KERNEL:LEXENV: 129,472 bytes, 1,156 objects.
  SB-C::FUN-INFO: 112,112 bytes, 1,001 objects.
  SB-C::COMBINATION: 109,296 bytes, 759 objects.
  SB-ASSEM:SSET: 103,440 bytes, 2,155 objects.
  SB-KERNEL::CONDITION-SLOT: 96,096 bytes, 1,001 objects.
  SB-IMPL::STRING-INPUT-STREAM: 95,488 bytes, 746 objects.
  SB-C::DEBUG-SOURCE: 93,840 bytes, 1,173 objects.
  SB-KERNEL:LAYOUT: 92,400 bytes, 825 objects.
  SB-KERNEL:NUMERIC-TYPE: 91,776 bytes, 956 objects.
  SB-C::IR2-BLOCK: 79,024 bytes, 449 objects.
  SB-PCL::STRUCTURE-EFFECTIVE-SLOT-DEFINITION: 74,752 bytes, 2,336 objects.
  SB-DISASSEM:INSTRUCTION: 67,088 bytes, 599 objects.
  SB-C::IR2-LVAR: 65,376 bytes, 1,362 objects.
  SB-KERNEL::CONDITION-CLASSOID: 63,504 bytes, 441 objects.
  STANDARD-METHOD: 61,888 bytes, 1,934 objects.
  SB-KERNEL:KEY-INFO: 61,696 bytes, 1,928 objects.
  SB-KERNEL::CLASSOID-CELL: 61,296 bytes, 1,277 objects.
  SB-ALIEN-INTERNALS:ALIEN-VALUE: 60,864 bytes, 1,902 objects.
  SB-KERNEL:DEFSTRUCT-DESCRIPTION: 57,904 bytes, 329 objects.
  SB-KERNEL:MEMBER-TYPE: 53,184 bytes, 831 objects.
  SB-C::CONSTRAINT: 50,432 bytes, 788 objects.
  SB-PCL::STRUCTURE-DIRECT-SLOT-DEFINITION: 50,016 bytes, 1,563 objects.
  SB-PCL::CACHE: 47,680 bytes, 596 objects.
  SB-PCL::CONDITION-EFFECTIVE-SLOT-DEFINITION: 46,592 bytes, 1,456 objects.
  SB-ASSEM:LABEL: 37,728 bytes, 1,179 objects.
  SB-PCL::PV-TABLE: 36,192 bytes, 754 objects.
  SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION: 35,904 bytes, 1,122 objects.
  SB-KERNEL:STRUCTURE-CLASSOID: 35,280 bytes, 315 objects.
  SB-PCL::WRAPPER: 34,304 bytes, 268 objects.
  SB-PCL::INITIAL: 34,272 bytes, 1,071 objects.
  SB-KERNEL:ARRAY-TYPE: 33,920 bytes, 424 objects.
  SB-PCL::CLASS-EQ-SPECIALIZER: 33,536 bytes, 1,048 objects.
  SB-KERNEL:UNION-TYPE: 31,488 bytes, 492 objects.
  SB-SYS:DEADLINE-TIMEOUT: 30,624 bytes, 957 objects.
  ASM-PERF: 30,336 bytes, 948 objects.
  SB-KERNEL:CONSTANT: 29,568 bytes, 264 objects.
  SB-ALIEN-INTERNALS:ALIEN-POINTER-TYPE: 29,328 bytes, 611 objects.
  SB-C::TRANSFORM: 27,936 bytes, 582 objects.
  SB-C::GLOBAL-VAR: 27,888 bytes, 249 objects.
  SB-INT:XSET: 27,008 bytes, 844 objects.
  SB-C:BIND: 21,920 bytes, 274 objects.
  SB-KERNEL:STANDARD-CLASSOID: 21,408 bytes, 223 objects.
  SB-C::CONSET: 20,736 bytes, 432 objects.
  SB-ALIEN:CAST: 19,584 bytes, 153 objects.
  SB-C::LOCATION-INFO: 19,440 bytes, 405 objects.
  SB-C::TAIL-SET: 18,384 bytes, 383 objects.
  SB-VM::EA: 16,384 bytes, 256 objects.
  SB-MOP:STANDARD-DIRECT-SLOT-DEFINITION: 15,904 bytes, 497 objects.
  SB-MOP:STANDARD-READER-METHOD: 15,040 bytes, 470 objects.
  SB-PCL::CONDITION-CLASS: 14,144 bytes, 442 objects.
  PACKAGE: 13,952 bytes, 109 objects.
  SB-IMPL::PACKAGE-HASHTABLE: 13,952 bytes, 218 objects.
  SB-C::CRETURN: 13,920 bytes, 145 objects.
  SB-THREAD:THREAD: 12,864 bytes, 134 objects.
  SB-ALIEN-INTERNALS:ALIEN-INTEGER-TYPE: 12,048 bytes, 251 objects.
  SB-ALIEN-INTERNALS:ALIEN-RECORD-FIELD: 11,856 bytes, 247 objects.
  SB-ALIEN::ALIEN-C-STRING-TYPE: 11,760 bytes, 147 objects.
  SB-C::CIF: 10,864 bytes, 97 objects.
  SB-MOP:STANDARD-WRITER-METHOD: 10,624 bytes, 332 objects.
  SB-C::ENTRY: 10,368 bytes, 108 objects.
  STRUCTURE-CLASS: 10,240 bytes, 320 objects.
  SB-PCL::CONDITION-DIRECT-SLOT-DEFINITION: 8,224 bytes, 257 objects.
  SB-MOP:EQL-SPECIALIZER: 7,744 bytes, 242 objects.
  SB-IMPL::EXTERNAL-FORMAT: 7,552 bytes, 59 objects.
  SB-PCL::ONE-CLASS: 7,488 bytes, 156 objects.
  SB-KERNEL:BUILT-IN-CLASSOID: 7,168 bytes, 64 objects.
  SB-PCL::CONSTANT-FAST-METHOD-CALL: 7,040 bytes, 110 objects.
  STANDARD-CLASS: 6,976 bytes, 218 objects.
  SB-C::IR2-PHYSENV: 6,720 bytes, 70 objects.
  SB-PCL::CACHING: 6,688 bytes, 209 objects.
  ASDF:CL-SOURCE-FILE: 6,592 bytes, 206 objects.
  Other types: 223,168 bytes, 4,206 objects.
  Dynamic instance total: 19,982,880 bytes, 227,577 objects.
|#
