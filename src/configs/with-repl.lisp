;;; track-memory.lisp --- augment `checkpoint' to print incremental memory usage

;;; Commentary:

;; Load this file with the -C option to start a swank listener on port
;; 4005.  This may then be connected to externally to open a REPL into
;; the running lisp image.  See http://common-lisp.net/project/slime/.

;;; Code:
(in-package :optimize)
(require :swank)
(swank:create-server :port 4005 :style :spawn :dont-close t)
