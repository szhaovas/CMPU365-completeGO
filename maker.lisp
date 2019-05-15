;; ===========================================
;;  CMPU-365, Spring 2019
;;  Basic Definitions for Othello and MCTS
;; ===========================================

;;  To ensure that the compiler efficiently handles tail recursion

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;;  To avoid annoying garbage-collection messages

(setf *global-gc-behavior* :auto)

(defparameter *basic-files*
  (list "maker"))

;;  MAKER
;; ------------------------------------
;;  Compiles and loads all files for the implementation

(defun maker
    ()
  (dolist (file *basic-files*)
    (compile-file file)
    (load file))
    (load "gomoku.lisp")
    (compile-file "gomoku")
    (load "gomoku")
    (compile-file "alpha-beta-for-gomoku")
    (load "alpha-beta-for-gomoku")
    (compile-file "mc-rave")
    (load "mc-rave")
    (compile-file "compete")
    (load "compete")
    (compile-file "data")
    (load "data")
    (compile-file "evol-alg")
    (load "evol-alg")
    (compile-file "mcts")
    (load "mcts")
    (compile-file "test")
    (load "test"))



