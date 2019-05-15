;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file contains the the functions I used to hand-tune the best value
;;  of k for MC-RAVE
;;  No fancy algorithm here, but LIST-GEN is used in some other functions
;;  outside of this file
;;
;;  The following are the average win rates among the 10 matches played between
;;  MC-RAVE with k values (0.0000001, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
;;  and num_sim 1000, and MC-RAVE with k = 0.5 and num_sim 1000. It seems that
;;  0.4 and 0.5, 0.7, 0.8, and 0.9 are all promising k values.
;;  (3/5 7/10 7/10 9/10 1 4/5 9/10 1 9/10 1)
;;
;;  However, (MC-RAVE g 1000 0.5) may not be a good enough benchmark itself
;;  and all the playouts here have (MC-RAVE g 1000 0.5) as the second hand player,
;;  which may not be fair considering our version of Gomoku is unfair.
;;  Moreover, these win rates do not count the numbers of draws and losses,
;;  so they can only mean at best that these k-values are relatively good
;;  at winning, but NOT that they are good at NOT LOSING.
;;
;;  I'll try to address these problems with a more formal evolutionary algorithm
;;  in evol-alg.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file can be mostly ignored
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generates a list of decimals from 0 to 1
;; separated by the interval STEP
;; e.g. STEP 0.5 would give '(0, 0.5, 1)
(defun list-gen
  (step)
  (let ((counter 0)
        (result nil))
    (while (<= counter 1)
      (push counter result)
      (incf counter step))
    (reverse result)))

(defun k-test
  (match-number step)
  (let
    ((k-values (list-gen step)))
    (while k-values
      (let
        ((k (pop k-values))
         (wins 0))
        ;; k cannot be 0
        (when (zerop k) (setf k 0.0000001))
        (dotimes (i match-number)
                 (when (eq (compete-mcrave-benchmark 1000 k) *black*)
                   (incf wins)))
        (with-open-file (str "k-test.txt"
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (format str "~A,~A~%" k (/ wins match-number)))))))
