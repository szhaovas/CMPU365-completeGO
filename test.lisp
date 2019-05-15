;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file contains the various competitions among MC-RAVE, UCT-RAVE, stock
;;  MCTS, Minimax, and human player. Most are merely for illustration purposes
;;  only COMPETE-MCRAVE-BENCHMARK and COMPETE-BENCHMARK-MCRAVE are used in other
;;  files (evol-alg.lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file can be mostly ignored
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  NEW-GOMOKU
;; --------------------------------------------------
;;  INPUTS: DIM, desired dimension of the gomoku board
;;          WIN, how many pieces need to be aligned to make a win?
;;  OUTPUT: a gomoku game
(defun new-gomoku
  (dim win)
  (make-gomoku :num-open (* dim dim)
               :board (make-array (list dim dim) :initial-element *blank*)
               :win-lineup-num win))

;; MC-RAVE vs. MC-RAVE
(defun compete
    (black-num-sims black-k white-num-sims white-k)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
       ((eq (gomoku-whose-turn g) *black*)
	(format t "BLACK'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k)))))
       (t
	(format t "WHITE'S TURN!~%")
	(format t "~A~%"
		(apply #'do-move! g (mc-rave g white-num-sims (* white-num-sims white-k)))))))))

;; UCT-RAVE vs. UCT-RAVE
(defun compete-uct
  (black-num-sims black-k black-c white-num-sims white-k white-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c))))
        (t
        	(format t "WHITE'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-rave g white-num-sims (* white-num-sims white-k) white-c))))))))

;; MC-RAVE (firsthand) vs. human (secondhand)
(defun compete-i-mcrave
  (black-num-sims black-k)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k)))))
        (t
        	(format t "YOUR TURN! enter your move:~%")
        	(format t "~A~%"
               		(apply #'do-move! g (read))))))))

;; UCT-RAVE (firsthand) vs. human (secondhand)
(defun compete-i-uctrave
  (black-num-sims black-k black-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c))))
        (t
        	(format t "YOUR TURN! enter your move:~%")
        	(format t "~A~%"
               		(apply #'do-move! g (read))))))))

;; stock MCTS vs. stock MCTS
(defun compete-mcuct
  (black-num-sims black-c white-num-sims white-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-search g black-num-sims black-c))))
        (t
        	(format t "WHITE'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-search g white-num-sims white-c))))))))

;; stock MCTS (firsthand) vs. human (secondhand)
(defun compete-i-mcuct
  (black-num-sims black-c)
  (setf *verbose* t)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(format t "BLACK'S TURN!~%")
        	(format t "~A~%"
               		(apply #'do-move! g (uct-search g black-num-sims black-c))))
        (t
        	(format t "YOUR TURN! enter move:~%")
        	(format t "~A~%"
               		(apply #'do-move! g (read))))))))

; (defun compete-mccutoff
;   (black-num-sims black-cutoff black-k white-num-sims white-cutoff white-k)
;   (setf *verbose* t)
;   (let ((g (new-gomoku 7 5)))
;     (while (not (game-over? g))
;       (cond
;         ((eq (gomoku-whose-turn g) *black*)
;         	(format t "BLACK'S TURN!~%")
;         	(format t "~A~%"
;                		(apply #'do-move! g (mc-rave-cutoff g black-num-sims black-cutoff (* black-num-sims black-k)))))
;         (t
;         	(format t "WHITE'S TURN!~%")
;         	(format t "~A~%"
;                		(apply #'do-move! g (mc-rave-cutoff g white-num-sims white-cutoff (* white-num-sims white-k)))))))))

;; MC-RAVE (firsthand) vs. Minimax cutoff = 1 (secondhand)
(defun compete-mcrave-benchmark
  (black-num-sims black-k)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (mc-rave g black-num-sims (* black-num-sims black-k))))
        (t
        	(apply #'do-move! g (compute-move g 1 *white*)))))
    (who-wins? g)))

;; Minimax cutoff = 1 (firsthand) vs. MC-RAVE (secondhand)
(defun compete-benchmark-mcrave
  (white-num-sims white-k)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (compute-move g 1 *black*)))
        (t
        	(apply #'do-move! g (mc-rave g white-num-sims (* white-num-sims white-k))))))
    (who-wins? g)))

;; UCT-RAVE (firsthand) vs. Minimax cutoff = 1 (secondhand)
(defun compete-uctrave-benchmark
  (black-num-sims black-k black-c)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (uct-rave g black-num-sims (* black-num-sims black-k) black-c)))
        (t
        	(apply #'do-move! g (compute-move g 1 *white*)))))
    (who-wins? g)))

;; Minimax cutoff = 1 (firsthand) vs. UCT-RAVE (secondhand)
(defun compete-benchmark-uctrave
  (white-num-sims white-k white-c)
  (setf *verbose* nil)
  (let ((g (new-gomoku 7 5)))
    (while (not (game-over? g))
      (cond
        ((eq (gomoku-whose-turn g) *black*)
        	(apply #'do-move! g (compute-move g 1 *black*)))
        (t
        	(apply #'do-move! g (uct-rave g white-num-sims (* white-num-sims white-k) white-c)))))
    (who-wins? g)))

; (defun compete-mcravecut-benchmark
;   (black-num-sims black-k cutoff-depth)
;   (setf *verbose* nil)
;   (let ((g (new-gomoku 7 5)))
;     (while (not (game-over? g))
;       (cond
;         ((eq (gomoku-whose-turn g) *black*)
;         	(apply #'do-move! g (mc-rave-cutoff g black-num-sims cutoff-depth (* black-num-sims black-k))))
;         (t
;         	(apply #'do-move! g (compute-move g 1 *white*)))))
;     (who-wins? g)))
;
; (defun compete-benchmark-mcravecut
;   (white-num-sims white-k cutoff-depth)
;   (setf *verbose* nil)
;   (let ((g (new-gomoku 7 5)))
;     (while (not (game-over? g))
;       (cond
;         ((eq (gomoku-whose-turn g) *black*)
;         	(apply #'do-move! g (compute-move g 1 *black*)))
;         (t
;         	(apply #'do-move! g (mc-rave-cutoff g white-num-sims cutoff-depth (* white-num-sims white-k))))))
;     (who-wins? g)))
