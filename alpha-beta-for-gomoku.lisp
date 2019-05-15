;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth for-player)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)
  (let* (
    (statty (make-stats))
    (result (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth for-player))
    (best-move (first result))
    (alpha (second result))
   )
    (format t "   ROOT NODE ALPHA: ~A~%" alpha)
    (format t "   NUM-MOVES-DONE: ~A, NUM-MOVES-PRUNED: ~A~%" 
      (stats-num-moves-done statty)
      (- (stats-num-potential-moves statty) (stats-num-moves-done statty)))
    (format t "   BEST MOVE: ~A~%" best-move)
    ;; return my-move
    best-move))

;;  LEGAL-MOVES-WITH-HEURISTIC
;; -------------------------------------------------------------
;;  INPUTS:  G, a GOMOKU struct
;;  OUTPUT:  Candidates, a vector of moves which are:
;;           1. Win moves: moves for current player to end game
;;           2. Defensive moves: moves current player has to take
;;              to prevent opponent from winning
;;           3. Sorted legal moves based on eval-func  

(defmethod legal-moves-with-heuristic
  ((g gomoku))
  (let* (
    (moves (legal-moves g))
    ;; a copy of board
    (kopy-board (copy-array (gomoku-board g)))

    (win-moves (list))
    (defensive-moves (list))
    (whose-turn (gomoku-whose-turn g))
    (next-turn (if (eq whose-turn *black*) *white* *black*))

    (candidates moves)
    )

    ;; Find mvoes for current player to end game
    (dotimes (i (length moves))
      (if (= *pos-inf* 
        (eval-move kopy-board
          (first (aref moves i))
          (second (aref moves i))
          whose-turn))
        (setf win-moves (cons (aref moves i) win-moves))
      ))

    (cond 
      ;; case 1: no win moves
      ;;  try to find defensive moves
      ((= 0 (length win-moves))
        (dotimes (i (length moves))
          (if (= *pos-inf* 
            (eval-move kopy-board
              (first (aref moves i))
              (second (aref moves i))
              next-turn))
            (setf defensive-moves (cons (aref moves i) defensive-moves))
          ))

        (cond 
          ;; case 1.1: no defensive moves
          ;; sort the legal-moves
          ((= 0 (length defensive-moves))
            (let ((moves-list (list)))
              (dotimes (i (length moves)) 
                (setf moves-list (cons (aref moves i) moves-list)))
              (labels (
                ;; function to sort moves
                ;; based on eval-func
                (compare-fun 
                  (m1 m2)
                  (let (
                    (v1 0)
                    (v2 0)
                    )
                    ;; see and record one move's effect
                    (apply #'do-move! g m1)
                    (setf v1 (eval-for-player g whose-turn))
                    (undo-move! g)
                    (apply #'do-move! g m2)
                    (setf v2 (eval-for-player g whose-turn))
                    (undo-move! g)
                    (>= v1 v2)
                  )
                ))
                (setf moves-list (sort moves-list #'compare-fun))
                (setf candidates
                  (make-array (length moves-list)
                            :initial-contents moves-list))
              )
            ))

          ;; case 1.2
          ;; make defensive moves the return val
          (T 
            (setf candidates 
              (make-array (length defensive-moves)
                            :initial-contents defensive-moves)))))

      ;; case 2
      ;; make win moves the only return val
      (T 
        (setf candidates 
          (make-array (length win-moves)
                        :initial-contents win-moves)))
    )

    candidates
  ))

;;  COMPUTE-MAX / COMPUTE-MIN
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth for-player)
    (let* (
      (winner (who-wins? g))
      (is-game-over (not (null winner)))
      )
      (cond 
        (is-game-over 
          (cond 
            ((eq for-player winner) (- *win-value* curr-depth))
            ((eq for-player *draw*) *draw-value*)
            (T (+ *loss-value* curr-depth))
            )
          )
         ;; reach cut-off
        ((>= curr-depth cutoff-depth) (eval-func g for-player))
        ;; otherwise
        (T
          (let* (
            (best-move nil)
            (value *neg-inf*)
            (moves (legal-moves-with-heuristic g))
            )
            ; (format T "~A ~A~%" moves (length moves))

            (if (and (= curr-depth 0) (= 1 (length moves)))
              (return-from compute-max (list (aref moves 0) 0)))

            ;; update num of potential moves
            (incf (stats-num-potential-moves statty) (length moves))

            ;; loop through available moves
            (dotimes (i (length moves))
              (let ((move (aref moves i)))
                  ;; do move!
                  (apply #'do-move! g move)
                  
                  ;; increment move actually done stat
                  (incf (stats-num-moves-done statty))

                  ;; record resulting value of chosen move
                  (let ((result (compute-min g (+ curr-depth 1) alpha beta statty cutoff-depth for-player)))
                    (cond
                      ;; case 1: if this move is better
                      ((> result value)
                        (setf value result)
                        (setf best-move move)
                      )
                      ;; else do nothing
                    )
                    ; (format T "~A ~A~%" move result)
                  )
                  ;; recover state
                  (undo-move! g)
                  ;; break
                  (if (>= value beta) (return-from compute-max value))
                  ;; update alpha if current value is larger
                  (setf alpha (max alpha value))
              )
            )
            ;; if curr-depth is 0, return best move and alpha
            ;; else, return value
            (if (= curr-depth 0) 
              (list best-move alpha)
              value)
          )
        )
      )
    ))

;;  COMPUTE-MIN
;; -------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the depth of this MIN node
;;           ALPHA, BETA, values received from parent MAX node
;;           STATTY, a stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The value of this MIN node according to rules
;;           of MINIMAX with ALPHA-BETA pruning

(defun compute-min (g curr-depth alpha beta statty cutoff-depth for-player)
  (let* (
      (winner (who-wins? g))
      (is-game-over (not (null winner)))
      )
      (cond 
        (is-game-over 
          (cond 
            ((eq for-player winner) (- *win-value* curr-depth))
            ((eq for-player *draw*) *draw-value*)
            (T (+ *loss-value* curr-depth))
            )
          )
         ;; reach cut-off
        ((>= curr-depth cutoff-depth) 
          ; (format T "~A ~A~%" (eval-func g for-player) curr-depth)
          (eval-func g for-player))
        ;; otherwise
        (T
          (let (
            (value *pos-inf*)
            (moves (legal-moves-with-heuristic g))
            )
            ;; update num of potential moves
            (incf (stats-num-potential-moves statty) (length moves))

            ;; loop through available moves
            (dotimes (i (length moves))
                (let ((move (aref moves i)))
                    ;; do move!
                    (apply #'do-move! g move)

                    ;; increment move actually done stat
                    (incf (stats-num-moves-done statty))
                  
                    ;; record resulting value of chosen move
                    (let ((result (compute-max g (+ curr-depth 1) alpha beta statty cutoff-depth for-player)))
                        (setf value (min value result))
                    )
                    ;; recover state
                    (undo-move! g)
                    ;; break
                    (if (<= value alpha) (return-from compute-min value))
                    ;; update beta if current value is smaller
                    (setf beta (min beta value))
                )
            )
            value
          )
        ))))

;; EXCHANGED-MOVES-TO-CUTOFF-DEPTH
;; -------------------------------------------------
;; INPUT: NUM-THINK-AHEAD
;; OUTPUT: corresponding cutoff depth
(defun exhanged-moves-to-cutoff-depth
  (num-think-ahead)
  (- (* 2 num-think-ahead) 1))

;;  PLAY-AGAINST-SELF
;; -------------------------------------------------------
;;  INPUTS:  DIM, board dimension
;;           NUM-THINK-AHEAD-B, number of exchanged moves black think ahead
;;           NUM-THINK-AHEAD-W, number of exchanged moves white think ahead
;;  OUTPUT:  don't care

(defun play-against-self
  (dim num-think-ahead-b num-think-ahead-w)
  (let (
    (res nil)
    (c1 (exhanged-moves-to-cutoff-depth num-think-ahead-b))
    (c2 (exhanged-moves-to-cutoff-depth num-think-ahead-w))
    (g (
      make-gomoku 
        :num-open (* dim dim)  
        :board (make-array (list dim dim) :initial-element *blank*)))
  )
    (loop while (null res) do
        (format t "~%~A~%" g)
        (if (eq *black* (gomoku-whose-turn g)) 
          (apply #'do-move! g (compute-move g c1 (gomoku-whose-turn g)))
          (apply #'do-move! g (compute-move g c2 (gomoku-whose-turn g))))
        (setf res (who-wins? g)))
    (format t "~%~A~%" g)
    (format t "Result: ~A~%Number of moves: ~A~%" 
        (cond 
            ((eq res *black*) "Black wins!")
            ((eq res *white*) "White wins!")
            (T "Draw!")
        )
        (- (* dim dim) (gomoku-num-open g)))))

;;  HUMAN-VS-MINIMAX
;; -------------------------------------------------------
;;  INPUTS:  DIM, board dimension
;;           MINIMAX-FIRST, which ai takes the first move
;;           NUM-THINK-AHEAD, number of exchanged moves AI think ahead
;;                            default 1
;;  OUTPUT:  don't care

(defun human-vs-minimax
  (dim minimax-first
    &optional 
    (num-think-ahead 1) 
    (num-sim 1000)
    (k 0.5))
  (let (
    (res nil)
    (c1 (exhanged-moves-to-cutoff-depth num-think-ahead))
    (g (init-gomoku dim))
    (minimax-ai (if minimax-first *black* *white*))
    (you (if minimax-first *white* *black*))
    )
    (loop while (null res) do
        (format t "~%~A~%" g)
        (if (eq *black* (gomoku-whose-turn g)) 
          (apply #'do-move! g 
            (if minimax-first
              (compute-move g c1 (gomoku-whose-turn g))
              (read)))
          (apply #'do-move! g 
            (if minimax-first
              (read)
              (compute-move g c1 (gomoku-whose-turn g)))))
        (setf res (who-wins? g)))
    (format t "~%~A~%" g)
    (format t "~A~%" 
        (cond 
            ((eq res *black*) "Black reaches 5 first!")
            ((eq res *white*) "White reaches 5 first!")
            (T "No one can reach 5!")
        ))
    (format t "Result: ~A~%" 
        (cond 
            ((eq res minimax-ai) "Minimax Wins!")
            ((eq res you) "You Wins!")
            (T "It's a Draw. Nobody wins!")
        ))))

