;;  COMPETE
;; -------------------------------------------------------
;;  INPUTS:  DIM, board dimension
;;           MINIMAX-FIRST, which ai takes the first move
;;           NUM-THINK-AHEAD, number of exchanged moves AI think ahead
;;                            default 1
;;           NUM-SIM, number of simulation for mcts-rave
;;                    default 1000
;;           K, parameter for mcts-rave
;;              default 0.5
;;  OUTPUT:  don't care

(defun compete
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
    (mcts-ai (if minimax-first *white* *black*))
    )
    (loop while (null res) do
        (format t "~%~A~%" g)
        (if (eq *black* (gomoku-whose-turn g)) 
          (apply #'do-move! g 
            (if minimax-first
              (compute-move g c1 (gomoku-whose-turn g))
              (mc-rave g num-sim k)))
          (apply #'do-move! g 
            (if minimax-first
              (mc-rave g num-sim k)
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
            ((eq res mcts-ai) "MCTS-RAVE Wins!")
            (T "It's a Draw. Nobody wins!")
        ))))