;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This file contains the implementation of MC-RAVE and UCT-RAVE for Gomoku
;;  Since most of it can be transferred from stock MCTS, I will only comment
;;  where changes are made
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  MC-NODE struct -- a node in the MCTS tree
;; ----------------------------------------------------------------------------
;;  On top of original mc-node, this RAVE version also stores AMAF information

(defstruct mc-node
  key
  whose-turn
  (num-visits 0)
  veck-moves
  veck-visits
  veck-scores
  amaf-visits
  amaf-scores
  )

(defstruct mc-tree
  (hashy (make-hash-table :test #'equal))
  root-key)

(defun get-root-node
    (tree)
  (gethash (mc-tree-root-key tree) (mc-tree-hashy tree)))

(defun new-mc-tree
    (game)
  (make-mc-tree :root-key (make-hash-key-from-game game)))


;;  INSERT-NEW-NODE
;; -----------------------------------------
;;  Adds initialization for AMAF information

(defun insert-new-node
  (game tree key)
  (let* ((moves (legal-moves game))
       	 (num-moves (length moves))
       	 (nodey (make-mc-node
              		 :key key
              		 :veck-moves moves
              		 :veck-visits (make-array num-moves :initial-element 0)
              		 :veck-scores (make-array num-moves :initial-element 0)
                   :amaf-visits (make-array num-moves :initial-element 0)
                   :amaf-scores (make-array num-moves :initial-element 0)
              		 :whose-turn (gomoku-whose-turn game))))
    ;; insert nodey into tree
    (setf (gethash key (mc-tree-hashy tree)) nodey)
    ;; return the node
    nodey))

;;  SELECT-MOVE
;; ------------------------------------------
;;  INPUTS:  NODEY, an MC-NODE struct
;;           k, equivalence parameter
;;  OUTPUT:  The INDEX of the selected move into the moves vector

(defun select-move
  (nodey k)
  (let*
    ((player (mc-node-whose-turn nodey))
   	 (moves (mc-node-veck-moves nodey))
   	 (num-moves (length moves)))
    (cond
      ((= num-moves 0)
       nil)
      ((= num-moves 1)
       0)
      (t
       (let*
         ((n (mc-node-num-visits nodey))
          ;; corresponds to β, derived from formula 19 from gelly-silver
          (beta (sqrt (/ k (+ (* n 3) k))))
          (mc-scoress (mc-node-veck-scores nodey))
          ;; amaf score
          (amaf-scoress (mc-node-amaf-scores nodey))
     	    (best-move-so-far nil)
     	    (best-score-so-far (if (eq player *black*)
                               *neg-inf*
                               *pos-inf*)))
         (dotimes
          (i num-moves)
     	    (let*
            ((mc-score (svref mc-scoress i))
             (amaf-score (svref amaf-scoress i))
             ;; weighted score between MC and AMAF, derived from formula 12
             ;; from gelly-silver
             (weighted-score (+ (* (- 1 beta) mc-score) (* beta amaf-score))))
     	      (when
              (or
               (and
                (eq player *black*)
                (> weighted-score best-score-so-far))
               (and
                (eq player *white*)
                (< weighted-score best-score-so-far)))
              (setf best-score-so-far weighted-score)
              (setf best-move-so-far i))))
         (if best-move-so-far
           best-move-so-far
           (random num-moves)))))))

;;  SELECT-MOVE-UCT
;; ------------------------------------------
;;  The UCT variant of select-move, only different by the addition of exploration
;;  term

(defun select-move-uct
  (nodey k c)
  (let*
    ((player (mc-node-whose-turn nodey))
   	 (moves (mc-node-veck-moves nodey))
   	 (num-moves (length moves)))
    (cond
      ((= num-moves 0)
       nil)
      ((= num-moves 1)
       0)
      (t
       (let*
         ((n (mc-node-num-visits nodey))
          (beta (sqrt (/ k (+ (* n 3) k))))
          (mc-scoress (mc-node-veck-scores nodey))
          (amaf-scoress (mc-node-amaf-scores nodey))
     	    (best-move-so-far nil)
     	    (best-score-so-far (if (eq player *black*)
                               *neg-inf*
                               *pos-inf*)))
         (when (zerop 0) (return-from select-move-uct (random num-moves)))
         (dotimes
          (i num-moves)
     	    (let*
            ((mc-score (svref mc-scoress i))
             (amaf-score (svref amaf-scoress i))
             (weighted-score (+ (* (- 1 beta) mc-score) (* beta amaf-score)))
             ;; adds exploration term
             (uct-score (+ weighted-score (* c (sqrt (/ (log n)
                                                        (svref (mc-node-veck-visits nodey) i)))))))
     	      (when
              (or
               (and
                (eq player *black*)
                (> uct-score best-score-so-far))
               (and
                (eq player *white*)
                (< uct-score best-score-so-far)))
              (setf best-score-so-far uct-score)
              (setf best-move-so-far i))))
         (if best-move-so-far
           best-move-so-far
           (random num-moves)))))))


;;  SIM-TREE
;; --------------------------------------
;;  INPUTS:  GAME, a game struct
;;           TREE, an MC-TREE struct
;;           k, equivalence parameter
;;  Identical to original SIM-TREE except for functions specific to gomoku

(defun sim-tree
  (game tree k)
  (let ((key-move-acc nil)
       	(hashy (mc-tree-hashy tree)))
    (while (not (game-over? game))
      (let* ((key (make-hash-key-from-game game))
       	     (nodey (gethash key hashy)))
       	(when (null nodey)
       	  (setf nodey (insert-new-node game tree key))
       	  (let* ((mv-index (select-move nodey k))
                 (move-veck (mc-node-veck-moves nodey))
                 (move (svref move-veck mv-index)))
       	    (apply #'do-move! game move)
       	    (push key key-move-acc)
       	    (push mv-index key-move-acc)
       	    (return-from sim-tree (reverse key-move-acc))))
       	(let* ((mv-index (select-move nodey k))
       	       (move-veck (mc-node-veck-moves nodey))
       	       (move (svref move-veck mv-index)))
       	  (apply #'do-move! game move)
       	  (push key key-move-acc)
       	  (push mv-index key-move-acc))))
    (reverse key-move-acc)))

;;  SIM-TREE-UCT
;; ------------------------------------------
;;  The UCT variant of sim-tree, only different by the call to SELECT-MOVE-UCT

(defun sim-tree-uct
  (game tree k c)
  (let ((key-move-acc nil)
       	(hashy (mc-tree-hashy tree)))
    (while (not (game-over? game))
      (let* ((key (make-hash-key-from-game game))
       	     (nodey (gethash key hashy)))
       	(when (null nodey)
       	  (setf nodey (insert-new-node game tree key))
       	  (let* ((mv-index (select-move-uct nodey k c))
                 (move-veck (mc-node-veck-moves nodey))
                 (move (svref move-veck mv-index)))
       	    (apply #'do-move! game move)
       	    (push key key-move-acc)
       	    (push mv-index key-move-acc)
       	    (return-from sim-tree-uct (reverse key-move-acc))))
       	(let* ((mv-index (select-move-uct nodey k c))
       	       (move-veck (mc-node-veck-moves nodey))
       	       (move (svref move-veck mv-index)))
       	  (apply #'do-move! game move)
       	  (push key key-move-acc)
       	  (push mv-index key-move-acc))))
    (reverse key-move-acc)))

;;  RESULT-WRAPPER
;; ----------------------------------------------
;;  helper function of sim-default
;;
;;  INPUT:   GAME, a game struct
;;           ORIG-GAME, the original game at the call of MC-RAVE
;;  OUTPUT:  who-wins, but multiplies value by 10 if the starting player at
;;           orig-game lost

(defun result-wrapper
  (game orig-game)
  (let ((result (who-wins? game)))
    (if (eq result *draw*)
      0
      (if (eq result (gomoku-whose-turn orig-game))
        (- result)
        ;; if player lost increase penalty
        (* (- result) 10)))))

;;  SIM-DEFAULT
;; ----------------------------------------------
;;  INPUT:   GAME, a game struct
;;           ORIG-GAME, the original game at the call of MC-RAVE
;;  OUTPUT:  A list, consisted of all the moves (in list) made by default policy
;;           to reach result, AND the result from result-wrapper at the end

(defun sim-default
  (game orig-game)
  (let ((move-acc nil))
    (while (not (game-over? game))
      (let ((move (random-move game)))
        (apply #'do-move! game move)
        (push move move-acc)))
    (push (result-wrapper game orig-game) move-acc)
    (reverse move-acc)))


;;  SIM-DEFAULT-CUTOFF
;; ----------------------------------------------
;;  A failed attempt to incorporate cutoff depth into MC-RAVE

; (defun sim-default-cutoff
;   (game orig-game cutoff-depth)
;   ;;(format t "sim-default-cutoff starts!")
;   (let*
;     ((move-acc nil)
;      (orig-num-open (gomoku-num-open orig-game))
;      (num-open (gomoku-num-open game))
;      (current-depth (- orig-num-open num-open)))
;     (while (and
;             (not (game-over? game))
;             (< current-depth cutoff-depth))
;       (let ((move (random-move game)))
;         (apply #'do-move! game move)
;         (incf current-depth)
;         ;;(format t "current-depth: ~A~%" current-depth)
;         (push move move-acc)))
;     (push (eval-func game (gomoku-whose-turn game))
;           move-acc)
;     (reverse move-acc)))

;;  MERGE-MOVES
;; ----------------------------------------------
;;  helper function for BACKUP
;;
;;  INPUTS: KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;          MOVE-ACC, which contains all the moves made to reach game's end
;;              and the result value at the end.
;;          HASHY, the hash-table for the MCTS
;;  OUTPUT: A list consisted of all the moves made from the START OF RAVE
;;              (including the tree moves) to the end of game
;;              All moves are represented as lists

(defun merge-moves
  (key-move-acc move-acc hashy)
  (let
    ((simtree-moves nil))
    (dotimes
     (i (/ (length key-move-acc) 2))
     (push
      ;; move
      (svref
       ;; vector of moves
       (mc-node-veck-moves
        ;; node
        (gethash
         ;; key
         (nth (* i 2) key-move-acc)
         ;; tree (hash table)
         hashy))
       ;; move-index
       (nth (+ (* i 2) 1) key-move-acc))
      ;; accumulator
      simtree-moves))
    (append (reverse simtree-moves) move-acc)))

;;  ARRAY-MEMBER
;; ----------------------------------------------
;;  helper for BACKUP
;;  Array membership check, item index if item in arr, nil if not

(defun array-member
  (item arr)
  (dotimes
   (i (length arr))
   (when
     (equal
      item
      (svref arr i))
     (return-from array-member i)))
  nil)

;;  BACKUP
;; ---------------------------------------------------
;;  INPUTS:  HASHY, the hash-table for the MCTS
;;           KEY-MOVE-ACC, the accumulated list of KEYs and MOVEs
;;              from a simulation run
;;           MOVE-ACC, which contains all the moves made to reach game's end
;;              and the result value at the end.
;;  SIDE EFFECT:  Updates the relevant nodes in the TREE

(defun backup
  (hashy key-move-acc move-acc)
  ;; masks the move-acc with the merged-moves
  (setf move-acc (merge-moves key-move-acc move-acc hashy))
  (let ((result (first (last move-acc))))
    (while key-move-acc
      (let*
        ((key (pop key-move-acc))
     	   (nodey (gethash key hashy))
     	   (mv-index (pop key-move-acc))
         (veck-moves (mc-node-veck-moves nodey))
         (mc-visits (mc-node-veck-visits nodey))
         (mc-scores (mc-node-veck-scores nodey))
         (amaf-visits (mc-node-amaf-visits nodey))
         (amaf-scores (mc-node-amaf-scores nodey)))
        ;; updates the MC visits and scores
        (incf (mc-node-num-visits nodey))
        (incf (svref mc-visits mv-index))
        (incf (svref mc-scores mv-index)
         	    (/ (- result (svref mc-scores mv-index))
         	       (svref mc-visits mv-index)))
        ;; updates the AMAF visits and scores
        ;; No need to check for repeated moves because no two moves can be the same
        (let ((i 0))
          (while (< i (- (length move-acc) 1))
            (let*
              ((mv (nth i move-acc))
               (legal-p (array-member mv veck-moves)))
              (when
                ;; when move is legal...
                legal-p
                ;; update
                (incf (svref amaf-visits legal-p))
                (incf (svref amaf-scores legal-p)
                 	    (/ (- result (svref amaf-scores legal-p))
                 	       (svref amaf-visits legal-p)))))
            ;; only consider moves of the same color
            (incf i 2))
          (setf move-acc (rest move-acc)))))))

;;  MC-RAVE
;; ---------------------------------
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           k, the equivalence parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of MCTS-RAVE.

(defparameter *verbose* t) ;; a global parameter used to ensure/suppress printing of stats

(defun mc-rave
  (orig-game num-sims k)
  (let* ((tree (new-mc-tree orig-game))
       	 (hashy (mc-tree-hashy tree))
         ;; k must NOT be 0, mask and reset with 0.0000001
       	 (k k))
    (when (zerop k) (setf k 0.0000001))
    (dotimes (i num-sims)
             (let* ((game (copy-game orig-game))
              	     (key-move-acc (sim-tree game tree k))
              	     (move-acc (sim-default game orig-game)))
              	(backup hashy key-move-acc move-acc)))
    (let* ((rootie (get-root-node tree))
       	   (mv-index (select-move rootie k))
       	   (move (svref (mc-node-veck-moves rootie) mv-index))
       	   (scores (mc-node-veck-scores rootie))
           (amaf-visits (mc-node-amaf-visits rootie))
           (amaf-scores (mc-node-amaf-scores rootie)))
      (format t ".")
      (when *verbose*
        (format t "moves veck: ~A~%" (mc-node-veck-moves rootie))
       	(format t "mc-scores veck: ")
       	(dotimes (i (length scores))
                	(format t "~5,3F, " (svref scores i)))
       	(format t "~%")
       	(format t "mc-visits veck: ")
       	(dotimes (i (length scores))
              	  (format t "~A " (svref (mc-node-veck-visits rootie) i)))
        (format t "~%")
        (format t "AMAF Visits veck: ~A" amaf-visits)
       	(format t "~%")
        (format t "AMAF Scores veck ~A~%" amaf-scores))
      move)))

;;  UCT-RAVE
;; ---------------------------------
;;  UCT variant of MC-RAVE
;;  INPUTS:  ORIG-GAME, a game struct
;;           NUM-SIMS, a positive integer
;;           k, the equivalence parameter
;;           c, the exploration parameter
;;  OUTPUT:  Best move from that state determined by
;;             doing *NUM-SIMS* simulations of UCT-RAVE.

(defun uct-rave
  (orig-game num-sims k c)
  (let* ((tree (new-mc-tree orig-game))
       	 (hashy (mc-tree-hashy tree))
         ;; k must NOT be 0, mask and reset with 0.0000001
       	 (k k))
    (when (zerop k) (setf k 0.0000001))
    (dotimes (i num-sims)
             (let* ((game (copy-game orig-game))
              	     (key-move-acc (sim-tree-uct game tree k c))
              	     (move-acc (sim-default game orig-game)))
              	(backup hashy key-move-acc move-acc)))
    (let* ((rootie (get-root-node tree))
       	   (mv-index (select-move-uct rootie k c))
       	   (move (svref (mc-node-veck-moves rootie) mv-index))
       	   (scores (mc-node-veck-scores rootie))
           (amaf-visits (mc-node-amaf-visits rootie))
           (amaf-scores (mc-node-amaf-scores rootie)))
      (format t ".")
      (when *verbose*
        (format t "moves veck: ~A~%" (mc-node-veck-moves rootie))
       	(format t "mc-scores veck: ")
       	(dotimes (i (length scores))
                	(format t "~5,3F, " (svref scores i)))
       	(format t "~%")
       	(format t "mc-visits veck: ")
       	(dotimes (i (length scores))
              	  (format t "~A " (svref (mc-node-veck-visits rootie) i)))
        (format t "~%")
        (format t "AMAF Visits veck: ~A" amaf-visits)
       	(format t "~%")
        (format t "AMAF Scores veck ~A~%" amaf-scores))
      move)))

; (defun mc-rave-cutoff
;   (orig-game num-sims cutoff-depth k)
;   ;; Want to use COPY of GAME struct for simulations...
;   ;; That way, can reset game struct before each simulation...
;   (let* ((tree (new-mc-tree orig-game))
;        	 (hashy (mc-tree-hashy tree))
;        	 ;;(player (whose-turn orig-game))
;        	 (k k))
;     (when (zerop k) (setf k 0.0000001))
;     (dotimes (i num-sims)
;              (let* (;; Work with a COPY of the original game struct
;               	     (game (copy-game orig-game))
;               	     ;; Phase 1:  SIM-TREE Destructively modifies game
;               	     (key-move-acc (sim-tree game tree k))
;               	     ;; Phase 2:  SIM-DEFAULT returns result
;               	     (move-acc (sim-default-cutoff game orig-game cutoff-depth)))
;               	;; Finally, backup the results
;                ;(format t "--------------------------------------backup~A~%" i)
;               	(backup hashy key-move-acc move-acc)))
;     ;; Select the best move (using c = 0 because we are not exploring anymore)
;     (let* ((rootie (get-root-node tree))
;        	   (mv-index (select-move rootie k))
;        	   (move (svref (mc-node-veck-moves rootie) mv-index))
;        	   (scores (mc-node-veck-scores rootie))
;            (amaf-visits (mc-node-amaf-visits rootie))
;            (amaf-scores (mc-node-amaf-scores rootie)))
;       (format t ".")
;       (when *verbose*
;        	;; Display some stats along with the best move
;         (format t "moves veck: ~A~%" (mc-node-veck-moves rootie))
;        	(format t "mc-scores veck: ")
;        	(dotimes (i (length scores))
;                 	(format t "~5,3F, " (svref scores i)))
;        	(format t "~%")
;        	(format t "mc-visits veck: ")
;        	(dotimes (i (length scores))
;               	  (format t "~A " (svref (mc-node-veck-visits rootie) i)))
;         (format t "~%")
;         (format t "AMAF Visits veck: ~A" amaf-visits)
;        	(format t "~%")
;         (format t "AMAF Scores veck ~A~%" amaf-scores))
;       ;; Output the move
;       move)))
