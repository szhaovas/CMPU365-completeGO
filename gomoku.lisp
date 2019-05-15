;; =================================================
;; FILE: gomoku.lisp

;; Critical functions:
  ;; (init-gomoku (dim))
  ;; (game-over? (game))
  ;; (who-wins? (game))
  ;; (do-move! (game row col))
  ;; (undo-move! (game))
  ;; (legal-moves (game))

;; Critical functions for file alpha-beta-for-gomoku.lisp
  ;; (eval-func (game for-player))
  ;; (eval-move (board x y for-player))

;; Critical functions for file mc-rave.lisp
  ;; (default-policy (game))
;; =================================================



;;  Some useful constants
;; -----------------------------------

;;  Colors of tokens/blank spaces

(defconstant *black* -1)
(defconstant *white* 1)
(defconstant *blank* 0)

;;  How tokens of each color are displayed

(defconstant *black-show* 'B)
(defconstant *white-show* 'W)
(defconstant *blank-show* '_)

;;  WIN-LOSS VALUES

(defconstant *win-value* 10000000)
(defconstant *draw-value* 0)
(defconstant *loss-value* -10000000)

;; SHAPE EVALUATION VALUES

(defconstant *win-shape-score* 100000)
(defconstant *free-space-val* 0.01)

;;  NEGATIVE and POSITIVE INFINITY
;; ----------------------------------------

(defconstant *neg-inf* -10000000000000)
(defconstant *pos-inf*  10000000000000)


;; Denote game is over and result is DRAW
(defconstant *draw* 'DRAW)


;;  *DIRNS*
;; --------------------------------------------------------
;;  An 4-by-2 array representing the 4 different directions
;;  one could travel from a given square on the board.

(defconstant *dirns* (make-array '(4 2)
                 :initial-contents
                 '((1 1) (1 0) (0 1) (-1 1))))

;;  *ALL-DIRNS*
;; --------------------------------------------------------
;;  a list of 8 possible directions, defined for convenience

(defconstant *all-dirns* 
  '((1 1) (1 0) (1 -1)
           (0 1) (0 -1) 
           (-1 1) (-1 0) (-1 -1)))

;;  A POSN is just a number from 0 to N - 1, that refers to one of the
;;  squares on the N-by-N gomoku game board.  The following macros
;;  convert between the POSN and ROW/COL representations.

;;  POSN->ROW
;; ---------------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to N * N - 1)
;;           N is the dimension of board
;;  OUTPUT:  The corresponding ROW (an integer from 0 to N - 1)

(defmacro posn->row (posn n) `(floor ,posn, n))

;;  POSN->COL
;; ---------------------------------------------------------------
;;  INPUT:   POSN (i.e., an integer from 0 to N * N - 1)
;;           N is the dimension of board
;;  OUTPUT:  The corresponding COLUMN (an integer from 0 to N - 1)

(defmacro posn->col (posn n) `(mod ,posn, n))

;;  ROW-COL->POSN
;; ------------------------------------------
;;  INPUTS:  ROW, COL, two integers each between 0 and N - 1, 
;;           N is the dimension of board
;;  OUTPUT:  The corresponding POSN (an integer between 0 and N * N - 1)

(defmacro row-col->posn (row col n) `(+ (* ,n ,row) ,col))

;;  IF-BLACK-TURN
;; -------------------------------------------------------
;;  INPUTS:  G, an GOMOKU struct
;;           THEN, ELSE, any two Lisp expressions
;;  OUTPUT:  If it's black's turn, then evaluate THEN;
;;           Otherwise, evaluate ELSE.

(defmacro if-black-turn (g then else)
  `(if (eq (gomoku-whose-turn ,g) ,*black*)
       ,then ,else))

;;  IF-BLACK
;; --------------------------------------------------------
;;  INPUTS:  PLR, either *BLACK* or *WHITE*
;;           THEN, ELSE, any Lisp expressions
;;  OUTPUT:  If PLR is *BLACK* then evaluate THEN;
;;           Otherwise, evaluate ELSE.

(defmacro if-black (plr then else)
  `(if (eq ,plr ,*black*) ,then ,else))

;;  TOGGLE-PLAYER!
;; ---------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The player whose turn it now is (either *BLACK* or *WHITE*)
;;  SIDE EFFECT:  Destructively modifies the game to toggle whose turn it is.

(defmacro toggle-player! (game)
  `(setf (gomoku-whose-turn ,game)
     (if-black (gomoku-whose-turn ,game) ,*white* ,*black*)))

;;  PLACE-TOKEN-AT-POSN
;; --------------------------------------------------------------
;;  INPUTS:  BOARD, an two dimensional array of dimension N
;;           TOKEN, either *BLACK* or *WHITE*
;;           POSN, an integer
;;  OUTPUT:  Doesn't matter
;;  SIDE EFFECT:  Destructively modifies BOARD by inserting TOKEN
;;                at the position determined by POSN

(defmacro place-token-at-posn
    (board token posn n)
    `(setf (aref ,board (posn->row ,posn, n) (posn->col ,posn, n)) ,token))
        
;;  PLACE-TOKEN
;; -------------------------------------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           BORED, an two dimensional array of dimension N
;;           PLR, either *BLACK* or *WHITE*
;;           ROW, COL, integers between 0 and N - 1
;;  OUTPUT:  Doesn't matter
;;  SIDE EFFECT:  Places TOKEN on BORED at specified ROW/COL.
;;    Also updates the (N*N)-bit vector for the appropriate player
;;    (see the STATE struct)

(defmacro place-token
    (game bored plr row col n)
      `(progn (setf (aref ,bored ,row ,col) ,plr)
          (if-black ,plr (incf (gomoku-black-pieces ,game)
                       (ash 1 (row-col->posn ,row ,col, n)))
                (incf (gomoku-white-pieces ,game)
                  (ash 1 (row-col->posn ,row ,col, n))))))

;;  The GOMOKU struct
;; --------------------------------------------------------

(defstruct (gomoku (:print-function print-gomoku))
  ;; BOARD:  an 9-by-9 array of *white*, *black* or *blank* 
  (board (make-array '(9 9) :initial-element *blank*))      
  ;; WHOSE-TURN:  either *BLACK* or *WHITE*
  (whose-turn *black*)
  ;; NUM-OPEN:  the number of open spaces on the BOARD (always <= N * N)
  (num-open 81)
  ;; WHITE-PIECES/BLACK-PIECES:  (N*N)-bit integers
  (white-pieces 0) 
  (black-pieces 0) 
  ;; win policy, i.e., how many in a row is a win
  ;; default is 5
  (win-lineup-num 5)
  (is-legal? `is-legal-default?)
  ;; for easier game-over? check and undo-move!
  ;; stores a list
  ;; (row col)
  (history (make-array 0 :adjustable T :fill-pointer 0))
)

;; INIT-GOMOKU
;; -------------------------------------------
;; INPUTS: DIM, board dimension
;; OUTPUT: a gomoku struct

(defun init-gomoku
  (dim)
  (make-gomoku
    :num-open (* dim dim)
    :board (make-array (list dim dim) 
    :initial-element *blank*)))

;;  PRINT-GOMOKU
;; --------------------------------------------------
;;  INPUTS:  G, an GOMOKU struct
;;           STR, output stream (or T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the GOMOKU game

(defun print-gomoku
    (g str d)
  (declare (ignore d))
  (let* (
    (bored (gomoku-board g))
    (dim (first (array-dimensions bored)))
    )
    (format str "~%  |")
    (dotimes (i dim) (format str " ~A~A" (if (< i 10) " " "") i))
    (format str "~%  ")
    (dotimes (i (+ 2 (* 3 dim) 
      (max 0 (* 2 (- dim 10))))) (format str "-"))
    (format str "~%")
    (dotimes (r dim)
      (format str "~A~A| " (if (< r 10) " " "") r)
      (dotimes (c dim)
        (let ((token (aref bored r c)))
          (format str " ~A " 
            (cond 
              ((eq token *black*) *black-show*)
              ((eq token *white*) *white-show*)
              (t *blank-show*))
            )))
      (format str "~%"))
    (format str "~%")
    ))

;;  COPY-ARRAY
;; -------------------------------------------------
;;  INPUT:   HARRY, a 2-dimensional array
;;  OUTPUT:  A copy of HARRY

(defun copy-array
    (harry)
  (let* ((dims (array-dimensions harry))
     (kopy (make-array dims)))
    (dotimes (r (first dims))
      (dotimes (c (second dims))
    (setf (aref kopy r c) (aref harry r c))))
    kopy))

;;  COPY-HISTORY
;; -------------------------------------------------
;;  INPUT:   HISTORY, a 1-dimensional array
;;  OUTPUT:  A copy of HISTORY

(defun copy-history
    (history)
  (let* ((n (length history))
     (kopy (make-array 0 :adjustable T :fill-pointer 0)))
    (dotimes (r n)
        (vector-push-extend (copy-list (aref history r)) kopy))
    kopy))

;;  COPY-GAME
;; ------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  A copy of GAME

(defmethod copy-game
    ((game gomoku))
  (make-gomoku :board (copy-array (gomoku-board game))
        :whose-turn (gomoku-whose-turn game)
        :num-open (gomoku-num-open game)
        :history (copy-history (gomoku-history game))
        :win-lineup-num (gomoku-win-lineup-num game)
        :is-legal? (gomoku-is-legal? game)
        :white-pieces (gomoku-white-pieces game)
        :black-pieces (gomoku-black-pieces game)))

;;  MAKE-HASH-KEY-FROM-GAME
;; --------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A list of the form (WHITE-PIECES BLACK-PIECES WHOSE-TURN)
;;    where the contents are as described in the STATE struct

(defmethod make-hash-key-from-game
    ((game gomoku))
  (list (gomoku-white-pieces game)
    (gomoku-black-pieces game)
    (gomoku-whose-turn game)))


;;  IS-LEGAL-COORD
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A VECTOR of the legal moves available to the current player.
;;  Note:  If no "legal" moves, then returns empty vector.

(defun is-legal-coord
  (x y n)
  (not (or (< x 0) (< y 0) (>= x n) (>= y n))))

;;  LEGAL-MOVES
;; -------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  A VECTOR of the legal moves available to the current player.
;;           Apply the formula:
;;           1. If first move, always center of the board, 
;;           2. else, {(x1, y1) | |x-x1| <= 2, |y-y1| <= 2, where (x,y) is a piece on board}
;;  Note:  If no "legal" moves, then returns empty vector.

(defmethod legal-moves
  ((game gomoku))
  (let* (
    (moves nil)
    (history (gomoku-history game))
    (num-moves 0)
    (num-cand (length history))
    (N (first (array-dimensions (gomoku-board game))))
    ;; prevent repeated coord
    (collected (make-hash-table))
    (is-legal? (gomoku-is-legal? game))
    )

    ;; set the first move to be the center
    (if (= 0 num-cand) 
      (return-from legal-moves 
        (vector (list (floor (/ (- N 1) 2)) 
          (floor (/ (- N 1) 2))))))

    ;; loop through candidates
    (dotimes (i num-cand)
      (let* (
          (cand-move (aref history i))
          (ori-x (first cand-move))
          (ori-y (second cand-move))
          )
          ;; x-1, x-2, x-0, x+1, x+2
          (dotimes (j 5)
            (let* (
              (dx (- j 2))
              (x (+ dx ori-x))
              )
              ;; y-1, y-2, y-0, y+1, y+2
              (dotimes (k 5)
                (let* (
                  (dy (- k 2))
                  (y (+ dy ori-y))
                  (key (row-col->posn x y n))
                  )
                  (cond
                    ;; case 1: a legal coord
                    ;;         b place empty on board
                    ;;         c never visited
                    ((and (is-legal-coord x y N)
                      (funcall is-legal? game x y)
                      (null (gethash key collected))
                      )
                      ;; record it
                      (setf (gethash key collected) (list x y))
                      (push (list x y) moves)
                      (incf num-moves)
                    ))))))))
    (make-array num-moves :initial-contents moves)))

;;  IS-LEGAL-DEFUALT?
;; -----------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           ROW, COL, integers
;;  OUTPUT:  T if this is a legal move without extra constraints
(defmethod is-legal-default?
    ((game gomoku) row col)
    (eq *blank* (aref (gomoku-board game) row col)))

;; UNDO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;  OUTPUT:  The modified GAME: 
;;           affected variables: 
;;           board, history, num-open, player-pieces, whose-turn

(defmethod undo-move!
    ((game gomoku))
    (let* (
        (n (first (array-dimensions (gomoku-board game))))
        ;; pop last state from history
        (last-move (vector-pop (gomoku-history game)))
        (r (first last-move))
        (c (second last-move))
        (player-pieces (ash 1 (row-col->posn r c n)))
        )
    ;; num-open += 1
    (incf (gomoku-num-open game))
    ;; toggle whose-turn
    (toggle-player! game)
    ;; set player-pieces
    (if (eq *black* (gomoku-whose-turn game))
        (decf (gomoku-black-pieces game) player-pieces)
        (decf (gomoku-white-pieces game) player-pieces))
    ;; set board
    (setf (aref (gomoku-board game) r c) *blank*)
    ))

;;  DO-MOVE!
;; -------------------------------------
;;  INPUTS:  GAME, an GOMOKU struct
;;           ROW, COL, two integers
;;  OUTPUT:  The modified GAME

(defmethod do-move!
    ((game gomoku) row col)
    (let* (
        (bored (gomoku-board game))
        (player (gomoku-whose-turn game))
        ;; player-pieces before this move
        (player-pieces 
            (if (eq player *black*)
                (gomoku-black-pieces game)
                (gomoku-white-pieces game)))
        (n (first (array-dimensions bored)))
    )
    ;; Place token on the board at (ROW COL)
    (place-token game bored player row col n)
    (toggle-player! game)
    (decf (gomoku-num-open game))
    ;; record move
    (vector-push-extend 
        (list row col) 
        (gomoku-history game))
    game
    ))

;;  GAME-OVER?
;; --------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: T if game is over

(defmethod game-over?
    ((game gomoku))
    (not (null (who-wins? game))))


;;  WHO-WINS?
;; --------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  NIL if no one wins and game is not over (or (zerop (gomoku-num-open game))
;;           *BLACK* if black wins
;;           *WHITE* if white wins
;;           *DRAW* if game is over and no one wins

(defmethod who-wins?
    ((game gomoku))
    (let* (
        (board (gomoku-board game))
        (history (gomoku-history game))
        (last-move 
            (if (> (length history) 0)
                (aref history (- (length history) 1))
                nil))
        (last-player (get-last-player game))
        (win-num (gomoku-win-lineup-num game))
        (N (first (array-dimensions board)))
        )
        ;; game has not started
        (if (not (null last-move))
            ;; game started, check if last move makes consecutive 
            (dotimes (i 4)
                (if (>= 
                        (count-consecutive-in-row
                            (list (aref *dirns* i 0) (aref *dirns* i 1))
                            board 
                            last-move 
                            (get-last-player game)
                            N) 
                        win-num
                    )
                    (return-from who-wins? last-player))
            ))
        ;; if no more place to place stone, it is a draw
        ;; else game continues
        (if (= 0 (gomoku-num-open game)) *draw* nil)
    ))

;; BOARD-DIM
;; -------------------------------------------
;; INPUT: BOARD, a 2-dimensional gomoku board (N x N)
;; OUTPUT: board's dimension

(defun board-dim (board) (first (array-dimensions board)))

;;  REVERSE-DIR
;; --------------------------------------------------------------------------
;;  INPUT:  DIR, direction a list of two
;;  OUTPUT: a list of two, representing reversed direction

(defun reverse-dir (dir) (list (* -1 (first dir)) (* -1 (second dir))))

;;  GET-LAST-PLAYER
;; --------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: the last turn player

(defmethod get-last-player 
    ((game gomoku))
    (if (eq (gomoku-whose-turn game) *black*) 
        *white* 
        *black*))

;; COUNT-CONSECUTIVE-IN-ROW
;; --------------------------------------------------------------------------
;; INPUT: DIR, a list of integers denoting direction
;;        BOARD, a two-dimensional array
;;        last-move, a list of integers denoting row and column and player
;;        PLAYER, last player
;;        N, board dimension
;; OUTPUT: T if player achieves WIN-NUM-in-a-row, otherwise nil.

(defun count-consecutive-in-row
    (dir board last-move player N)
    (let (
        (dx (first dir))
        (dy (second dir))
        (cur-x (first last-move))
        (cur-y (second last-move))
        )
        (- 
            (+ (count-consecutive-in-dir
                    board
                    N
                    dir
                    player
                    cur-x
                    cur-y 
                    )
                (count-consecutive-in-dir
                    board
                    N
                    (reverse-dir dir)
                    player
                    cur-x
                    cur-y 
                    ))
            1))
    )

;; COUNT-CONSECUTIVE-IN-DIR
;; --------------------------------------------------------------------------
;; INPUT: BOARD, a two-dimensional array
;;        DIR, direction
;;        PLAYER, whose stone to check
;;        N, board dimension
;;        CUR-X, current x
;;        CUR-Y, current y
;; OUTPUT: an integer, number of consecutive pieces in on direction

(defun count-consecutive-in-dir
    (board N dir player cur-x cur-y)
    (let* (
        (dx (first dir))
        (dy (second dir))
        (nx (+ dx cur-x))
        (ny (+ dy cur-y))
        )
        (cond
            ;; if position is not valid
            ((or (>= cur-x N) (>= cur-y N) (< cur-x 0) (< cur-y 0))
                0
            )
            ;; if not same piece color
            ((not (eq player (aref board cur-x cur-y))) 0)
            ;; else
            (T (+ 1 
                (count-consecutive-in-dir
                    board
                    N
                    dir 
                    player 
                    nx 
                    ny)))
        )
    ))
    
;;  RANDOM-MOVE
;; ------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT:  One of the legal moves available to the current
;;   player, chosen randomly.

(defmethod random-move
    ((game gomoku))
  (let* ((moves (legal-moves game)))
    (svref moves (random (length moves)))))

;;  DO-RANDOM-MOVE!
;; ------------------------------------------------
;;  INPUT:   GAME, an GOMOKU struct
;;  OUTPUT:  The modified game
;;  SIDE EFFECT:  Destructively modifies GAME by doing one of the 
;;   legal moves available to the current player, chosen randomly.

(defmethod do-random-move!
    ((game gomoku))
  (let ((move (random-move game)))
    (apply #'do-move! game move)))

;;  DEFAULT-POLICY
;; ---------------------------------------------------------------------------
;;  INPUT:  GAME, an GOMOKU struct
;;  OUTPUT: The result (from black's perspective) of playing out the
;;    current game using randomly selected moves.  The amount of the
;;    win/loss is reported by the SQUARE-ROOT of the absolute difference
;;    in the number of tokens for the two players.  For example, if the
;;    game ends with WHITE having 25 tokens and BLACK having 31 tokens,
;;    then black wins by 6 tokens, and the output is approximately 2.45.

(defmethod default-policy
  ((game gomoku))
  ;; Do random moves until the game is over
  (let ((res nil))
    (loop while (null res) do
        (do-random-move! game)
        (setf res (who-wins? game)))
    res
    ))


;; ==============================================
;; EVAL FUNCTIONS
;; ==============================================

;; NOTE: A SHAPE is defined as a list of elements
;;       ((x, y) num-consecutive direction), where
;;     - (x, y) is the last piece of this shape
;;     - num-consectuive is an int, length of the shape (>= 2)
;;     - direction, a list of two, which direction is this shape
;;       pointing to
;; For example, ((5, 5) 5 (0, 1)) meaning a shape looks like:
;; (5, 1) -> (5, 2) -> ... -> (5, 5)

;; FIND-SHAPE-FROM-COORD-FOR-HELPER
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         COLOR: represent player, *black* or *white*
;;         DIR: a list of two int, denoting direction
;; OUTPUT: A SHAPE including the input coordinate of given dir

(defun find-shape-from-coord-for-helper
  (board x y color dir)
  (let* (
    (N (board-dim board))
    (rev-dir (reverse-dir dir))
    ;; find the tail of shape
    (c1 (count-consecutive-in-dir
          board N
          dir
          color x y))
    ;; find the head of shape
    (c2 (count-consecutive-in-dir
          board N
          rev-dir
          color x y))
    ;; calc the tail of shape
    (sx (+ x (* (- c1 1) (first dir))))
    (sy (+ y (* (- c1 1) (second dir))))
    (consecutive (- (+ c1 c2) 1))
    )
  (if (> consecutive 1) 
    (list (list sx sy) consecutive dir)
    nil)
  )
)

;; FIND-SHAPE-FROM-COORD-FOR
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         COLOR: represent player, *black* or *white*
;;         DIR: a list of two int, denoting direction
;; OUTPUT: A list of SHAPEs including the input coordinate

(defun find-shapes-from-coord-for
  (board x y color)
  (let (
    (res (list))
    (tmp nil)
    )
    (dolist (dir (list '(0 1) '(1 0) '(1 1) '(1 -1)))
      (setf tmp (find-shape-from-coord-for-helper
            board x y color dir))
      ;; populate result list if there are shapes
      ;; including this input coord
      (if (not (null tmp)) 
        (setf res 
          (cons
            tmp
            res))))
    res
  ))

;; EVAL-MOVE
;; ----------------------------------------
;; INPUTS: KOPY-BOARD, must be a copy of game board
;;         X, Y: coordinates
;;         N: dimension of board
;;         COLOR: represent player, *black* or *white*
;; OUTPUT: An INTEGER, evaluating the threats a move may pose on
;; NOTE: if a win move, always *pos-inf*
;;       else if free end four, 10
;;       else, number of threats
;; A threat is a double three or a free end four

(defun eval-move
  (kopy-board x y color)
    (setf (aref kopy-board x y) color)
    (let (
      (shapes (find-shapes-from-coord-for kopy-board x y color))
      (threats 0)
      )
      (setf (aref kopy-board x y) *blank*)
      (dolist (shape shapes)
        (let* (
          (consecutive (second shape))
          ;; get criterions, ie, open-ends, free space from two ends
          (help-vals (count-open-end-and-free-space kopy-board shape))
          (open-ends (first help-vals))
          (f1 (second help-vals))
          (f2 (third help-vals))
          (fs (+ f2 f1))
          )
          (cond
            ;; a win move
            ((>= consecutive 5) (return-from eval-move *pos-inf*))
            ((= 4 consecutive)
              (cond
                ;; a free end four
                ((= 2 open-ends) (incf threats 10))
                ;; a threatening four
                ((= 1 open-ends) (incf threats))
              )
            )
            ((= 3 consecutive)
              ;; double three is a threat
              (if (= 2 open-ends) (incf threats))
            )
          )
        )
      )
      threats
    ))

;; COUNT-FREE-SPACE-FROM
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         N: dimension of board
;;         COLOR: represent player, *black* or *white*
;;         DIR: a list of two int, denoting direction
;; OUTPUT: A SHAPE including the input coordinate of given dir

(defun count-free-space-from
  (board x y n color dx dy)
  ;; case 1: if not a legal coord
  ;; return 0
  (if (not (is-legal-coord x y n)) 
    0
    ;; case 2: a legal coord
    (let ((stone (aref board x y)))
      ;; case 2.1: at this coord
      (cond
        ;; we find opponent stone
        ((and (not (eq *blank* stone)) 
            (not (eq stone color)))
          0)
        ;; case 2.2: it is *blank* or "my" stone
        ;; blank or own stone
        (T (+ 1 (count-free-space-from
          board 
          (+ x dx)
          (+ y dy)
          n
          color
          dx
          dy)))))))

;; IS-BLANK
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         N: dimension of board
;; OUTPUT: T if given position on board is blank

(defun is-blank
  (board x y n)
  (and (is-legal-coord x y n)
    (eq *blank* (aref board x y))))

;; COUNT-OPEN-END-AND-FREE-SPACE
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         SHAPE, a list of three as described in note
;; OUTPUT: a list of three 
;; (num-open-ends, free space from dir, free space from reverse dir)

(defun count-open-end-and-free-space
  (board shape)
  (let* (
    (N (board-dim board))
    (src (first shape))
    (color (aref board (first src) (second src)))
    (len (second shape))
    (dir (third shape))
    (rev-dir (reverse-dir dir))
    ;; first potential open-end
    (h1 (list
      (+ (first src) (* len (first rev-dir)))
      (+ (second src) (* len (second rev-dir)))
      ))
    ;; second potential open-end
    (h2 (list (+ (first src) (first dir)) (+ (second src) (second dir))))
    ;; num open end
    (open-end 0)
    )
    (if (is-blank board (first h1) (second h1) N) (incf open-end))
    (if (is-blank board (first h2) (second h2) N) (incf open-end))
    (list 
      open-end 
      (count-free-space-from board (first h1) (second h1) N color (first rev-dir) (second rev-dir)) 
      (count-free-space-from board (first h2) (second h2) N color (first dir) (second dir)) 
      )
  ))

;; FS-WEIGHTED
;; ----------------------------------------
;; INPUTS: SCORE,
;;         FS, total number of free space
;; OUTPUT: weighted score 

(defun fs-weighted
  (score fs)
  (+ score (* fs *free-space-val*)))

;; EVAL-SHAPE-SCORE
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         SHAPE, a list of three as described in note
;;         IS-MY-TURN, is current turn
;; OUTPUT: score
;; general idea is described in the doc

(defun eval-shape-score
  (board shape is-my-turn)
  (let* (
    (consecutive (second shape))
    (help-vals (count-open-end-and-free-space board shape))
    (open-ends (first help-vals))
    (f1 (second help-vals))
    (f2 (third help-vals))
    (fs (+ f2 f1))
    )
    (if (>= consecutive 5) (return-from eval-shape-score *win-shape-score*))
    (if (= 0 open-ends) (return-from eval-shape-score 0))
    (cond
      ;; case 1: 4 consecutives
      ((= 4 consecutive)
        (if is-my-turn 
          ;; a win!
          *win-shape-score*
          ;; maybe a win
          (if (= 2 open-ends) 
            ;; almost a win
            (fs-weighted 1000 fs) 
            ;; an immediate threat
            (fs-weighted 300 fs))
        )
      )
      ;; case 2: 3
      ((= 3 consecutive)
        (cond 
          ;; case 2.1
          ((= 2 open-ends)
            ;; if more than 2 free space,
            ;; free end 3, a big threat
            (if (and (> fs 2) is-my-turn) 
              (fs-weighted 500 fs) 
              ;; else almost equiv to one end four
              (fs-weighted 100 fs)
              )
          )
          ;; case 2.2
          (T
            ;; dead end
            (if (< fs 2) (return-from eval-shape-score 0))
            (if is-my-turn 
              (fs-weighted 15 fs) 
              (fs-weighted 10 fs))
          )
        )
      )
      ;; case 3
      ((= 2 consecutive)
        (cond
          ((= 2 open-ends)
            ;; dead end
            (if (< fs 3) (return-from eval-shape-score 0))
            (if is-my-turn 
              (fs-weighted 25 fs) 
              (fs-weighted 5 fs))
          )
          (T
            ;; dead end
            (if (<= fs 2) (return-from eval-shape-score 0))
            (if is-my-turn 
              (fs-weighted 0.5 fs) 
              (fs-weighted 0.1 fs))
            )
          )
        )
      ;; we calc score for a single piece in another func
      (T 0)
    )
  ))

;; FIND-LINE-SHAPES-FOR
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         PLAYER, symbol denoting player
;;         IS-HORIZ, scan it horizontally or not
;; OUTPUT: a list of shapes

(defun find-line-shapes-for
    (board player is-horiz)
    (let* (
        (N (board-dim board))
        (shapes (list))
        (consecutive 0)
        (dir (if is-horiz '(0 1) '(1 0)))
        (last-coord nil)
        )
    (dotimes (i N)
        (dotimes (j N)
            (let*
                (
                    (r (if is-horiz i j))
                    (c (if is-horiz j i))
                    (stone (aref board r c))
                )
                (cond
                    ;; if found target-player stone
                    ((eq stone player)
                        (incf consecutive)
                    )
                    ;; not finding any consecutive
                    ((= 0 consecutive))
                    ;; it is a shape
                    (T
                      ;; only record shape with more than one consecutive
                      (if (> consecutive 1)
                        (setf shapes
                          (cons
                            (list last-coord consecutive dir)
                            shapes)
                          ))
                      (setf consecutive 0)
                    )
                )
                (setf last-coord (list r c))
            )
        )
        ; when finishing searching a row
        (cond
            ((> consecutive 0)
                ;; only record shape with more than one consecutive
                (if (> consecutive 1)
                  (setf shapes
                    (cons
                      (list last-coord consecutive dir)
                      shapes)
                    ))
                (setf consecutive 0)
            )
        )
    )
    shapes
    ))

;; FIND-DIAGONAL-SHAPES-FOR
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         PLAYER, symbol denoting player
;;         LEFT-TO-RIGHT, scan it diagonally left to right or not
;; OUTPUT: a list of shapes

(defun find-diagonal-shape-for
    (board player left-to-right)
    (let* (
        (dir (if left-to-right '(1 1) '(1 -1)))
        (N (board-dim board))
        (shapes (list))
        (consecutive 0)
        (last-coor nil)
        ;; number of diagonals
        (num-line (- (* 2 N) 1))
        (dx (first dir))
        (dy (second dir))
        )
    (dotimes (i num-line)
        (let* (
            (start-row 
                (if left-to-right
                    (if (< i N) (- N (+ 1 i)) 0)
                    (if (< i N) 0 (- (+ 1 i) N))))
            (start-col 
                (if left-to-right
                    (if (< i N) 0 (- (+ i 1) N))
                    (if (< i N) i (- N 1))))
            (num-elm (if (>= i N) (- (* 2 N) (+ 1 i)) (+ 1 i)))
            )
            (dotimes (j num-elm)
                (let*
                    (
                        (r (+ start-row (* j dx)))
                        (c (+ start-col (* j dy)))
                        (stone (aref board r c))
                    )
                    (cond
                        ;; if found target-player stone
                        ((eq stone player)
                            (incf consecutive)
                        )
                        ;; not finding any consecutive
                        ((= 0 consecutive))
                        ;; it is a shape
                        (T
                          ;; only record shape with more than one consecutive
                          (if (> consecutive 1)
                            (setf shapes
                              (cons
                                (list last-coord consecutive dir)
                                shapes)
                              ))
                          (setf consecutive 0)
                        )
                    )
                    (setf last-coord (list r c))
                )
            )
        )
        ; when finishing searching a diagonal
        (cond
            ((> consecutive 0)
                ;; only record shape with more than one consecutive
                (if (> consecutive 1)
                  (setf shapes
                    (cons
                      (list last-coord consecutive dir)
                      shapes)
                    ))
                (setf consecutive 0)
            )
        )
    )
    shapes
    ))

;; EVAL-BREAK-THREATS
;; ----------------------------------------
;; INPUTS: KOPY-BOARD, must be a copy of game board
;;         SHAPE, a list of three as described in note
;;         IS-MY-TURN, is current turn
;; OUTPUT: score
;; general idea is described in the doc

(defun eval-break-threats
  (kopy-board x y color)
  (let ((val (eval-move kopy-board x y color)))
    (cond
      ;; case 1: threatening to be a win
      ((= val *pos-inf*) (/ *win-shape-score* 2))
      ;; case 2: big threats
      ((> val 1) 100)
      (T 0)
    )))

;; FIND-ALL-SHAPES-FOR
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         PLAYER, symbol denoting player
;; OUTPUT: a list of shapes

(defmethod find-all-shapes-for
  (board player)
  (nconc 
    (find-line-shapes-for board player T)
    (find-line-shapes-for board player nil)
    (find-diagonal-shape-for board player T)
    (find-diagonal-shape-for board player nil)
  ))

;; IS-SINGLETON-HELPER
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         PLAYER: a symbol, *black* or *white*
;;         DIRS: a list of list of two int, denoting direction
;; OUTPUT: T if it is an isolated piece

(defun is-singleton-helper
  (board x y player dirs)
  (cond 
    ((null dirs) T)
    (T
      (let* (
        (dir (first dirs))
        (nx (+ x (first dir)))
        (ny (+ y (second dir)))
        (N (board-dim board))
        )
        (and
          (or (not (is-legal-coord nx ny N))
            (not (eq (aref board nx ny) player))
          )
          (is-singleton-helper
            board
            x
            y
            player
            (rest dirs))
          )
      )
    ))
  )

;; FIND-SINGLETONS-FOR
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         PLAYER: a symbol, *black* or *white*
;; OUTPUT: a list of coord for isolated pieces of a player

(defun find-singletons-for
  (board player)
  (let* (
    (N (board-dim board))
    (res (list))
    )
    ;; walking through board
    (dotimes (i N)
      (dotimes (j N)
        (let ((stone (aref board i j)))
          (if (and 
            (eq stone player)
            (is-singleton-helper board i j player *all-dirns*)
            )
            (setf res (cons (list i j) res))
          )
        )
      )
    )
    res
  ))

;; EVAL-SINGLETON-SCORE-FOR-PLAYER-HELPER
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         WHOSE-TURN: current turn
;;         PLAYER: a symbol, *black* or *white*
;;         DX, DY: wanted changes in x and y
;; OUTPUT: score for a singleton score for a player
;;         general idea is described in doc

(defun eval-singleton-score-for-player-helper
  (board x y whose-turn player dx dy)
  (let* (
    (rdx (* -1 dx))
    (rdy (* -1 dy))
    (N (board-dim board))
    (f1 (- (count-free-space-from 
          board x y 
          N player 
          dx dy) 1)
    )
    (f2 (- (count-free-space-from 
          board x y 
          N player 
          rdx rdy) 1)
    )
    (fs (+ f1 f2))
    (open-ends 0)
    )
    ;; count open ends in this direction
    (if (is-blank board (+ x dx) (+ y dy) N) (incf open-ends))
    (if (is-blank board (+ x rdx) (+ y rdy) N) (incf open-ends))

    ;; if less than 4 free space, no value
    (if (< fs 4) (return-from eval-singleton-score-for-player-helper 0))
    (cond
      ;; case 1: 2 open-ends
      ((= 2 open-ends)
        (fs-weighted 0.7 fs)
      )
      ;; case 2: 1 open-end
      ((= 1 open-ends)
        (fs-weighted 0.02 fs)
      )
      (T 0)
    )
  ))

;; EVAL-SINGLETON-SCORE-FOR-PLAYER
;; ----------------------------------------
;; INPUTS: BOARD, a game board
;;         X, Y: coordinates
;;         WHOSE-TURN: current turn
;;         PLAYER: a symbol, *black* or *white*
;; OUTPUT: score for all singleton score for a player
;;         general idea is described in doc

(defun eval-singleton-score-for-player
  (board x y whose-turn player)
  (let ((score 0))
    ;; loop through 4 directions
    (dotimes (i 4)
      (incf score
        (eval-singleton-score-for-player-helper
          board
          x y
          whose-turn player
          (aref *dirns* i 0) (aref *dirns* i 1)  
        )
      )
    )
    score
  ))

;; EVAL-FOR-PLAYER
;; ----------------------------------------
;; INPUTS: GAME, a GOMOKU struct
;;         PLAYER: a symbol, *black* or *white*
;; OUTPUT: evaluation score for a player

(defmethod eval-for-player
  ((game gomoku) player)
  (let* (
    (score 0)
    (board (gomoku-board game))
    (kopy-board (copy-array board))
    (N (board-dim board))
    (shapes (find-all-shapes-for board player))
    (singletons (find-singletons-for board player))
    (board (gomoku-board game))
    (whose-turn (gomoku-whose-turn game))
    (is-my-turn (eq player whose-turn))
    (moves (legal-moves game))
    )
    ;; calc shape score
    (loop for s in shapes do 
      (incf score (eval-shape-score board s is-my-turn)))

    ;; calc singleton score
    (loop for s in singletons do
      (incf score 
        (eval-singleton-score-for-player
          board
          (first s) (second s)
          whose-turn player)))

    ;; calc break-pattern shape score
    (dotimes (i (length moves))
      (incf score
        (eval-break-threats
          kopy-board 
            (first (aref moves i))
            (second (aref moves i))
            player)))

    ;; max to be *win-value*
    (min *win-value* score)
  ))


;; EVAL-FUNC
;; ----------------------------------------
;; INPUTS: GAME, a GOMOKU struct
;;         FOR-PLAYER: a symbol, *black* or *white*
;;                     represent evaluation the board
;;                     from who's perspective
;; OUTPUT: evaluation score from one player's perspective

(defmethod eval-func
    ((game gomoku) for-player)
    (let (
        (whose-turn (gomoku-whose-turn game))
        (black-score 
          (eval-for-player game *black*)
          )
        (white-score 
          (eval-for-player game *white*)
          )
    )
    (if (eq for-player *black*)
        (- black-score white-score)
        (- white-score black-score))
    ))



;; TEST-WHO-WINS?-BLACK
;; ----------------------------

(defun test-who-wins?-black
  ()
  (let ((g (init-gomoku 5)))
    (apply #'do-move! g '(0 0))
    (apply #'do-move! g '(1 0))
    (apply #'do-move! g '(0 1))
    (apply #'do-move! g '(2 0))
    (apply #'do-move! g '(0 2))
    (apply #'do-move! g '(3 0))
    (apply #'do-move! g '(0 3))
    (apply #'do-move! g '(4 0))
    (apply #'do-move! g '(0 4))
    (print g)
    (eq *black* (who-wins? g))
    ))

;; TEST-WHO-WINS?-WHITE
;; ----------------------------

(defun test-who-wins?-white
  ()
  (let ((g (init-gomoku 5)))
    (apply #'do-move! g '(1 1))
    (apply #'do-move! g '(1 0))
    (apply #'do-move! g '(0 1))
    (apply #'do-move! g '(2 0))
    (apply #'do-move! g '(0 2))
    (apply #'do-move! g '(3 0))
    (apply #'do-move! g '(0 3))
    (apply #'do-move! g '(4 0))
    (apply #'do-move! g '(0 4))
    (apply #'do-move! g '(0 0))
    (print g)
    (eq *white* (who-wins? g))
    ))

;; TEST-GAME-OVER?1
;; ----------------------------
;; Should be over

(defun test-game-over?1
  ()
  (let ((g (init-gomoku 5)))
    (apply #'do-move! g '(0 0))
    (apply #'do-move! g '(1 0))
    (apply #'do-move! g '(0 1))
    (apply #'do-move! g '(2 0))
    (apply #'do-move! g '(0 2))
    (apply #'do-move! g '(3 0))
    (apply #'do-move! g '(0 3))
    (apply #'do-move! g '(4 0))
    (apply #'do-move! g '(0 4))
    (print g)
    (eq T (game-over? g))
    ))

;; TEST-GAME-OVER?2
;; ----------------------------
;; Should not be over

(defun test-game-over?2
  ()
  (let ((g (init-gomoku 5)))
    (apply #'do-move! g '(0 0))
    (apply #'do-move! g '(1 0))
    (apply #'do-move! g '(0 1))
    (apply #'do-move! g '(2 0))
    (print g)
    (null (game-over? g))
    ))

;; TEST-LEGAL-MOVES1
;; ----------------------------
;; Should be 24 of them

(defun test-legal-moves1
  ()
  (let ((g (init-gomoku 10)))
    (apply #'do-move! g '(4 4))
    (= 24 (length (legal-moves g)))
    ))

;; TEST-LEGAL-MOVES1
;; ----------------------------
;; Should be 34 of them, testing a group of pieces

(defun test-legal-moves2
  ()
  (let ((g (init-gomoku 10)))
    (apply #'do-move! g '(4 4))
    (apply #'do-move! g '(9 9))
    (apply #'do-move! g '(8 9))
    (= 34 (length (legal-moves g)))
    ))
