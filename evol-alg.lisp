;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Call (driver 5 5 0.02) to see how it runs!
;;  Make sure you do NOT have a file named "evol-log.txt" in the same directory
;;  "evol-log.txt" will be where driver stores evolutionary information accumatively
;;
;;  You may try any positive integer num-gen or pop-size, if you have a computer
;;  or enough power...
;;  for comparison, I run (driver 5 5 0.02) on ssh and was only able to complete
;;  1 generation in 3 hours (maybe the second gen was close to completion)
;;  The mut-sd value of 0.02 was hand-tuned and is therefore recommended for
;;  evolution of k
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  NORMAL-RANDOM
;; --------------------------------------------------------
;;  OUTPUT: Samples a value from the Gaussian distribution centered at mean
;;             and with standard deviation std-dev

(defun normal-random (mean std-dev)
  (do* ((rand-u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-s (+ (* rand-u rand-u) (* rand-v rand-v))
                (+ (* rand-u rand-u) (* rand-v rand-v))))
    ((not (or (= 0 rand-s) (>= rand-s 1)))
     (+ mean
      (* std-dev
        (* rand-u (sqrt (/ (* -2.0 (log rand-s)) rand-s))))))))

;;  LARGEST-NUMBER-INDEX
;; --------------------------------------------------------
;;  OUTPUT: A list '(x,y,z...), where x is the index of the largest entry within
;;            list(input), y is that of the second largest etc.

(defun largest-number-index (list)
  (mapcar #'second
          (stable-sort
           (loop
             for index from 0
             for element in list
             collect (list element index))
           #'>
           :key #'first)))

;;  COMPUTE-FITNESS
;; --------------------------------------------------------
;;  Computes the fitness of running MC-RAVE on k and 2000 sims
;;  A total of 10 trials are run between MC-RAVE and Minimax
;;  5 have MC-RAVE as firsthand player (i.e. *BLACK*) and 5 Minimax
;;  The reason for this is our version of Gomoku is not a fair game.
;;  The fitness value starts at 10, and may be incremented or decremented
;;  by 1 if MC-RAVE with the given k value and 2000 sims wins or loses
;;  against Minimax in one of the 10 matches. The choice of the starting fitness
;;  value was somewhat arbitrary, but it wouldn't matter anyways...

(defun compute-fitness
  (k)
  (let
    ;; min fitness 0 if MC-RAVE loses all 10 matches
    ;; max fitness 20 if MC-RAVE wins all 10 matches
    ;; If all 10 matches end with draw then fitness remains at 10
    ((fitness 10))
    ;; 5 matches with MC-RAVE as firsthand
    (dotimes (i 5)
             (let
               ((result (compete-mcrave-benchmark 2000 k)))
               (cond
                 ((eq result *black*) (incf fitness))
                 ((eq result *white*) (decf fitness)))))
    ;; 5 matches with MC-RAVE as secondhand
    (dotimes (i 5)
             (let
               ((result (compete-benchmark-mcrave 2000 k)))
               (cond
                 ;; since MC-RAVE is playing as secondhand, its pieces are
                 ;; *white*
                 ((eq result *white*) (incf fitness))
                 ((eq result *black*) (decf fitness)))))
    fitness))

;;  COMPUTE-ALL-FITNESS
;; --------------------------------------------------------
;;  applies compute-fitness with a list of k's

(defun compute-all-fitness
  (ks)
  (mapcar #'compute-fitness ks))

;;  MUTATE
;; --------------------------------------------------------
;;  Changes the given k value by a value sampled from Gaussian distribution
;;  featured by mean 0 and standard deviation mut-sd
;;  the mutated k returned is within the range (0,1]

(defun mutate
  (k mut-sd)
  (let
    ((result (+ k (normal-random 0 mut-sd))))
    (cond
      ;; k value, by its definition, has to be greater than 0 and no greater
      ;; than 1
      ((<= result 0) 0.0000001)
      ((> result 1) 1)
      (t result))))

;;  REPRODUCE
;; --------------------------------------------------------
;;  INPUTS: ks, a list of k values in this generation
;;          all-fitness, a list of all the fitness values of k values in ks
;;          pop-size, population size, the length of ks
;;          mut-sd, mutation standard deviation used in MUTATE
;;  OUTPUTS: a list new-ks, with the same length as ks (pop-size). new-ks
;;           contains the new k values to participate the next generation after
;;           selection has been made on fitness and mutation applied.

(defun reproduce
  (ks all-fitness pop-size mut-sd)
  (let*
    ;; sum up the fitness values of this generation's k values
    ;; this value will be used in scaling right below
    ((total-fitness (apply #'+ all-fitness))
     ;; scaling procedure
     ;; in a way, this represents how many offsprings can each k value have
     ;; at maximum
     (scaled-fitnesses (mapcar #'(lambda (x)
                                         ;; multiply by 2 to facilitate the
                                         ;; exclusion of low fitness k values
                                         ;; (for why this facilitates exclusion,
                                         ;;  please see sorted-indices below)
                                         (*
                                          ;; round them up so now we have a
                                          ;; list of INTEGERs that add up to
                                          ;; pop-size
                                          (round
                                           ;; Now these fitnesses add up to
                                           ;; pop-size, which is the goal length
                                           ;; of new-ks
                                           (*
                                            ;; scales all fitness values to add
                                            ;; up to 1
                                            (/ x total-fitness)
                                            pop-size))
                                          2))
                               all-fitness))
     ;; when k values reproduce to make new-ks, since its length is limited
     ;; by pop-size, we give high fitness k values priority to reproduce
     ;; the first entry on this list, index x, points to the k-value that
     ;; has the highest reproduction priority; the second entry on this list,
     ;; index y, points to the k-value with the second highest reproduction
     ;; priority, etc.
     ;; Since the entries of ks, and scaled-fitness have correspondance,
     ;; the same index can point to both a k value and its scaled fitness, i.e.
     ;; how many offsprings can it be allowed to have at maximum, if only there
     ;; were space in new-ks (Please see the while loop below for more)
     (sorted-indices (largest-number-index scaled-fitnesses))
     (new-ks nil))
    ;; As long as there is still space in new-ks, i.e. new-ks has length shorter
    ;; than pop-size, fill it with offsprings of k-values according to the order
    ;; specified by sorted-indices, if the number of offsprings made by a k-value
    ;; exceeds the maximum number allowed by scaled-fitness, move on to the next
    ;; k value pointed to by the next sorted-indices entry
    ;; Notice that because I multiplied scaled-fitness by 2, it is almost
    ;; guaranteed that there will be some k values who have no offspring
    ;; in the next generation
    (while (< (length new-ks) pop-size)
      (let*
        ;; the current index that has the highest reproduction priority
        ((max-ind (pop sorted-indices))
         ;; number of children the k value pointed by max-ind is allowed
         ;; to have at maximum
         (num-children (nth max-ind scaled-fitnesses))
         ;; k value pointed by max-ind
         (k (nth max-ind ks)))
        ;; fill new-ks until either num-children depletes or pop-size is reached
        (dotimes (i num-children)
                 (push (mutate k mut-sd) new-ks)
                 (when (= (length new-ks) pop-size)
                   (return-from reproduce new-ks)))))
    new-ks))

;;  DRIVER
;; ------------------------------------------------------------------
;;  Inputs:  NUM-GEN, the number of evolutionary generations
;;           POP-SIZE, the number of individuals (k values) in each generation
;;           MUT-SD, the standard deviation used in MUTATION
;;  Outputs: NONE
;;  Side Effects: logs evolutionary information, line by line, in the file
;;                "evol-log.txt" in the form "<genaration number>,<k-values
;;                 used in this generation>,<fitness values of the k-values>,
;;                 <k-values evolved from this generation and to be used in
;;                  the next>"


(defun driver
  (num-gen pop-size mut-sd)
    (let
      ;; Initializes a list of k values as the first generation
      ;; since all k values have to be within the range (0,1]
      ;; this list will be values between 0 and 1, with step size
      ;; 1 / pop-size, excluding the first value which would be close
      ;; to 0
      ((current-ks (rest (list-gen (/ 1 pop-size)))))
      (dotimes (i num-gen)
               (with-open-file (str "evol-log.txt"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                 (let*
                   ;; computes the fitness values of this generation's k values
                   ((all-fitness (compute-all-fitness current-ks))
                    ;; computes the k values of the next generation
                    (new-ks (reproduce current-ks all-fitness pop-size mut-sd)))
                   (format str "~A," i)
                   (format str "~A," current-ks)
                   (format str "~A," all-fitness)
                   (format str "~A~%" new-ks)
                   (setf current-ks new-ks))))))
