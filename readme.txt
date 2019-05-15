Under the file directory
1. aclemacs&
2. Enter the common-lisp mode, fi:common-lisp
3. (load "maker")
4. (maker)

5. Play with it
    a. minimax ai against minimax, e.g. (play-against-self 15 1 1)
    b. human vs minimax ai, e.g. (human-vs-minimax 15 T) or (human-vs-minimax 15 T 1), T if minimax first
    c. minimax ai vs mcts-rave ai, e.g. (compete 15 T) or (compete 15 T 1 1000 0.5), T if minimax first
    d. human vs mcts-rave; (compete-i-mcrave 1000 0.5); when prompted to enter move, type in the coordinates (x y) (without the ')
    e. genetic algorithm; (driver 3 2 0.02); come back after approximately 30 min and check the evolutionary information for each generation in evol-log.txt; please see comments in evol-alg.lisp for how to interpret these information. In short, enter one of the values in the last parentheses in place of the 0.5 in (compete 15 T 1 1000 0.5) and (compete-i-mcrave 1000 0.5). There should be a small improvement in performance, albeit this may not be observable in merely few trials. More ambitious evolution can be tried, such as (driver 5 5 0.02), but this will take significantly longer

6. When reviewing, the files mcts.lisp and data.lisp could be skipped, since they were included mainly for compatibility reasons

