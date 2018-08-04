;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This implements two functions, eval-path and plan that help move a
;; a robot through a chessboard-like world with blocks in certain locations

(require rackunit)
(require "extras.rkt")
(check-location "08" "robot.rkt")

(provide
 path
 eval-plan)

;; By Frederick and Oleksandr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN CONSTANTS

;;;;;;;;;;;;;;;;;;;;

(define 0STEP 0)
(define 1STEP 1)
(define 2STEP 2)

;;;;;;;;;;;;;;;;;;;;

;; END CONSTANTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Data definitions

;;;;;;;;;;;

;; A Position is a (list Integer Integer)
;; (list x y) represents the position (x,y).
;; Note: this is not to be confused with the built-in data type Posn.

;; TEMPLATE:
#; (define (pos-fn pos)
     (... (first pos)
          (second pos)))

;;;;;;;;;;;

;; A ListOfPosition (LOP) is one of
;; -- empty
;; -- (cons Position LOP)

;; TEMPLATE:
;; lop-fn : ListOfPosition -> ??
#; (define (lop-fn lop)
     (cond
       [(empty? lop)...]
       [else (...
              (pos-fn (first lop))
              (lop-fn (rest lop)))]))

;;;;;;;;;;;

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction.

;; TEMPLATE: 
#; (define (move-fn mv)
     (... (first mv)
          (second mv)))

;;;;;;;;;;;

;; A ListOfMove (LOM) is one of
;; -- empty
;; -- (cons Move LOM)

;; TEMPLATE:
;; lom-fn : ListOfMove -> ??
#; (define (lom-fn lom)
     (cond
       [(empty? lom)...]
       [else (...
              (move-fn (first lom))
              (lom-fn (rest lom)))]))

;;;;;;;;;;;

;; A ListOfListOfMove (LLOM) is one of
;; -- empty
;; -- (cons ListOfMove LLOM)

;; TEMPLATE:
;; llom-fn : ListOfListOfMove -> ??
#; (define (llon-fn llom)
     (cond
       [(empty? llom) ...]
       [else (...
              (lom-fn (first llom))
              (llon-fn (rest llom)))]))

;;;;;;;;;;;

;; ValidListOfListOfMove is a ListOfListOfMove that has at least 1 valid ListOfMove that
;;                       leads from starting position to target position without passing
;;                       over any of the blocks

;;;;;;;;;;;

;; A Direction is one of
;; -- "ne"
;; -- "se"
;; -- "sw"
;; -- "nw"

;; Direction constants:
(define NE "ne")
(define SE "se")
(define SW "sw")
(define NW "nw")

;; TEMPLATE:
#;(define (direction-fn d)
  (cond
    [(equal? d NE) ... ]
    [(equal? d SE) ... ]
    [(equal? d SW) ... ]
    [(equal? d NW) ... ]))

;;;;;;;;;;;

;; A Plan is a ListOfMove
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.

;;;;;;;;;;;

;; END Data definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Implementation / Tests data

;;;;;;;;;;;;;;;;;;;;

;; For Position
(define START_POS '(2 5))
(define TARGET_POS '(6 5))
(define TARGET_IS_NEAR '(2 7))
(define TEST_TARGET '(4 9))
(define UP_TARGET '(2 1))

;; For ListOfPosition
(define WALL1
  '((0 3)(2 3)(4 3)
    (0 5)     (4 5)
    (0 7)(2 7)(4 7)))

(define TWO_WALLS
  '((0 3)(4 3)
    (0 5)(4 5)
    (0 7)(4 7)
    (0 9)(4 9)
    (0 11)(4 11)))

(define FAR_AWAY_WALLS
  '((4 5) (10 10)))

(define ONE_STEP_VISIT (list '(2 5) '(3 4)))

;; For ListOfMove
(define GO_UP (list (list NE 1) (list NW 1) (list NE 1) (list NW 1)))
(define ONE_STEP_UP (list (list NE 1)))
(define ONE_STEP_DOWN (list (list SE 1)))
(define INTERIM_2_STEPS (list (list (list "ne" 1) (list "nw" 1) (list "ne" 1))
                              (list (list "ne" 1) (list "nw" 1) (list "se" 1))
                              (list (list "ne" 1) (list "nw" 1) (list "sw" 1))
                              (list (list "ne" 1) (list "nw" 1) (list "nw" 1))))

;; For Plan
(define THIS_MAY_BE_A_PLAN
  (list (list "nw" 2) (list "sw" 2)
        (list "se" 1) (list "sw" 1)
        (list "se" 2) (list "ne" 1)
        (list "se" 1) (list "ne" 1)
        (list "se" 1)))

;;;;;;;;;;;;;;;;;;;;

;; END Implementation / Tests data 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN functions required by problem set

;;;;;;;;;;;;;;;;;;;;
;;   Interfaces   ;;
;;;;;;;;;;;;;;;;;;;;

;; dirs-equal? : Direction Direction -> Boolean
;; GIVEN: 2 directions to compare
;; RETURNS: true if directions are equal, else - false
;; STRATEGY: Use simpler functions
(define (dirs-equal? dir1 dir2)
  (string=? dir1 dir2))

;; EXAMPLES:
;; (dirs-equal? NE SE) => false
;; (dirs-equal? NE NE) => true

;; TESTS: tested in path and eval-plan

;;;;;;;;;;;;;;;;;;;;

;; positions=? : Position Position -> Boolean
;; GIVEN: 2 Positions
;; RETURNS: true if coordinates of given positions are the same, else - false
;; STRATEGY: Use simpler functions
(define (positions=? pos1 pos2)
  (equal? pos1 pos2))

;; EXAMPLES:
;; (positions=? (22 33) (22 33)) => true
;; (positions=? (22 33) (44 33)) => false

;; TESTS: tested in path and eval-plan

;;;;;;;;;;;;;;;;;;;;

;; move-direction : Move -> Direction
;; move-steps : Move -> PosInt
;; GIVEN: a Move mv
;; RETURNS: direction or number of steps of the given move
;; STRATEGY: Use templete for Move on mv
(define (move-direction mv) (first mv))
(define (move-steps mv) (second mv))

;; EXAMPLES:
;; (move-direction (list NE 5)) => "ne"
;; (move-steps (list NE 5)) => 5

;; TESTS: tested in path and eval-plan

;;;;;;;;;;;;;;;;;;;;

;; pos-x : Position -> Integer
;; pos-y : Position -> Integer
;; GIVEN: a Position pos
;; RETURNS: x or y position of given Position
;; STRATEGY: Use templete for Position on pos
(define (pos-x pos) (first pos))
(define (pos-y pos) (second pos))

;; EXAMPLES:
;; (pos-x (list 2 5)) => 2
;; (pos-y (list 2 5)) => 5

;; TESTS: tested in path and eval-plan

;;;;;;;;;;;;;;;;;;;;
;;        1       ;;
;;;;;;;;;;;;;;;;;;;;

;; path : Position Position ListOfPosition -> MaybePlan
;; GIVEN:
;;   1. the starting position of the robot,
;;   2. the target position that robot is supposed to reach
;;   3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from the
;;          starting position to the target position without passing over
;;          any of the blocks, or false if no such sequence of moves exists.
;; STRATEGY: Divide into cases on start and tgt, and plan calculation
(define (path start tgt blocks)
  (cond
    [(positions=? start tgt) empty]
    [(boolean? (init-possible-moves start tgt blocks)) false]
    [else (first-valid-plan
           start
           tgt
           (init-possible-moves start tgt blocks))]))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (path START_POS TEST_TARGET (rest WALL1))
   THIS_MAY_BE_A_PLAN
   "There is a path around blocks")
  (check-equal?
   (path START_POS TEST_TARGET WALL1)
   false
   "No path, there are blocks around starting position")
  (check-equal?
   (path START_POS START_POS WALL1)
   empty
   "Empty path because start and target positions are same"))

;; EXAMPLES: todo move to tests
;(path (list 2 5) (list 2 6) empty)
;(path (list 2 5) (list 4 9) WALL1)
;(path (list 2 5) (list 4 9) (rest WALL1))
;(path (list -3 6) (list 7 6) TWO_WALLS)


;;;;;;;;;;;;;;;;;;;;

;; init-possible-moves : Position Position ListOfPosition -> MaybeValidListOfListOfMove
;; GIVEN:
;;  1. the starting position of the robot,
;;  2. the target position that robot is supposed to reach
;;  3. A list of the blocks on the board
;; RETURNS: a list of lists of moves or false if no such sequence of moves exists
;; WHERE: at least one of the lists of moves, when executed, will take the robot
;;        from the starting position to the target position without passing over
;;        any of the blocks, if a list is returned
;; STRATEGY: Call a more general function
(define (init-possible-moves start tgt blocks)
  (possible-moves
   start
   tgt
   blocks
   (- (depth start tgt blocks) 1STEP)
   (make-moves start empty empty (list start) blocks)
   (list start)))

;; EXAMPLE:
;; (init-possible-moves START_POS TARGET_IS_NEAR TWO_WALLS)
;; => (list (list (list "ne" 1) (list "nw" 1))
;;          (list (list "se" 1) (list "sw" 1))
;;          (list (list "sw" 1) (list "se" 1))
;;          (list (list "nw" 1) (list "ne" 1)))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; depth : Position Position ListOfPosition -> NonNegInt
;; GIVEN: 
;;   1. the starting position of the robot,
;;   2. the target position that robot is supposed to reach
;;   3. A list of the blocks on the board
;; RETURNS: depth that is used for halting measure and is calculated with
;;          the following formula: d = (blocks * 2) + |distance to target|
;; STRATEGY: Combiine simpler functions
(define (depth start tgt blocks)
  (+
   (* 2STEP (length blocks))
   (abs (- (pos-x start) (pos-x tgt)))
   (abs (- (pos-y start) (pos-y tgt)))))

;; EXAMPLE:
;; (depth '(2 1) '(4 1) '('(1 1) '(2 2))) => 6

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; make-moves : Position ListOfMove ListOfListOfMove ListOfPosition ListOfPosition
;;              -> ListOfListOfMove
;; GIVEN:
;;   1. a Position         as start, which will be verified and expended, if possible
;;   2. a ListOfMove       as lom, which represents list of already provided moves
;;   3. a ListOfListOfMove as llom, which represents ListOfListOfMove-s
;;                         that were performed from starting position before current step
;;   4. a ListOfPosition   as lovp, which represents list of positions that were visited
;;                         during the given llom
;;   5. a ListOfPosition   as blocks, which represents list of blocks
;; WHERE: lovp is a list of all visited positions, that were visited by lom and
;;        other list of moves that are not given to this function
;; RETURNS: ListOfListOfMove that expands given llom, except that new move was added
;;          in all directions, if positions in those directions were not yet visited and
;;          not blocked, else - empty list
;; STRATEGY: Combine simpler functions
(define (make-moves start lom llom lovp blocks)
  (if (not (empty? llom))
      (append llom
              (try-move-all-directions start blocks lovp lom))
      (try-move-all-directions start blocks lovp lom)))

;; EXAMPLE:
;; (make-moves START_POS ONE_STEP_UP empty ONE_STEP_VISIT TWO_WALLS)
;; => (list (list (list "ne" 1) (list "nw" 1)))
;; (make-moves START_POS ONE_STEP_UP empty ONE_STEP_VISIT FAR_AWAY_WALLS)
;; => (list (list (list "ne" 1) (list "ne" 1))
;;          (list (list "ne" 1) (list "nw" 1)))
;; (make-moves START_POS empty empty (list START_POS) TWO_WALLS)
;; => (list (list (list "ne" 1))
;;          (list (list "se" 1))
;;          (list (list "sw" 1))
;;          (list (list "nw" 1)))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; try-move-all-directions : Position ListOfPosition ListOfPosition ListOfMove
;;                           -> ListOfListOfMove
;; GIVEN:
;;   1. a Position       as start, which will be verified and expended, if possible
;;   2. a ListOfPosition as blocks, which represents list of blocks
;;   3. a ListOfPosition as lovp, which represents list of already visited positions
;;   4. a ListOfMove     as lom, which represents list of already provided moves
;; WHERE: lovp is a list of all visited positions, that were visited by lom and
;;        other list of moves that are not given to this function
;; RETURNS: ListOfListOfMove that contains list of given lom-s, which are expended
;;          in all possible directions, if at least one position in those directions
;;          was not yet visited and not blocked, else - empty list
;; STRATEGY: Combine simpler functions
(define (try-move-all-directions start blocks lovp lom)
  (append
   (move-if-possible start blocks lovp lom NE)
   (move-if-possible start blocks lovp lom SE)
   (move-if-possible start blocks lovp lom SW)
   (move-if-possible start blocks lovp lom NW)))

;; EXAMPLE:
;; (try-move-all-directions START_POS TWO_WALLS ONE_STEP_VISIT ONE_STEP_UP)
;; => (list (list (list "ne" 1) (list "nw" 1)))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; move-if-possible : Position ListOfPosition ListOfPosition ListOfMove Direction
;;                     -> ListOfListOfMove
;; GIVEN:
;;   1. a Position       as start, which will be verified and expended, if possible
;;   2. a ListOfPosition as blocks, which represents list of blocks
;;   3. a ListOfPosition as lovp, which represents list of already visited positions
;;   4. a ListOfMove     as lom, which represents list of already provided moves
;;   5. a Direction      as dir, which represents direction to verify and move
;; WHERE: lovp is a list of all visited positions, that were visited by lom and
;;        other list of moves that are not given to this function
;; RETURNS: ListOfListOfMove that contains given lom, except that new move was added 
;;          in given direction, if position in that direction was not yet visited and
;;          is not blocked, else - empty list
;; STRATEGY: Combine simpler functions
(define (move-if-possible start blocks lovp lom dir)
  (if (cannot-move? start blocks lovp lom dir)
      empty
      (list (append lom (list (list dir 1STEP))))))

;; EXAMPLE:
;; (move-if-possible START_POS TWO_WALLS empty empty NE)
;; => (list (list (list "ne" 1)))
;; (move-if-possible START_POS TWO_WALLS (list (list 3 4)) empty NE)
;; => '()

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; cannot-move? : Position ListOfPosition ListOfPosition ListOfMove Direction -> Boolean
;; GIVEN:
;;   1. a Position       as start, which will be verified and expended, if possible
;;   2. a ListOfPosition as blocks, which represents list of blocks
;;   3. a ListOfPosition as lovp, which represents list of already visited positions
;;   4. a ListOfMove     as lom, which represents list of already provided moves
;;   5. a Direction      as dir, which represents direction to verify and move
;; WHERE: lovp is a list of all visited positions, that were visited by lom and
;;        other list of moves that are not given to this function
;; RETURNS: false if position in given direction was not yet visited and is
;;          not blocked, else - true
;; STRATEGY: Combine simpler functions
(define (cannot-move? start blocks lovp lom dir)
  (or (member (move-from (eval-plan start blocks lom) dir)
              lovp)
      (member (move-from (eval-plan start blocks lom) dir)
              blocks)))

;; EXAMPLE:
;; (cannot-move? START_POS TWO_WALLS empty empty NE)
;; => false
;; (cannot-move? START_POS TWO_WALLS (list (list 3 4)) empty NE)
;; => true

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; possible-moves : Position Position ListOfPosition NonNegInt ListOfListOfMove ListOfPosition
;;                  -> MaybeListOfListOfMove
;; GIVEN:
;;   1. A Position         as start - the starting position of the robot
;;   2. A Position         as tgt - the target position that robot is supposed to reach
;;   3. A ListOfPosition   as blocks - list of the blocks on the board
;;   4. A NonNegInt        as d - depth of calculation
;;   5. A ListOfListOfMove as llom - list of lists of moves
;;   6. A ListOfPosition   as lovp - list of visited positions
;; WHERE: d is decreased by 1, by helper functions with every iteration
;;        , and llom represents lists of moves that lead from the start point till lovp
;;        , and lovp represents points that were reached untill now
;; RETURNS: lists of moves, where at least one list leads to target position,
;;          otherwise - false
;; STRATEGY: Recur on d if needed
;; HALTING MEASURE: (= 0 d) or (target-found? start tgt llom)
;; TERMINATION ARGUMENT: (= 0 d) is guaranteed to be non-negative, and it decreases by 1
;;                       at every recursive call, because of (- d 1STEP).
;;                       (target-found? start tgt llom) is not guaranteed to trigger
(define (possible-moves start tgt blocks d llom lovp) 
  (cond
    [(target-found? start tgt llom) llom]
    [(= 0STEP d) false]
    [else
     (calculate-visited-positions
      start
      tgt
      blocks
      (- d 1STEP)
      (expand-lists-of-moves start blocks lovp llom)
      lovp)]))

;; EXAMPLE:
;; (possible-moves
;;   START_POS TARGET_IS_NEAR TWO_WALLS 10 (list ONE_STEP_UP) (list START_POS))
;; => INTERIM_2_STEPS

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; target-found? : Position Position ListOfListOfMove -> Boolean
;; GIVEN:
;;   1. A Position         as start - the starting position of the robot
;;   2. A Position         as tgt - the target position that robot is supposed to reach
;;   3. A ListOfListOfMove as llom - list of lists of moves
;; WHERE: llom represents lists of moves that were perfomed till now
;; RETURNS: true if at least one of the given lists of moves leads to target position
;; STRATEGY: Use HOF ormap on llom
(define (target-found? start tgt llom)
  (ormap
   (lambda (lom)
     (positions=? tgt (eval-plan start empty lom)))
   llom))

;; EXAMPLE:
;; (target-found? (list 0 13) (list 6 13)
;;                (list (list (list SE 3) (list NE 3)) (list (list NE 3) (list NW 3))))
;; => true
;; (target-found? (list 0 13) (list 6 13)
;;                (list (list (list SE 2) (list NE 4)) (list (list NE 3) (list NW 3))))
;; => false

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; calculate-visited-positions : Position Position ListOfPosition NonNegInt ListOfPosition
;;                               ListOfListOfMove -> MaybeListOfListOfMove
;; GIVEN:
;;   1. A Position         as start - the starting position of the robot
;;   2. A Position         as tgt - the target position that robot is supposed to reach
;;   3. A ListOfPosition   as blocks - list of the blocks on the board
;;   4. A NonNegInt        as d - depth of calculation
;;   5. A ListOfPosition   as lovp - list of visited positions
;;   6. A ListOfListOfMove as llom - list of lists of moves
;; RETURNS: lists of moves that, are similar to given llom, except that new moves are 
;;          added, if there are unvisited, that are expanded comparing to
;;          given lovp, and unblocked positions, otherwise - false
;; STRATEGY: Combine simpler functions
(define (calculate-visited-positions start tgt blocks d llom lovp)
  (possible-moves
   start
   tgt
   blocks
   d
   llom
   (expand-visited-positions start lovp llom)))

;; EXAMPLE:
;; (calculate-visited-positions
;;  START_POS TARGET_IS_NEAR TWO_WALLS 10 empty (list ONE_STEP_DOWN))
;; => false

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; expand-visited-positions : Position ListOfPosition ListOfListOfMove -> ListOfPosition
;; GIVEN:
;;   1. A Position         as start - the starting position of the robot
;;   2. A ListOfPosition   as lovp - list of visited positions
;;   3. A ListOfListOfMove as llom - list of lists of moves
;; RETURNS: a ListOfPosition (visited) that is similar to given lovp, except that new 
;;          positions are added in case, when new moves were added to llom comparing 
;;          to previous iteration, otherwise - same lovp
;; STRATEGY: Use HOF foldl on llom
(define (expand-visited-positions start lovp llom)
  (foldl
   (lambda (lom lovp-accum)
     (if (member (eval-plan start empty lom) lovp-accum)
         lovp-accum
         (append lovp-accum (list (eval-plan start empty lom)))))
   lovp
   llom))

;; EXAMPLE:
;; (expand-visited-positions START_POS (list START_POS) INTERIM_2_STEPS)
;; => (list (list 2 5) (list 3 2) (list 3 4) (list 1 4) (list 1 2))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; expand-lists-of-moves : Position ListOfPosition ListOfPosition ListOfListOfMove
;;                         -> ListOfListOfMove
;; GIVEN:
;;   1. A Position         as start - the starting position of the robot
;;   2. A ListOfPosition   as blocks - list of the blocks on the board
;;   3. A ListOfPosition   as lovp - list of visited positions
;;   4. A ListOfListOfMove as llom - list of lists of moves
;; RETURNS: an expended llom if at least one of the ListOfMove has
;;          positions to move that were not visited and are not blocked,
;;          else empty list
;; STRATEGY: Use HOF foldl on llom
(define (expand-lists-of-moves start blocks lovp llom)
  (foldl
   (lambda (lom temp-llom)
     (make-moves start lom temp-llom lovp blocks))
   empty
   llom))

;; EXAMPLE:
;; (expand-lists-of-moves START_POS TWO_WALLS (list ONE_STEP_DOWN) (list ONE_STEP_DOWN))
;; => (list (list (list "se" 1) (list "sw" 1))
;;          (list (list "se" 1) (list "nw" 1)))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; first-valid-plan : Position Position ValidListOfListOfMove -> Plan
;; GIVEN:
;;   1. A Position              as start - the starting position of the robot
;;   1. A Position              as start - the target position that robot is
;;                                         supposed to reach
;;   3. A ValidListOfListOfMove as llom - list of lists of moves
;; WHERE: llom contains at least 1 valid ListOfMove
;; RETURNS: a plan that, when executed, will take the robot from
;;          the starting position to the target position without passing over any
;;          of the blocks
;; WHERE: returned Plan is the first valid ListOfMove that is transformed to Plan
;; STRATEGY: Recur on llom if needed
;; HALTING MEASURE/TERMINATION ARGUMENT: not used, because ListOfListOfMove
;;                                       contains at least 1 valid ListOfMove
(define (first-valid-plan start tgt llom)
  (if (positions=? tgt (eval-plan start empty (first llom)))
      (moves-to-plan (first llom))
      (first-valid-plan start tgt (rest llom))))

;; EXAMPLE:
;;  (first-valid-plan (list 0 13) (list 6 13)
;;                (list (list (list SE 3) (list NE 3)) (list (list NE 3) (list NW 3))))

;; (target-found? (list 0 13) (list 6 13) (list (list SE 3) (list NE 3)))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; moves-to-plan : ListOfMove -> Plan
;; GIVEN: a ListOfMove as lom
;; RETURNS: a Plan that is compressed from the given lom
;; STRATEGY: Call a more general functions
(define (moves-to-plan lom)
  (compress-moves (first lom) 1STEP (rest lom)))

;; EXAMPLE:
;; (moves-to-plan (list (list NE 1) (list NE 1) (list NE 1) (list SE 1) (list NE 1)))
;; => (list (list "ne" 3) (list "se" 1) (list "ne" 1))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;

;; compress-moves : Move Count ListOfMove -> Plan
;; GIVEN: a Move mv, a count cnt, and a sublist of some list of moves lst0
;; WHERE: the sublist is preceded in lst0 by cnt copies of mv
;; RETURNS: a Plan that is a compressed given ListOfMove
;; STRATEGY: recur on (rest lom) if needed
;; HALTING MEASURE: (max (- hi lo) 0) -> When to stop recursion
;; TERMINATION ARGUMENT: (empty? lom) is guaranteed to become empty
;;                       and it decreases at every recursive call,
;;                       because it is finite
(define (compress-moves mv cnt lom)
  (cond
    [(empty? lom) (list (list (move-direction mv) cnt))]
    [(dirs-equal? (move-direction mv) (move-direction (first lom)))
     (compress-moves mv (+ 1STEP cnt) (rest lom))]
    [else (cons
           (list (move-direction mv) cnt)
           (compress-moves (first lom) 1STEP (rest lom)))]))

;; EXAMPLE:
;; (compress-moves (list NE 1) 1 (list (list NE 1) (list NE 1) (list SE 1) (list NE 1)))
;; => (list (list "ne" 3) (list "se" 1) (list "ne" 1))

;; TESTS: tested in path function

;;;;;;;;;;;;;;;;;;;;
;;        2       ;;
;;;;;;;;;;;;;;;;;;;;

;; eval-plan : Position ListOfPosition Plan ->  MaybePosition
;; GIVEN:
;;   1. the starting position of the robot,
;;   2. A list of the blocks on the board
;;   3. A plan for the robot's motion
;; RETURNS: The position of the robot at the end of executing the plan,
;;          or false if the plan sends the robot to or through any block.
;; STRATEGY: Combine simpler functions
(define (eval-plan pos lop plan)
  (if (no-wall-crash? pos lop plan)
      (first (reverse (list-of-visited-positions pos plan)))
      false))

;; EXAMPLES\TESTS:
(begin-for-test
  (check-equal?
   (eval-plan START_POS TWO_WALLS GO_UP)
   UP_TARGET
   "No obstacles are met")
  (check-equal?
   (eval-plan START_POS WALL1 GO_UP)
   false
   "Will crash into a wall"))

;;;;;;;;;;;;;;;;;;;;

;; no-wall-crash? : Position ListOfPosition Plan -> Boolean
;; GIVEN:
;;   1. the starting position of the robot,
;;   2. A list of the blocks on the board
;;   3. A plan for the robot's motion
;; RETURNS: true if robot will meet no obstacles on his path, else false
;; STRATEGY: Use HOF andmap on (list-of-visited-positions pos plan)
(define (no-wall-crash? pos lop plan)
  (andmap
   (lambda (cur-pos) (not (member cur-pos lop)))
   (list-of-visited-positions pos plan)))

;; EXAMPLES:
;; (no-wall-crash? START_POS WALL1 GO_UP) => false
;; (no-wall-crash? START_POS TWO_WALLS GO_UP) => true

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; list-of-visited-positions : Position Plan -> ListOfPosition
;; GIVEN: a position pos and a plan
;; RETURNS: a ListOfPosition that are visited according to the given plan,
;;          starting from the given position
;; STRATEGY: Combine simpler functions
(define (list-of-visited-positions pos plan)
  (moves-to-positions pos (plan-to-moves plan)))

;; EXAMPLE: list-of-visited-positions START_POS GO_UP)
;; => (list (list 2 5) (list 3 4)
;;          (list 2 3) (list 3 2)
;;          (list 2 1))

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; moves-to-positions : Position ListOfMove -> ListOfPosition
;; GIVEN: starting position and list of moves
;; RETURNS: list of visited positions while performing given list of moves,
;;          starting from and with the given position
;; STRATEGY: Use HOF foldl on lop
(define (moves-to-positions pos lom)
  (foldl
   (lambda (mv lop)
     (expand-positions-by-moves mv lop))
   (list pos)
   lom))

;; EXAMPLE:
;; (moves-to-positions (list 1 1) (list (list "se" 1) (list "ne" 1) (list "se" 1))) =>
;; (list (list 1 1) (list 2 2) (list 3 1) (list 4 2))

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; expand-positions-by-moves : Move ListOfPosition -> ListOfPosition
;; GIVEN: a Move and a ListOfPosition
;; RETURNS: ListOfPosition that is similar to given, except that new Position is added
;;          in the end of the list, that results from the move
;;          from the last position from the given list
;; STRATEGY: Combine simpler functions
(define (expand-positions-by-moves mv lop)
  (append lop (list (move-from (first (reverse lop)) (move-direction mv)))))

;; EXAMPLE:
;; (expand-positions-by-moves (list NE 1) (list (list 3 1) (list 2 2)))
;; => (list (list 3 1) (list 2 2) (list 3 1))

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; move-from : Position Direction -> Position
;; GIVEN: Position and Direction to move
;; RETURNS: new position after taking 1 step in the given direction,
;; STRATEGY: Divide into cases on dir
(define (move-from pos dir)
  (cond
    [(dirs-equal? dir NE) (new-position pos + -)]
    [(dirs-equal? dir SE) (new-position pos + +)]
    [(dirs-equal? dir SW) (new-position pos - +)]
    [(dirs-equal? dir NW) (new-position pos - -)]))

;; EXAMPLE:
;; (move-from (list 2 2) NE) => (list 3 1)

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; new-position : Position Function Function -> Position
;; GIVEN: Position pos and 2 operators that describe directions
;; WHERE: + - is NE
;;        + + is SE
;;        - + is SW
;;        - - is NW
;; RETURNS: new position after moving one step in appropriate direction
;; STRATEGY: Use Template for Position on pos
(define (new-position pos fn1 fn2)
  (list (fn1 (pos-x pos) 1STEP)
        (fn2 (pos-y pos) 1STEP)))

;; EXAMPLES:
;; (new-position (list 2 2) + +) => (list 3 3)
;; (new-position (list 2 2) - +) => (list 1 3)

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; plan-to-moves : Plan -> ListOfMove
;; GIVEN: a Plan
;; RETURNS: ListOfMove where all moves that have more than 1 step are divided into
;;          several moves in appropriate quantity with same direction, but only 1 step
;;          per each move
;; STRATEGY: Use template for ListOfMove on plan + Recur on (rest plan), if needed
;; HALTING MEASURE: (empty? plan)
;; TERMINATION ARGUMENT: plan is guaranteed to be finite
;;                       and it decreases at every recursive call
(define (plan-to-moves plan)
  (cond
    [(empty? plan) empty]
    [else (append
           (divide-into-steps (first plan))
           (plan-to-moves (rest plan)))]))

;; EXAMPLE:
;; (plan-to-moves (list (list SE 3) (list NE 2))) =>
;; (list (list "se" 1) (list "se" 1) (list "se" 1)
;;       (list "ne" 1) (list "ne" 1))

;; TESTS: tested in eval-plan function

;;;;;;;;;;;;;;;;;;;;

;; divide-into-steps : Move -> ListOfMove
;; GIVEN: a Move
;; RETURNS: list of 1 move, similar to given, if Move contains only 1 step,
;;          else list of 1 step moves that have same direction as given move, and
;;          are repeated same amount of times as quantity of steps in given Move
;; STRATEGY: Use template for Move on mv + Recur on (move-steps mv) if needed
;; HALTING MEASURE: (= (move-steps mv) 0)
;; TERMINATION ARGUMENT: (= (move-steps mv) 0) is guaranteed to be non-negative,
;;                       and it decreases at every recursive call by 1
(define (divide-into-steps mv)
  (cond
    [(= (move-steps mv) 0STEP) empty]
    [else
     (cons (list (move-direction mv) 1STEP)
           (divide-into-steps (list (move-direction mv)
                                    (- (move-steps mv) 1STEP))))]))

;; EXAMPLE:
;; (divide-into-steps (list SE 3)) => (list (list "se" 1) (list "se" 1) (list "se" 1))

;; TESTS: tested in eval-plan function