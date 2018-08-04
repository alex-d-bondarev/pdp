;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screen saver #4
;; Similar to screen saver #3, except that pen functionality is added

;; start with (screensaver 2)
;; slower variant
;; OR (screensaver 0.5)
;; faster variant
;; smaller input number represents faster simulations 

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(check-location "04" "screensaver-4.rkt")

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 rect-selected?
 world-after-mouse-event
 rect-after-mouse-event
 world-rects
 rect-after-key-event
 rect-pen-down?
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; INTERPRETATION: according to the task

(define RECT_COLOR "blue")
(define SELECTED_RECT_COLOR "red")
(define RECT_STYLE "outline")
(define RECT_FONT 10)

(define RECT_WIDTH 60)
(define RECT_HIGH 50)
(define HALF_RECT_WIDTH (/ RECT_WIDTH 2))
(define HALF_RECT_HIGH (/ RECT_HIGH 2))

(define UNSELECTED_RECT (rectangle RECT_WIDTH RECT_HIGH RECT_STYLE RECT_COLOR))
(define SELECTED_RECT (rectangle RECT_WIDTH RECT_HIGH RECT_STYLE SELECTED_RECT_COLOR))

(define CIRCLE_RADIUS 5)
(define RED-CIRCLE (circle CIRCLE_RADIUS RECT_STYLE SELECTED_RECT_COLOR))

(define DOT_RADIUS 1)
(define DOT_COLOR "black")
(define DOT_STYLE "solid")
(define DOT (circle DOT_RADIUS DOT_STYLE DOT_COLOR))

(define PAUSE_KEY " ")
(define NEW_RECT_KEY "n")
(define LEFT_KEY "left")
(define RIGHT_KEY "right")
(define UP_KEY "up")
(define DOWN_KEY "down")
(define PEN_DOWN_KEY "d")
(define PEN_UP_KEY "u")

(define NEW_LOR empty)

(define CANVAS_WIDTH 400)
(define CANVAS_HIGH 300)
(define HALF_CANVAS_WIDTH (/ CANVAS_WIDTH 2))
(define HALF_CANVAS_HIGH (/ CANVAS_HIGH 2))
(define EMPTY_CANVAS (empty-scene CANVAS_WIDTH CANVAS_HIGH))

(define INCREASE_BY 2)

;; INTERPRETATION: Simplifies calculations of wall hit scenario

(define CANVAS_COUNTABLE_WIDTH (- CANVAS_WIDTH (/ RECT_WIDTH 2))) ;; right wall
(define CANVAS_COUNTABLE_HIGH (- CANVAS_HIGH (/ RECT_HIGH 2)))    ;; bottom wall

(define ZERO_COORDINATE_X (/ RECT_WIDTH 2))                       ;; left wall
(define ZERO_COORDINATE_Y (/ RECT_HIGH 2))                        ;; top wall

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct rect (x y vx vy selected? sel-x sel-y pen-down?))
;; A Rectangle is a (make-rect NonNegInt NonNegInt Int Int Boolean Int Int)
;; x and y are coordinates
;; vx and vy are represenations of velocity in the x- and y- directions
;; selected? describes whether or not the rectangle is selected
;; sel-x and sel-y represent distance from center of rectangle to mouse point, if selected
;; pen-down? describes whether or not the rectangle's pen is up or down

;; TEMPALTE:
;; rect-fn : Rectangle -> ??
#; (define (rect-fn rct)
     (...(rect-x rct)
         (rect-y rct)
         (rect-vx rct)
         (rect-vy rct)
         (rect-selected? rct)
         (rect-sel-x rct)
         (rect-sel-y rct)
         (rect-pen-down? rct)))

;; EXAMPLES/CONSTANTS for tests:
(define RECT_IN_LEFT_TOP_CORNER_NON_MOVING (make-rect 60 50 0 0 false 0 0 false))
(define RECT_NEAR_RIGHT_WAL_NON_MOVING (make-rect 350 100 -5 0 false 0 0 false))
(define RECT_IN_CENTER_SELECTED (make-rect 200 100 10 10 true 5 5 false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfRectangle (LOR) is either
;; -- empty
;; -- (cons Rectangle LOR)

;; TEMPLATE:
;; lor-fn : LOR -> ??
#; (define (lor-fn lor)
     (cond
       [(empty? lor)...]
       [else (...
              (rect-fn (first lor))
              (lor-fn (rest lor)))]))

;; EXAMPLES/CONSTANTS for tests:
(define TEST_LOR
  (list (make-rect 200 100 -12 20 false 0 0 false)
        (make-rect 200 200 23 -14 false 0 0 false)))
(define TEST_LOR_AFTER_TIC
  (list (make-rect 188 120 -12 20 false 0 0 false)
        (make-rect 223 186 23 -14 false 0 0 false)))
(define TEST_LOR_AFTER_KEY_EVENT
  (list RECT_IN_LEFT_TOP_CORNER_NON_MOVING
        RECT_NEAR_RIGHT_WAL_NON_MOVING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct dot (x-pos y-pos))
;; A Dot is a (make-dot NonNegInt NonNegInt)
;; x-pos and y-pos are coordinates of the Dot

;; TEMPLATE:
;; dot-fn : Dot -> ??
#; (define (dot-fn d)
     (...(dot-x-pos d)
         (dot-x-pos d)))

;; EXAMPLES/CONSTANTS for tests:
(define CENTER_DOT (make-dot HALF_RECT_WIDTH HALF_RECT_HIGH))
(define DOT_NEAR_CENTER (make-dot (+ 5 HALF_RECT_WIDTH) (+ 5 HALF_RECT_HIGH)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfDots (LOD) is either
;; -- empty
;; -- (cons Dot LOD)

;; TEMPLATE:
;; lod-fn : LOD -> ??
#; (define (lod-fn lod)
     (cond
       [(empty? lod)...]
       [else (...
              (rect-fn (first lod))
              (lod-fn (rest lod)))]))

;; EXAMPLES/CONSTANTS for tests:
(define EMPTY_LOD empty)
(define TWO_DOTS
  (list CENTER_DOT DOT_NEAR_CENTER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world (rects dots paused?))
;; A WorldState is a (make-world ListOfRectangle ListOfDots Boolean)
;; rects - is a list of created Rectangles (LOR)
;; dots - is a list of created Dots (LOD)
;; paused? describes whenever the world is paused

;; TEMPLATE:
;; world-fn : WorldSate -> ??
#; (define (world-fn w)
     (...(world-rects w)
         (world-dots w)
         (world-paused? w)))

;; EXAMPLES/CONSTANTS for tests:
(define TEST_WORLD (make-world TEST_LOR EMPTY_LOD false))
(define TEST_WORLD_AFTER_TIC (make-world TEST_LOR_AFTER_TIC EMPTY_LOD false))
(define TEST_WORLD_BEFORE_KEY_EVENT (make-world TEST_LOR_AFTER_KEY_EVENT EMPTY_LOD true))
(define TEST_WORLD_AFTER_KEY_EVENT (make-world TEST_LOR_AFTER_KEY_EVENT EMPTY_LOD false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part of the world struct

;; world-rects : WorldState -> ListOfRectangle
;; RETURNS: the specified attribute of the WorldState
;; NOTE: this replaces world-rect1 and world-rect2.
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; EXAMPLES:
; (world-rects (initial-world 88)) => empty
; (world-rects TEST_WORLD) =>
;    (list (make-rect 200 100 -12 20 false 0 0 false)
;          (make-rect 200 200 23 -14 false 0 0 false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; EXAMPLE:
; (world-paused? paused_world) => true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part of the rect struct

;; rect-x : Rectangle -> NonNegInt
;; rect-y : Rectangle -> NonNegInt
;; rect-vx : Rectangle -> Int
;; rect-vy : Rectangle -> Int
;; RETURNS: the coordinates of the center of the rectangle and its
;; velocity in the x- and y- directions.

;; EXAMPLES:
#|
(rect-x RECT_NEAR_RIGHT_WAL_NON_MOVING) => 250
(rect-y RECT_NEAR_RIGHT_WAL_NON_MOVING) => 100
(rect-vx RECT_NEAR_RIGHT_WAL_NON_MOVING) => -5
(rect-vy RECT_NEAR_RIGHT_WAL_NON_MOVING) => 0
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-selected? : Rectangle -> Boolean
;; RETURNS: true if the given rectangle is selected.

;; EXAMPLES: 
;  (rect-selected? RECT_NEAR_RIGHT_WAL_NON_MOVING) => false
;  (rect-selected? (make-rect 200 100 30 20 true 0 0 false) => true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main Function

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver sim-speed)
  (big-bang (initial-world sim-speed)
            (on-tick world-after-tick sim-speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;; TESTS: no tests for big-bang
;; EXAMPLES:
;  (screensaver 0.1) very fast screensaver, where tic speed = 0.1 sec
;  (screensaver 1) regular screensaver, where tic speed = 1 sec
;  (screensaver 10) very slow screensaver, where tic speed = 10 sec

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Number -> WorldState
;; GIVEN: any Number (ignored)
;; RETURNS: the initial world specified in the problem set
;; STARTEGY: Combine Simpler Functions
(define (initial-world x)
  (make-world NEW_LOR EMPTY_LOD true))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (initial-world 666)
   (make-world NEW_LOR EMPTY_LOD true)
   "Initial world is defined by problem set"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN: WorldState
;; RETURNS: the world state that should follow the given world state after a tick.
;; STRATEGY: Use template for WorldState on w
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (lor-after-tick (world-rects w))
      (dots-after-tick (world-rects w) (world-dots w))
      (world-paused? w))))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (world-after-tick TEST_WORLD) TEST_WORLD_AFTER_TIC
   "After tick given world should update accordingly:
Rectangle #1 should be: x 200 -> 188, y 100 -> 120, vx -12, vy 20
Rectangle #2 should be: x 200 -> 223, y 200 -> 186, vx 23, vy -14")
  (check-equal?
   (world-after-tick TEST_WORLD_BEFORE_KEY_EVENT)
   TEST_WORLD_BEFORE_KEY_EVENT
   "Paused world will not change after tick"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lor-after-tick : ListOfRectangle -> ListOfRectangle
;; GIVEN: A ListOfRectangle
;; RETURNS: The state of the given ListOfRectangle after tic.
;; STRATEGY: use template for ListOfRectangle on lor
(define (lor-after-tick lor)
  (cond
       [(empty? lor) empty]
       [else (cons
              (rect-after-tick (first lor))
              (lor-after-tick  (rest lor)))]))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (lor-after-tick
    (list (make-rect 200 100 -12 20 false 0 0 false)
          (make-rect 200 200 0 0 false 0 0 false)
          (make-rect 200 200 30 -5 true 0 0 false)))
    (list (make-rect 188 120 -12 20 false 0 0 false)
          (make-rect 200 200 0 0 false 0 0 false)
          (make-rect 200 200 30 -5 true 0 0 false))
    "Unly unselected rect with velocity > 0 will change"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-tick : Rectangle -> Rectangle
;; GIVEN: State of Rectangle rct
;; RETURNS: The state of given Rectangle after tic.
;; WHERE: State is not changed if rectangle is selected.
;; STRATEGY: Combine simpler functions
(define (rect-after-tick rct)
  (if (rect-selected? rct)
      rct
      (set-rect-x (set-rect-y rct))))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (rect-after-tick (make-rect 360 265 30 20 false 0 0 true))
   (make-rect 370 275 -30 -20 false 0 0 true)
   "After hitting bottom right corner, rectangle will stay in that corner and
 velocities will change to opposite")
  (check-equal?
   (rect-after-tick (make-rect 40 35 -30 -20 false 0 0 false))
   (make-rect 30 25 30 20 false 0 0 false)
   "After hitting top lefy corner, rectangle will stay in that corner and
 velocities will change to opposite")
  (check-equal?
   (rect-after-tick (make-rect 100 200 -10 20 false 0 0 false))
   (make-rect 90 220 -10 20 false 0 0 false)
   "Without hitting any wall x-coord will change to 90 and y-coord will change to 220.
 Velocities will not change")
  (check-equal?
   (rect-after-tick (make-rect 100 200 -10 20 true 0 0 false))
   (make-rect 100 200 -10 20 true 0 0 false)
   "State should not be changed. Rectangle is selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-rect-x : Rectangle -> Rectangle
;; GIVEN: Rectangle
;; RETURNS: given Rectangle, except that x-coordinate has changed.
;  In case of wall hit, velocity vx will also change.
;; STRATEGY: Combine simpler functions
(define (set-rect-x rct)
  (if (> (rect-vx rct) 0)
      (positive-x-direction rct)
      (negative-x-direction rct)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (set-rect-x (make-rect 360 265 30 20 false 0 0 false))
   (make-rect 370 265 -30 20 false 0 0 false)
   "After right wall hit x will equal 370 and x velocity will change from 30 to -30")
  (check-equal?
   (set-rect-x (make-rect 300 200 30 20 false 0 0 false))
   (make-rect 330 200 30 20 false 0 0 false)
   "Without right wall hit, x will increase by 30 and will equal 330")
  (check-equal?
   (set-rect-x (make-rect 40 35 -30 -20 false 0 0 false))
   (make-rect 30 35 30 -20 false 0 0 false)
   "After left wall hit x will equal 30 and x velocity will change from -30 to 30")
  (check-equal?
   (set-rect-x (make-rect 300 200 -30 -20 false 0 0 false))
   (make-rect 270 200 -30 -20 false 0 0 false)
   "Without left wall hit, x will decrease by 30 and will equal 180"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positive-x-direction : Rectangle -> Rectangle
;; GIVEN: Rectangle
;; RETURNS: Rectangle that has moved right, with updated x-coordinate based on velocity.
;  In case of wall hit, x-coordinate will be placed near the wall,
;  and velocity vx will change to negative number.
;; STRATEGY: Use template for Rectangle on rct
(define (positive-x-direction rct)
  (if (rect-hits-positive-wall (rect-x rct) (rect-vx rct) CANVAS_COUNTABLE_WIDTH)
      (make-rect CANVAS_COUNTABLE_WIDTH
                 (rect-y rct)
                 (- (rect-vx rct))
                 (rect-vy rct)
                 (rect-selected? rct)
                 (rect-sel-x rct)
                 (rect-sel-y rct)
                 (rect-pen-down? rct))
      (make-rect (+ (rect-x rct) (rect-vx rct))
                 (rect-y rct)
                 (rect-vx rct)
                 (rect-vy rct)
                 (rect-selected? rct)
                 (rect-sel-x rct)
                 (rect-sel-y rct)
                 (rect-pen-down? rct))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (positive-x-direction (make-rect 340 265 30 20 false 0 0 true))
   (make-rect 370 265 30 20 false 0 0 true))
  (check-equal?
   (positive-x-direction (make-rect 360 200 30 20 false 20 20 true))
   (make-rect 370 200 -30 20 false 20 20 true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negative-x-direction : Rectangle -> Rectangle
;; GIVEN: Rectangle
;; RETURNS: Rectangle that has moved left, with updated x-coordinate based on velocity.
;  In case of wall hit, x-coordinate will be placed near the wall,
;  and velocity vx will change to positive number.
;; STRATEGY: Use template for Rectangle on rct
(define (negative-x-direction rct)
  (if (rect-hits-negative-wall (rect-x rct) (rect-vx rct) ZERO_COORDINATE_X)
      (make-rect ZERO_COORDINATE_X
                 (rect-y rct)
                 (- (rect-vx rct))
                 (rect-vy rct)
                 (rect-selected? rct)
                 (rect-sel-x rct)
                 (rect-sel-y rct)
                 (rect-pen-down? rct))
      (make-rect (+ (rect-x rct) (rect-vx rct))
                 (rect-y rct)
                 (rect-vx rct)
                 (rect-vy rct)
                 (rect-selected? rct)
                 (rect-sel-x rct)
                 (rect-sel-y rct)
                 (rect-pen-down? rct))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (negative-x-direction (make-rect 340 265 -30 20 false 0 0 true))
   (make-rect 310 265 -30 20 false 0 0 true))
  (check-equal?
   (negative-x-direction (make-rect 40 200 -30 20 false 20 20 true))
   (make-rect 30 200 30 20 false 20 20 true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-hits-positive-wall : NonNegInt Int NonNegInt -> Boolean
;; Given: x or y coordinate coord, it's x or y velocity vel, and wall size wall
;; WHERE: wall is bottom or right wall
;; Returns: true if coordinate + velocity hits the wall, else - false
(define (rect-hits-positive-wall coord vel wall)
  (> (+ coord vel) wall))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-hits-positive-wall 10 20 100)
   false
   "Given wall is to far (false)")
  (check-equal?
   (rect-hits-positive-wall 90 20 100)
   true
   "Given wall is near enough for hit (true)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-hits-negative-wall : NonNegInt Int NonNegInt -> Boolean
;; Given: x or y coordinate coord, it's x or y velocity vel, and wall size wall
;; WHERE: wall is top or left wall
;; Returns: true if coordinate + velocity hits the wall, else - false
(define (rect-hits-negative-wall coord vel wall)
  (< (+ coord vel) wall))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-hits-negative-wall 100 -20 0)
   false
   "Given wall is to far (false)")
  (check-equal?
   (rect-hits-negative-wall 10 -20 0)
   true
   "Given wall is near enough for hit (true)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-rect-y : Rectangle -> Rectangle
;; GIVEN: Rectangle
;; RETURNS: given Rectangle, except that y-coordinate has changed.
;  In case of wall hit, velocity vy will also change.
;; STRATEGY: Combine simpler functions
(define (set-rect-y rct)
  (if (> (rect-vy rct) 0)
      (positive-y-direction rct)
      (negative-y-direction rct)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (set-rect-y (make-rect 360 265 30 20 false 0 0 false))
   (make-rect 360 275 30 -20 false 0 0 false)
   "After bottom wall hit y will equal 275 and y velocity will change from 20 to -20")
  (check-equal?
   (set-rect-y (make-rect 300 200 30 20 false 0 0 false))
   (make-rect 300 220 30 20 false 0 0 false)
   "Without bottom wall hit, y will increase by 20 and will equal 220")
  (check-equal?
   (set-rect-y (make-rect 360 35 30 -20 false 0 0 false))
   (make-rect 360 25 30 20 false 0 0 false)
   "After top wall hit y will equal 25 and y velocity will change from -20 to 20")
  (check-equal?
   (set-rect-y (make-rect 300 200 30 -20 false 0 0 false))
   (make-rect 300 180 30 -20 false 0 0 false)
   "Without top wall hit, y will decrease by 20 and will equal 180"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positive-y-direction : Rectangle -> Rectangle
;; GIVEN: Rectangle
;; RETURNS: Rectangle that has moved down, with updated y-coordinate based on velocity.
;  In case of wall hit, y-coordinate will be placed near the wall,
;  and velocity vy will change to negative number.
;; STRATEGY: Use template for Rectangle on rct
(define (positive-y-direction rct)
  (if (rect-hits-positive-wall (rect-y rct) (rect-vy rct) CANVAS_COUNTABLE_HIGH)
      (make-rect(rect-x rct)
                CANVAS_COUNTABLE_HIGH
                (rect-vx rct)
                (- (rect-vy rct))
                (rect-selected? rct)
                (rect-sel-x rct)
                (rect-sel-y rct)
                (rect-pen-down? rct))
      (make-rect(rect-x rct)
                (+ (rect-y rct) (rect-vy rct))
                (rect-vx rct)
                (rect-vy rct)
                (rect-selected? rct)
                (rect-sel-x rct)
                (rect-sel-y rct)
                (rect-pen-down? rct))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (positive-y-direction (make-rect 340 265 -30 20 false 0 0 true))
   (make-rect 340 275 -30 -20 false 0 0 true))
  (check-equal?
   (positive-y-direction (make-rect 40 200 -30 20 false 20 20 true))
   (make-rect 40 220 -30 20 false 20 20 true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negative-y-direction : Rectangle -> Rectangle
;; GIVEN: Rectangle
;; RETURNS: Rectangle that has moved up, with updated y-coordinate based on velocity.
;  In case of wall hit, y-coordinate will be placed near the wall,
;  and velocity vy will change to positive number.
;; STRATEGY: Use template for Rectangle on rct
(define (negative-y-direction rct)
  (if (rect-hits-negative-wall (rect-y rct) (rect-vy rct) ZERO_COORDINATE_Y)
      (make-rect(rect-x rct)
                ZERO_COORDINATE_Y
                (rect-vx rct)
                (- (rect-vy rct))
                (rect-selected? rct)
                (rect-sel-x rct)
                (rect-sel-y rct)
                (rect-pen-down? rct))
      (make-rect(rect-x rct)
                (+ (rect-y rct) (rect-vy rct))
                (rect-vx rct)
                (rect-vy rct)
                (rect-selected? rct)
                (rect-sel-x rct)
                (rect-sel-y rct)
                (rect-pen-down? rct))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (negative-y-direction (make-rect 340 265 -30 -20 false 0 0 true))
   (make-rect 340 245 -30 -20 false 0 0 true))
  (check-equal?
   (negative-y-direction (make-rect 40 30 -30 -20 false 20 20 true))
   (make-rect 40 25 -30 20 false 20 20 true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dots-after-tick : LOR LOD -> LOD
;; GIVEN: List of Rectangles lor and List of Dots lod
;; RETURNS: Given list of dots, except that new dots are added
;  for every rectangle with pen down
;; STRATEGY: use template for ListOfRectangle on lor
(define (dots-after-tick lor lod)
  (cond
       [(empty? lor) lod]
       [else (if (allowed-to-create-dot? (first lor))
                 (cons (dot-after-tick (first lor))
                       (dots-after-tick (rest lor) lod))
                 (dots-after-tick (rest lor) lod))]))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (dots-after-tick
    (list
     (make-rect 50 60 0 0 false 0 0 false)
     (make-rect 70 80 0 0 false 0 0 true))
    TWO_DOTS)
   (cons (make-dot 70 80) TWO_DOTS)
   "Only 1 Dot with (70, 80) coordinates should be added."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allowed-to-create-dot? : Rectangle -> Boolean
;; GIVEN: a Rectangle 
;; RETURNS: true if pen is down and rectangle is not selected, else false
;; STRATEGY: Use template for Rectangle on rct
(define (allowed-to-create-dot? rct)
  (and (rect-pen-down? rct)
       (not (rect-selected? rct))))

;; EXAMPLES/TESTS: 
(begin-for-test
  (check-equal?
   (allowed-to-create-dot? (make-rect 60 50 0 0 true 0 0 true))
   false
   "Not allowed because Rectangle is selected")
  (check-equal?
   (allowed-to-create-dot? (make-rect 60 50 0 0 false 0 0 false))
   false
   "Not allowed because Rectangle's pen is up")
  (check-equal?
   (allowed-to-create-dot? (make-rect 60 50 0 0 false 0 0 true))
   true
   "Allowed, all conditions are met"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dot-after-tick : Rectangle -> Dot
;; GIVEN: a Rectangle 
;; RETURNS: a Dot with the same center as a given Rectangle
;; STRATEGY: Use template for Rectangle on rct
(define (dot-after-tick rct)
  (make-dot (rect-x rct) (rect-y rct)))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (dot-after-tick (new-rectangle 50 100 -10 -20))
   (make-dot 50 100)
   "Dot with x=50 and y=100 should be returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : WorldState -> Scene
;; GIVEN: a WorldState
;; RETURNS: a drawn scene of current world state.
;; STRATEGY: Use template for WorldState on w
(define (world-to-scene w)
  (rect-to-scene (world-rects w) (world-dots w)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (world-to-scene TEST_WORLD_AFTER_KEY_EVENT)
   (place-image (rect-img (make-rect 50 50 -5 0 false 0 0 false)) 350 100
   (place-image (rect-img (make-rect 50 50 0 0  false 0 0 false))
               60 50 EMPTY_CANVAS))
   "Expected to see 2 blue rectangles, which are placed near left top corner and right edge"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-to-scene : ListOfRectangle ListOfDots -> Scene
;; GIVEN: a list of rectangles lor and a list of dots lod
;; RETURNS: a drawn scene of rectangles on canvas
;; STRATEGY: Use template for ListOfRectangle on lor
(define (rect-to-scene lor lod)
  (cond
    [(empty? lor) (dot-to-scene lod)]
    [else (draw-rect (first lor)
                     (rect-to-scene (rest lor) lod))]))

;; TESTS: tested in world-to-scene
;; EXAMPLE: 
;; (rect-to-scene (list (new-rectangle 350 250 -10 -20) EMPTY_CANVAS)) =>
;; (place-image (rect-img (make-rect 50 50 -10 -20 false 0 0 false)) 350 250 EMPTY_CANVAS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-rect : Rectangle Scene -> Scene
;; GIVEN: Rectangle rct and Scene scene
;; RETURNS: a scene like the given one, except that given rectangle is drawn on it
;; STRATEGY: Use template for Rectangle on rct
(define (draw-rect rct scene)
  (place-image 
    (rect-img rct)
    (rect-x rct) (rect-y rct)
    scene))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (draw-rect (new-rectangle 350 250 -10 -20) EMPTY_CANVAS)
   (place-image (rect-img (make-rect 50 50 -10 -20 false 0 0 false))
               350 250 EMPTY_CANVAS)
   "Expected to see blue rectangle, which is placed near right bottom corner"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-img : Rectangle -> Image
;; Given: Rectanglle rct
;; RETURNS: Image drawn from given Rectangle, with:
;  blue style for unselected rectangle and red style + red circle for selected rectangle
;; STRATEGY: Cases on ractangle on rct
(define (rect-img rct)
  (if (rect-selected? rct)
      (rect-img-selected rct)
      (rect-img-unselected (rect-vx rct) (rect-vy rct))))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (rect-img (make-rect 50 50 -10 5 false 0 0 false))
   (overlay (text "(-10, 5)" RECT_FONT RECT_COLOR)
            (rectangle RECT_WIDTH RECT_HIGH RECT_STYLE RECT_COLOR))
   "Expected to see blue solid rectangle with '(-10, 5)' text in it")
  (check-equal?
   (rect-img (make-rect 100 100 -10 5 true -10 10 false))
   (overlay/xy RED-CIRCLE -15 -30
           (overlay (text "(-10, 5)" RECT_FONT SELECTED_RECT_COLOR)
                    SELECTED_RECT))
   "Expected to see red solid rectangle with '(-10, 5)' text in it and circle
under mouse cursor"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-img-selected : Rectangle -> Image
;; Given: Rectanglle rct
;; RETURNS: Image drawn from given Rectangle, with: red style, velocity as text in the center
;  and red circle under mouse cursor with coordinates by formula =
;  -1 * (rect center - circle radius + rect-sel)
;; STRATEGY: Use template for Rectangle on rct
(define (rect-img-selected rct)
  (overlay/xy RED-CIRCLE
              (- CIRCLE_RADIUS HALF_RECT_WIDTH (rect-sel-x rct))
              (- CIRCLE_RADIUS HALF_RECT_HIGH  (rect-sel-y rct))
           (red-rect (rect-vx rct) (rect-vy rct))))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (rect-img-selected (make-rect 100 100 -10 5 true -5 -5 false))
   (overlay/xy RED-CIRCLE
           -20
           -15
           (overlay (text "(-10, 5)" RECT_FONT SELECTED_RECT_COLOR)
                    SELECTED_RECT))
   "Expected to see red solid rectangle with '(-10, 5)' text in it
and red circle in clicked point"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; red-rect : Int Int -> Image
;; GIVEN: x- and y- velocity of rectangle
;; RETURNS: Rectangle Image with: red style and given velocity as text in the center
;; STRATEGY: Combine simpler functions
(define (red-rect vx vy)
  (overlay
   (text (string-append "(" (number->string vx) ", " (number->string vy) ")")
         RECT_FONT SELECTED_RECT_COLOR)
   SELECTED_RECT))

;; TEST: is tested in rect-img-selected

;; EXAMPLE:
;; (red-rect -10 5)
; => (overlay (text "(-10, 5)" RECT_FONT SELECTED_RECT_COLOR) SELECTED_RECT)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-img-unselected : Int Int -> Image
;; GIVEN: x- and y- velocity of rectangle
;; RETURNS: Rectangle Image with: blue style, and given velocity as text in the center
;; STRATEGY: Combine simpler functions
(define (rect-img-unselected vx vy)
  (overlay
   (text (string-append "(" (number->string vx) ", " (number->string vy) ")")
         RECT_FONT RECT_COLOR)
   UNSELECTED_RECT))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (rect-img-unselected -10 5)
   (overlay (text "(-10, 5)" RECT_FONT RECT_COLOR)
            UNSELECTED_RECT)
   "Expected to see blue solid rectangle with '(-10, 5)' text in it"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dot-to-scene : ListOfDots -> Scene
;; GIVEN: a list of dots
;; RETURNS: a drawn scene of Dots on canvas
;; STRATEGY: Use template for ListOfDots on dot
(define (dot-to-scene lod)
  (cond
    [(empty? lod) EMPTY_CANVAS]
    [else (draw-dot (first lod)
                    (dot-to-scene (rest lod)))]))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (dot-to-scene (cons (make-dot 50 60) empty))
   (place-image DOT 50 60 EMPTY_CANVAS)
   "Expected to dot in (50, 60) coordinates"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-dot : Dot Scene -> Scene
;; GIVEN: Dot d and Scene scene
;; RETURNS: a scene like the given one, except that given dot is drawn on it
;; STRATEGY: Use template for Dot on d
(define (draw-dot d scene)
  (place-image 
    DOT
    (dot-x-pos d) (dot-y-pos d)
    scene))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (draw-dot (make-dot 50 60) EMPTY_CANVAS)
   (place-image DOT 50 60 EMPTY_CANVAS)
   "Expected to dot in (50, 60) coordinates"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: WorldState w and KeyEvent kev
;; RETURNS: the WorldState that should follow the given worldstate
;  after the given keyevent
;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev PAUSE_KEY) (world-after-pause-key w)]
    [(key=? kev NEW_RECT_KEY) (world-after-new-key w)]
    [else (world-after-rect-key w kev)]))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal? (world-after-key-event TEST_WORLD_BEFORE_KEY_EVENT " ")
                TEST_WORLD_AFTER_KEY_EVENT
   "Paused world will start moving after pause key is hit")
  (check-equal? (world-after-key-event TEST_WORLD "b")
                TEST_WORLD
   "World State will not change if key differs from pause key")
  (check-equal? (world-after-key-event (initial-world 7) "n")
                (make-world (cons (new-centered-rectangle 7) empty) EMPTY_LOD true)
   "New rectangle should be added in the center of canvas."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-pause-key : WorldState -> WorldState
;; RETURNS: given world, except that paused? is changed to opposite
;; STRATEGY: use template for WorldState on w
(define (world-after-pause-key w)
  (make-world
   (world-rects w)
   (world-dots w)
   (not (world-paused? w))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal? (world-after-pause-key TEST_WORLD_AFTER_KEY_EVENT)
                (make-world (list RECT_IN_LEFT_TOP_CORNER_NON_MOVING
                                  RECT_NEAR_RIGHT_WAL_NON_MOVING)
                            EMPTY_LOD
                            true))
   "Paused world will start moving after pause key is hit")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-new-key : WorldState -> WorldState
;; GIVEN: WorldState w
;; RETURNS: world like given, except that new rectangle with 0 velocity is added
;  in center
;; STRATEGY: use template for WorldState on w
(define (world-after-new-key w)
  (make-world
   (cons (new-centered-rectangle 66) (world-rects w))
   (world-dots w)
   (world-paused? w)))

;; TESTS: tested in world-after-key-event
;; EXAMPLE:
;; (world-after-new-key (make-world empty EMPTY_LOD true)) =>
;; (make-world (make-rect HALF_CANVAS_WIDTH HALF_CANVAS_HIGH 0 0 false 0 0 false) EMPTY_LOD true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-centered-rectangle : Number -> Rectangle
;; GIVEN: a Number x (ignored)
;; RETURNS: a rectangle that is placed in the center of the screen with 0 velocity.
;; STRATEGY: Combine simpler functions
(define (new-centered-rectangle x)
  (new-rectangle HALF_CANVAS_WIDTH HALF_CANVAS_HIGH 0 0))

;; TESTS: tested in world-after-key-event
;; EXAMPLE:
;; (new-centered-rectangle 9) =>
;; (make-rect HALF_CANVAS_WIDTH HALF_CANVAS_HIGH 0 0 false 0 0 false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with velocity (vx, vy),
;  and which is not selected by mouse.
;; STRATEGY: Combine simpler functions
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy false 0 0 false))

;; TEST\EXAMPLE:
(begin-for-test
  (check-equal? (new-rectangle 60 50 0 0) RECT_IN_LEFT_TOP_CORNER_NON_MOVING 
   "Non moving rectangle will be created with coordinates in upper-left corner"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I am defining it only because rect-after-key-event is requested. ;;
;; I'd prefer different approach                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-rect-key : WorldState KeyEvent -> WorldState
;; GIVEN: WorldState w and KeyEvent kev, that can be related to rectangle set up
;; RETURNS: for supported KeyEvent - world with updated rectangle, else - same world
;; STRATEGY: use template for WorldState on w
(define (world-after-rect-key w kev)
  (make-world
   (key-event-for-each-rect (world-rects w) kev)
   (world-dots w)
   (world-paused? w)))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal? (world-after-rect-key
                 (make-world (list (make-rect 50 50 0 0 true 0 0 false)) EMPTY_LOD false) "left")
                 (make-world (list (make-rect 50 50 -2 0 true 0 0 false)) EMPTY_LOD false)
                 "Rectangle vx should be -2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-event-for-each-rect : ListOfRectangle KeyEvent -> ListOfRectangle
;; GIVEN: ListOfRectangle lor and KeyEvent kev
;; RETURNS: for supported KeyEvent - updated list of rectangles
;  else - same list of rectangles
;; STRATEGY: use template for ListOfRectangle on lor
(define (key-event-for-each-rect lor kev)
  (cond
       [(empty? lor) empty]
       [else (cons
              (rect-after-key-event (first lor) kev)
              (key-event-for-each-rect (rest lor) kev))]))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal? (key-event-for-each-rect
                 (list (make-rect 50 50 0 0 true 0 0 false)) "up")
                 (list (make-rect 50 50 0 -2 true 0 0 false))
                 "Rectangle vy should be -2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-key-event : Rectangle KeyEvent -> Rectangle
;; GIVEN: Rectangle rct and KeyEvent kev
;; RETURNS: the state of the rectangle that should be updated in case selected,
;  and not updated, if not selected
;; STRATEGY: Combine simpler functions
(define (rect-after-key-event rct kev)
  (if (rect-selected? rct)
      (selected-rect-after-key-event rct kev)
      rct))

;; TESTS\EXAMPLES:
(begin-for-test
  (check-equal? (rect-after-key-event (make-rect 50 50 0 0 false 0 0 false) "left")
                (make-rect 50 50 0 0 false 0 0 false)
                "Unselected rectangle should not change")
  (check-equal? (rect-after-key-event (make-rect 50 50 0 0 true 0 0 false) "left")
                (make-rect 50 50 -2 0 true 0 0 false)
                "Selected rectangle should change it's x-velocity to -2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; selected-rect-after-key-event : Rectangle KeyEvent -> Rectangle
;; GIVEN: Rectangle rct and KeyEvent kev
;; WHERE: Rectangle is selected
;; RETURNS: the state of the rectangle that should be updated in case of supported
;  KeyEvent, else remain unchanged
;; STRATEGY: Cases on key event kev
(define (selected-rect-after-key-event rct kev)
  (cond
    [(string=? kev LEFT_KEY) (rect-velocity-increase-left rct)]
    [(string=? kev RIGHT_KEY) (rect-velocity-increase-right rct)]
    [(string=? kev UP_KEY) (rect-velocity-increase-up rct)]
    [(string=? kev DOWN_KEY) (rect-velocity-increase-down rct)]
    [(string=? kev PEN_DOWN_KEY) (rect-pen-down rct)]
    [(string=? kev PEN_UP_KEY) (rect-pen-up rct)]
    [else rct]))

;; TESTS\EXAMPLES:
(begin-for-test
  (check-equal? (selected-rect-after-key-event (new-rectangle 60 50 0 0) "left")
                (new-rectangle 60 50 -2 0)
                "Velocity vx should become -2")
  (check-equal? (selected-rect-after-key-event (new-rectangle 60 50 0 0) "right")
                (new-rectangle 60 50 2 0)
                "Velocity vx should become 2")
  (check-equal? (selected-rect-after-key-event (new-rectangle 60 50 0 0) "up")
                (new-rectangle 60 50 0 -2)
                "Velocity vy should become -2")
  (check-equal? (selected-rect-after-key-event (new-rectangle 60 50 0 0) "down")
                (new-rectangle 60 50 0 2)
                "Velocity vy should become 2")
  (check-equal? (selected-rect-after-key-event (make-rect 60 50 0 0 false 0 0 false) "d")
                (make-rect 60 50 0 0 false 0 0 true)
                "Unsupported key event. Rectangle should not change.")
  (check-equal? (selected-rect-after-key-event (make-rect 60 50 0 0 false 0 0 true) "u")
                (make-rect 60 50 0 0 false 0 0 false)
                "Unsupported key event. Rectangle should not change.")
  (check-equal? (selected-rect-after-key-event (new-rectangle 60 50 0 0) "r")
                (new-rectangle 60 50 0 0)
                "Unsupported key event. Rectangle should not change."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-velocity-increase-left : Rectangle -> Rectangle
;; rect-velocity-increase-right : Rectangle -> Rectangle
;; rect-velocity-increase-up : Rectangle -> Rectangle
;; rect-velocity-increase-down : Rectangle -> Rectangle
;; GIVEN: Rectangle rct
;; RETURNS: Given rectangle with velocity changed in specified direction
;; STRATEGY: Use template for Rectangle on rct
(define (rect-velocity-increase-left rct)
  (make-rect
   (rect-x rct)
   (rect-y rct)
   (- (rect-vx rct) INCREASE_BY)
   (rect-vy rct)
   (rect-selected? rct)
   (rect-sel-x rct)
   (rect-sel-y rct)
   (rect-pen-down? rct)))

(define (rect-velocity-increase-right rct)
  (make-rect
   (rect-x rct)
   (rect-y rct)
   (+ (rect-vx rct) INCREASE_BY)
   (rect-vy rct)
   (rect-selected? rct)
   (rect-sel-x rct)
   (rect-sel-y rct)
   (rect-pen-down? rct)))

(define (rect-velocity-increase-up rct)
  (make-rect
   (rect-x rct)
   (rect-y rct)
   (rect-vx rct)
   (- (rect-vy rct) INCREASE_BY)
   (rect-selected? rct)
   (rect-sel-x rct)
   (rect-sel-y rct)
   (rect-pen-down? rct)))

(define (rect-velocity-increase-down rct)
  (make-rect
   (rect-x rct)
   (rect-y rct)
   (rect-vx rct)
   (+ (rect-vy rct) INCREASE_BY)
   (rect-selected? rct)
   (rect-sel-x rct)
   (rect-sel-y rct)
   (rect-pen-down? rct)))

;; TESTS\EXAMPLES:
(begin-for-test
  (check-equal? (rect-velocity-increase-left (new-rectangle 60 50 0 0))
                (new-rectangle 60 50 -2 0)
                "Velocity vx should become -2")
  (check-equal? (rect-velocity-increase-right (new-rectangle 60 50 0 0))
                (new-rectangle 60 50 2 0)
                "Velocity vx should become 2")
  (check-equal? (rect-velocity-increase-up (new-rectangle 60 50 0 0))
                (new-rectangle 60 50 0 -2)
                "Velocity vy should become -2")
  (check-equal? (rect-velocity-increase-down (new-rectangle 60 50 0 0))
                (new-rectangle 60 50 0 2)
                "Velocity vy should become 2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-pen-down : Rectangle -> Rectangle
;; rect-pen-up : Rectangle -> Rectangle
;; GIVEN: Rectangle rct
;; RETURNS: Given rectangle with pen-down? changed accordingly
;; STRATEGY: Use template for Rectangle on rct

(define (rect-pen-down rct)
  (make-rect
   (rect-x rct)
   (rect-y rct)
   (rect-vx rct)
   (rect-vy rct)
   (rect-selected? rct)
   (rect-sel-x rct)
   (rect-sel-y rct)
   true))

(define (rect-pen-up rct)
  (make-rect
   (rect-x rct)
   (rect-y rct)
   (rect-vx rct)
   (rect-vy rct)
   (rect-selected? rct)
   (rect-sel-x rct)
   (rect-sel-y rct)
   false))

;; EXAMPLE/CONSTANT for tests:
(define RECT_PEN_UP (make-rect 100 200 10 10 true 10 10 false))
(define RECT_PEN_DOWN (make-rect 100 200 10 10 true 10 10 true))

;; TESTS: 
(begin-for-test
  (check-equal?
   (rect-pen-down RECT_PEN_UP)
   RECT_PEN_DOWN
   "Should return Rectangle with pen down")
  (check-equal?
   (rect-pen-up RECT_PEN_DOWN)
   RECT_PEN_UP
   "Should return Rectangle with pen up")
  (check-equal?
   (rect-pen-up RECT_PEN_UP)
   RECT_PEN_UP
   "Should return unchanged Rectangle with pen up"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the mouse event
;; RETURNS: the world that should follow the given world after the given mouse event.
;; STRATEGY: use template for WorldState on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   (lor-after-mouse-event (world-rects w) mx my mev)
   (world-dots w)
   (world-paused? w)))

;; EXAMPLE/TEST:
(begin-for-test
  (check-equal?
   (world-after-mouse-event (initial-world 99) 205 105 "button-down")
   (make-world NEW_LOR EMPTY_LOD true)
   "After tick initial world should update according to problem set:
Rectangle #1 is selected by right botom corner and has:
x=200, y=100, vx=-12, vy=20, selected?=true, sel-x=5, sel-y=5; 
Rectangle #2 is not changed:
x=200, y=200, vx=23, vy=-14, selected?=false, sel-x=0, sel-y=0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lor-after-mouse-event : ListOfRectangle Int Int MouseEvent -> ListOfRectangle
;; GIVEN: A ListOfRectangle, the x- and y-coordinates of a mouse event, and the mouse event
;; RETURNS: the ListOfRectangle that should follow the given ListOfRectangle
;  after the given mouse event.
;; STRATEGY: use template for ListOfRectangle on lor
(define (lor-after-mouse-event lor mx my mev)
  (cond
       [(empty? lor) empty]
       [else (cons
              (rect-after-mouse-event (first lor) mx my mev)
              (lor-after-mouse-event  (rest lor)  mx my mev))]))

;; EXAMPLES\TESTS: 
(begin-for-test
  (check-equal?
   (lor-after-mouse-event
    (cons (make-rect 200 100 10 20 false 0 0 false) empty) 200 100 "button-down")
   (cons (make-rect 200 100 10 20 true 0 0 false) empty)
   "Rectangle from ListOfRectangle should be selected")
  (check-equal?
   (lor-after-mouse-event
    (cons (make-rect 200 100 10 20 false 0 0 false) empty) 20 10 "button-down")
   (cons (make-rect 200 100 10 20 false 0 0 false) empty)
   "Rectangle from ListOfRectangle should remain unchanged"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-mouse-event :  Rectangle Int Int MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the mouse event
;; WHERE: button-down, drag and button-up are the only supported events
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event
;; STRATEGY: Cases on mouse event mev
(define (rect-after-mouse-event rct mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down rct mx my)]
    [(mouse=? mev "drag") (rect-after-drag rct mx my)]
    [(mouse=? mev "button-up") (rect-after-button-up rct)]
    [else rct]))

;; EXAMPLES\TESTS:
(begin-for-test
  (check-equal?
   (rect-after-mouse-event (make-rect 50 50 10 20 false 0 0 false) 200 100 "button-down")
   (make-rect 50 50 10 20 false 0 0 false)
   "Rectangle is unselected")
  (check-equal?
   (rect-after-mouse-event (make-rect 200 100 10 20 false 0 0 false) 200 100 "button-down")
   (make-rect 200 100 10 20 true 0 0 false)
   "Rectangle is selected in center")
  (check-equal?
   (rect-after-mouse-event (make-rect 200 100 10 20 true 1 1 false) 51 51 "drag")
   (make-rect 50 50 10 20 true 1 1 false)
   "Rectangle changed it's position to x=50, y=50")
  (check-equal?
   (rect-after-mouse-event (make-rect 50 50 10 20 true 1 1 false) 49 49 "button-up")
   (make-rect 50 50 10 20 false 0 0 false)
   "Rectangle became unselected")
  (check-equal?
   (rect-after-mouse-event (make-rect 50 50 10 20 false 0 0 false) 49 49 "enter")
   (make-rect 50 50 10 20 false 0 0 false)
   "Unsupported mouse button was used. Rectangle is not changed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-button-down : Rectangle Integer Integer -> Rectangle
;; RETURNS: the rectangle following a button-down mouse event at the given location
;  and calculated sel-x sel-y = mouse coordinates - rectangle center.
;; STRATEGY: Use template for Rectangle on rct
(define (rect-after-button-down rct mx my)
  (if (in-rect? rct mx my)
      (make-rect
       (rect-x rct)
       (rect-y rct)
       (rect-vx rct)
       (rect-vy rct)
       true
       (- mx (rect-x rct))
       (- my (rect-y rct))
       (rect-pen-down? rct))
      rct))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-after-button-down (make-rect 200 100 10 20 false 0 0 false) 200 100 )
   (make-rect 200 100 10 20 true 0 0 false)
   "Given rectangle was clicked in center")  
  (check-equal?
   (rect-after-button-down (make-rect 200 100 10 20 false 0 0 false) 195 95 )
   (make-rect 200 100 10 20 true -5 -5 false)
   "Given rectangle was clicked in left top corner")
  (check-equal?
   (rect-after-button-down (make-rect 200 100 10 20 false 0 0 false) 50 50 )
   (make-rect 200 100 10 20 false 0 0 false)
   "Given rectangle is not selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-rect? : Rectangle Integer Integer -> Boolean
;; GIVEN: Rectangle rct and x, y coordinate
;; RETURNS: true if the given coordinate is inside the bounding box of
;  the given Rectangle, else false
;; STRATEGY: Use template for Rectangle on rct
(define (in-rect? rct x y)
  (and
    (<= 
      (- (rect-x rct) HALF_RECT_WIDTH)
      x
      (+ (rect-x rct) HALF_RECT_WIDTH))
    (<= 
      (- (rect-y rct) HALF_RECT_HIGH)
      y
      (+ (rect-y rct) HALF_RECT_HIGH))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (in-rect? RECT_IN_CENTER_SELECTED 205 105) true
   "Given rectangle is selected 5 px away from center")
  (check-equal?
   (in-rect? RECT_IN_LEFT_TOP_CORNER_NON_MOVING 60 105) false
   "Given rectangle has the same x, but y is out of boundaries")
  (check-equal?
   (in-rect? RECT_IN_LEFT_TOP_CORNER_NON_MOVING 205 50) false
   "Given rectangle has x out of boundaries, even when y is the same")
  (check-equal?
   (in-rect? RECT_IN_LEFT_TOP_CORNER_NON_MOVING 205 105) false
   "Mouse coordinates are out of boundaries of given rectangle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-drag : Rectangle Integer Integer -> Rectangle
;; GIVEN: Rectangle rct and x, y coordinate
;; RETURNS: the rectangle following a drag mouse event at the given location
;; WHERE: rectangle's center is shifted from mouse location by sel-x sel-y
;  using formula = mouse coordinates - sel-x or sel-y
;; STRATEGY: Use template for Rectangle on rct
(define (rect-after-drag rct mx my)
  (if (rect-selected? rct)
      (make-rect
       (- mx (rect-sel-x rct))
       (- my (rect-sel-y rct))
       (rect-vx rct)
       (rect-vy rct)
       true
       (rect-sel-x rct)
       (rect-sel-y rct)
       (rect-pen-down? rct))
      rct))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-after-drag (make-rect 200 100 10 20 true 0 0 false) 50 50 )
   (make-rect 50 50 10 20 true 0 0 false)
   "Given rectangle was dragged by center")  
  (check-equal?
   (rect-after-drag (make-rect 200 100 10 20 true -5 -5 false) 50 50 )
   (make-rect 55 55 10 20 true -5 -5 false)
   "Given rectangle was dragged by left top corner")
  (check-equal?
   (rect-after-drag (make-rect 200 100 10 20 false 0 0 false) 50 50 )
   (make-rect 200 100 10 20 false 0 0 false)
   "Given rectangle is not selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-button-up : Rectangle -> Rectangle
;; GIVEN: Rectangle rct 
;; RETURNS: the rectangle following a button-up mouse event 
;; STRATEGY: Use template for Rectangle on rct
(define (rect-after-button-up rct)
  (if (rect-selected? rct)
      (make-rect
       (rect-x rct)
       (rect-y rct)
       (rect-vx rct)
       (rect-vy rct)
       false 
       0
       0
       (rect-pen-down? rct))
      rct))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-after-button-up (make-rect 200 100 10 20 false 0 0 false))
   (make-rect 200 100 10 20 false 0 0 false)
   "Given rectangle is not changed, because was not selected")
  (check-equal?
   (rect-after-button-up (make-rect 200 100 10 20 true 1 1 false))
   (make-rect 200 100 10 20 false 0 0 false)
   "Given rectangle should become unselected"))