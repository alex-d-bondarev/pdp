;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screen saver #2
;; Similar to screen saver #1, except that smooth dragging functionality is added

;; start with (screensaver 2)
;; slower variant
;; OR (screensaver 0.5)
;; faster variant
;; smaller input number represents faster simulations 

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(check-location "03" "screensaver-2.rkt")

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-rect1
 world-rect2
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 rect-selected?
 world-after-mouse-event
 rect-after-mouse-event
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define RECT_COLOR "blue")
(define RECT_STYLE "outline")
(define RECT_FONT 10)
(define RECT_WIDTH 60)
(define RECT_HIGH 50)

;; Added in version 2

(define SELECTED_RECT_COLOR "red")
(define CIRCLE_RADIUS 5)
(define HALF_RECT_WIDTH (/ RECT_WIDTH 2))
(define HALF_RECT_HIGH (/ RECT_HIGH 2))

(define UNSELECTED_RECT (rectangle RECT_WIDTH RECT_HIGH RECT_STYLE RECT_COLOR))
(define SELECTED_RECT (rectangle RECT_WIDTH RECT_HIGH RECT_STYLE SELECTED_RECT_COLOR))
(define RED-CIRCLE (circle CIRCLE_RADIUS RECT_STYLE SELECTED_RECT_COLOR))

;; End of version 2 changes

(define PAUSE_KEY " ")

(define CANVAS_WIDTH 400)
(define CANVAS_HIGH 300)
(define EMPTY_CANVAS (empty-scene CANVAS_WIDTH CANVAS_HIGH))

(define RECT1_START_X 200)
(define RECT1_START_Y 100)
(define RECT1_START_VELOCITY_X -12)
(define RECT1_START_VELOCITY_Y 20)

(define RECT2_START_X RECT1_START_X)
(define RECT2_START_Y 200)
(define RECT2_START_VELOCITY_X 23)
(define RECT2_START_VELOCITY_Y -14)

;; INTERPRETATION: according to the task

(define CANVAS_COUNTABLE_WIDTH (- CANVAS_WIDTH (/ RECT_WIDTH 2))) ;; right wall
(define CANVAS_COUNTABLE_HIGH (- CANVAS_HIGH (/ RECT_HIGH 2)))    ;; bottom wall

(define ZERO_COORDINATE_X (/ RECT_WIDTH 2))                       ;; left wall
(define ZERO_COORDINATE_Y (/ RECT_HIGH 2))                        ;; top wall

;; INTERPRETATION: Simplifies calculations of wall hit scenario

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct rect (x y vx vy selected? sel-x sel-y))
;; A Rectangle is a (make-rect NonNegInt NonNegInt Int Int Boolean Int Int)
;; x and y are coordinates
;; vx and vy are represenations of velocity in the x- and y- directions
;; selected? describes whether or not the rectangle is selected
;; sel-x and sel-y represent distance from center of rectangle to mouse point, if selected

;; TEMPALTE:
;; rect-fn : Rectangle -> ??
#; (define (rect-fn rct)
     (...(rect-x rct)
         (rect-y rct)
         (rect-vx rct)
         (rect-vy rct)
         (rect-selected? rct)
         (rect-sel-x rct)
         (rect-sel-y rct)))

;; EXAMPLES:
(define rect_in_0_point_non_moving (make-rect 60 50 0 0 false 0 0))
(define rect_in_250_100_point_slow_moving_only_left (make-rect 350 100 -5 0 false 0 0))
(define selected_rectangle_in_center (make-rect 200 100 10 10 true 5 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world (rect1 rect2 paused?))
;; A WorldState is a (make-world Rectangle Rectangle Boolean)
;; rect1 and rect2 are two Rectangles
;; paused? describes whenever the world is paused

;; TEMPLATE:
;; world-fn : WorldSate -> ??
#; (define (world-fn w)
     (...(world-rect1 w)
         (world-rect2 w)
         (world-paused? w)))

;; EXAMPLE:
(define paused_world
  (make-world rect_in_0_point_non_moving
   rect_in_250_100_point_slow_moving_only_left
   true))

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
(rect-x rect_in_250_100_point_slow_moving_only_left) => 250
(rect-y rect_in_250_100_point_slow_moving_only_left) => 100
(rect-vx rect_in_250_100_point_slow_moving_only_left) => -5
(rect-vy rect_in_250_100_point_slow_moving_only_left) => 0
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with velocity (vx, vy),
;  and which is not selected by mouse.

;; STRATEGY: Combine simpler functions
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy false 0 0))

;; TEST\EXAMPLE:
(begin-for-test
  (check-equal? (new-rectangle 60 50 0 0) rect_in_0_point_non_moving 
   "Non moving rectangle will be created with coordinates in upper-left corner"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part of the world struct

;; world-rect1 : WorldState -> Rectangle
;; world-rect2 : WorldState -> Rectangle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; EXAMPLES:
#|
(world-rect1 paused_world) => rect_in_0_point_non_moving
(world-rect2 paused_world) => rect_in_250_100_point_slow_moving_only_left
(world-paused? paused_world) => true
|#

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; Added in version 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the mouse event
;; RETURNS: the world that should follow the given world after the given mouse event.
;; STRATEGY: use template for WorldState on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   (rect-after-mouse-event (world-rect1 w) mx my mev)
   (rect-after-mouse-event (world-rect2 w) mx my mev)
   (world-paused? w)))

;; EXAMPLE/TEST:
(begin-for-test
  (check-equal?
   (world-after-mouse-event (initial-world 99) 205 105 "button-down")
   (make-world
    (make-rect 200 100 -12 20 true 5 5)
    (make-rect 200 200 23 -14 false 0 0)
    false)
   "After tick initial world should update according to problem set:
Rectangle #1 is selected by right botom corner and has:
x=200, y=100, vx=-12, vy=20, selected?=true, sel-x=5, sel-y=5; 
Rectangle #2 is not changed:
x=200, y=200, vx=23, vy=-14, selected?=false, sel-x=0, sel-y=0"))

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
   (rect-after-mouse-event (make-rect 50 50 10 20 false 0 0) 200 100 "button-down")
   (make-rect 50 50 10 20 false 0 0)
   "Rectangle is unselected")
  (check-equal?
   (rect-after-mouse-event (make-rect 200 100 10 20 false 0 0) 200 100 "button-down")
   (make-rect 200 100 10 20 true 0 0)
   "Rectangle is selected in center")
  (check-equal?
   (rect-after-mouse-event (make-rect 200 100 10 20 true 1 1) 51 51 "drag")
   (make-rect 50 50 10 20 true 1 1)
   "Rectangle changed it's position to x=50, y=50")
  (check-equal?
   (rect-after-mouse-event (make-rect 50 50 10 20 true 1 1) 49 49 "button-up")
   (make-rect 50 50 10 20 false 0 0)
   "Rectangle became unselected")
  (check-equal?
   (rect-after-mouse-event (make-rect 50 50 10 20 false 0 0) 49 49 "enter")
   (make-rect 50 50 10 20 false 0 0)
   "Unsupported mouse button was used. Rectangle is not changed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Wishlist version 2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;

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
       (- my (rect-y rct)))
      rct))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-after-button-down (make-rect 200 100 10 20 false 0 0) 200 100 )
   (make-rect 200 100 10 20 true 0 0)
   "Given rectangle was clicked in center")  
  (check-equal?
   (rect-after-button-down (make-rect 200 100 10 20 false 0 0) 195 95 )
   (make-rect 200 100 10 20 true -5 -5)
   "Given rectangle was clicked in left top corner")
  (check-equal?
   (rect-after-button-down (make-rect 200 100 10 20 false 0 0) 50 50 )
   (make-rect 200 100 10 20 false 0 0)
   "Given rectangle is not selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-drag : Rectangle Integer Integer -> Rectangle
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
       (rect-sel-y rct))
      rct))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-after-drag (make-rect 200 100 10 20 true 0 0) 50 50 )
   (make-rect 50 50 10 20 true 0 0)
   "Given rectangle was dragged by center")  
  (check-equal?
   (rect-after-drag (make-rect 200 100 10 20 true -5 -5) 50 50 )
   (make-rect 55 55 10 20 true -5 -5)
   "Given rectangle was dragged by left top corner")
  (check-equal?
   (rect-after-drag (make-rect 200 100 10 20 false 0 0) 50 50 )
   (make-rect 200 100 10 20 false 0 0)
   "Given rectangle is not selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-after-button-up : Rectangle -> Rectangle
;; RETURNS: the rectangle following a button-up mouse event 
;; STRATEGY: Use template for Rectangle on rct
(define (rect-after-button-up rct)
  (if (rect-selected? rct)
      (make-rect (rect-x rct) (rect-y rct) (rect-vx rct) (rect-vy rct) false 0 0)
      rct))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rect-after-button-up (make-rect 200 100 10 20 false 0 0))
   (make-rect 200 100 10 20 false 0 0)
   "Given rectangle is not changed, because was not selected")
  (check-equal?
   (rect-after-button-up (make-rect 200 100 10 20 true 1 1))
   (make-rect 200 100 10 20 false 0 0)
   "Given rectangle should become unselected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-rect? : Rectangle Integer Integer -> Boolean
;; RETURNS: true if the given coordinate is inside the bounding box of
;; the given Rectangle, else false
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
   (in-rect? selected_rectangle_in_center 205 105) true
   "Given rectangle is selected 5 px away from center")
  (check-equal?
   (in-rect? rect_in_0_point_non_moving 60 105) false
   "Given rectangle has the same x, but y is out of boundaries")
  (check-equal?
   (in-rect? rect_in_0_point_non_moving 205 50) false
   "Given rectangle has x out of boundaries, even when y is the same")
  (check-equal?
   (in-rect? rect_in_0_point_non_moving 205 105) false
   "Mouse coordinates are out of boundaries of given rectangle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Part of the world struct

;; rect-selected? : Rectangle -> Boolean
;; RETURNS: true if the given rectangle is selected.

;; EXAMPLES: 
;  (rect-selected? rect_in_250_100_point_slow_moving_only_left) => false
;  (rect-selected? (make-rect 200 100 30 20 true 0 0) => true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of version 2 changes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set

;; EXAMPLE:
; (initial-world 99) => WorldSate specified by problem state

;; STARTEGY: Combine Simpler Functions
(define (initial-world x)
  (make-world
   (make-rect
    RECT1_START_X RECT1_START_Y RECT1_START_VELOCITY_X RECT1_START_VELOCITY_Y false 0 0)
   (make-rect
    RECT2_START_X RECT2_START_Y RECT2_START_VELOCITY_X RECT2_START_VELOCITY_Y false 0 0)
   false))

;; TEST: 
(begin-for-test
  (check-equal?
   (initial-world 666)
   (make-world
    (make-rect
     RECT1_START_X RECT1_START_Y RECT1_START_VELOCITY_X RECT1_START_VELOCITY_Y false 0 0)
    (make-rect
     RECT2_START_X RECT2_START_Y RECT2_START_VELOCITY_X RECT2_START_VELOCITY_Y false 0 0)
    false)
   "Initial world is defined by problem set"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;; STRATEGY: Use template for WorldState on w
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (rect-after-tick (world-rect1 w))
      (rect-after-tick (world-rect2 w))
      (world-paused? w))))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (world-after-tick (initial-world 99))
   (make-world
    (make-rect 188 120 -12 20 false 0 0)
    (make-rect 223 186 23 -14 false 0 0)
    false)
   "After tick initial world should update according to problem set:
Rectangle #1 should be: x 200 -> 188, y 100 -> 120, vx -12, vy 20
Rectangle #2 should be: x 200 -> 223, y 200 -> 186, vx 23, vy -14")
  (check-equal?
   (world-after-tick paused_world) paused_world
   "Paused world will not change after tick"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent

;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (if (key=? kev PAUSE_KEY)
      (world-after-pause-key w)
      w))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal? (world-after-key-event paused_world " ")
                (make-world rect_in_0_point_non_moving
                            rect_in_250_100_point_slow_moving_only_left
                            false)
   "Paused world will start moving after pause key is hit")
  (check-equal? (world-after-key-event paused_world "b")
                paused_world
   "World State will not change if key differs from pause key"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Wishlist ;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-pause-key : WorldState -> WorldState
;; RETURNS: given world, except that paused? is changed to opposite

;; STRATEGY: use template for World on w
(define (world-after-pause-key w)
  (make-world
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal? (world-after-pause-key paused_world)
                (make-world rect_in_0_point_non_moving
                            rect_in_250_100_point_slow_moving_only_left
                            false)
   "Paused world will start moving after pause key is hit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : WorldState -> Scene
;; RETURNS: a drawn scene of current world state.

;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (draw-rect
    (world-rect1 w)
    (draw-rect
      (world-rect2 w)
      EMPTY_CANVAS)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (world-to-scene paused_world)
   (place-image (rect-img (make-rect 50 50 -5 0 false 0 0)) 350 100
   (place-image (rect-img (make-rect 50 50 0 0  false 0 0))
               60 50 EMPTY_CANVAS))
   "Expected to see 2 blue rectangles, which are placed near left top corner and right edge"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; draw-rect : Rectangle Scene -> Scene
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
   (place-image (rect-img (make-rect 50 50 -10 -20 false 0 0))
               350 250 EMPTY_CANVAS)
   "Expected to see blue rectangle, which is placed near right bottom corner"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updated in version 2 (added rect-img-selected and rect-img-unselected) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rect-img : Rectangle -> Image
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
   (rect-img (make-rect 50 50 -10 5 false 0 0))
   (overlay (text "(-10, 5)" RECT_FONT RECT_COLOR)
            (rectangle RECT_WIDTH RECT_HIGH RECT_STYLE RECT_COLOR))
   "Expected to see blue solid rectangle with '(-10, 5)' text in it")
  (check-equal?
   (rect-img (make-rect 100 100 -10 5 true -10 10))
   (overlay/xy RED-CIRCLE -15 -30
           (overlay (text "(-10, 5)" RECT_FONT SELECTED_RECT_COLOR)
                    SELECTED_RECT))
   "Expected to see red solid rectangle with '(-10, 5)' text in it and circle
under mouse cursor"))

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

;; rect-img-selected : Rectangle -> Image
;; RETURNS: Image drawn from given Rectangle, with: red style, velocity as text in the center
;  and red circle under mouse cursor with coordinates by formula =
;  -1 * (rect center - circle radius + rect-sel)

;; STRATEGY: Use template for Rectangle on rct
(define (rect-img-selected rct)
  (overlay/xy RED-CIRCLE
           (* -1 (+ (- HALF_RECT_WIDTH CIRCLE_RADIUS) (rect-sel-x rct)))
           (* -1 (+ (- HALF_RECT_HIGH CIRCLE_RADIUS) (rect-sel-y rct)))
           (red-rect (rect-vx rct) (rect-vy rct))))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (rect-img-selected (make-rect 100 100 -10 5 true -5 -5))
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
   (rect-after-tick (make-rect 360 265 30 20 false 0 0))
   (make-rect 370 275 -30 -20 false 0 0)
   "After hitting bottom right corner, rectangle will stay in that corner and
 velocities will change to opposite")
  (check-equal?
   (rect-after-tick (make-rect 40 35 -30 -20 false 0 0))
   (make-rect 30 25 30 20 false 0 0)
   "After hitting top lefy corner, rectangle will stay in that corner and
 velocities will change to opposite")
  (check-equal?
   (rect-after-tick (make-rect 100 200 -10 20 false 0 0))
   (make-rect 90 220 -10 20 false 0 0)
   "Without hitting any wall x-coord will change to 90 and y-coord will change to 220.
 Velocities will not change")
  (check-equal?
   (rect-after-tick (make-rect 100 200 -10 20 true 0 0))
   (make-rect 100 200 -10 20 true 0 0)
   "State should not be changed. Rectangle is selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-rect-x : Rectangle -> Rectangle
;; RETURNS: Rectangle which x-coordinate has changed. In case of wall hit, velocity
;  vx will also change.
;; STRATEGY: Combine simpler functions
(define (set-rect-x rct)
  (if (> (rect-vx rct) 0)
      (positive-x-direction rct)
      (negative-x-direction rct)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (set-rect-x (make-rect 360 265 30 20 false 0 0))
   (make-rect 370 265 -30 20 false 0 0)
   "After right wall hit x will equal 370 and x velocity will change from 30 to -30")
  (check-equal?
   (set-rect-x (make-rect 300 200 30 20 false 0 0))
   (make-rect 330 200 30 20 false 0 0)
   "Without right wall hit, x will increase by 30 and will equal 330")
  (check-equal?
   (set-rect-x (make-rect 40 35 -30 -20 false 0 0))
   (make-rect 30 35 30 -20 false 0 0)
   "After left wall hit x will equal 30 and x velocity will change from -30 to 30")
  (check-equal?
   (set-rect-x (make-rect 300 200 -30 -20 false 0 0))
   (make-rect 270 200 -30 -20 false 0 0)
   "Without left wall hit, x will decrease by 30 and will equal 180"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positive-x-direction : Rectangle -> Rectangle
;; RETURNS: Rectangle that is moving right, will update x-coordinate based on velocity.
;  In case of wall hit, x-coordinate will be placed near the wall,
;  and velocity vx will change to negative number.
;; STRATEGY: Use template for Rectangle on rct
(define (positive-x-direction rct)
  (if (> (+ (rect-x rct) (rect-vx rct)) CANVAS_COUNTABLE_WIDTH)
      (make-rect CANVAS_COUNTABLE_WIDTH
                 (rect-y rct)
                 (* -1 (rect-vx rct))
                 (rect-vy rct)
                 false 0 0)
      (make-rect (+ (rect-x rct) (rect-vx rct))
                 (rect-y rct)
                 (rect-vx rct)
                 (rect-vy rct)
                 false 0 0)))

;; TESTS: tested in set-rect-x
;; EXAMPLES: 
;; (positive-y-direction (make-rect 360 265 20 20 false 0 0))
;; => (make-rect 360 275 20 -20 false 0 0)
;; (positive-y-direction (make-rect 300 200 20 20 false))
;; => (make-rect 300 220 20 20 false 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negative-x-direction : Rectangle -> Rectangle
;; RETURNS: Rectangle that is moving left, will update x-coordinate based on velocity.
;  In case of wall hit, x-coordinate will be placed near the wall,
;  and velocity vx will change to positive number.
;; STRATEGY: Use template for Rectangle on rct
(define (negative-x-direction rct)
  (if (< (+ (rect-x rct) (rect-vx rct)) ZERO_COORDINATE_X)
      (make-rect ZERO_COORDINATE_X
                 (rect-y rct)
                 (* -1 (rect-vx rct))
                 (rect-vy rct)
                 false 0 0)
      (make-rect (+ (rect-x rct) (rect-vx rct))
                 (rect-y rct)
                 (rect-vx rct)
                 (rect-vy rct)
                 false 0 0)))

;; TESTS: tested in set-rect-x
;; EXAMPLES: 
;; (negative-x-direction (make-rect 360 35 -20 -20 false 0 0))
;; => (make-rect 370 35 20 -20 false 0 0)
;; (negative-x-direction (make-rect 300 200 -20 -20 false))
;; => (make-rect 280 200 -20 -20 false 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-rect-y : Rectangle -> Rectangle
;; RETURNS: Rectangle which y-coordinate has changed. In case of wall hit, velocity
;  vy will also change.
;; STRATEGY: Combine simpler functions
(define (set-rect-y rct)
  (if (> (rect-vy rct) 0)
      (positive-y-direction rct)
      (negative-y-direction rct)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (set-rect-y (make-rect 360 265 30 20 false 0 0))
   (make-rect 360 275 30 -20 false 0 0)
   "After bottom wall hit y will equal 275 and y velocity will change from 20 to -20")
  (check-equal?
   (set-rect-y (make-rect 300 200 30 20 false 0 0))
   (make-rect 300 220 30 20 false 0 0)
   "Without bottom wall hit, y will increase by 20 and will equal 220")
  (check-equal?
   (set-rect-y (make-rect 360 35 30 -20 false 0 0))
   (make-rect 360 25 30 20 false 0 0)
   "After top wall hit y will equal 25 and y velocity will change from -20 to 20")
  (check-equal?
   (set-rect-y (make-rect 300 200 30 -20 false 0 0))
   (make-rect 300 180 30 -20 false 0 0)
   "Without top wall hit, y will decrease by 20 and will equal 180"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positive-y-direction : Rectangle -> Rectangle
;; RETURNS: Rectangle that is moving down, will update y-coordinate based on velocity.
;  In case of wall hit, y-coordinate will be placed near the wall,
;  and velocity vy will change to negative number.
;; STRATEGY: Use template for Rectangle on rct
(define (positive-y-direction rct)
  (if (> (+ (rect-y rct) (rect-vy rct)) CANVAS_COUNTABLE_HIGH)
      (make-rect(rect-x rct)
                CANVAS_COUNTABLE_HIGH
                (rect-vx rct)
                (* -1 (rect-vy rct))
                false 0 0)
      (make-rect(rect-x rct)
                (+ (rect-y rct) (rect-vy rct))
                (rect-vx rct)
                (rect-vy rct)
                false 0 0)))

;; TESTS: tested in set-rect-y
;; EXAMPLES: 
;; (positive-y-direction (make-rect 360 265 20 20 false 0 0))
;; => (make-rect 360 275 20 -20 false 0 0)
;; (positive-y-direction (make-rect 300 200 20 20 false 0 0))
;; => (make-rect 300 220 20 20 false 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; negative-y-direction : Rectangle -> Rectangle
;; RETURNS: Rectangle that is moving up, will update y-coordinate based on velocity.
;  In case of wall hit, y-coordinate will be placed near the wall,
;  and velocity vy will change to positive number.
;; STRATEGY: Use template for Rectangle on rct
(define (negative-y-direction rct)
  (if (< (+ (rect-y rct) (rect-vy rct)) ZERO_COORDINATE_Y)
      (make-rect(rect-x rct)
                ZERO_COORDINATE_Y
                (rect-vx rct)
                (* -1 (rect-vy rct))
                false 0 0)
      (make-rect(rect-x rct)
                (+ (rect-y rct) (rect-vy rct))
                (rect-vx rct)
                (rect-vy rct)
                false 0 0)))

;; TESTS: tested in set-rect-y
;; EXAMPLES: 
;; (negative-y-direction (make-rect 360 35 20 -20 false 0 0))
;; => (make-rect 360 25 20 20 false 0 0)
;; (negative-y-direction (make-rect 300 200 20 -20 false 0 0))
;; => (make-rect 300 180 20 -20 false 0 0)
