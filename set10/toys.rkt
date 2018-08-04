#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; toys.rkt
;; -----------------------------------------------
;; This program simulates a marvelous toy in which
;; various types of widgets are created on a canvas
;; that the user can interact with.
;;
;; Use (run 0.5 10) to start the simulation at a rate
;; of 0.5 seconds per tick where square toys moved at
;; a speed of 10 pixels per tick.

(require rackunit) 
(require 2htdp/universe)   
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(check-location "10" "toys.rkt")
 
(provide
 make-playground
 run
 make-square-toy
 make-throbber
 make-clock
 make-football
 PlaygroundState<%>
 Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Constants
;; -----------------------------------------------

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    
(define TARGET-INITIAL-X (/ CANVAS-WIDTH 2))
(define TARGET-INITIAL-Y (/ CANVAS-HEIGHT 2))

(define NEW-THROBBER-KEY "t")
(define NEW-SQUARE-KEY "s")
(define NEW-CLOCK-KEY "w")
(define NEW-FOOTBALL-KEY "f")

(define TARGET-RADIUS 10)
(define TARGET-CIRCLE-MODE "outline")
(define TARGET-CIRCLE-COLOR "blue")
(define TARGET-IMAGE (circle TARGET-RADIUS
                             TARGET-CIRCLE-MODE
                             TARGET-CIRCLE-COLOR))

(define THROBBER-MIN-RADIUS 5)
(define THROBBER-MAX-RADIUS 20)
(define THROBBER-CIRCLE-MODE "solid")
(define THROBBER-CIRCLE-COLOR "green")

(define FOOTBALL-IMAGE (bitmap "football.png"))
(define FOOTBALL-WIDTH (image-width FOOTBALL-IMAGE))
(define FOOTBALL-HEIGHT (image-height FOOTBALL-IMAGE))
(define FOOTBALL-DEFLATE-RATE 0.05)

(define SIDE 40)
(define HALF-SIDE (/ SIDE 2)) 
(define SQUARE-IMAGE (square SIDE "outline" "red"))

(define CLOCK_COLOR "black")

(define ZERO_POINT 0)
(define 1_PX 1)
(define 2_PX 2)

;; -----------------------------------------------
;; For tests

(define CANVAS-HALF-WIDTH (/ CANVAS-WIDTH 2))
(define CANVAS-HALF-HEIGHT (/ CANVAS-HEIGHT 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Interfaces
;; -----------------------------------------------


;; A PlaygroundState represents a canvas with a target and a collection
;; of Toys
(define PlaygroundState<%>
  (interface (SWidget<%>) 
    
    ;; -> Integer
    ;; RETURNS: the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; RETURNS: Is the target selected?
    target-selected?
    
    ;; -> ListOfToy<%>
    ;; RETURNS: The list of toys in the playground
    get-toys))


;; -----------------------------------------------
;; The target represents a draggable circle on a canvas which marks
;; the creation point for new toys.
(define Target<%> 
  (interface (Widget<%>)   
    
    ;; -> Integer
    ;; RETURNS: the x and y coordinates of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; RETURNS: Is the target selected?
    target-selected?))


;; -----------------------------------------------
;; Every object that lives in the playground must implement the Toy<%> interface.
(define Toy<%> 
  (interface (SWidget<%>)  
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;;          this data depends on the class of the toy.
    ;;          for a square, it is the velocity of the square (rightward is positive)
    ;;          for a throbber, it is the current radius of the throbber
    ;;          for the clock, it is the current value of the clock
    ;;          for a football, it is the current size of the football (in
    ;;          arbitrary units; bigger is more)
    toy-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Classes
;; -----------------------------------------------

;; A World is a (new World% [toys ListOfToy<%>][square-speed PosInt][target Target<%>])
;; A World represents a playground with a target and some toys.
(define World%
  (class* object% (PlaygroundState<%>)
    
    ;; ------------------
    ;; Fields
    ;; ------------------

    
    ;; The WorldState% from WodgetWorks.rkt
    (init-field (world-state ZERO_POINT))
    ;; The list of toys in the world
    (init-field (toys empty))
    ;; The speed at which square toys move in the horizontal direction
    (init-field (square-speed TARGET-RADIUS)) ;; is used by big-bang
    ;; A draggable target which acts as the spawn point for new toys
    (init-field (target (new Target% [x TARGET-INITIAL-X][y TARGET-INITIAL-Y])))
    
    
    (super-new)
    
    ;; ------------------
    ;; Methods
    ;; ------------------

    ;; create-world-state : -> Void
    ;; EFFECT: initializes WorldState%
    ;; WHERE: World% is sent as SWidget to WorldState%
    ;; TEST: tested in initial-world
    ;; EXAMPLE: is called in world-state field
    (define/public (create-world-state)
      (begin (define w-s (make-world CANVAS-WIDTH CANVAS-HEIGHT))
             (send w-s add-stateful-widget this)
             (set! world-state w-s)))

    ;; run : PosReal -> World
    ;; GIVEN: a frame rate, in secs/tick
    ;; EFFECT: runs this world through WorlsState% at the given frame rate
    ;; RETURNS: the WorlsState% in its final state of the world
    ;; TEST: calls big-bang - not testable 
    (define/public (run rate)
      (send world-state run rate))

    
    ;; target-x : -> Integer
    ;; target-y : -> Integer
    ;; RETURN: the x and y coordinates of the target
    ;; EXAMPLES: let w be (new World% [toys empty][square-speed 10])
    ;; (send w target-x) => TARGET-INITIAL-X
    ;; (send w target-y) => TARGET-INITIAL-Y
    (define/public (target-x)
      (send target target-x))
    (define/public (target-y)
      (send target target-y))
    
    
    ;; target-selected? : -> Boolean
    ;; RETURNS: Whether or not the target is selected
    ;; EXAMPLES: let w be (new World% [toys empty][square-speed 10])
    ;; (send w target-selected) => false
    (define/public (target-selected?)
      (send target target-selected?))
    
    
    ;; get-toys : -> ListOfToy<%>
    ;; RETURNS: The list of toys in the playground
    ;; EXAMPLES: let w be (new World% [toys empty][square-speed 10])
    ;; (send w get-toys) => empty
    (define/public (get-toys)
      toys)
    
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: call after-tick method for all toys
    ;; EXAMPLES: each of the toys in the playground will be updated to the
    ;;           progressed state. The target and square-speed will be unaffected
    (define/public (after-tick)
        (for-each
         ; Toy<%> -> Toy<%>
         ; RETURNS: the state of the given toy after a tick
         (lambda (toy) (send toy after-tick))
         toys))
    

    ;; after-mouse-event: NonNegInt NonNegInt MouseEvent -> Void
    ;; GIVEN: the x and y coordinates in pixels of a the location
    ;;        and a MouseEvent
    ;; EFFECT: the state of the world that should follow the
    ;;         given mouse event at the given location.
    ;; EXAMPLES:
    ;;  (send (make-world 10) after-mouse-event
    ;;        CANVAS-HALF-WIDTH CANVAS-HALF-HEIGHT "button-down")
    ;;   => target will become selected
    ;; STRATEGY: Divide into cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else this]))

    
    ;; after-button-down : Integer Integer -> Void
    ;; after-button-up : Integer Integer -> Void
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;;         following the specified mouse event at the given location.
    (define/public (after-button-down mx my) (world-after-button-down mx my))
    (define/public (after-button-up mx my) (world-after-button-up mx my))
    (define/public (after-drag mx my) (world-after-drag mx my))
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates the state of the world that should follow the
    ;;         given key event, for supported keyevents
    ;; EXAMPLES:
    ;;  (send world1 after-key-event "x") => no effect
    ;;  (send world1 after-key-event "s") => a square toy is added to the world
    ;; STRATEGY: Divide into cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-KEY)
         (world-with-new-toy (make-throbber (target-x) (target-y)))]
        [(key=? kev NEW-SQUARE-KEY)
         (world-with-new-toy (make-square-toy (target-x) (target-y) square-speed))]
        [(key=? kev NEW-CLOCK-KEY)
         (world-with-new-toy (make-clock (target-x) (target-y)))]
        [(key=? kev NEW-FOOTBALL-KEY)
         (world-with-new-toy (make-football (target-x) (target-y)))]
        [else this]))
    

    ;; to-scene : -> Scene
    ;; RETURNS: a scene that depicts this World, with the target and
    ;;          all toys painted on it
    ;; EXAMPLES:
    ;;  (send (make-world 10) to-scene) => a canvas with a blue circle for the target
    ;;  in the middle
    ;; STRATEGY: Use HOF foldr on toys
    (define/public (to-scene)
      (send target add-to-scene
            (foldr
             ; Toy<%> Scene -> Scene
             ; RETURNS: A scene like the given one with the given toy painted on it
             (lambda (toy scene) (send toy add-to-scene scene))
             EMPTY-CANVAS
             toys)))

    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene that depicts this World, with the target and
    ;;          all toys painted on it
    ;; EXAMPLES:
    ;;  (send (make-world 10) to-scene) => a canvas with a blue circle for the target
    ;;  in the middle
    ;; STRATEGY: Use HOF foldr on toys
    (define/public (add-to-scene new-scene)
      (send target add-to-scene
            (foldr
             ; Toy<%> Scene -> Scene
             ; RETURNS: A scene like the given one with the given toy painted on it
             (lambda (toy scene) (send toy add-to-scene scene))
             new-scene
             toys)))
    
    
    ;; world-after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN: the x and y coordinates of a mouse down event
    ;; EFFECT: updates the state of this world after a mouse down event
    ;;         at the given coordinates. The event is sent to the target and all toys.
    ;; STRATEGY: Use HOF map on toys
    (define (world-after-button-down mx my)
      (begin
        (map
         ; Toy<%> -> Toy<%>
         ; RETURNS: the state of the given toy after the button down event
         (lambda (toy) (send toy after-button-down mx my))
         toys)
        (send target after-button-down mx my)))
    
   
    ;; world-after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: the x and y coordinates of a mouse up event
    ;; EFFECT: updates the state of the world after a mouse up event
    ;;         at the given coordinates. The event is sent to the target and all toys.
    ;; STRATEGY: Use HOF map on toys
    (define (world-after-button-up mx my)
      (begin
        (map
         ; Toy<%> -> Toy<%>
         ; RETURNS: the state of the given toy after the button up event
         (lambda (toy) (send toy after-button-up mx my))
         toys)
        (send target after-button-up mx my)))
    
    
    ;; world-after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: the x and y coordinates of a mouse drag event
    ;; EFFECT:  updates the state of the world after a mouse drag event
    ;;          at the given coordinates. The event is sent to the target and all toys.
    ;; STRATEGY: Use HOF map on toys
    (define (world-after-drag mx my)
      (begin
        (map
         ; Toy<%> -> Toy<%>
         ; RETURNS: the state of the given toy after the drag event
         (lambda (toy) (send toy after-drag mx my))
         toys)
        (send target after-drag mx my)))
    
    
    ;; world-with-new-toy : Toy<%> -> Void
    ;; GIVEN: a new toy
    ;; EFFECT: adds a new toy to the world
    ;; STRATEGY: Combine simpler functions
    (define (world-with-new-toy toy)
      (set! toys (cons toy toys)))

    ;; for-test:get-square-speed : -> PosInt
    ;; RETURNS: square-speed
    ;; EXAMPLE: for default world TARGET-RADIUS value will be returned
    (define/public (for-test:get-square-speed) square-speed)))

;; -----------------------------------------------
;; A Target is a (new Target% [x Int][y Int][selected? Boolean][dx Int][dy Int])
;; A Target represents a target in a playground state. It is draggable but is
;; unaffected by key events or ticks.
(define Target%
  (class* object% (Target<%>)

    ;; -------------------
    ;; Fields
    ;; ------------------
    
    ;; the x and y coordinates of the center of the target in pixels
    (init-field x y)
    
    ;; is the target selected? Defaults to false
    (init-field (selected? false))
    
    ;; The x and y offsets between the last mouse down event that was inside
    ;; the target and the center of the target.
    (init-field (dx ZERO_POINT) (dy ZERO_POINT))
    
    (super-new)

    
    ;; -------------------
    ;; Methods
    ;; ------------------
    
    ;; target-x : -> Integer
    ;; target-y : -> Integer
    ;; RETURNS: the x and y coordinates of the target
    (define/public (target-x) x)
    (define/public (target-y) y)
    
    
    ;; target-selected? : -> Boolean
    ;; RETURNS: Is the target selected?
    (define/public (target-selected?) selected?)
    
    
    ;; after-tick : -> Target<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a target exactly the same as this one, as targets are
    ;;          not affected by ticks
    (define/public (after-tick) this)
    
   
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-down event
    ;; EFFECT: updates the state of this target that should follow a
    ;;         mouse down event at the given location.
    ;; EXAMPLES:
    ;; (send target-at-100-100 after-button-down 100 100) => target becomes selected
    ;; (send target-at-100-100 after-button-down 500 500) => no effect
    ;; STRATEGY: Cases on whether the event is in the target
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (begin
            (set! selected? true)
            (set! dx (- mx x))
            (set! dy (- my y)))
          false))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  updates the state of this target that should follow a
    ;;          mouse up event at the given location.
    ;; EXAMPLES:
    ;; (send target-at-100-100 after-button-up 100 100) => target becomes unselected
    ;; (send target-at-100-100 after-button-up 500 500) => no effect
    ;; STRATEGY: Cases on whether the event is in the target
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! dx ZERO_POINT)
        (set! dy ZERO_POINT)))
    

    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  updates the state of this object that should follow a
    ;;          mouse drag event at the given location.
    ;; EXAMPLES:
    ;; (send unselected-target drag 100 100) => no effect
    ;; (send selected-target drag 100 100) => target moved to (100,100)
    ;; STRATEGY: Cases on whether the target is selected
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx dx))
            (set! y (- my dy)))
          false))
    
    
    ;; after-key-event : KeyEvent -> Target<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of this target that should follow the
    ;;          given key event. The target should be the same since
    ;;          targets ignore key events 
    (define/public (after-key-event kev) this)
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this target
    ;;          painted on it.
    ;; EXAMPLES:
    ;;  (send target-at-100-100 add-to-scene (empty-scene 200 200)) => blue outline circle
    ;;   centered at (100,100) on a canvas
    (define/public (add-to-scene s)
      (place-image TARGET-IMAGE x y s))
    
    
    ;; in-target? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: the x and y coordinates of a mouse event
    ;; RETURNS: true iff the given coordinate is inside the circle that represents
    ;;          the target
    ;; STRATEGY: Combine simpler functions
    (define (in-target? mx my)
      (in-circle? mx my x y TARGET-RADIUS))))


;; -----------------------------------------------
;; A Throbber is a (new Throbber% [x Int][y Int][selected? Bool][dx Int][dy Int]
;;                                [radius PosInt][radius-change Int])
;; A Throbber represents a circle in a playground state which expands and contracts
;; between a maximum and minimum radius on each tick. It is draggable when clicking
;; inside the circle. The throbber remains selected if the point where it became
;; selected is no longer in the circle after a tick.
(define Throbber%
  (class* object% (Toy<%>)

    ;; ------------------
    ;; Fields
    ;; ------------------
    
    ;; the x and y coordinates of the center of the throbber in pixels
    (init-field x y)
    
    ;; is the throbber selected? Defaults to false
    (init-field (selected? false))
    
    ;; The x and y offsets between the last mouse down event that was inside
    ;; the throbber and the center of the throbber.
    (init-field (dx ZERO_POINT) (dy ZERO_POINT))
    
    ;; the current radius of the throbber
    (init-field (radius THROBBER-MIN-RADIUS))
    
    ;; the size by which the radius expands or shrinks each tick. If positive,
    ;; the radius expands by that many pixels per tick. If negative, it contracts
    ;; by that amount
    (init-field (radius-change 1_PX))
    
    (super-new)
    
    ;; ------------------
    ;; Methods
    ;; ------------------
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS: the x or y position of the center of the toy
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    
    ;; toy-data : -> Int
    ;; RETURNS: the current radius of the throbber
    (define/public (toy-data) radius)
    
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: Updates the state of this throbber that should follow one tick of the world
    ;;         The radius increase or decreases according to the radius-change. When the
    ;;         radius reaches it minimum or maximum value, it reverses whether it is
    ;;         growing/shrinking
    (define/public (after-tick)
      (begin
        (set! radius (+ radius radius-change))
        (set! radius-change (radius-change-after-tick (+ radius radius-change)))))
    
    
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-down event
    ;; EFFECT: update the state of this throbber that should follow a
    ;;         mouse down event at the given location.
    ;; EXAMPLES:
    ;;  button-down event in throbber => selected throbber
    ;;  button-down event outside of throbber => no effect on throbber
    ;; STRATEGY: Cases on whether the event is in the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! selected? true)
            (set! dx (- mx x))
            (set! dy (- my y)))
          false))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  update the state of this throbber that should follow a
    ;;          mouse up event at the given location.
    ;; EXAMPLES:
    ;;  button-up event in throbber => unselected throbber
    ;;  button-up event outside of throbber => no effect on throbber
    ;; STRATEGY: Cases on whether the event is in the throbber
    (define/public (after-button-up mx my)
      (if (in-throbber? mx my)
          (set! selected? false)
          false))
    

    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  update the state of this object that should follow a
    ;;          mouse drag event at the given location.
    ;; EXAMPLES:
    ;;  drag unselected throbber => no effect on throbber
    ;;  drag selected throbber => throbber moved so that the vector from the center to
    ;;                            the drag event is equal to (mx, my)
    ;; STRATEGY: Cases on whether the throbber is selected
    (define/public (after-drag mx my)
      (if selected?
          (begin
               (set! x (- mx dx))
               (set! y (- my dy)))
          false))
    
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of this throbber that should follow the
    ;;          given key event. The throbber should be the same since
    ;;          throbbers ignore key events
    (define/public (after-key-event kev) this)
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this throbber
    ;;          painted on it.
    ;; EXAMPLE:
    ;;  add-to-scene empty-canvas, throbber current radius is 10 => scene with solid green
    ;;   circle of radius 10 painted on it at (x,y)
    (define/public (add-to-scene s)
      (place-image (circle radius THROBBER-CIRCLE-MODE THROBBER-CIRCLE-COLOR) x y s))
    
    
    ;; in-throbber? : NonNegInt NonNegInt -> Boolean
    ;; RETURNS: true iff the given coordinate is inside the circle that represents
    ;;          the throbber
    ;; STRATEGY: Call a more general function
    (define (in-throbber? mx my)
      (in-circle? mx my x y radius))
    
    
    ;; radius-change-after-tick : Int -> Int
    ;; GIVEN: the new radius of the throbber after the tick
    ;; RETURNS: the new rate at which the radius of the throbber should be changing after
    ;;          a tick. Radius change is inverted if the new radius has reached
    ;;          the max or min radius for a throbber.
    ;; STRATEGY: Cases on whether radius = THROBBER-MAX-RADIUS or THROBBER-MIN-RADIUS
    (define (radius-change-after-tick new-radius)
      (if (or (>= new-radius THROBBER-MAX-RADIUS) (<= new-radius THROBBER-MIN-RADIUS))
          (- radius-change)
          radius-change))
    
    
    ;; for-test:selected? : -> Boolean
    ;; RETURNS: true iff the football is selected
    (define/public (for-test:selected?)
      selected?)
    
    ;; for-test:radius-change : -> Integer
    ;; RETURNS: the current rate at which the throbber's radius changes on each tick
    (define/public (for-test:radius-change)
      radius-change)))


;; -----------------------------------------------
;; A square is a (new Square% [x Int][y Int][selected? bool][saved-mx Int]
;;                            [saved-my Int][velocity Int])
;; A square represents a square toy in a playground state. It is draggable
;; according to the bounding box of its image.
(define Square%
  (class* object% (Toy<%>)

    ;; ---------------------
    ;; Fields
    ;; ---------------------
    
    ; the x, y position of the center of the square
    (init-field x y)
    
    ;; the current velocity of the square
    (init-field velocity)
    
    ; is the square selected? Default is false.
    (init-field [selected? false]) 
    
    ;; The x and y offsets between the last mouse down event that was inside
    ;; the square and the center of the square.
    (init-field [saved-mx ZERO_POINT] [saved-my ZERO_POINT])
    
    (super-new)

    ;; -------------------
    ;; Methods
    ;;-------------------
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS: the x or y position of the center of the square
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    
    ;; toy-data : -> Int
    ;; RETURNS: the current velocity of the square (rightward is positive)
    (define/public (toy-data) velocity)
    

    ;; after-tick : Time -> Void
    ;; GIVEN: no arguments
    ;; EFFECT:  Update the state of the square that should follow after the tick of the
    ;;          world
    ;; EXAMPLES:
    ;;  (send unselected-square after-tick) => square moved according to its velocity
    ;;  (send selected-square after-tick) => no effect
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          false
          (begin
               (set! x (get-new-x))
               (set! velocity (get-new-velocity)))))
    
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of this square toy that should follow the
    ;;          given key event. The square should be the same since
    ;;          squares ignore key events
    ;; EXAMPLES:
    ;;  let square be (make-square-toy 100 100 50)
    ;;  (send square after-key-event) => square
    (define/public (after-key-event kev)
      this)
    
    
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-down event
    ;; EFFECT:  update the state of this object that should follow a
    ;;          mouse down event at the given location.
    ;; EXAMPLES:
    ;;  after-button-down event inside of square => square becomes selected
    ;;  after-button-down event outside of square => no effect
    ;; STRATEGY: Cases on whether the event is in the square
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
               (set! selected? true)
               (set! saved-mx (- mx x))
               (set! saved-my (- my y)))
          this))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  update the state of this object that should follow a
    ;;          mouse up event at the given location.
    ;; EXAMPLES:
    ;;  after-button-up event inside of square => square becomes unselected
    ;;  after-button-up event outside of square => no effect
    ;; STRATEGY: Cases on whether the event is in the square
    (define/public (after-button-up mx my)
      (if (in-square? mx my)
          (set! selected? false)
          this))   
    
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  nupdate the state of this object that should follow a
    ;;          mouse drag event at the given location.
    ;; EXAMPLES:
    ;;  drag with selected square => square moves
    ;;  drag with unselected square => no effect
    ;; STRATEGY: Cases on whether the square is selected
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))   
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this square painted
    ;;          on it.
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMAGE x y scene))
    
    
    ;; in-square? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: a location on the canvas as (x,y) coordinates
    ;; RETURNS: true iff the location is inside this square.
    ;; STRATEGY: Call a more general function
    (define (in-square? mx my)
      (in-rectangle? mx my x y HALF-SIDE HALF-SIDE))
    
    
    ;; get-new-velocity : -> Integer  
    ;; RETURNS: The new velocity of the square after a tick in the world.
    ;;          If the next tick would cause it to hit the wall, it reverses direction.
    ;; STRATEGY: Cases on would-hit-left-wall?/would-hit-right-wall?
    (define (get-new-velocity)
      (if (or (would-hit-left-wall?) (would-hit-right-wall?))
          (- velocity)
          velocity))
    
    
    ;; get-new-x : -> Integer  
    ;; RETURNS: The new x position of the center of the square after a tick
    ;;          If the next tick would cause it to collide with or go past the wall, it
    ;;          is placed with its side squarely against the wall
    ;; STRATEGY: Cases on would-hit-left-wall?/would-hit-right-wall?
    (define (get-new-x)
      (cond
        [(would-hit-left-wall?) HALF-SIDE]
        [(would-hit-right-wall?) (- CANVAS-WIDTH HALF-SIDE)]
        [else (+ x velocity)]))
    
    
    ;; would-hit-right-wall : -> Boolean
    ;; would-hit-left-wall : -> Boolean
    ;; RETURNS: true iff the square would collide with the right or left wall in the
    ;;          next tick of the world
    ;; STRATEGY: Combine simpler functions
    (define (would-hit-right-wall?)
      (>= (+ x velocity) (- CANVAS-WIDTH HALF-SIDE)))
    (define (would-hit-left-wall?)
      (<= (+ x velocity) HALF-SIDE))
    
    
    ;; for-test:selected? : -> Boolean
    ;; RETURNS: true iff the square is selected
    (define/public (for-test:selected?)
      selected?)))


;; -----------------------------------------------
;; A Clock is a (new Clock% [x Int][y Int][selected? bool][saved-mx Int][saved-my Int]
;;                          [tick-rate NonNegInt])
;; A Clock represents a piece of text that displays the number of ticks since its
;; creation. The clock tick stops while the clock is selected.
(define Clock%
  (class* object% (Toy<%>)

    ;; ---------------------
    ;; Fields
    ;; ---------------------
    
    ;; the x and y position of the center of the Clock and the clock tick 
    (init-field x y [tick-rate ZERO_POINT])
    
    ;; is the clock selected? Default is false.
    (init-field [selected? false]) 
    
    ;; The x and y offsets between the last mouse down event that was inside
    ;; the clock and the center of the clock.
    (init-field [saved-mx ZERO_POINT] [saved-my ZERO_POINT])
    
  
    ;; --------------------
    ;; Helper functions for Clock (work as constants)
    ;; --------------------
    ;; CLOCK-IMAGE : -> Scene
    ;; clock-image-width : -> Scene
    ;; clock-image-height : -> Scene
    ;; clock-image-half-width : -> Scene
    ;; clock-image-HALF-HEIGHT : -> Scene
    ;; RETURNS: Clock image and information about it
    ;; STRATEGY: Use simpler functions
    (define/public (clock-image) (text (number->string tick-rate)
                                       THROBBER-MAX-RADIUS
                                       "black"))
    (define/public (clock-image-width) (image-width (clock-image)))
    (define/public (clock-image-height) (image-height (clock-image)))
    (define/public (clock-image-half-width) (/ (clock-image-width) 2_PX))
    (define/public (clock-image-half-height) (/ (clock-image-height) 2_PX))
    
    
    (super-new)
    
    ;; -------------------
    ;; Methods
    ;; -------------------
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS: the x or y position of the center of the clock
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    
    ;; toy-data : -> Int
    ;; RETURNS: the current value of the clock
    (define/public (toy-data) tick-rate)
    
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT:  update the state of this clock that should follow one tick of
    ;;          the world
    ;; EXAMPLES:
    ;;  (send unselected-clock after-tick) => clock value increases by one
    ;;  (send selected-clock after-tick) => clock value is unaffected
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          false
          (set! tick-rate (+ 1_PX tick-rate))))
    
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of this clock that should follow the
    ;;          given key event. The clock should be the same since
    ;;          clocks ignore key events
    (define/public (after-key-event kev) this)      
    
    
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-down event
    ;; EFFECT:  update the state of this clock that should follow a
    ;;          mouse down event at the given location.
    ;; EXAMPLES:
    ;;  button-down event in clock => selected clock
    ;;  button-down event outside of clock => no effect on clock
    ;; STRATEGY: Cases on whether the event is in the clock
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    
    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFFECT: update the state of the clock that should follow the button-up event
    ;; EXAMPLES:
    ;;  button-up event in clock => unselected clock
    ;;  button-up event outside of clock => no effect on clock
    ;; STRATEGY: Cases on whether the event is in the clock.
    ;; NOTE: If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (set! selected? false))
          
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT:  update the state of the clock that should follow the drag event
    ;;          If it is selected, move it so that the vector from the center to
    ;;          the drag event is equal to (mx, my)
    ;; STRATEGY: Cases on whether the clock is selected.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))
    
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;;          on it.
    ;; EXAMPLES:
    ;; add-to-scene empty-canvas, clock at 42
    ;;   => Black text "42", size 20, painted at (x,y)
    (define/public (add-to-scene scene)
      (place-image (clock-image) x y scene))
    
    
    ;; in-clock? : NonNegInt NonNegInt -> Boolean 
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside the rectangle that bounds this clock.
    ;; STRATEGY: Call a more general function
    (define (in-clock? mx my)
      (in-rectangle? mx my x y (clock-image-half-width) (clock-image-half-height)))
    
    
    ;; for-test:selected? : -> Boolean
    ;; RETURNS: true iff the clock is selected
    (define/public (for-test:selected?)
      selected?)))


;; -----------------------------------------------
;; A Football is a (new Football% [x Int][y Int][selected? bool][dx Int][dy Int]
;;                                [fscale Real])
;; WHERE: 0 <= fscale <= 1
;; A Football represents a football in a playground state. It is draggable according
;; to the bounding box of its image. It decreases in size on each clock tick until
;; it disappears.
(define Football%
  (class* object% (Toy<%>)    

    ;; ------------------
    ;; Fields
    ;; ------------------
    
    ;; the x and y coordinates of the center of the football in pixels
    (init-field x y)
    
    ;; is the football selected? Defaults to false
    (init-field (selected? false))
    
    ;; The x and y offsets between the last mouse down event that was inside
    ;; the football and the center of the football.
    (init-field (dx ZERO_POINT) (dy ZERO_POINT))
    
    ;; The scale of football image, which is a real number 0 <= fscale <= 1
    (init-field (fscale 1_PX))

    
    (super-new)
    
    ;; ------------------
    ;; Methods
    ;; ------------------
    
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS: the x or y position of the center of the football
    (define/public (toy-x) x)
    (define/public (toy-y) y)
    
    
    ;; toy-data : -> Int
    ;; RETURNS: the current size of the football
    ;;          fscale is normally a real number in [0,1], so to convert
    ;;          it to an integer we multiply by 10 and take the floor.
    ;;          As such a full size football will return 10, and a completely
    ;;          deflated football will return 0. The units are arbitrary.
    ;; EXAMPLES:
    ;; fscale is 1 => 10
    ;; fscale is 0.5 => 5
    ;; fscale is 0 => 0
    (define/public (toy-data) (floor (* fscale TARGET-RADIUS)))
    
  
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT:  update the state of this football that should follow one tick of
    ;;          the world. The scale of the football (fscale) decreases by
    ;;          FOOTBALL-DEFLATE-RATE until it reaches or would go below zero, at which
    ;;          point it stays at zero.
    (define/public (after-tick)
      (set! fscale (scale-after-tick)))
    
    
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-down event
    ;; EFFECT:  update the state of this football that should follow a
    ;;          mouse down event at the given location.
    ;; EXAMPLES:
    ;;  button-down in football => selected football
    ;;  button-down outside of football => no effect on football
    ;; STRATEGY: Cases on whether the event is in the football
    (define/public (after-button-down mx my)
      (if (in-football? mx my)
          (begin
            (set! selected? true)
            (set! dx (- mx x))
            (set! dy (- my y)))
          this))
    

    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  update the state of this football that should follow a
    ;;          mouse up event at the given location.
    ;; EXAMPLES:
    ;;  button-down in football => unselected football
    ;;  button-down outside of football => no effect on football
    ;; STRATEGY: Cases on whether the event is in the football
    (define/public (after-button-up mx my)
      (if (in-football? mx my)
          (set! selected? false)
          false))
    

    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a button-up event
    ;; EFFECT:  update the state of this football that should follow a
    ;;          mouse drag event at the given location.
    ;; EXAMPLES:
    ;;  drag unselected football => no effect on football
    ;;  drag selected football => football moved so that the vector from the center to
    ;;                            the drag event is equal to (mx, my)
    ;; STRATEGY: Cases on whether the football is selected
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx dx))
            (set! y (- my dy))
            (set! selected? true))
          false))
    

    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of this football that should follow the
    ;;          given key event. The football should be the same since
    ;; footballs ignore key events
    (define/public (after-key-event kev) this)
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this football
    ;;          painted on it.
    ;; EXAMPLES:
    ;;  add-to-scene empty-canvas, football scale is one => the full-sized football
    ;;  image painted on the scene centered at (x,y)
    ;;  add-to-scene empty-canvas, football scale is zero => empty-canvas
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene s)
      (place-image (football-image) x y s))
    
    
    ;; in-football? : NonNegInt NonNegInt -> Boolean
    ;; RETURNS: true iff the given coordinate is inside the football
    ;; STRATEGY: Call a more general function
    (define (in-football? mx my)
      (in-rectangle? mx my x y (football-half-width) (football-half-height)))
    
    
    ;; scale-after-tick : Real
    ;; RETURNS: the scale of the football after a tick in the world. The
    ;;          scale will always be in [0,1]
    ;; STRATEGY: Divide into cases on whether or not scale would go below 0
    (define (scale-after-tick)
      (if (>= (- fscale FOOTBALL-DEFLATE-RATE) ZERO_POINT)
          (- fscale FOOTBALL-DEFLATE-RATE)
          ZERO_POINT))
    
    
    ;; football-image : -> Image
    ;; RETURNS: the image representing the current football, with its scale
    ;;          applied
    ;; STRATEGY: Divide into cases on the size of scale
    (define (football-image)
      (if (> fscale ZERO_POINT)
          (scale fscale FOOTBALL-IMAGE)
          empty-image))
    
    
    ;; football-half-height : -> NonNegInt
    ;; football-half-width : -> NonNegInt
    ;; RETURNS: half of the height or width of the football, with its scale
    ;;          taken into account
    ;; STRATEGY: Combine simpler functions
    (define (football-half-height)
      (/ (image-height (football-image)) 2_PX))
    (define (football-half-width)
      (/ (image-width (football-image)) 2_PX))
  
    
    ;; for-test:selected? : -> Boolean
    ;; RETURNS: true iff the football is selected
    (define/public (for-test:selected?)
      selected?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Helper functions
;; -----------------------------------------------
;; run : NonNegInt NonNegInt PosInt -> Void
;; GIVEN: a frame rate (in ticks)
;;        a square speed (in px per tick)
;; EFFECT: creates and runs world at the given frame rate and given square speed
;; TEST: calls big-bang - not testable
;; EXAMPLES: calls run method, from WidgetWorks, which calls big-bang
;; STRATEGY: Combine simpler functions
(define (run rate speed)
  (begin
    (define playground (make-playground speed))
    (send playground run rate)))


;; -----------------------------------------------
;; make-playground : PosInt -> PlaygroundState<%>
;; RETURNS: a world with a target, but no toys, and in which any
;;          square toys created in the future will travel at the given speed (in
;;          pixels/tick).
;; EXAMPLES:
;;  (make-world 10) => a new world with no toys in which squares move at
;;  10 pixels per tick
;; STRATEGY: Combine simpler functions
(define (make-playground speed)
  (begin
    (define world (new World% [square-speed speed]))
    (send world create-world-state)
    world))


;; -----------------------------------------------
;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; STRATEGY: Combine simpler functions
(define (make-football x y)
  (new Football% [x x][y y]))


;; -----------------------------------------------
;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; EXAMPLES:
;;  (make-throbber 10 10) => a throbber centered at (10,10) with initial radius 5 which
;;                           is expanding
;; STRATEGY: Combine simpler functions
(define (make-throbber x y)
  (new Throbber% [x x][y y]))


;; -----------------------------------------------
;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;;          travelling right at the given speed
;; EXAMPLES:
;;  (make-square-toy 50 50 10) => a square at (50,50) which moves initially rightward
;;  at 10 pixels/second
;; STRATEGY: Combine simpler functions
(define (make-square-toy x y speed)
  (new Square% [x x] [y y] [velocity speed]))


;; -----------------------------------------------
;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; STRATEGY: Combine simpler functions
(define (make-clock x y)
  (new Clock% [x x] [y y]))


;; -----------------------------------------------
;; in-circle? : NonNegInt NonNegInt NonNegInt NonNegInt NonNegInt -> Boolean
;; GIVEN: the x and y coordinates to test, the x and y coordinates of the center
;;        of the circle, and the radius of the circle
;; RETURNS: true iff the given coordinate is inside the circle described by the given
;;          center and radius
;; EXAMPLES:
;;  (in-circle? 3 3 0 0 6) => true
;;  (in-circle? 7 7 0 0 6) => false
;; STRATEGY: Combine simpler functions
(define (in-circle? mx my x y r)
  (<=
   (+ (sqr (- mx x))
      (sqr (- my y)))
   (sqr r)))


;; -----------------------------------------------
;; in-rectangle? : NonNegInt NonNegInt NonNegInt NonNegInt NonNegInt NonNegInt -> Boolean
;; GIVEN: the (x,y) coordinates of the location to test, the (x,y) coordinates of the
;;        center of the rectange, half the width of the rectangle, and half the height of
;;        the rectangle
;; RETURNS: true iff the given coordinate is inside the rectangle described by the given
;;          center coordinate, half-width, and half-height
;; EXAMPLES:
;;  (in-rectangle? 4 5 0 0 6 3) => false
;;  (in-rectangle? 4 5 0 0 6 5) => true
;; STRATEGY: Combine simpler functions
(define (in-rectangle? mx my x y half-width half-height)
  (and
   (<= (- x half-width) mx (+ x half-width))
   (<= (- y half-height) my (+ y half-height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; TESTS
;; -----------------------------------------------
;; World tests

;; Initial world
(begin-for-test
  (local ((define initial-world (make-playground 10))
          (define initial-world2 (make-playground 10)))
    (send initial-world after-drag 0 0)
    (send initial-world after-button-up 0 0)
    (send initial-world after-button-down 0 0)
    (check-false (send initial-world target-selected?)
                 "Target should be unselected in the initial world state")
    (check-equal? (send initial-world target-x)
                  CANVAS-HALF-WIDTH
                  "Target should start in the middle of the canvas")
    (check-equal? (send initial-world target-y)
                  CANVAS-HALF-HEIGHT
                  "Target should start in the middle of the canvas")
    (check-equal? (send initial-world get-toys)
                  empty
                  "The initial world should have no toys")
    (check-equal? (send initial-world for-test:get-square-speed)
                  TARGET-RADIUS
                  "Default world has speed = TARGET-RADIUS")
    (send initial-world2 after-key-event NEW-THROBBER-KEY)
    (send initial-world2 add-to-scene EMPTY-CANVAS)
    (check-equal? (send initial-world to-scene)
                  (place-image
                   TARGET-IMAGE TARGET-INITIAL-X TARGET-INITIAL-Y EMPTY-CANVAS)
                  "The initial world should have no toys")))


;; key-event tests
(begin-for-test
  (local ((define w1 (make-playground 10)))
    (send w1 after-key-event "q")
    (check-equal? (send w1 get-toys)
                  empty
                  "No toys should be added for key events other than s,t,w,f")
    (send w1 after-key-event "s")
    (check-equal? (length (send w1 get-toys))
                  1
                  "1 toy should have been added after an s key")
    (send w1 after-key-event "t")
    (check-equal? (length (send w1 get-toys))
                  2
                  "1 toy should have been added after an t key")
    (send w1 after-key-event "w")
    (check-equal? (length (send w1 get-toys))
                  3
                  "1 toy should have been added after an w key")
    (send w1 after-key-event "f")
    (check-equal? (length (send w1 get-toys))
                  4
                  "1 toy should have been added after an f key")))


;; mouse-event tests
(begin-for-test
  (local ((define w1 (make-playground 10)))
    (send w1 after-key-event "f")
    (send w1 after-mouse-event CANVAS-HALF-WIDTH CANVAS-HALF-HEIGHT "button-down")
    (check-true (send w1 target-selected?)
                "World should send mouse down to target and select it")
    (check-true (send (first (send w1 get-toys)) for-test:selected?)
                "World should send mouse down to all toys")
    (send w1 after-mouse-event 100 150 "drag")
    (check-equal? (send w1 target-x)
                  100
                  "Target should have been moved by drag")
    (check-equal? (send w1 target-y)
                  150
                  "Target should have been moved by drag")
    (check-equal? (send (first (send w1 get-toys)) toy-x)
                  100
                  "Toy should have been moved by drag")
    (check-equal? (send (first (send w1 get-toys)) toy-y)
                  150
                  "Toy should have been moved by drag")
    (send w1 after-mouse-event 100 150 "button-up")
    (check-false (send w1 target-selected?)
                 "Target should not be selected after button-up event")
    (check-false (send (first (send w1 get-toys)) for-test:selected?)
                 "Toy should not be selected after button-up event")
    (send w1 after-mouse-event 300 300 "move")
    (check-equal? (send w1 target-x)
                  100
                  "Target should be unaffected by move event")))


;; after-tick tests
(begin-for-test
  (local ((define w1 (make-playground 10)))
    (send w1 after-key-event "s")
    (check-equal? (send (first (send w1 get-toys)) toy-x)
                  CANVAS-HALF-WIDTH
                  "Square toy x coordinate should start at center of canvas")
    (send w1 after-tick)
    (check-equal? (send (first (send w1 get-toys)) toy-x)
                  (+ CANVAS-HALF-WIDTH 10)
                  "Tick should have moved the square toy to the right")))



;; to-scene tests
(begin-for-test
  (local ((define w1 (make-playground 10))
          (define w1-scene (place-image
                            (circle 10 "outline" "blue")
                            CANVAS-HALF-WIDTH
                            CANVAS-HALF-HEIGHT
                            (place-image
                             (square 40 "outline" "red")
                             CANVAS-HALF-WIDTH
                             CANVAS-HALF-HEIGHT
                             (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))))
    (send w1 after-key-event "s")
    (check-equal? (send w1 to-scene)
                  w1-scene
                  "Scene should have a circle for the target and a square for the toy")))

;; -----------------------------------------------
;; Tartget tests

;; mouse-event tests
(begin-for-test
  (local
    ((define t1 (new Target% [x 100] [y 100])))
    (check-false (send t1 target-selected?)
                 "Target should be unselected initially")
    (send t1 after-button-down 100 100)
    (check-true (send t1 target-selected?)
                "Target should be selected after a button-down inside it")
    (send t1 after-drag 60 50)
    (check-equal? (send t1 target-x)
                  60
                  "Target x coordinate should have changed after drag")
    (check-equal? (send t1 target-y)
                  50
                  "Target y coordinate should have changed after drag")
    (send t1 after-button-up 60 50)
    (check-false (send t1 target-selected?)
                 "Target should not be selected after a button up event")
    (send t1 after-button-down 300 300)
    (check-false (send t1 target-selected?)
                 "Target should not be selected after button down outside of it")
    (send t1 after-drag 250 250)
    (check-equal? (send t1 target-x)
                  60
                  "Target x coordinate should not have changed after unselected drag")
    (check-equal? (send t1 target-y)
                  50
                  "Target y coordinate should not have changed after unselected drag")
    (send t1 after-button-up 250 250)
    (check-false (send t1 target-selected?)
                 "Target should not be selected after a button up event")))


;; key-event tests
(begin-for-test
  (local
    ((define t1 (new Target% [x 100][y 100]))
     (define t2 (send t1 after-key-event "k")))
    (check-equal? (send t1 target-selected?) (send t2 target-selected?)
                  "Target selected? should not be affected by key events")
    (check-equal? (send t1 target-x) (send t2 target-x)
                  "Target x coordinate should not be affected by key events")
    (check-equal? (send t1 target-y) (send t2 target-y)
                  "Target y coordinate should not be affected by key events")))


;; after-tick tests
(begin-for-test
  (local
    ((define t1 (new Target% [x 100][y 100]))
     (define t2 (send t1 after-tick)))
    (check-equal? (send t1 target-selected?) (send t2 target-selected?)
                  "Target selected? should not be affected by tick")
    (check-equal? (send t1 target-x) (send t2 target-x)
                  "Target x coordinate should not be affected by tick")
    (check-equal? (send t1 target-y) (send t2 target-y)
                  "Target y coordinate should not be affected by tick")))


;; add-to-scene tests
(begin-for-test
  (local
    ((define t1 (new Target% [x 100][y 100]))
     (define empty-canvas (empty-scene 500 500))
     (define t1-scene (place-image (circle 10 "outline" "blue") 100 100 empty-canvas)))
    (check-equal? (send t1 add-to-scene empty-canvas)
                  t1-scene
                  "Scene should have a blue outline circle with radius 10 on it")))


;; -----------------------------------------------
;; Football tests

;; mouse-event tests
(begin-for-test
  (local
    ((define f1 (make-football 100 100)))
    (check-false (send f1 for-test:selected?)
                 "Football should be unselected initially")
    (send f1 after-button-down 100 100)
    (check-true (send f1 for-test:selected?)
                "Football should be selected after a button-down inside it")
    (send f1 after-drag 60 50)
    (check-equal? (send f1 toy-x)
                  60
                  "Football x coordinate should have changed after drag")
    (check-equal? (send f1 toy-y)
                  50
                  "Football y coordinate should have changed after drag")
    (send f1 after-button-up 60 50)
    (check-false (send f1 for-test:selected?)
                 "Football should not be selected after a button up event")
    (send f1 after-button-down 300 300)
    (check-false (send f1 for-test:selected?)
                 "Football should not be selected after button down outside of it")
    (send f1 after-drag 250 250)
    (check-equal? (send f1 toy-x)
                  60
                  "Football x coordinate should not have changed after unselected drag")
    (check-equal? (send f1 toy-y)
                  50
                  "Football y coordinate should not have changed after unselected drag")
    (send f1 after-button-up 250 250)
    (check-false (send f1 for-test:selected?)
                 "Football should not be selected after a button up event")))


;; key-event tests
(begin-for-test
  (local
    ((define f1 (make-football 100 100))
     (define f2 (make-football 100 100)))
    (send f2 after-key-event "k")
    (check-equal? (send f1 for-test:selected?) (send f2 for-test:selected?)
                  "Football selected? should not be affected by key events")
    (check-equal? (send f1 toy-x) (send f2 toy-x)
                  "Football x coordinate should not be affected by key events")
    (check-equal? (send f1 toy-y) (send f2 toy-y)
                  "Football y coordinate should not be affected by key events")
    (check-equal? (send f1 toy-data) (send f2 toy-data)
                  "Football size should not be affected by key events")))


;; after-tick tests
(begin-for-test
  (local
    ((define f1 (make-football 100 100))
     (define f2 (make-football 100 100))
     (define nearly-flat-football (new Football% [x 100][y 100][fscale 0.01])))
     (send f2 after-tick)
    (send nearly-flat-football after-tick)
    (check-true (> (send f1 toy-data) (send f2 toy-data))
                "The football should be smaller after a tick")
    (check-equal? (send nearly-flat-football toy-data)
                  0
                  "The football should be completely flat")))


;; add-to-scene tests
(begin-for-test
  (local
    ((define empty-canvas (empty-scene 500 500))
     (define non-flat-football (make-football 100 100))
     (define flat-football (new Football% [x 100][y 100][fscale 0]))
     (define non-flat-football-scene (place-image (bitmap "football.png") 100 100
                                                  empty-canvas)))
    (check-equal? (send non-flat-football add-to-scene empty-canvas)
                  non-flat-football-scene
                  "There should be a football painted on the canvas")
    (check-equal? (send flat-football add-to-scene empty-canvas)
                  empty-canvas
                  "The scene should still be empty")))

;; -----------------------------------------------
;; Throbber tests

;; mouse-event tests
(begin-for-test
  (local
    ((define t1 (make-throbber 100 100)))
    (check-false (send t1 for-test:selected?)
                 "Throbber should be unselected initially")
    (send t1 after-button-down 100 100)
    (check-true (send t1 for-test:selected?)
                "Throbber should be selected after a button-down inside it")
    (send t1 after-drag 60 50)
    (check-equal? (send t1 toy-x)
                  60
                  "Throbber x coordinate should have changed after drag")
    (check-equal? (send t1 toy-y)
                  50
                  "Throbber y coordinate should have changed after drag")
    (send t1 after-button-up 60 50)
    (check-false (send t1 for-test:selected?)
                 "Throbber should not be selected after a button up event")
    (send t1 after-button-down 300 300)
    (check-false (send t1 for-test:selected?)
                 "Throbber should not be selected after button down outside of it")
    (send t1 after-drag 250 250)
    (check-equal? (send t1 toy-x)
                  60
                  "Throbber x coordinate should not have changed after unselected drag")
    (check-equal? (send t1 toy-y)
                  50
                  "Throbber y coordinate should not have changed after unselected drag")
    (send t1 after-button-up 250 250)
    (check-false (send t1 for-test:selected?)
                 "Throbber should not be selected after a button up event")))


;; key-event tests
(begin-for-test
  (local
    ((define t1 (make-throbber 100 100))
     (define t2 (send t1 after-key-event "k")))
    (check-equal? (send t1 for-test:selected?) (send t2 for-test:selected?)
                  "Throbber selected? should not be affected by key events")
    (check-equal? (send t1 toy-x) (send t2 toy-x)
                  "Throbber x coordinate should not be affected by key events")
    (check-equal? (send t1 toy-y) (send t2 toy-y)
                  "Throbber y coordinate should not be affected by key events")
    (check-equal? (send t1 toy-data) (send t2 toy-data)
                  "Throbber size should not be affected by key events")))


;; after-tick tests
(begin-for-test
  (local
    ((define min-size-throbber-after-tick
       (make-throbber 100 100))
     (define max-size-throbber-after-tick
       (new Throbber% [x 100][y 100][radius 19][radius-change 1])))
    (send max-size-throbber-after-tick after-tick)
    (check-equal? (send min-size-throbber-after-tick for-test:radius-change)
                  1
                  "Throbber should be initially expanding")
    (send min-size-throbber-after-tick after-tick)
    (check-equal? (send min-size-throbber-after-tick for-test:radius-change)
                  1
                  "Throbber should still be expanding after 1 tick")
    (check-equal? (send min-size-throbber-after-tick toy-data)
                  6
                  "Radius should have increased by 1 after 1 tick")
    (check-equal? (send max-size-throbber-after-tick toy-data)
                  20
                  "Radius of throbber should be 20 at maximum")
    (check-equal? (send max-size-throbber-after-tick for-test:radius-change)
                  -1
                  "Throbber should start contracting once it reaches its maximum size")))

;; add-to-scene tests
(begin-for-test
  (local
    ((define t1 (new Throbber% [x 100][y 100][radius 5]))
     (define t2 (new Throbber% [x 100][y 100][radius 20]))
     (define empty-canvas (empty-scene 500 500))
     (define t1-scene (place-image (circle 5 "solid" "green") 100 100 empty-canvas))
     (define t2-scene (place-image (circle 20 "solid" "green") 100 100 empty-canvas)))
    (check-equal? (send t1 add-to-scene empty-canvas)
                  t1-scene
                  "Scene should have a green circle of radius 5 on it")
    (check-equal? (send t2 add-to-scene empty-canvas)
                  t2-scene
                  "Scene should have a green circle of radius 20 on it")))

;; -----------------------------------------------
;; Square tests

(begin-for-test
  (local
    ((define t1 (make-square-toy 100 100 10)))
    (check-false (send t1 for-test:selected?)
                 "Square Toy should be unselected initially")
    (send t1 after-button-down 100 100)
    (check-true (send t1 for-test:selected?)
                "Square Toy should be selected after a button-down inside it")
    (send t1 after-drag 60 50)
    (check-equal? (send t1 toy-x)
                  60
                  "Square Toy x coordinate should have changed after drag")
    (check-equal? (send t1 toy-y)
                  50
                  "Square Toy y coordinate should have changed after drag")
    (send t1 after-button-up 60 50)
    (check-false (send t1 for-test:selected?)
                 "Square Toy should not be selected after a button up event")
    (send t1 after-button-down 300 300)
    (check-false (send t1 for-test:selected?)
                 "Square Toy should not be selected after button down outside of it")
    (send t1 after-drag 250 250)
    (check-equal? (send t1 toy-x)
                  60
                  "Square Toy x coordinate should not have changed after unselected drag")
    (check-equal? (send t1 toy-y)
                  50
                  "Square Toy y coordinate should not have changed after unselected drag")
    (send t1 after-button-up 250 250)
    (check-false (send t1 for-test:selected?)
                 "Square Toy should not be selected after a button up event")))


;; key-event tests
(begin-for-test
  (local
    ((define t1 (make-square-toy 100 100 10))
     (define t2 (make-square-toy 100 100 10)))
    (send t2 after-key-event "k")
    (check-equal? (send t1 for-test:selected?) (send t2 for-test:selected?)
                  "Square Toy selected? should not be affected by key events")
    (check-equal? (send t1 toy-x) (send t2 toy-x)
                  "Square Toy x coordinate should not be affected by key events")
    (check-equal? (send t1 toy-y) (send t2 toy-y)
                  "Square Toy y coordinate should not be affected by key events")
    (check-equal? (send t1 toy-data) (send t2 toy-data)
                  "Square Toy speed should not be affected by key events")))


;; after-tick tests
(begin-for-test
  (local
    ((define square-toy (make-square-toy 100 100 10))
     (define square-toy-01 (new Square% [x 100][y 100][selected? true][velocity 10]))
     (define square-toy-no-move (new Square% [x 100][y 100][selected? true][velocity 10]))
     (define square-after-tick (make-square-toy 110 100 10))
     (define square-toy-02 (make-square-toy 480 100 10))
     (define square-toy-03 (make-square-toy 480 100 10))
     (define square-toy-04 (new Square% [x 20][y 100][selected? false][velocity -10]))
     (define square-toy-05 (make-square-toy 20 100 -10))
     (define square-toy-before-tick (make-square-toy 100 100 10)))
    (send square-toy-before-tick after-tick)
    (check-equal? (send square-toy-before-tick toy-x)
                  (send square-after-tick toy-x)
                  "Square Toy should move by 10 ")
    (send square-toy-01 after-tick)
    (check-equal? (send square-toy-01 toy-x)
                  (send square-toy-no-move toy-x)
                  "Square Toy should not move by 10 ")
    (send square-toy-02 after-tick)
    (check-equal? (send square-toy-02 toy-x)
                  (send square-toy-03 toy-x)
                  "Square Toy should not move by 10 ")
    (send square-toy-04 after-tick)
    (check-equal? (send square-toy-04 toy-x)
                  (send square-toy-05 toy-x)
                  "Square Toy should not move by 10 ")))

;; add-to-scene tests
(begin-for-test
  (local
    ((define s1 (new Square% [x 100][y 100][velocity 10]))
     (define s2 (new Square% [x 100][y 100][velocity 10]))
     (define empty-canvas (empty-scene 500 500))
     (define s1-scene (place-image (square 40 "outline" "red") 100 100 empty-canvas)))
    (check-equal? (send s1 add-to-scene empty-canvas) s1-scene
                  "Scene should have a red square of on it")))

;; -----------------------------------------------
;; Clock tests

(begin-for-test
  (local
    ((define t1 (make-clock 100 100)))
    (check-false (send t1 for-test:selected?)
                 "Clock Toy should be unselected initially")
    (send t1 after-button-down 100 100)
    (check-true (send t1 for-test:selected?)
                "Clock Toy should be selected after a button-down inside it")
    (send t1 after-drag 60 50)
    (check-equal? (send t1 toy-x)
                  60
                  "Clock Toy x coordinate should have changed after drag")
    (check-equal? (send t1 toy-y)
                  50
                  "Clock Toy y coordinate should have changed after drag")
    (send t1 after-button-up 60 50)
    (check-false (send t1 for-test:selected?)
                 "Clock Toy should not be selected after a button up event")
    (send t1 after-button-down 300 300)
    (check-false (send t1 for-test:selected?)
                 "Clock Toy should not be selected after button down outside of it")
    (send t1 after-drag 250 250)
    (check-equal? (send t1 toy-x)
                  60
                  "Clock Toy x coordinate should not have changed after unselected drag")
    (check-equal? (send t1 toy-y)
                  50
                  "Clock Toy y coordinate should not have changed after unselected drag")
    (send t1 after-button-up 250 250)
    (check-false (send t1 for-test:selected?)
                 "Clock Toy should not be selected after a button up event")))


;; key-event tests
(begin-for-test
  (local
    ((define t1 (make-clock 100 100))
     (define t2 (make-clock 100 100)))
    (send t2 after-key-event "k")
    (check-equal? (send t1 for-test:selected?) (send t2 for-test:selected?)
                  "Clock Toy selected? should not be affected by key events")
    (check-equal? (send t1 toy-x) (send t2 toy-x)
                  "Clock Toy x coordinate should not be affected by key events")
    (check-equal? (send t1 toy-y) (send t2 toy-y)
                  "Clock Toy y coordinate should not be affected by key events")
    (check-equal? (send t1 toy-data) (send t2 toy-data)
                  "Clock Toy speed should not be affected by key events")))


;; after-tick tests
(begin-for-test
  (local
    ((define clock-toy (make-clock 100 100))
     (define clock-toy-01 (new Clock% [x 100][y 100][selected? true][tick-rate 2]))
     (define clock-after-tick (make-clock 100 100))
     (define clock-toy-02 (new Clock% [x 100][y 100][selected? true][tick-rate 2]))
     (define clock-toy-03 (new Clock% [x 100][y 100][selected? true][tick-rate 2]))
     (define clock-toy-before-tick (make-clock 100 100)))
    (send clock-toy-before-tick after-tick)
    (check-equal? (send clock-toy-before-tick toy-x)
                  (send clock-after-tick toy-x)
                  "Clock Toy should increase tick count")
    (send clock-toy-02 after-tick)
    (check-equal? (send clock-toy-02 toy-x)
                  (send clock-toy-03 toy-x)
                  "Clock Toy should not increase tick count")))

;; add-to-scene tests
(begin-for-test
  (local
    ((define t1 (new Clock% [x 100][y 100][tick-rate 1]))
     (define t2 (new Clock% [x 100][y 100][tick-rate 1]))
     (define empty-canvas (empty-scene 500 500))
     (define t1-scene (place-image (text (number->string 1) 20 "black")
                                   100 100 empty-canvas)))
    (check-equal? (send t1 add-to-scene empty-canvas) t1-scene
                  "Scene should have a clock on it")))