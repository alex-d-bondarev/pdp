#lang racket

(require rackunit)
(require 2htdp/image)
(require "Model.rkt")
(require "extras.rkt")
(require "Interfaces.rkt")
(require "ParentController.rkt")

(provide xyController%)

;; -----------------------------------------------
;; Constants

(define ZERO 0)
(define DOUBLE 2)
(define CIRCLE_RADIUS 10)
(define HANDLE_SIZE 10)
(define CONTROLLER_COLOR "black")
(define CONTROLLER_STYLE "outline")
(define PARTICLE_COLOR "red")
(define PARTICLE_STYLE "solid")
(define PARTICLE_SHIFT -8)
(define DO_NOTHING "do nothing")
(define MARGIN_SIZE 30)

;; -----------------------------------------------
;; An xyController is a (new xyController% [model Model%])
;; xController is used for representing a controller with draggable circle on a canvas,
;; which represents a particle. Can be dragged in both x and y dimensions
(define xyController%
  (class* ParentController%
    (Controller<%>)
    
    ;; ------------------
    ;; Fields
    ;; ------------------

    ;; The Model% from Model.rkt
    (inherit-field model)
    ;; Controller x and y coordinates
    (inherit-field c-x c-y)
    ;; Controller mouse distance
    (inherit-field dist-x dist-y)
    ;; Controller size
    (inherit-field width height half-width half-height)
    ;; true if mouse is in handle, else false. Used for controller dragging
    (inherit-field view-selected?)
    ;; true if mouse is in blue square, else false. Used for particle dragging
    (inherit-field controller-selected?)
    ;; particle from problem set
    (field [particle (make-particle ZERO ZERO ZERO ZERO)])
    ;; rectangle that contains particle
    (field [rect (make-rect ZERO ZERO CONTROLLER_COLOR)])
    ;; square mouse x and y coordinate
    (field [smx ZERO] [smy ZERO])
    ;; square distance from mouse x and y coordinates
    (init-field [s-dist-x ZERO] [s-dist-y ZERO])
    ;; margin x and y fields are used for drawing controller
    (init-field [margin-x MARGIN_SIZE] [margin-y MARGIN_SIZE])
    (init-field [half-margin-x (/ margin-x DOUBLE)] [half-margin-y (/ margin-y DOUBLE)])


    (super-new)

    
    (send model register this)

    ;; ------------------
    ;; Methods
    ;; ------------------
    
    ;; receive-signal : Signal -> Void
    ;; GIVEN: signal from Model%
    ;; EFFECT: decodes signal and updates particle or rect
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Devide into cases on sig
    (define/override (receive-signal sig)
      (cond
        [(particle? sig) (set! particle sig)]
        [(rect? sig) (set! rect sig)]
        [else DO_NOTHING]))


    ;; in-handle? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: x and y coordinates
    ;; RETURNS: true if given coordinates are in tiggle of current controller
    ;;          else - false
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions    
    (define/override (in-handle? mx my)
      (and
       (<= (- c-x half-width)  mx (+ (- c-x half-width) HANDLE_SIZE))
       (<= (- c-y half-height) my (+ (- c-y half-height) HANDLE_SIZE))))


    ;; perform-in-handle : NonNegInt NonNegInt -> Void
    ;; GIVEN: x and y coordinates
    ;; EFFECT: updates distance from given coordinates to controller, 
    ;;         sets controller draggable
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions
    (define/override (perform-in-handle mx my)
      (begin
          (set! dist-x (- mx c-x))
          (set! dist-y (- my c-y))
          (set! view-selected? true)))


    ;; after-button-up : NonNegInt NonNegInt -> Void
    ;; GIVEN: x and y coordinates
    ;; EFFECT: makes this controller unselected
    ;;         unpauses Model%
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions    
    (define/override (after-button-up mx my)
      (begin
        (set! view-selected? false)
        (set! controller-selected? false)
        (send model pause-signal false)))
    

    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT: If it is selected in handle, move it so that the vector from its 
    ;;         position to the drag event is equal to dist-x.
    ;;         If it selected in blue square, update particle position
    ;;         Else nothing happens
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions + Cases on whether this controller is selected.
    (define/override (after-drag mx my)
      (cond
        [view-selected? (drag-controller mx my)]
        [controller-selected? (drag-particle mx my)]
        [else DO_NOTHING]))
    

    ;; drag-controller : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT: Updates controller position, based on given location
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define (drag-controller mx my)
      (begin
        (set! c-x (- mx dist-x))
        (set! c-y (- my dist-y))))
    

    ;; drag-particle : NonNegInt NonNegInt -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT: Updates particle position, based on given location
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define/public (drag-particle mx my)
      (begin
        (get-mouse-on-square mx my)
        (send model execute-command (make-set-x-position (+ smx s-dist-x)))
        (send model execute-command (make-set-y-position (+ smy s-dist-y)))))
    

    ;; in-this? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: x and y coordinates
    ;; EFFECT: Updates internal square mouse coordinates
    ;; RETURNS: true if given coordinates are in blue square, else false
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions (adapter pattern)
    (define/override (in-this? mx my)
      (begin
        (get-mouse-on-square mx my)
        (in-square? mx my)))
    

    ;; in-this? : NonNegInt NonNegInt -> Void
    ;; GIVEN: x and y coordinates
    ;; EFFECT: Updates internal square mouse coordinates
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions
    (define/public (get-mouse-on-square mx my)
      (begin
        (set! smx (- mx (+ (- c-x half-width) half-margin-x)))
        (set! smy (- my (+ (- c-y half-height) half-margin-y)))))
    

    ;; in-square? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: x and y coordinates
    ;; RETURNS: true if given coordinates are in blue square, else false
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define (in-square? mx my)
      (and (<= (+ (- c-x half-width) half-margin-x)
               mx
               (- (+ c-x half-width) half-margin-x))
           (<= (+ (- c-y half-height) half-margin-y)
               my
               (- (+ c-y half-height) half-margin-y))))
    

    ;; perform-in-controller : NonNegInt NonNegInt -> Void
    ;; GIVEN: x and y coordinates
    ;; EFFECT: marks controller as selected
    ;;         updates internal sqaure mouse distance based on given coordinates
    ;;         pauses model
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define/override (perform-in-controller mx my)
      (begin
        (set! controller-selected? true)
        (get-particle-distance mx my)
        (send model pause-signal true)))
    

    ;; perform-in-controller : NonNegInt NonNegInt -> Void
    ;; GIVEN: x and y coordinates
    ;; EFFECT: updates internal sqaure mouse distance based on given coordinates
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define (get-particle-distance mx my)
      (begin
        (set! s-dist-x (- (particle-x particle) smx))
        (set! s-dist-y (- (particle-y particle) smy))))
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN a scene
    ;; EFFECT: updates image size information
    ;; RETURNS: a scene like the given one, but with this controller painted on it.
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define/override (add-to-scene scene)
      (begin 
        (define c-image (viewer-image))
        (set! width (image-width c-image))
        (set! height (image-height c-image))
        (set! half-width  (/ width  DOUBLE))
        (set! half-height (/ height DOUBLE))
        (place-image c-image c-x c-y scene)))
    
    
    ;; viewer-image : -> Image
    ;; RETURNS: a controller image with handle, blue square and a particle on it
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define/override (viewer-image)
      (begin 
        (define d-image (data-image))
        (overlay d-image
                 (handle-to-scene
                  (controller-to-scene d-image)))))
    
    
    ;; handle-to-scene : Scene -> Scene
    ;; GIVEN a scene
    ;; RETURNS: a scene, similar to given one, except that handle is drawn on it
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define (handle-to-scene scene)
      (overlay/xy
       (square HANDLE_SIZE CONTROLLER_STYLE
               (send this current-color))
       ZERO ZERO scene))
    
    
    ;; controller-to-scene : Scene -> Scene
    ;; RETURNS: an empty controller image with size adjusted to given scene
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define (controller-to-scene scene)
      (rectangle
       (+ (image-width scene) margin-x)
       (+ (image-height scene) margin-y)
       CONTROLLER_STYLE CONTROLLER_COLOR))
    
    
    ;; data-image : -> Image
    ;; RETURNS: a blue square with particle on it
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define/override (data-image)
      (color-frame (rect-color rect)
                   (place-image (particle-image)
                                (particle-x particle)
                                (particle-y particle)
                                (empty-scene (rect-width rect) (rect-height rect)))))
    
    
    ;; particle-image : -> Image
    ;; RETURNS: an image with black particle and red circle surrounding it
    ;; EXAMPLES/TESTS: in file bottom
    ;; STRATEGY: Combine simpler functions 
    (define/public (particle-image)
      (overlay/xy (circle DOUBLE PARTICLE_STYLE CONTROLLER_COLOR)
                  PARTICLE_SHIFT PARTICLE_SHIFT
                  (circle CIRCLE_RADIUS PARTICLE_STYLE PARTICLE_COLOR)))
    

    ;; after-key-event : -> Void
    ;; EFFECT: none - stub for ParentController 
    (define/override (after-key-event kev) DO_NOTHING)
    

    ;; change-controller-view : -> Void
    ;; EFFECT: none - stub for ParentController 
    (define/override (change-controller-view) DO_NOTHING)


))

;; ------------------
;; TESTS
;; ------------------

(begin-for-test
  (local
    ((define xyc (new xyController%
                     [model (new Model%)]))
     (define xyz (new xyController%
                     [model (new Model%)])))
    ;; stub non-used methods
    (send xyc after-key-event 1)
    (send xyc change-controller-view)
    ;; end stub
    (send xyc after-button-down 150 100)
    (send xyc after-drag 175 125)
    (send xyc after-button-up 175 125)
    
    (check-equal? (send xyz data-image)
                  (color-frame "blue"
                               (place-image
                                (send xyc particle-image)
                                75
                                50
                                (empty-scene 150 100)))
                  "Scene should be shown with red circle")
    (check-equal? (send xyc in-handle? 75 50)
                  true
                  "Controller is selected in 75 50 coordinates")
    (send xyc after-button-down 75 50)
    (send xyc after-drag 100 120)
    (send xyc after-button-up 100 120)
    (check-equal? (send xyc in-handle? 75 90)
                  false
                  "Controller is not selected in 75 50 coordinates anymore")
    (send xyc after-button-down 300 400)
    (send xyc after-drag 350 450)
    (send xyc after-button-up 350 450)
    (send xyc add-to-scene (empty-scene 600 500))
    (check-equal? (send xyc in-handle? 75 90)
                  false
                  "Controller is still not selected in 75 50 coordinates")))
