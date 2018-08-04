
#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")

(provide ParentController%)


(define COLOR-RED "red")
(define COLOR-BLACK "black")
(define OUTLINE "outline")
(define SQUARE-SIDE 10)
(define ZERO 0)


(define ParentController%
  (class* object% (Controller<%>)

    (init-field model)  ; the model

    ;; the position of the center of the controller
    (init-field [c-x 150] [c-y 100])


    (init-field [width 150][height 100])

    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])

    ;; the position of the particle
    (field [particle-x 0])
    (field [particle-y 0])
    (field [particle-vx 0])
    (field [particle-vy 0])


    ;; fields for dragging
    (field [view-selected? false])
    (field [controller-selected? false])
    (field [dist-x 0])
    (field [dist-y 0])
    ;(field [dist-controller-x 0])
    ;(field [dist-controller-y 0])

    (define WIDTH-BETWEEN-RECT 30)
    (super-new)

    (send model register this)
    
    ;; receive-signal -> Void
    ;; GIVEN: a signal
    ;; EFFECT: based on signal updates the x y vx vy 
    ;; STRATEGY: Cases on typ of signal
    (define/public (receive-signal sig)
      (cond
        [(report-x-position? sig)
           (set! particle-x (report-x-position-pos sig))]
        [(report-y-position? sig)
           (set! particle-y (report-y-position-pos sig))]
        [(report-x-velocity? sig)
           (set! particle-vx (report-x-velocity-v sig))]
        [(report-y-velocity? sig)
         (set! particle-vy (report-y-velocity-v sig))]))
  
   
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: the mouse location of a button-down 
    ;; EFFECT: chnages particle as per the button down event
    ;; STRATEGY: Cases on whether button down is in or out
    (define/public (after-button-down mx my)
      (cond
        [(in-handle? mx my)
        (perform-in-handle mx my)]
        [(in-this? mx my)
          (perform-in-controller mx my)]))
    
    ;; perform-in-handle : Integer Integer -> Void
    ;; GIVEN : the mouse coordinates
    ;; EFFECT : updates the mouse coordiantes
    ;; STRATEGY: COmbining simpler functions
    (define/public (perform-in-handle mx my)
      (begin
          (set! dist-x (- mx c-x))
          (set! dist-y (- my c-y))
          (set! view-selected? true)))

    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: the (x,y) location of a button-up event
    ;; EFFECT: makes this unselected
    (define/public (after-button-up mx my)
      (begin
        (set! view-selected? false)
        (set! controller-selected? false)
        (send model execute-command
             (make-set-selected controller-selected?))))

    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: the location of a drag event
    ;; STRATEGY: Cases on whether this is selected.
    ;; If it is selected, move it so that the vector from its position to
    ;; the drag event is equal to dist-x.  Report the new position to
    ;; the registered balls.
    (define/public (after-drag mx my)
      (if view-selected?
        (begin
          (set! c-x (- mx dist-x))
          (set! c-y (- my dist-y)))
        4356))
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN : A scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/public (add-to-scene scene)
      (local ((define viewer (viewer-image)))
        (place-image
         (overlay
          (overlay/xy
           (square SQUARE-SIDE OUTLINE (current-color)) ZERO ZERO
           (rectangle width height OUTLINE COLOR-BLACK))
          viewer) c-x c-y scene)))

    ;; get-width-of-controller : NonNegInt -> Void
    ;; GIVEN : the width of the controller image without includng the handler
    ;; EFFECT : the width of the controller image includng the handler
    (define/public (get-width-of-controller wController)
      (set! width (+ wController WIDTH-BETWEEN-RECT)))

    ;; get-height-of-controller : NonNegInt -> Void
    ;; GIVEN  : the width of the controller image without includng the handler
    ;; EFFECT : the width of the controller image includng the handler
    (define/public (get-height-of-controller hController)
      (set! height (+ hController WIDTH-BETWEEN-RECT)))

    ;; after-tick :-> Void  
    ;; GIVEN : No arguments
    ;; EFFECT: no effect
    (define/public (after-tick) 'viewer1-after-tick-trap) 

    ;; current-color: -> String
    ;; GIVEN: No arguments
    ;; RETURNS: color chnage based on whether it is selected or not.
    ;; STRATEGY: Combining simpler functions
    (define/public (current-color)
      (if view-selected? COLOR-RED COLOR-BLACK))

    ;; definition in the class inheriting the function
     (abstract change-controller-view)

    ;; definition in the class inheriting the function
    (abstract perform-in-controller)

    ;; definition in the class inheriting the function
    (abstract viewer-image)

    ;; definition in the class inheriting the function
    (abstract data-image)

    ;; definition in the class inheriting the function
    (abstract in-this?)

    ;; definition in the class inheriting the function
    (abstract in-handle?)

    ;; definition in the class inheriting the function 
    (abstract after-key-event)))

;; testcases:
;; impossible to test abstract class