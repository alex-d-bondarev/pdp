
;; PositionController.rkt:
;; We use the arrow keys to move the particle in the x or y direction

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "ParentController.rkt")
(require "textController.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "extras.rkt")

(provide PositionController%)


(define RIGHT "right")
(define LEFT "left")
(define UP "up")
(define DOWN "down")
;; A PositionController% is a
;; (new PositionController% [text-image-width PosInt][text-image-height PosInt])


(define PositionController%
  (class* textController% (Controller<%>)

    ;; The height and width of the image of the controller
    (init-field [text-image-width 0])
    (init-field [text-image-height 0])

    ;; the center coordinates of the controller
    (inherit-field c-x c-y)
    
    ;; model
    (inherit-field model)  

     ;; the width and height of the controller
    (inherit-field width height)
   
     ;; the half width and height of the controller
    (inherit-field half-width )
    (inherit-field half-height)
   
    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)
    (inherit-field particle-vx)
    (inherit-field particle-vy)
        
    ;; fields required for dragging 
    (inherit-field view-selected?)
    (inherit-field controller-selected?)
    (inherit-field dist-x)
    (inherit-field dist-y)
    
    (define WIDTH-BETWEEN-RECT 30)
    (define ALTER-POS 5)
    (super-new)
    
    (send model register this) 
    
    ;; in-handle?: Integer Integer -> Boolean
    ;; GIVEN: The mouse click position coordinates 
    ;; RETURNS: True iff the mouse click is inside the handler
    ;; STRATEGY : Combining simpler functions
     (define/override (in-handle? other-x other-y)
      (and
       (<= (- c-x (+ (/ text-image-width 2) (/ WIDTH-BETWEEN-RECT 2))) other-x
           (+ (- c-x (+ (/ text-image-width 2) (/ WIDTH-BETWEEN-RECT 2))) 10) )
       (<= (- c-y (+ (/ text-image-height 2) (/ WIDTH-BETWEEN-RECT 2))) other-y
           (+ (- c-y (+ (/ text-image-height 2) (/ WIDTH-BETWEEN-RECT 2))) 10) )))

    
    ;; in-this?: Integer Integer -> Boolean
    ;; GIVEN: The mouse click coordinates 
    ;; RETURNS: True iff the mouse click is inside this controller
    ;; STRATEGY : Combining simpler functions
    (define/override (in-this? other-x other-y)
      (and
       (<= (- c-x (+ (/ text-image-width 2) (/ WIDTH-BETWEEN-RECT 2))) other-x
           (+ c-x (+ (/ text-image-width 2) (/ WIDTH-BETWEEN-RECT 2))))
       (<= (- c-y (+ (/ text-image-height 2) (/ WIDTH-BETWEEN-RECT 2))) other-y
           (+ c-y (+ (/ text-image-height 2) (/ WIDTH-BETWEEN-RECT 2))))))
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: keyevent 'kev'
    ;; EFFECT: Change in the position of the particle in x and y direction
    ;; STRATEGY: Cases on key event kev
    (define/override (after-key-event kev)
      (if controller-selected? 
          (cond
            [(key=? RIGHT kev) (send-command-to-model (+ particle-x ALTER-POS) make-set-x-position)]
            [(key=? LEFT kev) (send-command-to-model (- particle-x ALTER-POS) make-set-x-position)]
            [(key=? UP kev) (send-command-to-model (- particle-y ALTER-POS) make-set-y-position)]
            [(key=? DOWN kev) (send-command-to-model (+ particle-y ALTER-POS) make-set-y-position)])
          3459))
    
    ;; send-command-to-model:  Integer Integer -> Void
    ;; GIVEN: The quantity of change in the position in x and y direction
    ;; EFFECT: Updates the model with the change in the position
    ;; STRATEGY: Combining simpler functions
    (define (send-command-to-model inc func)
      (send model execute-command (func inc)))
    
    ;; perform-in-controller : Integer Integer -> Void
    ;; GIVEN : The mouse click location 
    ;; EFFECT: Updates the controller-selected to true
    ;; STRATEGY : Combing simpler functions
    (define/override (perform-in-controller mx my)
       (set! controller-selected? true))

   
    ;; viewer-image:  -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: Assembles the image of the viewer
    ;; STRATEGY : Combing simpler functions
    (define/override (viewer-image)
      (let ((the-data-image (send this data-image-text "Arrow keys change position")))
        (begin
          (set! text-image-width (image-width the-data-image))
          (set! text-image-height (image-height the-data-image))
          (send this get-width-of-controller (image-width the-data-image))
          (send this get-height-of-controller (image-height the-data-image))
          the-data-image)))
    
     (define/public (get-particle-x) particle-x)))

;; Test-Cases:

(define P-CONTROL (new PositionController% [model (new Model%)] ))
(define DATA-IMAGE-HEIGHT (image-height (send P-CONTROL data-image-text "Arrow keys change position")))
(define DATA-IMAGE-WIDTH (image-width (send P-CONTROL data-image-text "Arrow keys change position")))
(define VIEWR-IMAGE (send P-CONTROL viewer-image))
(define MOUSE-X DATA-IMAGE-WIDTH)
(define MOUSE-Y DATA-IMAGE-HEIGHT)

(begin-for-test

  (check-equal? (begin
                  (send P-CONTROL after-button-down 990 890)
                  (send P-CONTROL after-key-event "up")
                  (send P-CONTROL get-particle-x)) 0 ) 
  (check-equal? (begin
                  (send P-CONTROL after-button-down 90 90)
                  (send P-CONTROL after-key-event "up")
                  (send P-CONTROL after-key-event "down")
                  (send P-CONTROL after-key-event "left")
                  (send P-CONTROL after-key-event "right")
                  (send P-CONTROL get-particle-x)) 75 ))

(begin-for-test
  (check-equal? (send P-CONTROL in-this?
                      (+ DATA-IMAGE-WIDTH 5) DATA-IMAGE-HEIGHT) false)
  (check-equal? (send P-CONTROL in-handle?
                      (+ DATA-IMAGE-WIDTH 5) DATA-IMAGE-HEIGHT) false)
  (check-equal? (send P-CONTROL in-handle?
                      74 78) true))




