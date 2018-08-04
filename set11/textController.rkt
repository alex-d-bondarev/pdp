#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "ParentController.rkt")

(provide textController%)

(define COLOR-RED "red")
(define COLOR-BLACK "black")

(define textController%
  (class* ParentController% (Controller<%>)

    
    (inherit-field c-x c-y dist-x dist-y)
    (inherit-field view-selected? controller-selected? width height)
    
    ;(inherit-field dist-controller-x)
    ;(inherit-field dist-controller-y)

    (inherit-field model)

    (inherit-field particle-x)
    (inherit-field particle-y)
    (inherit-field particle-vx)
    (inherit-field particle-vy)
    
    (init-field [img-w 0] [img-h 0] [half-img-w 0] [half-img-h 0])
    
    (super-new)
    
    (send model register this)    

    (define/override (after-key-event kev) "do nothing")


    ;; change-controller-view: -> String
    ;; GIVEN: No arguments
    ;; RETURNS: the colour of the controller based on whether it is selected or
    ;;          not.
    ;; STRATEGY: Cases on if controller is selected
     (define/override (change-controller-view)
      (if controller-selected?
          COLOR-RED
          COLOR-BLACK))

    
    (define/override (data-image) "this")

    ;; data-image-text -> String
    ;; GIVEN: No arguments
    ;; EFFECT: Displays the string on the screen
    ;; STRATEGY: Combine simpler function
    (define/public (data-image-text content)
      (above
       (text content 10 (send this change-controller-view))
       (text (string-append "X = " (number->string particle-x)
              " Y = " (number->string particle-y)) 11 (send this change-controller-view))
       (text (string-append " VX = " (number->string particle-vx)
              " VY = " (number->string particle-vy)) 11 (send this change-controller-view))))

    ))

;; testcases:
;; impossible to test because not all abstract methods are overriden here
;; for example in-handle?,