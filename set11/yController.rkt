#lang racket


(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "xyController.rkt")
(require "Model.rkt")

(check-location "11" "yController.rkt")

(provide yController%)

;; -----------------------------------------------
;; Constants

(define SHRINKED_MARGIN -1)
(define ZERO_HALF_MARGIN 0)
(define SQUARE_WIDTH 50)
(define SQUARE_X_CENTER (/ SQUARE_WIDTH 2))

;; -----------------------------------------------
;; An yController is a (new yController% [model Model%])
;; yController is used for representing a controller with draggable circle on a canvas,
;; which represents a particle. Only y dimension is changable here.
(define yController%
  (class* xyController%
    (Controller<%>)
    
    ;; ------------------
    ;; Fields
    ;; ------------------

    ;; The Model% from Model.rkt
    (inherit-field model)
    ;; particle from problem set
    (inherit-field particle)
    ;; rectangle that contains particle
    (inherit-field rect)
    ;; square mouse y coordinate
    (inherit-field smy)
    ;; square distance from mouse y coordinate
    (inherit-field s-dist-y)
    ;; margin x fields are used for drawing controller
    (inherit-field margin-x half-margin-x)

    
    (super-new)

    
    ;; override margin values from xyController in order to make controller look less tall
    (set! margin-x SHRINKED_MARGIN)
    (set! half-margin-x ZERO_HALF_MARGIN)
    

    ;; add this to model
    (send model register this)

    ;; ------------------
    ;; Methods
    ;; ------------------

    ;; drag-particle : NonNegInt NonNegInt -> Void
    ;; GIVEN: the x and y coordinates of a mouse
    ;; EFFECT: Updates particle's y coordinate
    ;; EXAMPLES/TESTS: below
    ;; STRATEGY: Combine simpler functions
    (define/override (drag-particle mx my)
      (begin
        (send this get-mouse-on-square mx my)
        (send model execute-command (make-set-y-position (+ smy s-dist-y)))))


    ;; data-image : -> Image
    ;; RETURNS: blue square with particle in it's coordinates
    ;; EXAMPLES/TESTS: below
    ;; STRATEGY: Combine simpler functions
    (define/override (data-image)
      (color-frame (rect-color rect)
                   (place-image (send this particle-image)
                                SQUARE_X_CENTER
                                (particle-y particle)
                                (empty-scene SQUARE_WIDTH (rect-height rect)))))

))

;; ------------------
;; TESTS
;; ------------------

(begin-for-test
  (local
    ((define yc (new yController%
                     [model (new Model%)])))
    (send yc drag-particle 80 90)
    (check-equal? (send yc data-image)
                  (color-frame "blue"
                               (place-image
                                (send yc particle-image)
                                25
                                25
                                (empty-scene 50 100)))
                  "Scene should be shown with red circle")))