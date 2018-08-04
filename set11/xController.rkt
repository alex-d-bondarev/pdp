#lang racket


(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "xyController.rkt")
(require "Model.rkt")

(check-location "11" "xController.rkt")

(provide xController%)

;; -----------------------------------------------
;; Constants

(define SHRINKED_MARGIN -1)
(define ZERO_HALF_MARGIN 0)
(define SQUARE_HEIGHT 50)
(define SQUARE_Y_CENTER (/ SQUARE_HEIGHT 2))

;; -----------------------------------------------
;; An xController is a (new xController% [model Model%])
;; xController is used for representing a controller with draggable circle on a canvas,
;; which represents a particle. Only x dimension is changable here.
(define xController%
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
    ;; square mouse x coordinate
    (inherit-field smx)
    ;; square distance from mouse x coordinate
    (inherit-field s-dist-x)
    ;; margin y fields are used for drawing controller
    (inherit-field margin-y half-margin-y)

    
    (super-new)

    
    ;; override margin values from xyController in order to make controller look less tall
    (set! margin-y SHRINKED_MARGIN)
    (set! half-margin-y ZERO_HALF_MARGIN)
    

    ;; add this to model
    (send model register this)

    ;; ------------------
    ;; Methods
    ;; ------------------

    ;; drag-particle : NonNegInt NonNegInt -> Void
    ;; GIVEN: the x and y coordinates of a mouse
    ;; EFFECT: Updates particle's x coordinate
    ;; EXAMPLES/TESTS: below
    ;; STRATEGY: Combine simpler functions
    (define/override (drag-particle mx my)
      (begin
        (send this get-mouse-on-square mx my)
        (send model execute-command (make-set-x-position (+ smx s-dist-x)))))


    ;; data-image : -> Image
    ;; RETURNS: blue square with particle in it's coordinates
    ;; EXAMPLES/TESTS: below
    ;; STRATEGY: Combine simpler functions
    (define/override (data-image)
      (color-frame (rect-color rect)
                   (place-image (send this particle-image)
                                (particle-x particle)
                                SQUARE_Y_CENTER
                                (empty-scene (rect-width rect) SQUARE_HEIGHT))))
    
    
))

;; ------------------
;; TESTS
;; ------------------

(begin-for-test
  (local
    ((define xc (new xController%
                     [model (new Model%)])))
    (send xc drag-particle 115 80)
    (check-equal? (send xc data-image)
                  (color-frame "blue"
                               (place-image
                                (send xc particle-image)
                                25
                                25
                                (empty-scene 150 50)))
                  "Scene should be shown with red circle")))