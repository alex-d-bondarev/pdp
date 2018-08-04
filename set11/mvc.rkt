#lang racket

(require "Model.rkt")
(require "ParticleWorld.rkt")
(require "ControllerFactory.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Constants
;; -----------------------------------------------

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

(define HALF_CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF_CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; mvc.rkt
;; ----------------------------------------------- 
;; This program simulates a particle
;; bouncing in a 150x100 rectangle, with its controllers.

;; Use (run 0.5) to start the simulation at a rate
;; of 0.5 seconds per tick where square toys moved at

;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation with the given frame rate.
(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      (send w add-widget
        (new ControllerFactory% [m m][w w]
             [controller-x HALF_CANVAS-WIDTH]
             [controller-y HALF_CANVAS-HEIGHT]))
      (send w run rate))))


;; test-cases
;; impossible to test - calls big-bang