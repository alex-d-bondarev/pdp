#lang racket

(require "Interfaces.rkt")
(require "Model.rkt")
(require "ParticleWorld.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "xController.rkt")
(require "yController.rkt")
(require "xyController.rkt")

;; -----------------------------------------------
;; Constants

(define DO_NOTHING "do nothing")

(require 2htdp/universe)

(provide ControllerFactory%)

(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the world in which the controllers will live
    (init-field w)   ; World<%>

    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>

    (init-field controller-x)
    (init-field controller-y)

    (super-new)

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN : keyevent kev
    ;; EFFECT : New controller added to the world
    ;; STRATEGY : cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-controller VelocityController%)]
        [(key=? kev "p") (add-controller PositionController%)]
        [(key=? kev "x") (add-controller xController%)]
        [(key=? kev "y") (add-controller yController%)]
        [(key=? kev "z") (add-controller xyController%)]
        ))

    ;; add-controller : Controller<%> -> Void
    ;; GIVEN : A controller
    ;; EFFECT : adds a new controller on the world
    ;; STRATEGY : Combining simple functions
    (define/public (add-controller controller-class)
      (send w add-widget (new controller-class [model m]
                              [c-x controller-x]
                              [c-y controller-y])))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: the same scene
    ;; EFFECT: Stub for Controller intrface
    (define/public (add-to-scene s) s)

    ;; after-tick : -> Void
    ;; after-button-down : NonNegInt NonNegInt -> Void
    ;; after-tick : NonNegInt NonNegInt -> Void
    ;; after-tick : NonNegInt NonNegInt -> Void
    ;; EFFECT: none, stubs for Controller intrface
    (define/public (after-tick) DO_NOTHING)
    (define/public (after-button-down mx my) DO_NOTHING)
    (define/public (after-drag mx my) DO_NOTHING)
    (define/public (after-button-up mx my) DO_NOTHING)))


;; testcases:
;; impossible to test because of ParticleWorld.rkt
;; ParticleWorld class lacks methods that can be used for testing
;; for example: get-widgets to check that widgets were added
