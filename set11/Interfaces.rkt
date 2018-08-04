#lang racket

(provide 
 (struct-out set-x-position)
 (struct-out set-y-position)
 (struct-out incr-x-velocity)
 (struct-out incr-y-velocity)
 (struct-out report-x-position)
 (struct-out report-y-position)
 (struct-out report-x-velocity)
 (struct-out report-y-velocity)
 (struct-out set-selected)
 (struct-out particle)
 (struct-out rect)
 SWidget<%>
 Controller<%>
 Model<%>)


;; -----------------------------------------------
;; DATA DEFINITIONS
;; -----------------------------------------------


;; A ListOfController<%> (LOC) is one of
;; -- empty
;; (cons Controller<%> LOC)


;; -----------------------------------------------
(define-struct particle (x y vx vy) #:transparent)
;; (make-particle NonNegInt NonNegInt Int Int)
;; represents a Particle from task, where
;; x and x   - are coordinates of a center of a particle
;; vx and vx - represent velocity in x and y ways

;; TEMPLATE:
;; node-fn : Node -> ??
#; (define (particle-fn p)
     (...(particle-x p)
         (particle-y p)
         (particle-vx p)
         (particle-vy p)))


;; -----------------------------------------------
(define-struct rect (width height color) #:transparent)
;; (make-rect NonNegInt NonNegInt Color)
;; represents a rectangle that shows particle, where
;; width height - represent the size of rectangle
;; color - represents rectsngle's color

;; TEMPLATE:
;; node-fn : Node -> ??
#; (define (rect-fn r)
     (...(rect-width r)
         (rect-height r)
         (rect-color r)))


;; -----------------------------------------------
;; A Command is one of 
;; -- (set-selected sel)
;; -- (set-x-position x)
;; -- (set-y-position y
;; -- (incr-x-velocity vx)
;; -- (incr-y-velocity vy)


;; -----------------------------------------------
(define-struct set-selected (sel) #:transparent)
;; (set-selected Boolean)
;; represents a command that updates controller-selected? status of the controller, where
;; sel - represents whether it is selected (true) or not (false)

;; TEMPLATE:
;; set-selected-fn : Boolean -> ??
#; (define (set-selected-fn cmd)
     (...(set-selected-sel cmd)))


;; -----------------------------------------------
(define-struct set-x-position (pos) #:transparent)
;; (make-set-x-position NonNegInt)
;; represents a command that updates x position of the particle, where
;; pos - represents x position

;; TEMPLATE:
;; set-x-position-fn : Command -> ??
#; (define (set-x-position-fn cmd)
     (...(set-x-position-pos cmd)))


;; -----------------------------------------------
(define-struct set-y-position (pos) #:transparent)
;; (make-set-y-position NonNegInt)
;; represents a command that updates y position of the particle, where
;; pos - represents y position

;; TEMPLATE:
;; set-y-position-fn : Command -> ??
#; (define (set-y-position-fn cmd)
     (...(set-y-position-pos cmd)))


;; -----------------------------------------------
(define-struct incr-x-velocity (dv) #:transparent)
;; (make-incr-x-velocity Int)
;; represents a command that updates x velocity of the particle, where
;; dv - represents x velocity

;; TEMPLATE:
;; incr-x-velocity-fn : Command -> ??
#; (define (incr-x-velocity-fn cmd)
     (...(incr-x-velocity-dv cmd)))


;; -----------------------------------------------
(define-struct incr-y-velocity (dv) #:transparent)
;; (make-incr-y-velocity Int)
;; represents a command that updates y velocity of the particle, where
;; dv - represents y velocity

;; TEMPLATE:
;; incr-y-velocity-fn : Command -> ??
#; (define (incr-y-velocity-fn cmd)
     (...(incr-y-velocity-dv cmd)))


;; -----------------------------------------------
;; A Signal is one of
;; -- (report-x-position x)
;; -- (report-y-position y)
;; -- (report-x-velocity vx)
;; -- (report-y-velocity vy)


;; -----------------------------------------------
(define-struct report-x-position (pos) #:transparent)
;; (make-report-x-position NonNegInt)
;; represents a signal that updates x position of the particle, where
;; pos - represents x position

;; TEMPLATE:
;; report-x-position-fn : Signal -> ??
#; (define (report-x-position-fn sig)
     (...(report-x-position-pos sig)))


;; -----------------------------------------------
(define-struct report-y-position (pos) #:transparent)
;; (make-report-y-position NonNegInt)
;; represents a signal that updates y position of the particle, where
;; pos - represents y position

;; TEMPLATE:
;; report-y-position-fn : Signal -> ??
#; (define (report-y-position-fn sig)
     (...(report-y-position-pos sig)))


;; -----------------------------------------------
(define-struct report-x-velocity (v) #:transparent)
;; (make-report-x-velocity Int)
;; represents a signal that updates x velocity of the particle, where
;; dv - represents x velocity

;; TEMPLATE:
;; report-x-velocity-fn : Signal -> ??
#; (define (report-x-velocity-fn sig)
     (...(report-x-velocity-dv sig)))


;; -----------------------------------------------
(define-struct report-y-velocity (v) #:transparent)
;; (make-report-y-velocity Int)
;; represents a signal that updates y velocity of the particle, where
;; dv - represents y velocity

;; TEMPLATE:
;; report-y-velocity-fn : Signal -> ??
#; (define (report-y-velocity-fn sig)
     (...(report-y-velocity-dv sig)))



;; -----------------------------------------------
;; Interfaces
;; -----------------------------------------------


;; represents widget (controller) that will be added to UI
(define SWidget<%>
  (interface ()
    
    ;; Scene -> Scene
    add-to-scene
    
    ;; NonNegInt NonNegInt -> Void
    after-button-up
    
    ;; NonNegInt NonNegInt -> Void
    after-button-down

    ; NonNegInt NonNegInt -> Void
    after-drag

    ; KeyEvent -> Void
    after-key-event        
))


;; -----------------------------------------------
;; represents controller that will change particle
(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model in order to update this controller
    receive-signal

    ;; NonNegInt NonNegInt -> Void
    ;; Update cotroller when handle is selected
    perform-in-handle

    ;; -> Color
    ;; Returns handle color
    current-color
))


;; -----------------------------------------------
;; represents model that will calculate particle changes
(define Model<%>
  (interface ()

    ;; Boolean -> Void
    ;; Pauses particle calculations, when particle is selected
    pause-signal
    
    ;; -> Void
    ;; updates particle after tick, if not paused
    after-tick        

    ;; Controller<%> -> Void
    ;; Adds the given controller to controllers
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command
))

;; test-cases
;; nothing to test in this file