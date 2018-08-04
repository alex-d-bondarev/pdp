#lang racket

(require "Interfaces.rkt")

(provide Model%)

;; -----------------------------------------------
;; Constants

(define ZERO 0)
(define DOUBLE 2)
(define RECTANGLE_WIDTH 150)
(define RECTANGLE_HEIGHT 100)
(define RECTANGLE_COLOR "blue")
(define PAUSE "pause when dragging particle")

;; -----------------------------------------------
;; A Model is a (new Model%)
;; Model is used to calculate particle changes after tick
;; and after receiving signals from controllers
;; It also sends updated particle back to all controllers
(define Model%
  (class* object% (Model<%>)
    
    ;; ------------------
    ;; Fields
    ;; ------------------
    
    ;; pauses this after-tick when particle is selected in one of the
    ;; xController, yController or xyController controllers
    (init-field [pause? false])
    ;; ListOfController<%> that are shown in UI
    (init-field [controllers empty])
    ;; rect represents rectangle, where particle is moving
    (init-field [rect (make-rect RECTANGLE_WIDTH
                                 RECTANGLE_HEIGHT
                                 RECTANGLE_COLOR)])
    ;; represents a particle from problem set
    (init-field [particle (make-particle (/ RECTANGLE_WIDTH DOUBLE)
                                         (/ RECTANGLE_HEIGHT DOUBLE)
                                         ZERO ZERO)])
    
    (super-new)
    
    
    ;; ------------------
    ;; Methods
    ;; ------------------
    
    ;; pause-signal : Boolean -> Void
    ;; GIVEN: a boolean signal
    ;; EFFECT: pauses model after-tick function
    ;;         if given signal = true
    ;;         else unpauses model after-tick function
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Combine simpler functions  
    (define/public (pause-signal sig)
      (set! pause? sig))
    
    
    ;; after-tick : -> Void
    ;; EFFECT: Updates particle coordinates if not paused
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Combine simpler functions    
    (define/public (after-tick)
      (if pause? PAUSE (update-particle)))
    
    
    ;; update-particle : -> Void
    ;; EFFECT: Updates particle coordinates according to velocity
    ;;         if the resulting coordinates are out of rect
    ;;         appropriate velocity is changed to opposite
    ;;         and coordinates are sticked to rect borders
    ;;         resulted particle is sent to all controllers
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; NOTE: not using particle-after-tick because of Piazza - "Perfect Bounce error" @771
    ;;       (set! particle (particle-after-tick particle rect)))
    ;; STRATEGY: Combine simpler functions  
    (define (update-particle)
      (begin
        (define x  (x-after-tick))
        (define y  (y-after-tick))
        (define vx (vx-after-tick x))
        (define vy (vy-after-tick y))
        (set! particle (make-particle x y vx vy))
        (publish-updated-particle)))
    
    
    ;; x-after-tick : -> NonNegInt
    ;; y-after-tick : -> NonNegInt
    ;; RETURNS: x or y coordinate of particle after tick
    ;;          if the resulting coordinate is out of rect
    ;;          coordinate is sticked to rect border
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Combine simpler functions  
    (define (x-after-tick)
      (max ZERO
           (min (+ (particle-x particle) (particle-vx particle))
                RECTANGLE_WIDTH)))
    
    (define (y-after-tick)
      (max ZERO
           (min (+ (particle-y particle) (particle-vy particle))
                RECTANGLE_HEIGHT)))
    
    
    ;; vx-after-tick : NonNegInt -> Int
    ;; vy-after-tick : NonNegInt -> Int
    ;; GIVEN: x or y coordinate
    ;; RETURNS: vx or vy velocity of particle after tick
    ;;          that is opposite in case of particle is
    ;;          sticked to rect border, else same as before tick
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Combine simpler functions  
    (define (vx-after-tick x)
      (if (or (= x ZERO) (= x RECTANGLE_WIDTH))
          (- (particle-vx particle))
          (particle-vx particle)))
    
    (define (vy-after-tick y)
      (if (or (= y ZERO) (= y RECTANGLE_HEIGHT))
          (- (particle-vy particle))
          (particle-vy particle)))
    
    
    ;; publish-updated-particle : -> Void
    ;; EFFECT: sends updated particle data to all controllers
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Combine simpler functions  
    (define (publish-updated-particle)
      (publish-y-velocity (particle-vy particle))
      (publish-x-velocity (particle-vx particle))
      (publish-y-location (particle-y particle))
      (publish-x-location (particle-x particle))
      (publish-particle))

    
    ;; publish-particle : -> Void
    ;; publish-y-velocity : -> Void
    ;; publish-x-velocity : -> Void
    ;; publish-y-location : -> Void
    ;; publish-x-location : -> Void
    ;; EFFECT: sends particle or appropriate particle parameter to all controllers
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Use HOF for-each on controllers
    (define (publish-particle)
      (for-each
       (lambda (obs) (send obs receive-signal particle))
       controllers))
    
    (define (publish-y-velocity vy)
      (begin (define msg (make-report-y-velocity vy))
             (for-each
              (lambda (obs) (send obs receive-signal msg))
              controllers)))
    
    (define (publish-x-velocity vx)
      (begin (define msg (make-report-x-velocity vx))
             (for-each
              (lambda (obs) (send obs receive-signal msg))
              controllers)))
    
    (define (publish-y-location y)
      (begin (define msg (make-report-y-position y))
             (for-each
              (lambda (obs) (send obs receive-signal msg))
              controllers)))
    
    (define (publish-x-location x)
      (begin (define msg (make-report-x-position x))
             (for-each
              (lambda (obs) (send obs receive-signal msg))
              controllers)))
    
    
    ;; register : Controller -> Void
    ;; GIVEN: Controller%
    ;; EFFECT: adds given controller to controllers (ListOfController<%> )
    ;;         and sends it particle and rectangle data
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Use simpler functions
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal particle)
        (send c receive-signal rect)))
    
    
    ;; execute-command : Command -> Void
    ;; GIVEN: a command to execute
    ;; EFFECT: decodes given command and updates particle accordingly
    ;;         sends updated particle to all controllers.
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Use simpler functions    
    (define/public (execute-command cmd)
      (begin       
        (define x  (x-after-command cmd))
        (define y  (y-after-command cmd))
        (define vx (vx-after-command cmd))
        (define vy (vy-after-command cmd))
        (set! particle (make-particle x y vx vy))
        (publish-updated-particle)))
    
    
    ;; x-after-command : Command -> NonNegInt
    ;; y-after-command : Command -> NonNegInt
    ;; vx-after-command : Command -> Int
    ;; vy-after-command : Command -> Int
    ;; GIVEN: a command to decode
    ;; RETURNS: decoded command
    ;; TESTS/EXAMPLES: not applicable for super class @ 781
    ;; STRATEGY: Devide into cases on cmd
    (define (x-after-command cmd)
      (if (set-x-position? cmd)
          (set-x-position-pos cmd)
          (particle-x particle)))
    
    (define (y-after-command cmd)
      (if (set-y-position? cmd)
          (set-y-position-pos cmd)
          (particle-y particle)))
    
    (define (vx-after-command cmd)
      (if (incr-x-velocity? cmd)
          (+ (particle-vx particle) (incr-x-velocity-dv cmd))
          (particle-vx particle)))
    
    (define (vy-after-command cmd)
      (if (incr-y-velocity? cmd)
          (+ (particle-vy particle) (incr-y-velocity-dv cmd))
          (particle-vy particle)))
    
    ))

;; testcases:
;; impossible to test because of call cycles