;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(check-location "02" "probe.rkt")

(provide
  probe-at
  probe-turned-left
  probe-turned-right
  probe-forward
  probe-north?
  probe-south?
  probe-east?
  probe-west?
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define PROBE_DIAMETER 40)
(define SQUARE_SIDE 347)
(define POSITIVE_EDGE (- (/ (- SQUARE_SIDE 1) 2) (/ PROBE_DIAMETER 2)))
(define NEGATIVE_EDGE (* POSITIVE_EDGE -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Direction is one of
;-- "north"
;-- "south"
;-- "east"
;-- "west"
;; INTERPRETATION: Direction is representing appropriate cardinal directions 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct probe (x y direction))

;; A Probe is a (make-probe Integer Integer String)

;; INTERPRETATION:

;; (make-probe x y dir) represents a Probe
;; with graphics-style x-coordinate x
;; with graphics-style y-coordinate y
;; and with direction that probe is facing

;; TEMPLATE:
;; probe-fn : Probe -> ??
#; (define (probe-fn pr)
     (... (probe-x pr)
          (probe-y pr)
          (probe-direction pr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-at : Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; WHERE: these coordinates leave the robot entirely inside the trap
;; RETURNS: a probe with its center at those coordinates, facing north.
;; EXAMPLE: a set of coordinates that put the probe in contact with the
;  wall is not consistent with the contract.  Note that this means that
;  the behavior of probe-at in this situation is unspecified; you don't
;  need to check for this.

;; TEST\EXAMPLE:
(begin-for-test
  (check-equal? (probe-at 20 30) (make-probe 20 30 "north")
   "Probe lands in right botton part of trap, with x-coordinate = 20, y-coordinate=30 and facing north"))

;; STRATEGY: Combine simpler functions
(define (probe-at x y)
  (make-probe x y "north"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-turned-left : Probe -> Probe
;; probe-turned-right : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees either left
;; or right.

;; TEST\EXAMPLE:
(begin-for-test
  (check-equal? (probe-turned-left (probe-at 20 30)) (make-probe 20 30 "west")
   "Probe will change it's diresction from 'north' to 'west")
  (check-equal? (probe-turned-left (make-probe 20 30 "east")) (make-probe 20 30 "north")
   "Probe will change it's diresction from east to north")
  (check-equal? (probe-turned-left (make-probe 20 30 "south")) (make-probe 20 30 "east")
   "Probe will change it's diresction from south to east")
  (check-equal? (probe-turned-left (make-probe 20 30 "west")) (make-probe 20 30 "south")
   "Probe will change it's diresction from west to south"))

;; STRATEGY: Dividing into cases on (probe-direction pr)
(define (probe-turned-left pr)
  (cond [(string=? (probe-direction pr) "north") (set-probe-direction pr "west")]
        [(string=? (probe-direction pr) "east") (set-probe-direction pr "north")]
        [(string=? (probe-direction pr) "south") (set-probe-direction pr "east")]
        [(string=? (probe-direction pr) "west") (set-probe-direction pr "south")]))

;;;;;;;;;;;;;;;;;

;; TEST\EXAMPLE:
(begin-for-test
  (check-equal? (probe-turned-right (probe-at 20 30)) (make-probe 20 30 "east")
   "Probe will change it's diresction from north to east")
  (check-equal? (probe-turned-right (make-probe 20 30 "east")) (make-probe 20 30 "south")
   "Probe will change it's diresction from east to south")
  (check-equal? (probe-turned-right (make-probe 20 30 "south")) (make-probe 20 30 "west")
   "Probe will change it's diresction from south to west")
  (check-equal? (probe-turned-right (make-probe 20 30 "west")) (make-probe 20 30 "north")
   "Probe will change it's diresction from west to north"))

;; STRATEGY: Combine simpler functions
(define (probe-turned-right pr)
  (cond [(probe-north? pr) (set-probe-direction pr "east")]
        [(probe-east? pr) (set-probe-direction pr "south")]
        [(probe-south? pr) (set-probe-direction pr "west")]
        [(probe-west? pr) (set-probe-direction pr "north")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-forward : Probe PosInt -> Probe
;; GIVEN: a probe pr and a distance dis
;; RETURNS: a probe like the given one, but moved forward by the
;  specified distance.  If moving forward the specified distance would
;  cause the probe to hit any wall of the trap, then the probe should 
;  move as far as it can inside the trap, and then stop.

;; TESTS\EXAMPLES:
(begin-for-test
  (check-equal? (probe-forward (probe-at 20 30) 153)
                (make-probe 20 -123 "north")
                "Probe will stop 30 cm before northern wall")
  (check-equal? (probe-forward (make-probe 20 30 "south") 93)
                (make-probe 20 123 "south")
                "Probe will stop 30 cm before southern wall")
  (check-equal? (probe-forward (make-probe 20 30 "east") 153)
                (make-probe -133 30 "east")
                "Probe will stop 20 cm before eatern wall")
  (check-equal? (probe-forward (make-probe 20 30 "west") 93)
                (make-probe 113 30 "west")
                "Probe will stop 40 cm before western wall"))

;; STRATEGY: Combine simpler functions
(define (probe-forward pr dis)
  (cond [(probe-north? pr) (move-north pr dis)]
        [(probe-south? pr) (move-south pr dis)]
        [(probe-east? pr) (move-east pr dis)]
        [(probe-west? pr) (move-west pr dis)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-north? : Probe -> Boolean
;; probe-south? : Probe -> Boolean
;; probe-east? : Probe -> Boolean
;; probe-west? : Probe -> Boolean
;; GIVEN: a probe pr
;; ANSWERS: whether the probe is facing in the specified direction.

;; EXAMPLES/TESTS :
(begin-for-test
  ; test north
  (check-equal? (probe-north? (make-probe 20 30 "south")) false
                "Actual direction is south")
  (check-equal? (probe-north? (make-probe 20 30 "north")) true
                "Actual direction is north")
  ; test south
  (check-equal? (probe-south? (make-probe 20 30 "south")) true
                "Actual direction is south")
  (check-equal? (probe-south? (make-probe 20 30 "north")) false
                "Actual direction is north")
  ; test east
  (check-equal? (probe-east? (make-probe 20 30 "east")) true
                "Actual direction is east")
  (check-equal? (probe-east? (make-probe 20 30 "west")) false
                "Actual direction is west")
  ; test west
  (check-equal? (probe-west? (make-probe 20 30 "east")) false
                "Actual direction is east")
  (check-equal? (probe-west? (make-probe 20 30 "west")) true
                "Actual direction is west"))

;; STRATEGY: Combine simpler functions
(define (probe-north? pr)
  (string=? (probe-direction pr) "north"))
;; STRATEGY: Combine simpler functions
(define (probe-south? pr)
  (string=? (probe-direction pr) "south"))
;; STRATEGY: Combine simpler functions
(define (probe-east? pr)
  (string=? (probe-direction pr) "east"))
;; STRATEGY: Combine simpler functions
(define (probe-west? pr)
  (string=? (probe-direction pr) "west"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-probe-direction : Probe Direction -> Probe
;; GIVEN: a probe pr and a direction dir
;; RETURNS: a given probe, exept that direction is changed to a given one

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (set-probe-direction (probe-at 20 30) "south") (make-probe 20 30 "south")
   "Direction is changed to 'south'"))

;; STRATEGY: Use template for Probe on pr
(define (set-probe-direction pr dir)
  (make-probe (probe-x pr)
              (probe-y pr)
              dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-north : Probe PosInt -> Probe
;; move-south : Probe PosInt -> Probe
;; move-east : Probe PosInt -> Probe
;; move-west : Probe PosInt -> Probe
;; GIVEN: a probe pr
;; ANSWERS: move probe to the specified direction

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (move-north (probe-at 20 30) 30) (make-probe 20 0 "north")
   "Changed y-coordinate to 0")
  (check-equal?
   (move-south (probe-at 20 30) 30) (make-probe 20 60 "north")
   "Changed y-coordinate to 60")
  (check-equal?
   (move-east (probe-at 20 30) 30) (make-probe -10 30 "north")
   "Changed x-coordinate to -10")
  (check-equal?
   (move-west (probe-at 20 30) 30) (make-probe 50 30 "north")
   "Changed x-coordinate to 50"))

;; STRATEGY: Use template for Probe on pr
(define (move-north pr dis)
  (make-probe (probe-x pr)
              (move NEGATIVE_EDGE (probe-y pr) dis)
              (probe-direction pr)))

;; STRATEGY: Use template for Probe on pr
(define (move-south pr dis)
  (make-probe (probe-x pr)
              (move POSITIVE_EDGE (probe-y pr) dis)
              (probe-direction pr)))

;; STRATEGY: Use template for Probe on pr
(define (move-east pr dis)
  (make-probe (move NEGATIVE_EDGE (probe-x pr) dis)
              (probe-y pr)
              (probe-direction pr)))

;; STRATEGY: Use template for Probe on pr
(define (move-west pr dis)
  (make-probe (move POSITIVE_EDGE (probe-x pr) dis)
              (probe-y pr)
              (probe-direction pr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move : Integer Integer Integer -> Integer
;; GIVEN: an edge ed to move to, a coordinate c that is to be changed and a distance dis
;; RETURNS: a changed coordinate after move. If given distance is bigger than
;  distance to edge, probe coordinate will be placed near edge (wall). Otherwise
;  it will be moved on given distance.

;; TEST\EXAMPLE:
(begin-for-test
  ; tests for hitting the wall
  (check-equal?
   (move POSITIVE_EDGE  100 400) POSITIVE_EDGE
   "Probe will hit the wall at 173,5")
  (check-equal?
   (move NEGATIVE_EDGE  100 400) NEGATIVE_EDGE
   "Probe will hit the wall at 173,5")
  ; tests for moving without hitting the wall
  (check-equal?
   (move POSITIVE_EDGE  100 50) 150
   "Probe will move till coordinate=150")
  (check-equal?
   (move NEGATIVE_EDGE  100 150) -50
   "Probe will move till coordinate=50"))

;; STRATEGY: Combine Simpler functions
(define (move ed c dis)
  (cond 
    [(< ed 0) (if (positive-distance-enough? ed c dis)
                  ed
                  (- c dis))]
    [(> ed 0) (if (negative-distance-enough? ed c dis)
                  ed
                  (+ c dis))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positive-distance-enough? : Integer Integer Integer -> Boolean
;; negative-distance-enough? : Integer Integer Integer -> Boolean
;; GIVEN: an edge ed to move to, a coordinate c that is to be changed and a distance dis
;; ANSWERS: true if probe will hit the wall, else - false

;; EXAMPLES:
;  (define enough-distance-to-north (positive-distance-enough? NEGATIVE_EDGE 100 80) = true
;  (define not-enough-distance-to-south (positive-distance-enough? POSITIVE_EDGE 100 80) = false

;; STRATEGY: Combine Simpler functions
(define (positive-distance-enough? ed c dis)
  (< (+ (* -1 ed) c) dis))

;; STRATEGY: Combine Simpler functions
(define (negative-distance-enough? ed c dis)
  (< (+ ed c) dis))