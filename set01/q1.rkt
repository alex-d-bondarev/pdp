;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide distance-to-origin)

;; Exercise 13

;; distance-to-origin : Number Number -> PosReal
;; RETURNS: computed distance from given point (x,y) to the origin
;; EXAMPLES\TESTS:
(begin-for-test
  (check-equal? (distance-to-origin 3 4) 5
                "Distance of point (x, x) from the origin (0,0)")
  (check-equal? (distance-to-origin 0 -125) 125
                "Distance will always be positive")
  (check-equal? (distance-to-origin 0 0) 0
                "Point (0,0) is the same as origin, so there will be 0 distance"))

;; STRATEGY: Combine simpler functions
(define (distance-to-origin x y)
  (sqrt (+ (* x x) (* y y))))


