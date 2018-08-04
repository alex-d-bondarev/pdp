;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide string-first)

;; Exercise 15

;; string-first : String -> 1String
;; GIVEN: string str
;; WHERE: str is not empty
;; RETURNS: first 1String from the given string
;; EXAMPLE\TEST:
(begin-for-test
  (check-equal? (string-first "qwerty") "q"
                "1String of qwerty is q"))

;; STRATEGY: Combine simpler functions
(define (string-first str)
  (substring str 0 1))
