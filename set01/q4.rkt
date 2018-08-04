;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide string-insert)

;; Exercise 21

;; string-insert : String Number -> String
;; GIVEN: some string str and a position number i
;; WHERE: number i >= 0 and number i < length of the string str
;; RETURNS: given string str, except that "_" SYMBOL is added
; after letter specified by given number i
;; EXAMPLES\TESTS:
(begin-for-test
  (check-equal? (string-insert "helloworld" 5) "hello_world"
         "_ symbol should be added to helloworld after 5th letter to make hello_world")
  (check-equal? (string-insert "" 0) "_"
         "_ is added to all strings, even empty ones")
  (check-equal? (string-insert "A" 0) "_A"
         "Position number 0 should add _ before text, so A will be _A"))

;; STRATEGY: Combine simpler functions
(define (string-insert str i)
  (string-append (left-part str i) "_" (right-part str i)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; left-part : String Number -> String
;; RETURNS: part of the given string 
;; to the left from letter specified by given number 
;; EXAMPLES: 
;; (left-part ("abcZ") 4) => "abc"

;; right-part : String Number -> String
;; RETURNS: part of the given string to the right from letter specified by given number
; including specified letter
;; EXAMPLES: 
;; (right-part ("abcZ") 4) => "Z"

(define (left-part str i) (substring str 0 i))
(define (right-part str i) (substring str i (string-length str)))