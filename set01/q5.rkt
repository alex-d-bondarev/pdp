;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide string-delete)

;; Exercise 22

;; string-delete : String Number -> String
;; GIVEN: some string str and a position number i
;; WHERE: number i >= 0 and number i < length of the string str
;; RETURNS: given string str, except that ith char is deleted
;; EXAMPLES\TESTS:
(begin-for-test
  (check-equal? (string-delete "rage" 0) "age"
                "Position 0 is for the first letter. Rage should be updated to age")
  (check-equal? (string-delete "Some TText" 5) "Some Text"
                "Letter #5 is T. So result will have only one T")
  (check-equal? (string-delete "" 0) ""
                "Function with empty string input should return empty result"))

;; STRATEGY: Combine simpler functions
(define (string-delete str i)
  (cond [(equal? str "") ""]
        [else (string-append (left-part str i) (right-part str i))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; left-part : String Number -> String
;; RETURNS: part of the given string 
;; to the left from letter specified by given number 
;; EXAMPLES: 
;; (left-part ("abcZ") 4) => "abc"

;; right-part : String Number -> String
;; RETURNS: part of the given string to the right from letter specified by given number
; excluding specified letter
;; EXAMPLES: 
;; (right-part ("abcZ") 4) => "Z"

(define (left-part str i) (substring str 0 i))
(define (right-part str i) (substring str (+ 1 i) (string-length str)))