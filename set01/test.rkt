;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(require "q1.rkt")
(require "q2.rkt")
(require "q3.rkt")
(require "q4.rkt")
(require "q5.rkt")



  (test-equal?
   "a rectangle image with width 3 length 5 should have area 15 "
   (image-area (rectangle 3 5 "solid" "yellow"))
   15)

  (test-equal?
   "the image of circle with radios 2 should have area 16"
   (image-area (circle 2 "solid" "yellow"))
   16)

  (test-equal?
   "insert to the front of a non-empty string"
   (string-insert "Racket" 0)
   "_Racket")

  (test-equal?
   "insert to the middle of a non-empty string"
   (string-insert "Racket" 2)
   "Ra_cket")

  (test-equal?
   "insert to the last of a non-empty string"
   (string-insert "Racket" 6)
   "Racket_")

  (test-equal? "insert to an empty string" (string-insert "" 0) "_")

  (test-equal?
   "delete the first position from string"
   (string-delete "Racket" 0)
   "acket")

  (test-equal?
   "delete the middle position from string"
   (string-delete "Racket" 2)
   "Raket")

  (test-equal?
   "delete the last position from string"
   (string-delete "Racket" 5)
   "Racke")
