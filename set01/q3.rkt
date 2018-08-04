;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(require 2htdp/image)

(provide image-area)

;; Exercise 18

;; image-area : Image -> Number
;; GIVEN: image img to calculate
;; WHERE: image is not null
;; RETURNS: returns number of pixels from given image
;; EXAMPLE\TEST:
(begin-for-test
  (check-equal? (image-area (rectangle 10 25 "solid" "olive")) 250
                "Area of 10x20 image is 250"))

;; STRATEGY: Combine simpler functions
(define (image-area img)
  (* (image-width img) (image-height img)))