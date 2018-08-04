;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This is a program that consumes a list of numbers representing daily
;; rainfall amounts . The list may contain the number -999 indicating
;; the end of the data of interest. The programs produces the average of
;; the non-negative values in the list up to the first -999 (if it shows up).
;; There may be negative numbers other than -999 in the list.

(require rackunit)
(require "extras.rkt")
(check-location "07" "rainfall.rkt")

(provide rainfall)

;; By Frederick and Oleksandr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define END_REPORT_MARKER -999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Data definitions

;;;;;;;;;;;;;;;;;;;;

;; A RainfallReport is a ListOfInt
;; WHERE: -999, if present, represents the end of the list

;;;;;;;;;;;;;;;;;;;;

;; A ListOfInt (LOI) is either
;; -- empty
;; -- (cons Int LOI)

;; TEMPLATE:
;; loi-fn : LOI -> ??
;; (define (loi-fn loi)
;;   (cond
;;     [(empty? loi) ...]
;;     [else (...
;;             (int-fn (first loi))
;;             (loi-fn (rest loi)))]))

;;;;;;;;;;;;;;;;;;;;

;; END Data definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Implementation / Tests data

;;;;;;;;;;;;;;;;;;;;

;; For RainfallReport
(define RAINFALL_TEST_REPORT (list -100 20 -5 30 -998 25 END_REPORT_MARKER 125))
(define RAINFALL_TEST_REPORT_2 (list -100 20 -5 30 -998 25 -1000 125))
(define RAINFALL_REPORT_WITHOUT_NEGATIVES (list 20 30 25 125))

;; For results
(define RAINFALL_EXPECTED_RESULT 25)
(define RAINFALL_EXPECTED_RESULT_2 50)

;;;;;;;;;;;;;;;;;;;;

;; END Implementation / Tests data 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN functions required by problem set

;;;;;;;;;;;;;;;;;;;;

;; rainfall : RainfallReport -> NonNegReal
;; GIVEN: a RainfallReport as report
;; RETURNS: the average of the positive numbers from the given report
;; WHERE: all positive numbers are counted or, if -999 number is present, only positive
;;        numbers that come before -999 number are counted.
;; STRATEGY: combine simpler functions
(define (rainfall report)
  (average
   (filter-negative-nums
    (get-list-of-interest report))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (rainfall RAINFALL_TEST_REPORT)
   RAINFALL_EXPECTED_RESULT
   "Test result should equal 25. It ignores all negative numbers and all numbers after -999")
  (check-equal?
   (rainfall RAINFALL_TEST_REPORT_2)
   RAINFALL_EXPECTED_RESULT_2
   "All positive numbers should be calculated, because -999 is missing now."))

;;;;;;;;;;;;;;;;;;;;

;; get-list-of-interest : RainfallReport -> ListOfInt
;; GIVEN: a RainfallReport
;; WHERE: a -999 number marks the end of the report
;; RETURNS: a ListOfInt, that is similar to given report, except that, if -999 number
;  is present, ListOfInt will vontain only the numbers that are to the left of -999.
;; STRATEGY: Use template of ListOfInt on report
(define (get-list-of-interest report)
  (cond
    [(empty? report) empty]
    [(equal? (first report) END_REPORT_MARKER) empty]
    [else (cons
           (first report)
           (get-list-of-interest (rest report)))]))

;; EXAMPLE:
;; (get-list-of-interest (list 1 2 3 -999 4 5)) => (list 1 2 3)

;; TESTS: tested in rainfall function

;;;;;;;;;;;;;;;;;;;;

;; average : ListOfInt -> Real
;; GIVEN: a ListOfInt lst
;; RETURNS: the average of all numbers from the given list
;; STRATEGY: combine simpler functions
(define (average lst)
  (/ (sum lst)
     (length lst)))

;; EXAMPLE:
;; (average (list 1 4 10)) => 5

;; TESTS: tested in rainfall function

;;;;;;;;;;;;;;;;;;;;

;; sum : (Int Int -> Int) ListOfInt -> Int
;; GIVEN: a ListOfInt lst
;; RETURNS:: the sum of all numbers from the given list
;; STRATEGY: use HOF foldr on lst
(define (sum lst)
  (foldr
   ;; Int Int
   (lambda (r accum-sum) (+ r accum-sum))
   0
   lst))

;; EXAMPLE:
;; (average (list 1 4 10)) => 15

;; TESTS: tested in rainfall function

;;;;;;;;;;;;;;;;;;;;

;; filter-negative-nums : (Int -> Boolean) ListOfInt -> ListOfInt
;; GIVEN: a ListOfInt as loi
;; RETURNS: a ListOfInt like the given one, except, that the negative numbers are filtered out
;; STRATEGY: use HOF filter on loi
(define (filter-negative-nums loi)
  (filter
   ;; Int -> Boolean
   (lambda (num) (> num 0))
   loi))

;; TEST/EXAMPLE
(begin-for-test
  (check-equal?
   (filter-negative-nums RAINFALL_TEST_REPORT)
   RAINFALL_REPORT_WITHOUT_NEGATIVES
   "Given function should just remove all negative numbers, even -999"))