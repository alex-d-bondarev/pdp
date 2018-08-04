;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; class-lists
;; Provide functions for processing lists

(require rackunit)
(require "extras.rkt")

(check-location "05" "class-lists.rkt")

(provide
 felleisen-roster
 shivers-roster
 make-slip
 slip-color
 slip-name1
 slip-name2
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Color is one of
;; -- "yellow"
;; -- "blue"
;; INTERPRETATION: self-evident
;; EXAMPLES/CONSTANTS for tests:
(define YELLOW "yellow")
(define BLUE "blue")

;; TEMPLATE:
;; clr-fn : Color -> ??
#; (define (clr-fn c)
     (cond
       [(colors-equal? YELLOW c)...]
       [(colors-equal? BLUE c)...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct slip (color name1 name2))

;; A Slip is a (make-slip Color String String)
;; color is a Color of a sleep
;; name1 and name2 are First and Last names in any order

;; TEMPLATE:
;; slip-fn : Slip -> ??
#; (define (slip-fn slip)
     (...(slip-color slip)
         (slip-name1 slip)
         (slip-name2 slip)))

;; EXAMPLES/CONSTANTS for tests:
(define ROBERT_DOWNEY_JR (make-slip "yellow" "Robert" "Downey JR"))
(define ROBERT_DOWNEY_JR_2 (make-slip "blue" "Robert" "Downey JR"))
(define ROBERT_DOWNEY_JR_REVERSE (make-slip "yellow" "Downey JR" "Robert"))
(define DANIEL_CRAIG (make-slip "blue" "Daniel" "Craig"))
(define DANIEL_CRAIG_2 (make-slip "yellow" "Daniel" "Craig"))
(define CRIS_PRATT (make-slip "blue" "Cris" "Pratt"))
(define SCARLETT_JOHANSSON (make-slip "yellow" "Scarlett" "Johansson"))
(define ANGELINA_JOLIE (make-slip "blue" "Angelina" "Jolie"))
(define ANGELINA_JOLIE_REVERSE (make-slip "blue" "Jolie" "Angelina"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfSlip (LOS) is either:
;; -- empty
;; -- (cons Slip LOS)

;; TEMPLATE:
;; los-fn : LOS -> ??
#; (define (los-fn los)
     (cond
       [(empty? los)...]
       [else (...
              (slip-fn (first los))
              (los-fn (rest los)))]))

;; EXAMPLES/CONSTANTS for tests:
(define TEST_INPUT
  (list ROBERT_DOWNEY_JR ROBERT_DOWNEY_JR_2 ROBERT_DOWNEY_JR_REVERSE
        DANIEL_CRAIG DANIEL_CRAIG_2 CRIS_PRATT SCARLETT_JOHANSSON
        ANGELINA_JOLIE ANGELINA_JOLIE_REVERSE CRIS_PRATT DANIEL_CRAIG))
(define EXPECTED_FELLEISEN
  (list ROBERT_DOWNEY_JR_REVERSE DANIEL_CRAIG_2 SCARLETT_JOHANSSON))
(define EXPECTED_SHIVERS
  (list ROBERT_DOWNEY_JR_2 ANGELINA_JOLIE_REVERSE CRIS_PRATT DANIEL_CRAIG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; colors-equal? : Color Color -> Boolean
;; GIVEN: 2 color to comapre
;; RETURNS: true for equal colors, else - false
(define (colors-equal? c1 c2)
  (string=? c1 c2))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (colors-equal? "orange" BLUE) false
   "Orange does not equal to blue.")
  (check-equal?
   (colors-equal? "blue" BLUE) true
   "2 blue color are equal."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Felleisen' (yellow) class, without duplication.
;; STRATEGY: Combune simpler functions
(define (felleisen-roster los)
  (remove-slip-duplicates (get-students-by-color los YELLOW)))

;; TEST/EXAMPLE:  
(begin-for-test
  (check-equal? (felleisen-roster TEST_INPUT) EXPECTED_FELLEISEN
                "Only expected students from Professor Felleisen' class should remain"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster :  ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Shivers' (blue) class, without duplication.
;; STRATEGY: Combune simpler functions
(define (shivers-roster los)
  (remove-slip-duplicates (get-students-by-color los BLUE)))


;; TEST/EXAMPLE:  
(begin-for-test
  (check-equal? (shivers-roster TEST_INPUT) EXPECTED_SHIVERS
                "Only expected students from Professor Shivers' class should remain"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-students-by-color : ListOfSlip Color -> ListOfSlip
;; GIVEN: a list of all slips los and Color c
;; RETURNS: a list of slips containing all the students with same color as given color.
;; STRATEGY: Use HOF filter on los
(define (get-students-by-color los c)
  (filter
   (lambda (student) (colors-equal? (slip-color student) c))
   los))

;; TEST/EXAMPLE: TODO - nlos is always empty
(begin-for-test
  (check-equal?
   (get-students-by-color
    (list (make-slip "yellow" "a" "b")
          (make-slip "blue" "a" "b")
          (make-slip "yellow" "b" "a")
          (make-slip "yellow" "c" "d"))
    "yellow")
   (list (make-slip "yellow" "a" "b")
          (make-slip "yellow" "b" "a")
          (make-slip "yellow" "c" "d"))
   "Only students with 'yellow' color should remain"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-slip-duplicates : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips los
;; RETURNS: a given list of slips, except that duplicate slips are removed
;  (from left to right)
;; STRATEGY: Use template for ListOfSlip on los
(define (remove-slip-duplicates los)
  (cond
    [(empty? los) empty]
    [else (if (slip-list-has-matches? (first los) (rest los))
              (remove-slip-duplicates (rest los))
              (cons (first los) (remove-slip-duplicates(rest los))))]))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (remove-slip-duplicates (list (make-slip "yellow" "a" "b")
                                 (make-slip "yellow" "a" "b")
                                 (make-slip "yellow" "b" "a")
                                 (make-slip "yellow" "c" "d")))
   (list (make-slip "yellow" "b" "a")
         (make-slip "yellow" "c" "d"))
   "Only ba and cd records should remain"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slip-list-has-matches? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip slp and a list of slips los to compare
;; RETURNS: true if given list of slips contains slip similar to given, else - false
;; STRATEGY: Use HOF ormap on los
(define (slip-list-has-matches? slp los)
  (ormap
   (lambda (slip) (or (names-match? slp (first los))
                      (slip-list-has-matches? slp (rest los))))
   los))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (slip-list-has-matches?
    (make-slip "yellow" "a" "b")
    (list (make-slip "yellow" "a" "b")
          (make-slip "yellow" "b" "a")
          (make-slip "yellow" "c" "d")))
    true
   "ab has 2 matches in given list.")
  (check-equal?
   (slip-list-has-matches?
    (make-slip "yellow" "e" "f")
    (list (make-slip "yellow" "a" "b")
          (make-slip "yellow" "b" "a")
          (make-slip "yellow" "c" "d")))
    false
   "ef has no matches in given list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; names-match? : Slip Slip -> Boolean
;; GIVEN: 2 slips to compare
;; RETURNS: true if any combination of names matches for given slips, else - false
;; STRATEGY: Combine simpler functions
(define (names-match? slp1 slp2)
  (or (and (string=? (slip-name1 slp1) (slip-name1 slp2))
           (string=? (slip-name2 slp1) (slip-name2 slp2)))
      (and (string=? (slip-name2 slp1) (slip-name1 slp2))
           (string=? (slip-name1 slp1) (slip-name2 slp2)))))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (names-match? (make-slip "yellow" "a" "b") (make-slip "yellow" "a" "b"))
   true "These are completely similar slips")
  (check-equal?
   (names-match? (make-slip "yellow" "b" "a") (make-slip "yellow" "a" "b"))
   true "These are slips with similar names in different order")
  (check-equal?
   (names-match? (make-slip "yellow" "e" "f") (make-slip "yellow" "a" "b"))
   false "These are completely different slips")
  (check-equal?
   (names-match? (make-slip "yellow" "c" "b") (make-slip "yellow" "a" "b"))
   false "One of the names is different")
  (check-equal?
   (names-match? (make-slip "yellow" "a" "c") (make-slip "yellow" "a" "b"))
   false "One of the names is different"))