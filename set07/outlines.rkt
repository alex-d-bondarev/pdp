;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This implements the ability to check if a flap representation of an outline is valid
;; and the ability to convert and outline to it's appropriate flat represenation
;; Code employs HOFs in all necessary and appropriate places

(require rackunit)
(require "extras.rkt")
(check-location "07" "outlines.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;; By Frederick and Oleksandr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN CONSTANTS

;;;;;;;;;;;;;;;;;;;;

(define FIRST-SECT-NUM (list 1))

;;;;;;;;;;;;;;;;;;;;

;; END CONSTANTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Data definitions

;;;;;;;;;;;;;;;;;;;;

;; An Outline is a ListOfSection

;; REPRESENTATION EXAMPLE for Outline :
#; (list 
    (make-section "The first section"
                  (list
                   (make-section "A subsection with no subsections" empty)
                   (make-section "Another subsection"
                                 (list
                                  (make-section "This is a subsection of 1.2" empty)
                                  (make-section "This is another subsection of 1.2"
                                                empty)))
                   (make-section "The last subsection of 1" empty)))
    (make-section "Another section"
                  (list
                   (make-section "More stuff" empty)
                   (make-section "Still more stuff" empty))))

;;;;;;;;;;

;; A ListOfSection (LOS) is one of
;; -- empty
;; -- (cons Section LOS)

;; TEMPLATE:
#;(define (los-fn los)
    (cond
      [(empty? los) ...]
      [else (... (section-fn (first los))
                 (los-fn (rest los)))]))

;;;;;;;;;;

(define-struct section (header secs))
;; A Section is a (make-section String ListOfSection)
;; INTERP: (make-section str secs) is a section where
;;         str  is the header text of the section
;;         secs is the list of subsections of the section

;; TEMPLATE:
#;(define (section-fn s)
    (... (section-header s)
         (los-fn (section-secs s))))

;;;;;;;;;;;;;;;;;;;;

;; A FlatRep is a ListOfLine
;; WHERE the lon of the first element is (list 1).
;;       For a given lon as (list x1 x2 ... xn) the next
;;       lon must one of:
;;                (list (+ x1 1))
;;                (list (x1 (+ x2 1))
;;                (list (x1 x2 (+ x3 1))
;;                ...
;;                (list (x1 x2 ... (+ xn 1))

;; REPRESENTATION EXAMPLE for FlatRep :
#; (list
    (make-line (list 1) "The first section")
    (make-line (list 1 1) "A subsection with no subsections")
    (make-line (list 1 2) "Another subsection")
    (make-line (list 1 2 1) "This is a subsection of 1.2")
    (make-line (list 1 2 2) "This is another subsection of 1.2")
    (make-line (list 1 3) "The last subsection of 1")
    (make-line (list 2) "Another section")
    (make-line (list 2 1) "More stuff")
    (make-line (list 2 2) "Still more stuff"))

;;;;;;;;;;

;; A ListOfLine (LOL) is either
;; -- empty
;; -- (cons Int LOL)

;; lon-fn : LOL -> ??
;; (define (lol-fn lol)
;;   (cond
;;     [(empty? lol) ...]
;;     [else (...
;;             (line-fn (first lol))
;;             (lol-fn (rest lol)))]))

;;;;;;;;;;

(define-struct line (sect-num name))
;; A Line is a (make-line ListOfNumber String)
;; INTERP: (make-line lon str) is a line where
;;         lon is the list of natural numbers that represents section number
;;         str is the header text of the section

;; TEMPLATE:
#;(define (line-fn l)
    (... (line-sect-num l)
         (line-name l)))

;;;;;;;;;;

;; A ListOfNumber (LON) is either
;; -- empty
;; -- (cons Int LON)

;; lon-fn : LON -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;             (int-fn (first lon))
;;             (lon-fn (rest lon)))]))


;;;;;;;;;;;;;;;;;;;;

;; END Data definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Implementation / Tests data

;;;;;;;;;;;;;;;;;;;;

;; For FlatRep
(define FLATREP1(list
                 (make-line (list 1) "The first section")
                 (make-line (list 1 1) "A subsection with no subsections")
                 (make-line (list 1 2) "Another subsection")
                 (make-line (list 1 2 1) "This is a subsection of 1.2")
                 (make-line (list 1 2 2) "This is another subsection of 1.2")
                 (make-line (list 1 3) "The last subsection of 1")
                 (make-line (list 2) "Another section")
                 (make-line (list 2 1) "More stuff")
                 (make-line (list 2 2) "Still more stuff")))

(define WRONG_FLATREP(list
                      (make-line (list 1) "The first section")
                      (make-line (list 1 1) "A subsection with no subsections")
                      (make-line (list 1 2) "Another subsection")
                      (make-line (list 2 2 1) "This is a subsection of 1.2")
                      (make-line (list 1 2 2) "This is another subsection of 1.2")
                      (make-line (list 1 3) "The last subsection of 1")
                      (make-line (list 2) "Another section")
                      (make-line (list 2 1) "More stuff")
                      (make-line (list 2 2) "Still more stuff")))

(define WRONG_FLATREP_2(list
                        (make-line (list 1) "The first section")
                        (make-line (list 1 1) "A subsection with no subsections")
                        (make-line (list 1 2) "Another subsection")
                        (make-line (list 1 2 2) "This is a subsection of 1.2")
                        (make-line (list 1 2 3) "This is another subsection of 1.2")
                        (make-line (list 1 3) "The last subsection of 1")
                        (make-line (list 2) "Another section")
                        (make-line (list 2 1) "More stuff")
                        (make-line (list 2 2) "Still more stuff")))

(define WRONG_FLATREP_3(list
                        (make-line (list 1) "The first section")
                        (make-line (list 1 1) "A subsection with no subsections")
                        (make-line (list 1 2) "Another subsection")
                        (make-line (list 1 2 1) "This is a subsection of 1.2")
                        (make-line (list 1 2 2) "This is another subsection of 1.2")
                        (make-line (list 1 3) "The last subsection of 1")
                        (make-line (list 2) "Another section")
                        (make-line (list 2 1) "More stuff")
                        (make-line (list 2 2) "Still more stuff")
                        (make-line (list 4) "Last test line")))

(define WRONG_FLATREP_4(list
                        (make-line (list 1) "The first section")
                        (make-line (list 1 1) "A subsection with no subsections")
                        (make-line (list 1 2) "Another subsection")
                        (make-line (list 1 2 1) "This is a subsection of 1.2")
                        (make-line (list 1 2 2) "This is another subsection of 1.2")
                        (make-line (list 1 3) "The last subsection of 1")
                        (make-line (list 2) "Another section")
                        (make-line (list 2 1) 12)
                        (make-line (list 2 2) "Still more stuff")))

(define WRONG_FLATREP_5(list
                        (make-line (list 1) "The first section")
                        (make-line empty "A subsection with no subsections")
                        (make-line (list 1 2) "Another subsection")
                        (make-line (list 1 2 1) "This is a subsection of 1.2")
                        (make-line (list 1 2 2) "This is another subsection of 1.2")
                        (make-line (list 1 3) "The last subsection of 1")
                        (make-line (list 2) "Another section")
                        (make-line (list 2 1) "More stuff")
                        (make-line (list 2 2) "Still more stuff")))

;; For Outline
(define OUTLINE1 (list 
                  (make-section "The first section"
                                (list
                                 (make-section "A subsection with no subsections" empty)
                                 (make-section "Another subsection"
                                               (list
                                                (make-section
                                                 "This is a subsection of 1.2" empty)
                                                (make-section
                                                 "This is another subsection of 1.2"
                                                 empty)))
                                 (make-section "The last subsection of 1" empty)))
                  (make-section "Another section"
                                (list
                                 (make-section "More stuff" empty)
                                 (make-section "Still more stuff" empty)))))

;;;;;;;;;;;;;;;;;;;;

;; END Implementation / Tests data 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN functions required by problem set

;;;;;;;;;;;;;;;;;;;;
;;        1       ;;
;;;;;;;;;;;;;;;;;;;;

;; legal-flat-rep? : ListOfLine -> Boolean
;; GIVEN: ListOfLine as lol
;; RETURNS: true iff given list is a legal flat representation of an outline.
;; WHERE: first line is always (list 1) or FIRST-SECT-NUM, and the following lines have
;;        a list of number where each additional number represents an additional level
;;        in the outline. For the following line, either 1 is added in the deepest level of
;;        the previous level, one of the previous levels is incremented and everything
;;        is removed the right (or deeper) than said level, or new level is added,
;;        starting with 1 number and previous numbers are not incremented
;; STRATEGY: template for ListOfLine on lol + call a more general function
(define (legal-flat-rep? lol)
  (cond
    [(empty? lol) true]
    [(equal? (line-sect-num (first lol)) FIRST-SECT-NUM)
     (legal-comparing-to-prev? FIRST-SECT-NUM (rest lol))]))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (legal-flat-rep? empty)
   true
   "Empty ListOfLine is considered to be legal")
  (check-equal?
   (legal-flat-rep? FLATREP1)
   true
   "Should return true for the problem spec of flat rep")
  (check-equal?
   (legal-flat-rep? WRONG_FLATREP)
   false
   "Should return false for the incorrect flat rep")
  (check-equal?
   (legal-flat-rep? WRONG_FLATREP_2)
   false
   "Should return false for the incorrect flat rep, where incorrect is one of the
sub-levels")
  (check-equal?
   (legal-flat-rep? WRONG_FLATREP_3)
   false
   "Should return false for the incorrect flat rep, where one of the lines is
incremented twice")
  (check-equal?
   (legal-flat-rep? WRONG_FLATREP_4)
   false
   "Should return false for the incorrect flat rep, where one of the lines has number
instead of a name")
  (check-equal?
   (legal-flat-rep? WRONG_FLATREP_5)
   false
   "Should return false for the incorrect flat rep, where line number is empty"))

;;;;;;;;;;;;;;;;;;;;

;; legal-comparing-to-prev? : ListOfNumber ListOfLine -> Boolean
;; GIVEN: a ListOfNumber as lon-prev for previous section number
;;        and the a ListOfLine as lol
;; WHERE: given lol is a subListOfLine with a context lon that represents
;;        the section number of the line before lol
;; RETURNS: true if current line is legal
;;          and section numbers for the given ListOfLine
;;          are legal comparing to given the section number of the previos line
;; STRATEGY: Use ListOfLine template on lol
(define (legal-comparing-to-prev? lon-prev lol)
  (cond
    [(empty? lol) true]
    [else (and (legal-line-sect-num? lon-prev
                                     (line-sect-num (first lol)))
               (line-is-legal? (first lol))
               (legal-comparing-to-prev? (line-sect-num (first lol))
                                         (rest lol)))]))

;; EXAMPLE:
;; (legal-comparing-to-prev?
;;    (list 1)
;;    (list
;;     (make-line (list 1 1) "A subsection with no subsections"))) => true

;; TEST: tested in legal-flat-rep? function

;;;;;;;;;;;;;;;;;;;;

;; legal-line-sect-num? : ListOfNumber ListOfNumber -> Boolean
;; GIVEN: a ListOfNumber as prev representing previous section and ListOfNumber as cur
;;        representing current section
;; RETURN: true if cur represents a valid section number comparing to prev
;;         else false
;; STRATEGY: Divide into cases on prev or cur
(define (legal-line-sect-num? prev cur)
  (cond
    [(empty? prev) (and (equal? (first cur) 1)
                        (empty? (rest cur)))]
    [(empty? cur) false]
    [(is-first-higher-by-1? (first cur) (first prev)) (empty? (rest cur))]
    [(equal? (first cur) (first prev)) (legal-line-sect-num? (rest prev) (rest cur))]
    [else false]))

;; EXAMPLE:
;; (legal-line-sect-num?
;;    (list 1)
;;    (list 1 1)) => true

;; TEST: tested in legal-flat-rep? function

;;;;;;;;;;;;;;;;;;;;

;; is-first-higher-by-1? : Int Int -> Boolean
;; GIVEN: a num1 and num2
;; RETURNS: true if num1 level is higher than num2 level only by 1, otherwise false
;; STRATEGY: combine simpler functions
(define (is-first-higher-by-1? num1 num2)
  (equal? num1 (+ num2 1)))

;; EXAMPLE:
;; (is-first-higher-by-1? 4 3) => true
;; (is-first-higher-by-1? 4 2) => false
;; (is-first-higher-by-1? 1 2) => false

;; TEST: tested in legal-flat-rep? function

;;;;;;;;;;;;;;;;;;;;

;; line-is-legal? : Line -> Boolean
;; GIVEN: a Line as l
;; RETURNS: true if first parameter is list and second parameter is String, else false
;; STRATEGY: use Line template on l
(define (line-is-legal? l)
    (and (list? (line-sect-num l))
         (string? (line-name l))))

;; EXAMPLE:
;; (line-is-legal? (make-line (list 2) "Another section")) => true
;; (line-is-legal? (make-line (list 2) 10)) => false

;; TEST: tested in legal-flat-rep? function

;;;;;;;;;;;;;;;;;;;;
;;        2       ;;
;;;;;;;;;;;;;;;;;;;;

;; tree-rep-to-flat-rep : Outline -> FlatRep
;; GIVEN: the representation of an outline as a list of Sections
;; RETURNS: the flat representation of the given outline
;; STRATEGY: Call a more general function
(define (tree-rep-to-flat-rep los)
  (los-to-flat-rep los FIRST-SECT-NUM))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (tree-rep-to-flat-rep OUTLINE1)
   FLATREP1
   "Problem spec example of outline should return problem spec example of flat rep")
  (check-equal?
   (tree-rep-to-flat-rep empty)
   empty
   "Empty is not changed"))

;;;;;;;;;;;;;;;;;;;;

;; los-to-flat-rep : ListOfSections ListOfNumber -> ListOfLine
;; GIVEN: a ListOfSection as los, and ListOfNumber as lon
;; WHERE: los does not contain sections that were already calculated
;;        and lon represents current section level that was incremented
;;        after each previous section that was already calculated
;; RETURNS: the flat representaiton for the given list of sections
;; STRATEGY: Use template for ListOfSection on los
(define (los-to-flat-rep los lon)
  (cond
    [(empty? los) empty]
    [else (append (section-to-line (first los) lon)
                  (los-to-flat-rep (rest los) (increment lon)))]))

;; TESTS/EXAMPLES: in tree-rep-to-flat-rep function

;;;;;;;;;;;;;;;;;;;;

;; section-to-line : Section ListOfNumber -> ListOfLine
;; GIVEN: a Section s and ListOfNumber lon
;; RETURNS: the flat Line representaiton for the given section
;; WHERE: sub sections of current section are also changed to lines
;; STRATEGY: Use template for Section on s
(define (section-to-line s lon)
  (cons (make-line lon
                   (section-header s))
        (los-to-flat-rep (section-secs s) (append lon (list 1)))))

;; EXAMPLE:
;; (section-to-line (make-section "This is a subsection of 1"
;;        (make-section "This is another subsection of 1.1" empty)) (list 1 1)) =>
;; (list (make-line (list 1 1) "This is a subsection of 1")
;;       (make-line (list 1 1 1) "This is another subsection of 1.1"))

;; TEST: tested in tree-rep-to-flat-rep function

;;;;;;;;;;;;;;;;;;;;

;; increment : ListOfNumber -> ListOfNumber
;; GIVEN: ListOfNumber lon
;; RETURNS: same ListOfNumber as given, except, that last list element in ListOfNumber
;;          is incremented by 1
;; STRATEGY: use ListOfN template on lon
(define (increment lon)
  (cond
    [(empty? (rest lon)) (list (+ (first lon) 1))]
    [else (cons (first lon) (increment (rest lon)))]))

;; EXAMPLE: (increment (1 2 1)) -> (1 2 2)

;; TEST: tested in tree-rep-to-flat-rep function