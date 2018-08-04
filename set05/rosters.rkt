;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rosters
;; Provide class roster for each class that has at least one student enrolled.

(require rackunit)
(require "extras.rkt")
(require racket/set)

(check-location "05" "rosters.rkt")

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SetOfX is a list of X's without duplication.  Two SetOfX's are
;; considered equal if they have the same members.

;; A SetOfX is either
;; -- empty
;; -- (cons X SetOfX)

;; TEMPLATE:
;; sox-fn : SetOfX -> ??
#; (define (sox-fn lst)
     (cond
       [(empty? lst) ...]
       [else (... 
              (first lst)
              (sox-fn (rest lst)))]))

;; Example: (list (list 1 2) (list 2 1)) is NOT a SetOfSetOfNumber,
;; because (list 1 2) and (list 2 1) represent the same set of numbers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a Student is unspecified, but you may assume that students may be
;; compared for equality with equal?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfStudent (LOS) is either
;; -- empty
;; -- (cons Student LOS)

;; TEMPLATE:
;; los-fn : LOR -> ??
#; (define (los-fn stds)
     (cond
       [(empty? stds)...]
       [else (...
              (std-fn (first stds))
              (los-fn (rest stds)))]))

;; EXAMPLES/CONSTANTS for tests: TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SetOfStudent is a ListOfStudent with no duplicates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a Class is unspecified, but you may assume that classes may be
;; compared for equality with equal?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct enrollment (student class))
;; An Enrollment is a (make-enrollment Student Class)
;; student is a Student
;; class is a Class
;; INTERPRETATION: self-evident

;; TEMPLATE:
;; enrl-fn : Enrollment -> ??
#; (define (enrl-fn enrl)
     (...(enrollment-student enrl)
         (enrollment-class enrl)))

;; EXAMPLE:
;; (make-enrollment s c) represents the assertion that student s is enrolled in class c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct roster (classname students))
;; A ClassRoster is a (make-roster Class SetOfStudent)
;; classname is a Class
;; students is a SetOfStudent
;; INTERPRETATION: self-evident

;; TEMPLATE:
;; rstr-fn : ClassRoster -> ??
#; (define (rstr-fn rstr)
     (...(roster-classname rstr)
         (roster-students rstr)))

;; EXAMPLE:
;; (make-roster c ss) represents that the students in class c are exactly the students in set ss.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SetOfClassRoster is a SetOfX where X is ClassRoster

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Two ClassRosters are equal if they have the same class and equal sets of students.

;; roster=? : ClassRoster ClassRoster -> Boolean
;; GIVEN: 2 ClassRosters
;; RETURNS: true if ClassRosters are equal, else - false
;; STRATEGY: Use templete for ClassRoster on cr1 and cr2
(define (roster=? cr1 cr2)
  (and (equal? (roster-classname cr1) (roster-classname cr2))
       (is-subset? (roster-students cr1) (roster-students cr2))
       (is-subset? (roster-students cr2) (roster-students cr1))))

;; EXAMPLES/TESTS: rosterset=?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-subset? : SetOfX SetOfX -> Boolean
;; GIVEN: 2 SetOfX st1 and st2
;; RETURNS: true if st1 is a subset of st2
;; STRATEGY: Use more general function
(define (is-subset? st1 st2)
  (general-subset? st1 st2 equal?))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (is-subset? (set "a" "b" "c") (set "c" "b" "a"))
   true
   "a b c and c b a sets are the same")  
  (check-equal?
   (is-subset? (set "a" "b" "c") (set "c" "b" "z"))
   false
   "a b c and c b z sets are not the same"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; general-subset? : SetOfX SetOfX -> Boolean
;; GIVEN: 2 SetOfX st1 and st2
;; RETURNS: true if st1 is a subset of st2
;; STRATEGY: Use HOF andmap on st
(define (general-subset? st1 st2 fn)
  (andmap
   (lambda (el) (general-member-of? el st2 fn))
   (set->list st1)))

;; EXAMPLES/TESTS: in is-subset? and rosterset=?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; general-member-of? : X SetOfX -> Boolean
;; GIVEN: an X x and a set of X's st
;; RETURNS: true if the X is an element of the set
;; STRATEGY: Use HOF ormap on st
(define (general-member-of? x st fn)
  (ormap
   (lambda (el) (fn x el))
   (set->list st)))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (general-member-of? "a" (set "a" "b" "c") equal?)
   true
   "a is one of a b c")
  (check-equal?
   (general-member-of? "z" (list "a" "b" "c") equal?)
   false
   "z is not one of a b c")
  (check-equal?
   (general-member-of? (make-roster "x" (set "a" "b" "c"))
                       (set (make-roster "x" (set "a" "b" "c"))
                            (make-roster "y" (set "e" "f" "g")))
                       roster=?)
   true
   "There should be class x with students a b c")
  (check-equal?
   (general-member-of? (make-roster "z" (set "a" "b" "c"))
                       (set (make-roster "x" (set "a" "b" "c"))
                            (make-roster "y" (set "e" "f" "g")))
                       roster=?)
   false
   "There is no z class")
  (check-equal?
   (general-member-of? (make-roster "x" (set "a" "b"))
                       (list (make-roster "x" (list "a" "b" "c"))
                             (make-roster "y" (list "e" "f" "g")))
                       roster=?)
   false
   "Missing student c in class x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN: 2 sets of ClassRosters
;; RETURNS: true iff the two arguments represent the same set of rosters
;; STRATEGY: Use HOF andmap on socr1
(define (rosterset=? socr1 socr2)
  (general-subset? socr1 socr2 roster=?))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (rosterset=? (list (make-roster "x" (list "a" "b" "c"))
                      (make-roster "y" (list "e" "f" "g")))
                (list (make-roster "x" (list "a" "b" "c"))
                      (make-roster "y" (list "e" "f" "g"))))
   true
   "Given sets of Roasters are the same")
  (check-equal?
   (rosterset=? (set (make-roster "a" (set "x" "y" "z"))
                     (make-roster "y" (set "e" "f" "g")))
                (set (make-roster "x" (set "a" "b" "c"))
                     (make-roster "y" (set "e" "f" "g"))))
   false
   "Given sets of Roasters are not the same"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters: SetOfEnrollment -> SetOfClassRoster
;; GIVEN: a set of enrollments
;; RETURNS: the set of class rosters for the given enrollments

;; EXAMPLE:
;  (enrollments-to-rosters
;    (list (make-enrollment "John" "PDP")
;          (make-enrollment "Kathryn" "Networks")
;          (make-enrollment "Feng" "PDP")
;          (make-enrollment "Amy" "PDP")
;          (make-enrollment "Amy" "Networks")))
; =>
; (list
;   (make-roster "PDP" (list "John" "Feng" "Amy"))
;   (make-roster "Networks" (list "Kathryn" "Amy")))

;; In the output, the classes may be in any order, and the students in
;; each class may be in any order.

;; STARTEGY: Combine Simpler Functions
(define (enrollments-to-rosters soe)
  (make-roaster-set soe (list-of-unique-classes soe)))

;; TEST: 
(begin-for-test
  (check-equal?
   (enrollments-to-rosters
    (list (make-enrollment "John" "PDP")
          (make-enrollment "Kathryn" "Networks")
          (make-enrollment "Feng" "PDP")
          (make-enrollment "Amy" "PDP")
          (make-enrollment "Amy" "Networks")))
   (list
    (make-roster "Networks" (list "Kathryn" "Amy"))
    (make-roster "PDP" (list "John" "Feng" "Amy")))
   "According to the given example"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list-of-unique-classes ; SetOfEnrollment -> ListOfStudent
;; GIVEN: a set of enrollments
;; RETURNS: the set of class rosters for the given enrollments
;; STARTEGY: Use HOF map on soe
(define (list-of-unique-classes soe)
  (set->list (list->set
              (map
               (lambda (enr) (enrollment-class enr))
               soe))))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (list-of-unique-classes (list (make-enrollment "John" "PDP")
                                 (make-enrollment "Kathryn" "Networks")
                                 (make-enrollment "Feng" "PDP")))
   (list "Networks" "PDP")
   "Only PDP and Networks are unique"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-roaster-set : SetOfEnrollment ListOfStudent -> SetOfClassRoster
;; GIVEN: a set of enrollments soe and list of unique students
;; RETURNS: the set of class rosters based on input
;; STARTEGY: Use HOF map on los
(define (make-roaster-set soe los)
  (map
   (lambda (class) (make-roster class
                                (get-students (get-enrollments-by-class class soe))))
   los))

;; EXAMPLES/TESTS: in enrollments-to-rosters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-students : SetOfEnrollment -> ListOfStudent
;; GIVEN: a set of enrollments soe
;; RETURNS: a list of students from given enrollments
;; STRATEGY: Use HOF map on soe
(define (get-students soe)
  (map
   (lambda (enr) (enrollment-student enr))
   soe))

;; EXAMPLES/TESTS: in enrollments-to-rosters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-enrollments-by-class : String SetOfEnrollment -> ListOfStudent
;; GIVEN: a class name cl and a set of enrollments soe
;; RETURNS: a list of students that belong to given class
;; STRATEGY: Use HOF filter on soe
(define (get-enrollments-by-class cl soe)
  (filter
   (lambda (enr) (equal? cl (enrollment-class enr)))
   soe))

;; EXAMPLES/TESTS: in enrollments-to-rosters