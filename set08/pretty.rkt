;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This implements a pretty printer of expressions

(require rackunit)
(require "extras.rkt")
(check-location "08" "pretty.rkt")

(provide
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs);expr-to-strings)

;; By Frederick and Oleksandr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN CONSTANTS

;;;;;;;;;;;;;;;;;;;;

(define BEGIN_SUM_EXP "(+ ")

(define BEGIN_DIFF_EXP "(- ")

(define OPEN_PAREN "(")

(define CLOSE_PAREN ")")

(define EMPTY_ACCUM_STRING "")

(define NO_END_PAREN 0)

(define NO_OFFSET_SPACES 0)

(define START-EXPRESSION-LENGTH 3)

;;;;;;;;;;;;;;;;;;;;

;; END CONSTANTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Data definitions

;;;;;;;;;;;;;;;;;;;;

(define-struct sum-exp (exprs))
;; A SumExpression is a (make-sum-exp NELOExpr)
;; INTERP: (make-sum-exp exprs) is a sum-exp where
;;         exprs  is a list of expression

;; TEMPLATE:
#;(define (sum-exp-fn se)
    (... (sum-exp-exprs se)))

;;;;;;;;;;;

(define-struct diff-exp (exprs))
;; A DifferenceExpression is a (make-diff-exp NELOExpr)
;; INTERP: (make-diff-exp exprs) is a diff-exp where
;;         exprs  is a list of expression

;; TEMPLATE:
#;(define (diff-exp-fn de)
    (... (diff-exp-exprs de)))

;;;;;;;;;;;

;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-diff-exp NELOExpr)
;; Interpretation: a sum-exp represents a sum and a diff-exp
;; represents a difference calculation.

;; TEMPLATE:
#;(define (expr-fn exp)
  (cond
    [(number? exp) ...]
    [(sum-exp? exp) (sum-exp-fn exp)]
    [(diff-exp? exp) (diff-exp-fn exp)]))

;;;;;;;;;;;

;; A LOExpr is one of
;; -- empty
;; -- (cons Expr LOExpr)

;; loe-fn : LOExpr -> ??
;; (define (loe-fn loe)
;;   (cond
;;     [(empty? loe) ...]
;;     [else (...
;;             (expr-fn (first loe))
;;             (loe-fn (rest loe)))]))

;; A NELOExpr is a non-empty LOExpr.

;;;;;;;;;;;;

;; A LOS is one of
;; -- empty
;; -- (cons String LOS)

;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (first los))
;;             (los-fn (rest los)))]))

;;;;;;;;;;;;;;;;;;;;

;; END Data definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Implementation / Tests data

;;;;;;;;;;;;;;;;;;;;

#;(define (display-expr expr n)
    (display-strings! (expr-to-strings expr n)))

(define hw-example-1 (make-sum-exp (list 22 333 44)))

(define hw-example-2 (make-sum-exp
                      (list
                       (make-diff-exp (list 22 3333 44))
                       (make-diff-exp
                        (list
                         (make-sum-exp (list 66 67 68))
                         (make-diff-exp (list 42 43))))
                       (make-diff-exp (list 77 88)))))

(define hw-example-3 (make-sum-exp
                      (list
                       (make-diff-exp (list 222222222222222222222 3333 444))
                       (make-diff-exp
                        (list
                         (make-sum-exp (list 66 67 68))
                         (make-diff-exp (list 42 43))))
                       (make-diff-exp (list 77 88)))))

(define hw2-100
  (list
   "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"))

(define hw2-50
  (list
"(+ (- 22 3333 44)"
"   (- (+ 66 67 68) (- 42 43))"
"   (- 77 88))"))

(define hw2-20
(list
"(+ (- 22 3333 44)"
"   (- (+ 66 67 68)"
"      (- 42 43))"
"   (- 77 88))"))

(define hw2-15
  (list 
"(+ (- 22"
"      3333"
"      44)"
"   (- (+ 66"
"         67"
"         68)"
"      (- 42"
"         43))"
"   (- 77 88))"))


;;;;;;;;;;;;;;;;;;;;

;; END Implementation / Tests data 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN functions required by problem set

;;;;;;;;;;;;;;;;;;;;
;;        1       ;;
;;;;;;;;;;;;;;;;;;;;

;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines, with
;;          each line represented as a string of length not greater than the width.
;; EXAMPlES/TESTS
(begin-for-test
  (check-equal?
   (expr-to-strings hw-example-2 15)
   hw2-15
   "Should follow example hw2 for width 15")

  (check-equal?
   (expr-to-strings hw-example-2 20)
   hw2-20
   "Should follow example hw2 for width 20")

  (check-equal?
   (expr-to-strings hw-example-2 50)
   hw2-50
   "Should follow example hw2 for width 50")

  (check-equal?
   (expr-to-strings hw-example-2 100)
   hw2-100
   "Should follow example hw2 for width 100")
  
  (check-error
   (expr-to-strings hw-example-1 5)
   "not enough room")

  (check-error
   (expr-to-strings hw-example-1 4)
   "not enough room")

  (check-error
   (expr-to-strings hw-example-3 10)
   "not enough room"))
;; STRATEGY: call a more general function
(define (expr-to-strings exp width)
  (expr-to-strings-at-offset exp width 0 0))

;; expr-to-ostrings-at-offset : Expr NonNegInt NonNegInt NonNegInt  -> ListOfString
;; GIVEN: a subexpression, width, and offset
;; WHERE: the offset is the number of spaces that occur before the expression
;;        depending on how "deep" or nested the subexpression is within the original
;;        expression.  
;; RETURNS: a strings with an "offset" amount of spaces
;; TEST: tested in expr-to-strings function
;; STRATEGY: template of Expr on exp
(define (expr-to-strings-at-offset exp width end-paren offset)
  (cond
    [(number? exp) (list (handle-int exp width end-paren offset))]
    [else
     (handle-sum-diff exp width end-paren offset (sum-diff->symbol exp))]))


;; neloe-to-strings-at-offset : NELOExpr NonNegInt NonNegInt NonNegInt -> ListOfString
;; GIVEN: a NELOExp, a specified width, the number of end parenthesese and number
;;        of spaces for padding before expression
;; WHERE: end-paren represents whether the current exp is at the end of a neloe
;;        offset represents the number of space indent that an expression should
;;        occur at and is dictated by the 
;; TEST: tested in expr-to-strings function
;; STRATEGY: template of NELOExp on neloe
(define (neloe-to-strings-at-offset neloe width end-paren offset)
  (cond
    [(empty? (rest neloe))
     (expr-to-strings-at-offset (first neloe) width (+ end-paren 1) offset)]
    [else
     (append
      (expr-to-strings-at-offset (first neloe) width end-paren offset)
      (neloe-to-strings-at-offset (rest neloe) width end-paren offset))]))

;; handle-sum-diff : Expr NonNegInt NonNegInt NonNegInt 1String -> ListOfString
;; GIVEN: a subexpression, width, number of end parens, length of space offset, and
;;        a 1String
;; WHERE: the subexpression is either sum-exp or diff-exp, the 1String is the
;;        mathematical symbol representing the type of expression.
;;        space-offset is dictated by the depth of the current
;;        subexpression within the original expression.
;;        And end-paren represents the number of parents of the subexpression
;;        that are the last elements of a NELOExp
;; RETURNS: a List of String representation of the sum-exp or diff-exp where
;;         (if it is multiple lines), each line is indented by space-offset amount
;; TEST: tested in expr-to-strings function
;; STRATEGY: cases on exp, space-offset, end-paren, and width
(define (handle-sum-diff exp width end-paren space-offset symbol)
  (cond
    [(<= (estimate-exp-length exp space-offset end-paren EMPTY_ACCUM_STRING) width)
     (list (string-append (make-space-padding space-offset)
                          (expr-to-flat-string exp)))]
    [else
      (reduce-first-line exp width
                         space-offset
                         end-paren
                         (make-space-padding space-offset))]))

;; reduce-first-line : Expr NonNegInt NonNegInt NonNegInt String -> ListOfString
;; GIVEN: a subexpression, a width, number of spaces to put in front of expression,
;;        the number of close parentheses, and a accum-string
;; WHERE: accum-string represents the most reduced first line up to the point
;;        of the subexpression.  And space-offset is dictated by teh depth of current
;;        subexpression within original expression. and end-paren is the number
;;        of parents of the subexpression who are the last element in a NELOExp
;; RETURNS: a List of String where the first line has the accum-string at the front
;;         and the following elements in the LOS are indented at "space-offset"
;;         amount
;; TEST: tested in expr-to-strings function
;; STRATEGY: template for Exp on exp
(define (reduce-first-line exp width space-offset end-paren accum-string)
  (cond
    ;; since you cannot reduce a number to make room on line, emit an error
    [(number? exp)
     (error "not enough room")]
    [else
     (if (<= (estimate-first-line-length exp accum-string) width)
         ;; if so, we have finished creating first line
         (cons
          (string-append (build-first-line-exp exp accum-string)
                         (expr-to-flat-string (first (sum-diff->neloe exp))))
          (add-paren-to-last-string (get-rest-lines exp width space-offset end-paren)))
         ;; if not, continue recursing adding more expressions to first line
         ;; each time
         (append 
          (reduce-first-line (first (sum-diff->neloe exp)) width
                             (+ space-offset START-EXPRESSION-LENGTH)
                             end-paren
                             (build-first-line-exp exp accum-string))
          (add-paren-to-last-string (get-rest-lines exp width space-offset end-paren))))]))

;; get-rest-lines : Expr NonNegInt NonNegInt -> ListOfString
;; GIVEN: an subexpression, a width expression must satisfy, and number of end-parens
;; WHERE: the subexpression is a sum or diff expressions
;;        the space-offset represents how deep the subexpression is within the
;;        original expression and the end-paren represents how many parents
;;        of the subexpression are the last element in a neloexp
;; RETURNS: a List of Strings for expressions AFTER the first expression.
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (get-rest-lines exp width space-offset end-paren)
  (neloe-to-strings-at-offset (rest (sum-diff->neloe exp)) width end-paren (+ space-offset 3)))

;; expr-to-flat-string : Exp -> String
;; GIVEN: an Expression
;; RETURNS: the expression as a string
;; TEST: tested in expr-to-strings function
;; STRATEGY: template of Exp on exp
(define (expr-to-flat-string exp)
  (cond
    [(number? exp) (create-num-string exp 0)]
    [else
     (sum-diff-exp-to-flat-string (sum-diff->neloe exp) (sum-diff->symbol exp))]))

;; sum-diff-exp-to-flat string : NELOExpr 1String -> String
;; GIVEN: NELOExpr the symbol that is performed on it (either a + or -)
;;        return a string representation of the sum/difference expression
;; RETURNS: a the string represnetaiton of a neloexpr with  "(+ " on the left
;;          and ")" on the right of it (with a '-' instead of '+' depending on the
;;          symbol input).
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (sum-diff-exp-to-flat-string neloe symbol)
  (string-append
  (prepend-space-paren-symbol-to-string 0 symbol (neloe-exp-to-flat-string neloe))
   CLOSE_PAREN))

;; neloe-exp-to-string : NELOExpr -> String
;; GIVEN: a Non Empyt LOE, return it in it's string representation with a space
;;        between each element in the NELOExp
;; RETURNS: the string representation of each neloexp with a space separating
;;          each.
;; STRATEGY: template of NELOExpr on neloe
;; TEST: tested in expr-to-strings function
(define (neloe-exp-to-flat-string neloe)
  (cond
    [(empty? (rest neloe)) (expr-to-flat-string (first neloe))]
    [else
     (link-with-space
      (expr-to-flat-string (first neloe))
      (neloe-exp-to-flat-string (rest neloe)))]))

;; handle-int : Integer NonNegInt NonNegInt -> String
;; GIVEN: a Number expression, required width for line, the number of end-parens
;;        and offset space count
;; RETURNS: either error if the string is two big for the line
;;          or creates the string and returns it
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (handle-int num width end-paren offset)
  (if (> (estimate-exp-length num offset end-paren EMPTY_ACCUM_STRING) width)
      (error "not enough room")
      (create-num-string num offset)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions

;; estimate-first-line-length : Exp String -> NonNegInt
;; GIVEN: a Expression, number of spaces, number of end-parens and a accum-string
;;        that is supposed to go to front of exp
;; WHERE: the expression is of type sum-exp or diff-exp
;; RETURNS: the length of proposed string
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (estimate-first-line-length exp accum-string)
  (+ (string-length (expr-to-flat-string (first (sum-diff->neloe exp))))
     START-EXPRESSION-LENGTH
     (string-length accum-string)))

;; build-first-line-exp : Exp String -> String
;; GIVEN: a Expression, number of spaces, and an accumulated string from
;;        previous calls to reduce-first-line
;; WHERE: the expression is of type sum-exp or diff-exp
;; RETURNS: the accum-string prepended to a new "(+ " or "(- " string
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (build-first-line-exp exp accum-string)
  (string-append accum-string "(" (sum-diff->symbol exp) " "))

;; estimate-exp-length : Exp NonNegInt NonNegInt String -> NonNegInt
;; GIVEN: a Expression, number of spaces, number of end-parens and a accum-string
;;        that is supposed to go to front of exp
;; RETURNS: the length of proposed string
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (estimate-exp-length exp space-offset end-paren accum-string)
  (+ (string-length (expr-to-flat-string exp))
     (string-length accum-string)
     space-offset
     end-paren))

;; link-with-space : String String -> String
;; GIVEN: two strings
;; RETURNS: a string with two strings concatenated to each other with a space in between
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (link-with-space s1 s2)
  (string-append s1 " " s2))

;; create-num-string : Integer NonNegInt -> String
;; GIVEN: a Number expression and number of spaces
;; RETURNS: a string with the number of spaces concactenated to the string
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (create-num-string num num_spaces)
  (string-append (build-string num_spaces (lambda (i) #\ ))
                 (number->string num)))

;; make-space-padding : NonNegInt -> String
;; GIVEN: a non negative integer representing the number of spaces
;; RETURNS: a string with said number of spaces
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (make-space-padding num)
  (build-string num (lambda (i) #\ )))

;; prepend-space-paren-symbol-to-string : NonNegInt 1String String -> String
;; GIVEN: a non negative integer representing the number of spaces
;;        a mathematical symbol, and the rest of the string (s)
;; RETURNS: a string with all components concatenated to each other
;; TEST: tested in expr-to-strings function
;; STRATEGY: combine simpler functions
(define (prepend-space-paren-symbol-to-string space-offset symbol s)
  (string-append
   (make-space-padding space-offset)
   "(" symbol " " s))

;; add-paren-to-last-string : NonEmtpyListOfString -> NonEmptyListOfString
;; GIVEN: a ListOfStrings
;; RETURNS: the same list of strings with a close_paren at end of last string
;; TEST: tested in expr-to-strings function
;; STRATEGY: use ListOfString template on los
(define (add-paren-to-last-string los)
  (cond
    [(empty? (rest los)) (list (string-append (first los) CLOSE_PAREN))]
    [else
     (cons
      (first los)
      (add-paren-to-last-string (rest los)))]))

;; sum-diff->neloe : Exp -> NELOExp
;; GIVEN: an expression
;; WHERE: the expression is either a sum or difference expression
;; RETURNS: the exprs (a NELOExp) for the expressions
;; TEST: tested in expr-to-strings function
;; STRATEGY: cases on exp
(define (sum-diff->neloe exp)
  (cond
    [(sum-exp? exp) (sum-exp-exprs exp)]
    [(diff-exp? exp) (diff-exp-exprs exp)]))


;; sum-diff->symbol : Exp -> 1String
;; GIVEN: an expression
;; WHERE: the expression is either a sum or difference expression
;; RETURNS: mathematical symbol associated with the expression type
;; TEST: tested in expr-to-strings function
;; STRATEGY: cases on exp
(define (sum-diff->symbol exp)
  (cond
    [(sum-exp? exp) "+"]
    [(diff-exp? exp) "-"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;