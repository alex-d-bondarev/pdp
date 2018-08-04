;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(check-location "02" "fsm.rkt")

(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A MachineImput is one of: 
; – "a"
; – "b"
; – "c"
; – "d"
; – "e"
; – "f"
;; INTERPRETATION: according to the problem set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A State is one of: 
; – "Q0" INTERP: initial-state, expects "a", "b" or "c". "c" will process to interim-state
; – "Q1" INTERP: interim-state, expects "a", "b" or "d". "d" will process to accepting-state
; – "Q2" INTERP: accepting-state, expects "e" or "f".
; – "Q3" INTERP: error-state, is processed when unexpected input is given to other states

;; State-transition diagram can be found in fsm.png

(define Q0 "initial-state")
(define Q1 "interim-state")
(define Q2 "accepting-state")
(define Q3 "error-state")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-state : Number -> State
;; GIVEN: a Number x
;; RETURNS: a representation of the initial state
;  of your machine. The given number is ignored.
;; EXAMPLE\TEST:
(begin-for-test
  (check-equal? (initial-state 666) "initial-state"
                "Any imput number will provide initial-state"))

;; STRATEGY: Combine simpler functions
(define (initial-state x) Q0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accepting-state? : State -> Boolean
;; GIVEN: a state st of the machine
;; RETURNS: true if the given state is a final (accepting) state
;; EXAMPLE\TEST: 

(begin-for-test
  ; my test scenario
  (check-equal? (accepting-state? "accepting-state") true
                "Given state should be accepting-state")
  ; test scenarios from problem set
  (check-equal? (accepting-state? CD_INPUT) true
                "According to task, 'cd' should be in accepting-state.")
  (check-equal? (accepting-state? ABCD_INPUT) true
                "According to task, 'abcd' should be in accepting-state.")
  (check-equal? (accepting-state? ABCBDEF_INPUT) true
                "According to task, 'abcbdef' should be in accepting-state.")
  (check-equal? (accepting-state? AACBADF_INPUT) true
                "According to task, 'aacbadf' should be in accepting-state.")
  (check-equal? (accepting-state? ABC_INPUT) false
                "According to task, 'abc' should not be in accepting-state."))

;; STRATEGY: Combine simpler functions
(define (accepting-state? st)
  (string=? st Q2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; error-state? : State -> Boolean
;; GIVEN: a state st of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;  state to an accepting state
;; EXAMPLE\TEST:

(begin-for-test
  ; my test scenario
  (check-equal? (error-state? "error-state") true
                "Given state should be error-state")
  ; test scenarios from problem set
  (check-equal? (error-state? ABDBCEF_INPUT) true
                "According to task, 'abdbcef' should be in error-state.")
  (check-equal? (error-state? ACBDED_INPUT) true
                "According to task, 'acbded' should be in error-state."))

;; STRATEGY: Combine simpler functions
(define (error-state? st)
  (string=? st Q3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state : State MachineInput -> State
;; GIVEN: a state st of the machine and a machine input in
;; RETURNS: the state that should follow the given input.
;; EXAMPLES\TESTS:

(begin-for-test
  ; initial-state tests
  (check-equal? (next-state JUST_INITIALIZED "a") Q0
                "initial-state with 'a' input is still initial-state")
  (check-equal? (next-state JUST_INITIALIZED "b") Q0
                "initial-state with 'b' input is still initial-state")
  ; interim-state tests
  (check-equal? (next-state JUST_INITIALIZED "c") Q1
                "initial-state with 'c' input should be processed to interim-state")
  (check-equal? (next-state AC_INPUT "a") Q1
                "interim-state with 'a' input is still interim-state")
  ; accepting-state tests
  (check-equal? (next-state ACD_INPUT "e") Q2
                "accepting-state with 'e' input is still accepting-state")
  (check-equal? (next-state AC_INPUT "d") Q2
                "interim-state with 'd' input should be processed to accepting-state")
  ; error-state tests
  (check-equal? (next-state AC_INPUT "e") Q3
                "interim-state with 'e' input should be processed to error-state")
  (check-equal? (next-state ACD_INPUT "a") Q3
                "accepting-state with 'a' input should be processed to error-state")
  (check-equal? (next-state JUST_INITIALIZED "f") Q3
                "initial-state with 'f' input should be processed to error-state"))

;; STRATEGY: Dividing into cases on 'in' input
(define (next-state st in)
  (cond
    [(or (no-state-change? st in) (no-accepting-state-change? st in)) st]
    [(initial-to-interim? st in) Q1]
    [(interim-to-accepting? st in) Q2]
    [else Q3]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no-state-change? : State MachineInput -> Boolean
;; GIVEN: a State st of the machine and a MachineInput in
;; RETURNS: true if initial-state or interim-state should remain unchanged, else - false

;; EXAMPLE\TEST:
(begin-for-test
  ; positive tests
  (check-equal? (no-state-change? Q0 "a")
                true
                "initial-state will not change with 'a' input")
  (check-equal? (no-state-change? Q1 "b")
                true
                "interim-state will not change with 'b' input")
  ; negative tests
  (check-equal? (no-state-change? Q0 "c")
                false
                "initial-state will change with 'c' input")
  (check-equal? (no-state-change? Q1 "e")
                false
                "interim-state will change with 'e' input")
  (check-equal? (no-state-change? Q2 "b")
                false
                "This is not initial-state nor interim-state")
  (check-equal? (no-state-change? Q3 "b")
                false
                "This is not initial-state nor interim-state"))

;; STRATEGY: Combine simpler functions
(define (no-state-change? st in)
  [and (or (string=? st Q0)
           (string=? st Q1))
       (or (string=? in "a")
            (string=? in "b"))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no-accepting-state-change? : State MachineInput -> Boolean
;; GIVEN: a State st of the machine and a MachineInput in
;; RETURNS: true if accepting-state is not processed to error-state, else - false

;; EXAMPLE\TEST:
(begin-for-test
  ; positive test
  (check-equal? (no-accepting-state-change? Q2 "f")
                true
                "accepting-state will not change with 'f' input")
  ; negative tests
  (check-equal? (no-accepting-state-change? Q2 "b")
                false
                "accepting-state will change with 'b' input")
  (check-equal? (no-accepting-state-change? Q3 "e")
                false
                "Given state is not accepting-state"))

;; STRATEGY: Combine simpler functions
(define (no-accepting-state-change? st in)
  [and (string=? st Q2)
       (or (string=? in "e")
           (string=? in "f"))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-to-interim? : State MachineInput -> Boolean
;; GIVEN: a State st of the machine and a MachineInput in
;; RETURNS: true if initial-state should be processed to interim-state, else - false

;; EXAMPLE\TEST:
(begin-for-test
  ; positive test
  (check-equal? (initial-to-interim? Q0 "c")
                true
                "initial-state with 'c' input should be processed to interim-state")
  ; negative tests
  (check-equal? (initial-to-interim? Q0 "d")
                false
                "initial-state with 'd' input won't be processed to interim-state")
  (check-equal? (initial-to-interim? Q1 "c")
                false
                "This is not initial-state"))

;; STRATEGY: Combine simpler functions
(define (initial-to-interim? st in)
  [and (string=? st Q0) (string=? in "c")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interim-to-accepting? : State MachineInput -> Boolean
;; GIVEN: a State st of the machine and a MachineInput in
;; RETURNS: true if interim-state should be processed to accepting-state, else - false

;; EXAMPLE\TEST:
(begin-for-test
  ; positive test
  (check-equal? (interim-to-accepting? Q1 "d")
                true
                "interim-state with 'd' input should be processed to accepting-state")
  ; negative tests
  (check-equal? (interim-to-accepting? Q1 "e")
                false
                "interim-state with 'e' input won't be processed to accepting-state")
  (check-equal? (interim-to-accepting? Q2 "d")
                false
                "This is not interim-state"))

;; STRATEGY: Combine simpler functions
(define (interim-to-accepting? st in)
  [and (string=? st Q1) (string=? in "d")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
;; Most of the given constatnts are test constants. They are using initial-state and
;  next-state functions, so they can be put only after next-state function. Putting them
;  in the middle of file will make code less readable. That is why constants are put in
;  the end of file.

; My
(define JUST_INITIALIZED (initial-state 1))
(define AC_INPUT (next-state (next-state JUST_INITIALIZED "a") "c"))
(define ACD_INPUT (next-state AC_INPUT "d"))

; From task, negative
(define ABC_INPUT (next-state (next-state (next-state JUST_INITIALIZED "a") "b") "c"))
(define ABDBCEF_INPUT (next-state (next-state (next-state (next-state (next-state (next-state (next-state JUST_INITIALIZED "a") "b") "d") "b") "c") "e") "f"))
(define ACBDED_INPUT (next-state (next-state (next-state (next-state (next-state (next-state (next-state JUST_INITIALIZED "a") "c") "b") "d") "d") "e") "d"))

; From task, positive
(define CD_INPUT (next-state (next-state JUST_INITIALIZED "c") "d"))
(define ABCD_INPUT (next-state ABC_INPUT "d"))
(define ABCBDEF_INPUT (next-state (next-state (next-state(next-state ABC_INPUT "b") "d") "e") "f"))
(define AACBADF_INPUT (next-state (next-state (next-state (next-state (next-state (next-state (next-state JUST_INITIALIZED "a") "a") "c") "b") "a") "d") "f"))