;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(check-location "02" "coffee-machine.rkt")

(provide
  initial-machine
  machine-next-state
  machine-output
  machine-remaining-coffee
  machine-remaining-chocolate
  machine-bank
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
(define COFFEE_PRICE 150)
(define HOT_CHOCOLATE_PRICE 60)
(define OUT_OF_ITEM_MESSAGE "Out of Item")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;A CustomerInput is one of
;-- a PosInt          interp: insert the specified amount of money, in cents
;-- "coffee"          interp: request a coffee
;-- "hot chocolate"   interp: request a hot chocolate
;-- "change"          interp: return all the unspent money that the
;                             customer has inserted


;A MachineOutput is one of
;-- "coffee"         interp: machine dispenses a cup of coffee
;-- "hot chocolate"  interp: machine dispenses a cup of hot chocolate
;-- "Out of Item"    interp: machine displays "Out of Item"
;-- a PosInt         interp: machine releases the specified amount of
;                            money, in cents
;-- "Nothing"        interp: the machine does nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct machine (remaining-coffee remaining-chocolate bank change prep-output))

;; A MachineState is a (make-machine NonNegInt NonNegInt NonNegInt NonNegInt MachineOutput)

;; INTERPRETATION:

;; (make-machine cf cho b cha out) represents a MachineState
;; with quantity of remaining coffee cups cf
;; with quantity of remaining hot chocolate cups cho
;; with bank, that contains money from purchase b in cents
;; with change, that contains money put by customer cha in cents
;; and with machine output, prepared by changing state


;; TEMPLATE:
;; machine-fn : MachineState -> ??
#; (define (machine-fn ma)
     (... (machine-remaining-coffee ma)
          (machine-remaining-chocolate ma)
          (machine-bank ma)
          (machine-change ma)
          (machine-prep-output ma)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-remaining-coffee : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of coffee left in the machine

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (machine-remaining-coffee (make-machine 1 2 3 10 "Nothing")) 1
   "Only 1 cup of coffee remains"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-remaining-chocolate : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of hot chocolate left in the machine

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (machine-remaining-chocolate (make-machine 1 2 3 10 "Nothing")) 2
   "Only 2 cups of hot chocolate remain"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (machine-bank (make-machine 1 2 3 10 "Nothing")) 3
   "Only 3 cents are in the bank"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of cups of coffee and of hot chocolate
;; RETURNS: the state of a machine loaded with the given number of cups
;  of coffee and of hot chocolate, with an empty bank, empty change, and 'Nothing'
;  for output.

;; EXAMPLES/TESTS :
(begin-for-test
  (check-equal?
   (initial-machine 10 15) (make-machine 10 15 0 0 "Nothing")
   "Initialized machine should contain 10 coffee and 15 chocolate cups. Nothing else."))

;; Strategy: Combine simpler functions
(define (initial-machine n-coffee n-chocolate)
  (make-machine n-coffee n-chocolate 0 0 "Nothing"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;  input

;; EXAMPLES/TESTS :
(begin-for-test
  ; a PosInt input test
  (check-equal?
   (machine-next-state (make-machine 1 2 3 4 "Nothing") 25)
    (make-machine 1 2 3 29 "Nothing")
    "Adding 25 cents to machine with 4, will result in change=29")
  ; a "coffee" input test
  (check-equal?
   (machine-next-state (make-machine 10 10 300 150 "Nothing") "coffee")
   (make-machine 9 10 450 0 "coffee")
   "150 cents is enough to make coffee")
  ; a "hot chocolate" input test
  (check-equal?
   (machine-next-state (make-machine 10 10 300 60 "coffee") "hot chocolate")
   (make-machine 10 9 360 0 "hot chocolate")
   "60 cents is enough to make hot chocolate")
  ; a "change" input test
  (check-equal?
   (machine-next-state (make-machine 10 10 300 60 "coffee") "change")
   (make-machine 10 10 300 0 60)
   "change is reduced to 0 and output should equal old change value 60"))


;; Strategy: Dividing into cases on 'ci' input
(define (machine-next-state ma ci)
  (cond [(number? ci) (increase-change ma ci)]
        [(string=? ci "coffee") (prepare-coffee ma)]
        [(string=? ci "hot chocolate") (prepare-hot-chocolate ma)]
        [(string=? ci "change") (prepare-change ma)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;  customer input

;; EXAMPLES/TESTS :
(begin-for-test
  ; a PosInt input test
  (check-equal?
   (machine-output (make-machine 10 10 300 60 "coffee") 123)
   "Nothing"
   "No output is specified for CustomerInput = PosInt")  
  ; a "coffee" input tests
  (check-equal?
   (machine-output (make-machine 10 10 300 60 "coffee") "coffee")
   "coffee"
   "Coffee was prepared")  
  (check-equal?
   (machine-output (make-machine 10 10 300 300 "Nothing") "coffee")
   "coffee"
   "There is enough input money for coffee") 
  (check-equal?
   (machine-output (make-machine 10 10 300 0 "Nothing") "coffee")
   "Nothing"
   "There is not enough input money for coffee") 
  ; a "hot chocolate" input tests
  (check-equal?
   (machine-output (make-machine 10 10 300 60 "hot chocolate") "hot chocolate")
   "hot chocolate"
   "hot chocolate was prepared")  
  (check-equal?
   (machine-output (make-machine 10 10 300 300 "Nothing") "hot chocolate")
   "hot chocolate"
   "There is enough input money for hot chocolate") 
  (check-equal?
   (machine-output (make-machine 10 10 300 0 "Nothing") "hot chocolate")
   "Nothing"
   "There is not enough input money for coffee") 
  ; a "change" input tests
  (check-equal?
   (machine-output (make-machine 10 10 300 0 "Nothing") "change")
   0
   "There is no change left") 
  (check-equal?
   (machine-output (make-machine 10 10 300 0 50) "change")
   50
   "50 cents were prepared when customer changed state using CustomerInput = change"))

;; Strategy: Dividing into cases on 'ci' input
(define (machine-output ma ci)
  (cond [(number? ci) "Nothing"]
        [(string=? ci "coffee") (coffee-output ma)]
        [(string=? ci "hot chocolate") (hot-chocolate-output ma)]
        [(string=? ci "change") (change-output ma)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; increase-change : MachineState PosInt -> MachineState
;; GIVEN: a machine state ma and an amount of money mon
;; RETURNS: a MachineState that is with increased change

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (increase-change (make-machine 1 2 3 4 "Nothing") 25)
   (make-machine 1 2 3 29 "Nothing")
   "Adding 25 cents to test machine with 4, will result in change=29"))


;; Strategy: Use template for MachineState on ma
(define (increase-change ma mon)
  (make-machine (machine-remaining-coffee ma)
                (machine-remaining-chocolate ma)
                (machine-bank ma)
                (+ (machine-change ma) mon)
                "Nothing"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prepare-coffee : MachineState -> MachineState
;; GIVEN: a machine state ma
;; RETURNS: in case input change is enough and there are remaining cups of coffee:
;  a MachineState that is with descreased number of remaining coffee cups and prepare
;  'coffee' output. Otherwise same MachineState

;; EXAMPLE/TEST :
(begin-for-test
  ; positive test
  (check-equal?
   (prepare-coffee (make-machine 1 2 3 200 "Nothing"))
   (make-machine 0 2 153 50 "coffee")
   "200 cents is enough to make coffee")
  ; negative tests
  (check-equal?
   (prepare-coffee (make-machine 1 2 3 100 "Nothing"))
   (make-machine 1 2 3 100 "Nothing")
   "100 cents is not enough to make coffee")
  (check-equal?
   (prepare-coffee (make-machine 0 2 3 200 "Nothing"))
   (make-machine 0 2 3 200 "Nothing")
   "No more coffee is left"))


;; Strategy: Use template for MachineState on ma
(define (prepare-coffee ma)
  (cond [(= (machine-remaining-coffee ma) 0) ma]
        [(< (machine-change ma) COFFEE_PRICE) ma]
        [else (make-machine (- (machine-remaining-coffee ma) 1)
                            (machine-remaining-chocolate ma)
                            (+ (machine-bank ma) COFFEE_PRICE)
                            (- (machine-change ma) COFFEE_PRICE)
                            "coffee")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prepare-hot-chocolate : MachineState -> MachineState
;; GIVEN: a machine state ma
;; RETURNS: in case input change is enough and there are remaining cups of hot chocolate
;  a MachineState that with descreased number of remaining hot chocolate cups and prepare
;  'hot chocolate' output. Otherwise same MachineState

;; EXAMPLE/TEST :
(begin-for-test
  ; positive test
  (check-equal?
   (prepare-hot-chocolate (make-machine 1 2 3 200 "Nothing"))
   (make-machine 1 1 63 140 "hot chocolate")
   "200 cents is enough to make coffee")
  ; negative tests
  (check-equal?
   (prepare-hot-chocolate (make-machine 1 2 3 10 "Nothing"))
   (make-machine 1 2 3 10 "Nothing")
   "100 cents is not enough to make coffee")
  (check-equal?
   (prepare-hot-chocolate (make-machine 1 0 3 200 "Nothing"))
   (make-machine 1 0 3 200 "Nothing")
   "Not enough remaining cups of hot chocolate"))


;; Strategy: Use template for MachineState on ma
(define (prepare-hot-chocolate ma)
  (cond [(< (machine-change ma) HOT_CHOCOLATE_PRICE) ma]
        [(= (machine-remaining-chocolate ma) 0) ma]
        [else (make-machine (machine-remaining-coffee ma)
                            (- (machine-remaining-chocolate ma) 1)
                            (+ (machine-bank ma) HOT_CHOCOLATE_PRICE)
                            (- (machine-change ma) HOT_CHOCOLATE_PRICE)
                            "hot chocolate")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prepare-change : MachineState -> MachineState
;; GIVEN: a machine state ma
;; RETURNS: same machine, exept that change is set to 0 and output is equal to
;  input change amount

;; EXAMPLE/TEST :
(begin-for-test
  ; There is some change
  (check-equal?
   (prepare-change (make-machine 1 2 3 200 "Nothing"))
   (make-machine 1 2 3 0 200)
   "200 cents is prepared")
  ; There is no change
  (check-equal?
   (prepare-change (make-machine 1 2 3 0 "Nothing"))
   (make-machine 1 2 3 0 0 )
   "No change was inputed. 0 to prepare"))


;; Strategy: Use template for MachineState on ma
(define (prepare-change ma)
  (make-machine (machine-remaining-coffee ma)
                (machine-remaining-chocolate ma)
                (machine-bank ma)
                0
                (machine-change ma)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; coffee-output : MachineState -> MachineOutput
;; GIVEN: a machine state ma
;; RETURNS: MachineOutput for coffee, depending on machine state

;; EXAMPLE/TEST :
(begin-for-test
  ; positive tests
  (check-equal?
   (coffee-output (make-machine 1 2 3 200 "Nothing"))
   "coffee"
   "200 cents is enough for making coffee")
  (check-equal?
   (coffee-output (make-machine 1 2 3 0 "coffee"))
   "coffee"
   "Coffee was already prepared")
  ; negative tests
  (check-equal?
   (coffee-output (make-machine 1 2 3 0 "Nothing"))
   "Nothing"
   "Coffee was not prepared and input money is not enough")
  (check-equal?
   (coffee-output (make-machine 0 2 3 200 "Nothing"))
   "Out of Item"
   "0 coffee cups are left"))


;; STRATEGY: Dividing into cases on 'prep-output', 'remaining-coffee' and 'change'
(define (coffee-output ma)
  (cond [(string=? (machine-prep-output ma) "coffee") "coffee"]
        [(= (machine-remaining-coffee ma) 0) "Out of Item"]
        [(< (machine-change ma) COFFEE_PRICE) "Nothing"]
        [(>= (machine-change ma) COFFEE_PRICE) "coffee"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hot-chocolate-output : MachineState -> MachineOutput
;; GIVEN: a machine state ma
;; RETURNS: MachineOutput for hot chocolate, depending on machine state

;; EXAMPLE/TEST :
(begin-for-test
  ; positive tests
  (check-equal?
   (hot-chocolate-output (make-machine 1 2 3 200 "Nothing"))
   "hot chocolate"
   "200 cents is enough for making hot chocolate")
  (check-equal?
   (hot-chocolate-output (make-machine 1 2 3 0 "hot chocolate"))
   "hot chocolate"
   "hot chocolate was already prepared")
  ; negative tests
  (check-equal?
   (hot-chocolate-output (make-machine 1 2 3 0 "Nothing"))
   "Nothing"
   "hot chocolate was not prepared and input money is not enough")
  (check-equal?
   (hot-chocolate-output (make-machine 1 0 3 200 "Nothing"))
   "Out of Item"
   "0 hot chocolate cups are left"))


;; STRATEGY: Dividing into cases on 'prep-output', 'remaining-hot-chocolate' and 'change'
(define (hot-chocolate-output ma)
  (cond [(string=? (machine-prep-output ma) "hot chocolate") "hot chocolate"]
        [(= (machine-remaining-chocolate ma) 0) "Out of Item"]
        [(< (machine-change ma) HOT_CHOCOLATE_PRICE) "Nothing"]
        [(>= (machine-change ma) HOT_CHOCOLATE_PRICE) "hot chocolate"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change-output : MachineState -> MachineOutput
;; GIVEN: a machine state ma
;; RETURNS: MachineOutput for amount of change, depending on machine state

;; EXAMPLE/TEST :
(begin-for-test
  (check-equal?
   (change-output (make-machine 1 2 3 200 "Nothing"))
   200
   "Change is 200")
  (check-equal?
   (change-output (make-machine 1 2 3 0 30))
   30
   "30 is a prepared change"))


;; Strategy: Combine simpler functions
(define (change-output ma)
  (cond [(number? (machine-prep-output ma)) (machine-prep-output ma)]
        [else (machine-change ma)]))

