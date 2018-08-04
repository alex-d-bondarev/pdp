;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(check-location "02" "editor.rkt")

(provide
  make-editor
  editor-pre
  editor-post
  edit
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct editor (pre post))

;; INTERPRETATION:

;; (make-editor pre post) represents an editor 
;; with text to the left of cursor pre
;; and with text to the right of cursor post

;; TEMPLATE:
;; editor-fn : Editor -> ??
#;(define (editor-fn ed)
    (... (editor-pre ed)
         (editor-post ed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; suitcase-fits-scaner: Editor KeyEvent -> Editor
;; GIVEN: an Editor ed and a KeyEvent key
;; RETURNS: Editor that was changed by KeyEvent

;; EXAMPLES/TESTS:

(begin-for-test
  ; simple inputs
  (check-equal?
   (edit (make-editor "Hello" "World") " ")
   (make-editor "Hello " "World"))
  (check-equal?
   (edit (make-editor "Ger" "nimo") "o")
   (make-editor "Gero" "nimo"))
  (check-equal?
   (edit (make-editor "Hey Br" "") "o")
   (make-editor "Hey Bro" ""))
  (check-equal?
   (edit (make-editor "" "k") "O")
   (make-editor "O" "k"))
  ; testing backspace
  (check-equal?
   (edit (make-editor "Geronimo" "") "\b")
   (make-editor "Geronim" "") )
  (check-equal?
   (edit (make-editor "Hello o" "World") "\b")
   (make-editor "Hello " "World"))
  (check-equal?
   (edit (make-editor "" "Hi") "\b")
   (make-editor "" "Hi"))
   ; testiing left key
  (check-equal?
   (edit (make-editor "Hello " "World") "left")
   (make-editor "Hello" " World"))
  (check-equal?
   (edit (make-editor "" "Empty") "left")
   (make-editor "" "Empty"))
  (check-equal?
   (edit (make-editor "Empty" "") "left")
   (make-editor "Empt" "y"))
   ; testing right key
  (check-equal?
   (edit (make-editor "Hello" " World") "right")
   (make-editor "Hello " "World"))
  (check-equal?
   (edit (make-editor "" "Empty") "right")
   (make-editor "E" "mpty"))
  (check-equal?
   (edit (make-editor "Empty" "") "right")
   (make-editor "Empty" "")))

;; STRATEGY: Combine simpler functions
(define (edit ed key)
  (cond
    [(key=? "\b" key) (backspace-button ed)]
    [(key=? "left" key) (left-button ed)]
    [(key=? "right" key) (right-button ed)]
    [else (other-button ed key)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; backspace-button : Editor -> Editor
;; GIVEN: an Editor ed
;; RETURNS: given Editor, exept that 1 symbol is deleted to the left of cursor
;; EXAMPLES:
;; (backspace-button (make-editor "ab" "cd")) = (make-editor "a" "cd")
;; (backspace-button (make-editor "" "abc")) = (make-editor "" "abc")
;; (backspace-button (make-editor "abc" "")) = (make-editor "ab" "")
;; STRATEGY: Use template for Editor on ed
(define (backspace-button ed)
  (cond [(equal? (editor-pre ed) "") ed]
        [else (make-editor (reduce-editor-pre ed) (editor-post ed))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; left-button : Editor -> Editor
;; GIVEN: an Editor ed
;; RETURNS: given Editor, exept that cursor is moved one position to the left
;; EXAMPLES:
;; (left-button (make-editor "ab" "cd")) = (make-editor "a" "bcd")
;; (left-button (make-editor "" "abc")) = (make-editor "" "abc")
;; STRATEGY: Use template for Editor on ed
(define (left-button ed)
  (cond [(equal? (editor-pre ed) "") ed]
        [else (make-editor (reduce-editor-pre ed)
                           (string-append (last-char (editor-pre ed))
                                          (editor-post ed)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; right-button : Editor -> Editor
;; GIVEN: an Editor ed
;; RETURNS: given Editor, exept that cursor is moved one position to the right
;; EXAMPLES:
;; (right-button (make-editor "ab" "cd")) = (make-editor "abc" "d")
;; (right-button (make-editor "abc" "")) = (make-editor "abc" "")
;; STRATEGY: Use template for Editor on ed
(define (right-button ed)
  (cond [(equal? (editor-post ed) "") ed]
        [else (make-editor (string-append (editor-pre ed)
                                          (first-char (editor-post ed)))
                           (reduce-editor-post ed))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; other-button : Editor KeyEvent -> Editor
;; GIVEN: an Editor ed and a KeyEvent key
;; RETURNS: given Editor, exept that key is added to the left of cursor
;; EXAMPLES:
;; (other-button (make-editor "a" "c") "b") = (make-editor "ab" "c")
;; (other-button (make-editor "" "c") "b") = (make-editor "b" "c")
;; STRATEGY: Use template for Editor on ed
(define (other-button ed key)
  (make-editor (string-append (editor-pre ed) key)
               (editor-post ed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reduce-editor-pre : Editor -> String
;; RETURNS: given editor, except that last character
;; from the left of a cursor is deleted 
;; EXAMPLES:
;; (reduce-editor-pre (make-editor "ab" "cd")) = "a"
;; STRATEGY: Combine simpler functions
(define (reduce-editor-pre ed)
  (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reduce-editor-post : Editor -> String
;; GIVEN: Editor
;; WHERE: text to the right of cursor is not empty
;; RETURNS: text to the right of cursor from given editor, except that
;; first character from right of a cursor is deleted 
;; EXAMPLES:
;; (reduce-editor-post (make-editor "ab" "cd")) = "d"
;; STRATEGY: Combine simpler functions
(define (reduce-editor-post ed)
  (substring (editor-post ed) 1 (string-length (editor-post ed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; first-char : String -> 1String
;; GIVEN: String str
;; WHERE: given String is not empty
;; RETURNS: first 1String from the given String
;; EXAMPLE:
;; (first-char "qwerty") = "q"
;; STRATEGY: Combine simpler functions
(define (first-char str)
  (substring str 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; last-char : String -> 1String
;; GIVEN: String str
;; WHERE: given String is not empty
;; RETURNS: last 1String from the given string
;; EXAMPLE:
;; (last-char "qwerty") = "y"
;; STRATEGY: Combine simpler functions
(define (last-char str)
  (substring str
             (- (string-length str) 1)
             (string-length str)))