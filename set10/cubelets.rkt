#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; cubelets.rkt
;; -----------------------------------------------
;; Your boss at the toy factory asks you to produce
;; a new toy inspired by Cubelets, which are square
;; blocks that stick together
;;
;; Use (run 1) to start the simulation at a rate
;; of 1 seconds per tick 

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(check-location "10" "cubelets.rkt")

(provide 
 make-block
 Block<%>
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Constants
;; -----------------------------------------------

(define CANVAS_WIDTH 500)
(define CANVAS_HEIGHT 600)
(define CANVAS_CENTER_X (/ CANVAS_WIDTH 2))
(define CANVAS_CENTER_Y (/ CANVAS_HEIGHT 2))

(define BLOCK_COLOR "green")
(define SELECTED_BLOCK_COLOR "red")
(define BLOCK_SIZE 20)
(define BLOCK_STYLE "outline")
(define NEW-BLOCK-KEY "b")
(define TEST-SCENE (empty-scene 300 300))
(define ZERO_POINT 0)
(define DISTANCE_TO_BLOCK_EDGE 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; DATA DEFINITIONS
;; -----------------------------------------------

;; ListOfBlock<%> is a list of blocks present in the Block<%>,
;; A ListOfBlock<%> (LOB) is either
;; -- empty               INTERPRETATION : An empty ListOfBlock
;; -- (cons Block<%> LOB) INTERPRETATION : A ListOfBlock 
;; TEMPLATE:
;; lob-fn : lob -> ??
 #;(define (lob-fn lob)
  (cond
    [(empty? lob) ...]
    [else ... (first lob)
          lob-fn(rest lob)]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Interfaces and classes
;; -----------------------------------------------

;; Every object that is created must implement the Block<%> interface.
(define Block<%>
  (interface(SWidget<%>)   

    ;; x-coordinate of the centre of the block.
    block-x

    ;; y-coordinate of the centre of the block.
    block-y

    ;; -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    get-team

    ;; Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    add-teammate

    ;; NonNegInteger NonNegInteger  -> Void
    ;; GIVEN : The x and y coordinates of the block
    ;; EFFECT : Updates the coordinates of the block with the given coordinates. 
    set-block-coordinates

    ;; ListOfBlock -> Void
    ;; GIVEN : The list of block 'lblock'
    ;; EFFECT : Updates the blocks in the block with given list of blocks
    update-blocks-with))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Block is a
;;         (new Block% [x NonNegInteger] [y NonNegInteger] [selected? Boolean]
                       [blocks ListOfSWidgets] [teammates ListOfSWidgets])
;; A Block is a part of cubelet which can be dragged and joined to other cubelet in
;; the world.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
 
(define Block%
  (class* object% (Block<%>)      

    ;; ------------------
    ;; Fields
    ;; ------------------

    ;; The x and y of the last mouse down event that was inside
    ;; the square and the center of the square
    (init-field saved-mx saved-my)

   
    ;; x is the x coordinate of the  centre of block
    ;; y is the y coordinate of the centre of block
    ;; selected? is the Block selected or not.
    (init-field x y selected?)

    
    ;; blocks is a list of all the other blocks except this block.
    ;; teammates is a list of blocks that are teammates of this block.
    (init-field blocks teammates)

    
    ; dx      : Mouse click location, x-coordinate.
    ; dy      : Mouse click location, y-coordinate.
    (init-field [dx ZERO_POINT] [dy ZERO_POINT])

 
    (super-new)

    ;; ------------------
    ;; Methods
    ;; ------------------
   
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: the state of this block that should follow the
    ;;         key event
    ;; EXAMPLES:
    ;;  let block be (make-block 30 30 '())
    ;;  (send block after-key-event) => block
     ;; STRATEGY: Cases on key event kev
    (define/public (after-key-event kev)
      (cond 
        [(key=? kev NEW-BLOCK-KEY)
         (begin (set! blocks (cons (make-block saved-mx saved-my blocks) blocks))
                (update-references-for-all-blocks))]              
        [else this]))

   
    ;; update-references-for-all-blocks :  -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: Updates the list of blocks of a block
    ;; STRATEGY: Use of hOF for each on blocks
    (define (update-references-for-all-blocks)
      (for-each (lambda (b) (send b update-blocks-with blocks))
                blocks))


    ;; update-blocks-with : ListOfBlock<%> -> Void
    ;; GIVEN: list of blocks
    ;; EFFECT: Sets current list of blocks similar to given,
    ;;         except that self reference is removed
    ;; STRATEGY:Combining simpler functions
    (define/public (update-blocks-with lst-blocks)
      (set! blocks (set-minus lst-blocks this)))

   
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a drag event
    ;; EFFECT:  updates the state of this object that should follow a
    ;;          mouse drag event at the given location.
    ;; STRATEGY:Use of HOF for-each on (cons this blocks)
    (define/public (after-drag mx my)
     (for-each (lambda (b) (send b block-after-drag mx my))
               (cons this blocks)))

   
    ;; block-after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location of a drag event
    ;; EFFECT:  updates the state of this object that should follow a
    ;;          mouse drag event at the given location.
    ;; STRATEGY:Cases on selected?
    (define/public (block-after-drag mx my)
      (if selected?
          (local
            ((define old-x x)
             (define old-y y)
             (define old-team teammates))
            (begin
              (set! x (- mx dx))
              (set! y (- my dy))
              (set-coordinates-of-teammates (- x old-x) (- y old-y))
              (set! teammates (append teammates (get-intersection-of-teammates old-team)))
              (set-teammates-for-each-block)))
          this))

   
    ;; set-coordinates-of-teammates: NonNegInteger NonNegInteger -> void
    ;; GIVEN: x and y offset of a block.
    ;; EFFECT:sets the position x, y coordinate of each block in teammates
    ;; STRATEGY: Use HOF for-each on teammates
    (define (set-coordinates-of-teammates dx dy)
      (for-each
       (lambda (sobj) (send sobj set-block-coordinates dx dy))
       teammates))                         

   
    ;; set-block-coordinates: NonNegInteger NonNegInteger -> Void
    ;; GIVEN: Distance moved by this selected block after drag
    ;; EFFECT: sets the new center position of the block according to dx, dy.
    ;; STRATEGY: Combining simpler functions.
    (define/public (set-block-coordinates dx dy)
      (begin
        (set! x (+ x dx))
        (set! y (+ y dy))))

    
    ;; get-intersection-of-teammates ListOfBlock<%> -> ListOfBlock<%>
    ;; GIVEN: ListOfBlock<%> 'old-team' representing the teammates
    ;; RETURN: List of block which are the teammates of the intersected
    ;; block with this block
    ;; STRATEGY: Combining simpler functions.
    (define (get-intersection-of-teammates old-team)
      (get-teammates-of-intersecting-blocks (intersecting-blocks old-team)))

    
    ;; get-teammates-of-intersecting-blocks ListOfBlock<%> -> ListOfBlock<%>
    ;; GIVEN: ListOfBlock<%> lob 
    ;; RETURN: List of block which are the teammates of the intersected
    ;; block with this block
    ;; STRATEGY: Use Template of LOB on lob
    (define (get-teammates-of-intersecting-blocks lob)
      (cond 
        [(empty? lob) empty]
        [else (set-union (cons (first lob) (send (first lob) get-team))
                    (get-teammates-of-intersecting-blocks (rest lob)))]))

    
    ;; intersecting-blocks: ListOfBlock<%> -> ListOfBlock<%>
    ;; GIVEN: ListOfBlock<%> 'old-tm' representing the teammates
    ;; RETURN: List of block which intersect(any of the coordinates overlap)
    ;; with this block
    ;; STRATEGY: Combining simpler functions.
    (define (intersecting-blocks old-team)
      (filter
       (lambda (b) (intersects? (send b block-x) (send b block-y)))
       (set-diff blocks old-team)))

    
    ;; set-teammates-for-each-block: -> void
    ;; GIVEN: No Argument
    ;; EFFECT: Updates the teammates of member of teammates of this block.
    ;; STRATEGY: Combining simpler functions.
    (define (set-teammates-for-each-block)
      (for-each
       (lambda (sobj) (send sobj add-teammates-of-block
                            (set-minus (cons this teammates)
                                       sobj)))teammates))

    
    ;; add-teammates-of-block : ListOfBlock<%> -> Void
    ;; GIVEN: new block that should be added to team
    ;; EFFECT: adds the given block to this block's team
    ;; STRATEGY: Use simpler functions   
    (define/public (add-teammates-of-block blocks)
      (set! teammates (set-union teammates (set-diff blocks teammates))))

    
    ;; add-teammate : Block<%> -> Void
    ;; GIVEN: new block that should be added to team
    ;; EFFECT: adds the given block to this block's team
    ;; STRATEGY: Use simpler functions
    (define/public (add-teammate block)
      (begin
        (set! teammates
              (append teammates (get-teammates-of-intersecting-blocks (list block))))
        (set-teammates-for-each-block)))

    
    ;; intersects? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: the x and y coordinates of another block
    ;; RETURNS: true iff block in given coordinates is intersecting this block
    ;;          else - false
    ;; STRATEGY: Combine simpler functions
    (define/public (intersects? other-x other-y)
      (and (>= (+ other-x BLOCK_SIZE) x (- other-x BLOCK_SIZE))
           (>= (+ other-y BLOCK_SIZE) y (- other-y BLOCK_SIZE))))   

    
    ;; is-selected? : -> Boolean
    ;; RETURNS: true if this block is selected, else - false
    (define/public (is-selected?) selected?)


    ;; after-button-down : NonNegInteger NonNegInteger -> Void
    ;; after-button-up : NonNegInteger NonNegInteger -> Void
    ;; GIVEN: a location (x and y coordinates)
    ;; EFFECT: updates all blocks to the state that they should
    ;;         follow after mouse button down and mouse button up
    ;;         events at the given location.
    ;; STRATEGY: Use HOF for-each on blocks
    (define/public (after-button-down mx my)
      (begin (set! saved-mx mx)
             (set! saved-my my)
             (for-each
              (lambda (block) (send block get-block-after-button-down mx my))
              (cons this blocks))))
    
    (define/public (after-button-up mx my)
      (begin (set! saved-mx mx)
             (set! saved-my my)
             (for-each
              (lambda (block) (send block get-block-after-button-up mx my))
              (cons this blocks))))


    ;; get-block-after-button-down : NonNegInteger NonNegInteger -> Void
    ;; get-block-after-button-up : NonNegInteger NonNegInteger -> Void
    ;; GIVEN: a location (x and y coordinates)
    ;; EFFECT: updates state of this block that should 
    ;;         follow mouse button down and mouse button up
    ;;         events at the given location.
    (define/public (get-block-after-button-down mx my)
      (begin (set! dx (- mx x))
             (set! dy (- my y))
             (set! saved-mx mx)
             (set! saved-my my)
             (set! selected? (in-block? mx my))))
    
    (define/public (get-block-after-button-up mx my)
      (begin (set! selected? false)
             (set! saved-mx mx)
             (set! saved-my my)))

    
    ;; after-tick : Time -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: the state of this block that should follow the
    ;;         tick. The block should be the same since
    ;;         squares ignore key events
    ;; EXAMPLES:
    ;;  let block be (make-block 30 30 '())
    ;;  (send block after-tick) => block
    (define/public (after-tick) this) 

    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: Scene similar to given, except that all block that are created are
    ;;          placed on it
    ;; WHERE: block color is red if block is selected, else green
    ;; STRATEGY: Use HOF foldr on blocks
    (define/public (add-to-scene scene)
      (foldr
       (lambda (block scene) (block-to-scene block scene))
       scene
       blocks))

    
    ;; block-to-scene : Block% Scene -> Scene
    ;; GIVEN: a block to place and a scene
    ;; RETURNS: Scene similar to given, except that block is placed on it
    ;; WHERE: block color is red if block is selected, else green
    ;; STRATEGY: Use simpler functions
    (define/public (block-to-scene block scene)
      (begin
        (define color (if (send block is-selected?)
                          SELECTED_BLOCK_COLOR
                          BLOCK_COLOR))
        (place-image (square BLOCK_SIZE BLOCK_STYLE color)
                     (send block block-x) (send block block-y)
                     scene)))

   
    ;; block-x: -> Integer
    ;; block-y: -> Integer
    ;; RETURNS: the x or y coordinates of this block
    (define/public (block-x) x)    
    (define/public (block-y) y)

    
    ;; get-team: -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    (define/public (get-team) teammates)

    
    ;; in-block?: NonNegInt NonNegInt -> Boolean
    ;; GIVEN: the x and y coordinates
    ;; RETURNS: true iff the given coordinates are inside this block
    ;; STRATEGY: Combine simpler functions
    (define/public (in-block? mx my)
      (and
       (<= (- x  DISTANCE_TO_BLOCK_EDGE) mx (+ x DISTANCE_TO_BLOCK_EDGE))
       (<= (- y  DISTANCE_TO_BLOCK_EDGE) my (+ y DISTANCE_TO_BLOCK_EDGE))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; Helper functions
;; -----------------------------------------------
;; make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates
;; NOTE: it is up to you as to whether you use the third argument or
;;       not. Some implementations may use the third argument; others may not.
(define (make-block mx my blocks)
  (new Block%
       [saved-mx mx] [saved-my my]
       [x mx] [y my]
       [selected? false]
       [blocks blocks]
       [teammates empty]))


;; -----------------------------------------------
;; run : NonNegInt -> Void
;; GIVEN: a frame rate (in ticks)
;; EFFECT: creates and runs world at the given frame rate
;; TEST: calls big-bang - not testable
;; EXAMPLES: calls run method, from WidgetWorks, which calls big-bang
;; STRATEGY: Combine simpler functions
(define (run rate)
  (local
    ((define the-world (make-world CANVAS_WIDTH CANVAS_HEIGHT)))
    (begin
      (send the-world add-stateful-widget
            (make-block CANVAS_CENTER_X CANVAS_CENTER_Y '()))  
      (send the-world run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------
;; TESTS
;; -----------------------------------------------

;; Examples and test for scene, button down, button up, blocks intersection, add teammate
(begin-for-test
  (local
    ((define test-block (make-block 150 150 empty))
     (define test-block2 (make-block 50 50 empty))
     (define selected-block
       (new Block% [x 100] [y 100] [selected? true]
            [saved-mx 0] [saved-my 0]
            [blocks (list test-block)] [teammates (list test-block)])))
    (check-equal?
     (send test-block block-to-scene test-block TEST-SCENE)
     (place-image (square BLOCK_SIZE BLOCK_STYLE BLOCK_COLOR)
                  150 150 TEST-SCENE)
     "Scene should have a green block in center")
    (check-equal?
     (send selected-block block-to-scene selected-block TEST-SCENE)
     (place-image (square BLOCK_SIZE BLOCK_STYLE SELECTED_BLOCK_COLOR)
                  100 100 TEST-SCENE)
     "Scene should have a red block left to scene center")
    (check-equal?
     (send selected-block add-to-scene TEST-SCENE)
     (place-image (square BLOCK_SIZE BLOCK_STYLE BLOCK_COLOR)
                  150 150 TEST-SCENE)
     "Scene should have a green block in center")
    (send selected-block after-button-down 150 150)
    (check-equal?
     (send (first (send selected-block get-team)) is-selected?)
     true
     "Test block should be selected, because it has same coordinates as a mouse")
    (send selected-block after-button-up 150 150)
    (check-equal?
     (send (first (send selected-block get-team)) is-selected?)
     false
     "Test block should be un selected, because - mouse button up")
    (send test-block after-tick)
    (check-equal?
     (send test-block intersects? 170 170)
     true
     "Test block will intersect with block in given coordinates")
    (check-equal?
     (send test-block intersects? 149 171)
     false
     "Test block will not intersect with block in given coordinates")))


(define test-block3 (make-block 100 100 '()))
(define test-block-after-button-down (send test-block3 after-button-down 100 100))
(define test-block-after-drag (send test-block3 after-drag 150 150))
(define test-block-after-key-event (send test-block3 after-key-event NEW-BLOCK-KEY))
(define test-block-after-invalid-key-event (send test-block3 after-key-event "v"))
(begin-for-test
    (begin test-block-after-drag
           (check-equal? (send test-block3 block-x) 150 "block not dragged correctly"))
    (begin test-block-after-key-event
           (check-equal? (send test-block3 block-x) 150 "block not created correctly"))  
    (begin test-block-after-invalid-key-event
           (check-equal? (send test-block3 block-x) 150 "block created inspite
of invalid key event")))

(define test-block4 (make-block 20 30 '()))
(define test-block5 (make-block 50 80 '()))
(define test-block6 (make-block 150 80 '()))
(define test-block7 (make-block 10 70 '()))
(define test-block1-add-teammate (send test-block4 add-teammate test-block6))
(define test-block2-add-teammate (send test-block5 add-teammate test-block7))
(define test-block3-add-teammate (send test-block4 add-teammate test-block5))
(begin-for-test
           (check-equal? (length (send test-block4 get-team)) 3
                         "Teammates not added correctly")
           (check-equal? (length (send test-block5 get-team)) 3
                         "Teammates not added correctly")
           (check-equal? (length (send test-block7 get-team)) 3
                         "Teammates not added correctly")) 
(define test-block8 (make-block 100 100 '()))
(define test-block9 (make-block 110 110 '()))
(define test-block4-add-teammate (send test-block8 add-teammate test-block9))

(begin-for-test (begin test-block8
           (check-equal? (send (first (send test-block8 get-team)) block-x) 110 "")))
(define test-block10 (make-block 200 200 (list test-block8 test-block9)))
(define test-block11 (make-block 210 210 '()))
(define test-block10-ADD-TEAMMATES (send test-block10 add-teammate test-block11))
(define test-block10-ADD-TEAMMATES-SELECT (send test-block10  after-button-down 200 200))
(define team-intersecting (send test-block10 after-drag 120 110))
(begin-for-test (begin team-intersecting
                       (check-equal? (length (send test-block8 get-team)) 3
                         "Teams not intersecting correctly"))) 

  




