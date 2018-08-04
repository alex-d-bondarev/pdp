;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This implements a graphical interface for trees that allows the user
;; to create and manipulate trees on a canvas created by Big Bang
;; Code employs HOFs in all necessary and appropriate places

;; start with (run 2)
;; slower variant
;; OR (run 0.5)
;; faster variant
;; smaller input number represents faster simulations 

(require rackunit)
(require 2htdp/universe)
(require htdp/image)
(require "extras.rkt")
(check-location "06" "trees.rkt")

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;; By Frederick and Oleksandr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN CONSTANTS

;;;;;;;;;;;;;;;;;;;;

(define NODE_RADIUS 10)
(define NODE_COLOR "green")
(define NODE_UNSELECTED_MODE "outline")
(define NODE_SELECTED_MODE "solid")
(define UNSELECTED_NODE (circle NODE_RADIUS NODE_UNSELECTED_MODE NODE_COLOR))
(define SELECTED_NODE (circle NODE_RADIUS NODE_SELECTED_MODE NODE_COLOR))
(define NEW-NODE-OFFSET-X (* 3 NODE_RADIUS))
(define NEW-NODE-OFFSET-Y (* 3 NODE_RADIUS))

(define CANVAS_WIDTH 500)
(define CANVAS_HEIGHT 400)
(define CANVAS_CENTER_X (/ CANVAS_WIDTH 2))
(define CANVAS_CENTER_Y (/ CANVAS_HEIGHT 2))
(define EMPTY_CANVAS (empty-scene CANVAS_WIDTH CANVAS_HEIGHT))

(define LINE_COLOR "blue")

(define MOUSE_BUTTON_DOWN "button-down")
(define MOUSE_BUTTON_UP "button-up")
(define MOUSE_DRAG "drag")

(define NEW_TREE_KEY "t")
(define ADD_SON_KEY "n")
(define DELETE_KEY "d")
(define DELETE_LEFT_KEY "l")

;;;;;;;;;;;;;;;;;;;;

;; END CONSTANTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Data definitions

;;;;;;;;;;;;;;;;;;;;

(define-struct node (pos-x pos-y dist-x dist-y selected? movable? sons))
;; (make-node NonNegInt NonNegInt NonNegInt NonNegInt Boolean Boolean ListOfTree)
;; represents a Node where
;; pos-x and pos-y   - are coordinates of a center of a node
;; dist-x and dist-y - represent distance from center of rectangle to mouse point
;;                     in coordinates and if selected or needs to be moved
;; selected?         - describes whether or not the rectangle is selected
;; movable?          - describes whether or not the Node needs to be moved with
;;                     selected parent
;; sons              - is a list of Nodes that are descendants of current Node

;; TEMPLATE:
;; node-fn : Node -> ??
#; (define (node-fn nd)
     (...(node-pos-x nd)
         (node-pos-y nd)
         (node-dist-x nd)
         (node-dist-y nd)
         (node-selected? nd)
         (node-movable? nd)
         (node-sons nd)))

;;;;;;;;;;;;;;;;;;;;

;; A Tree is a Node

;;;;;;;;;;;;;;;;;;;;

;; A ListOfTree (LOT) is one of
;; -- empty
;; -- (cons Tree LOT)

;; TEMPLATE:
;; lot-fn : ListOfTree -> ??
#; (define (lot-fn lot)
     (cond
       [(empty? lot)...]
       [else (...
              (node-fn (first lot))
              (lot-fn (rest lot)))]))

;;;;;;;;;;;;;;;;;;;;

(define-struct world (trees))
;; (make-world ListOfTree) represents a world, where
;; trees - is a list of all created trees

;; TEMPLATE:
;; world-fn : world -> ??
#; (define (world-fn w)
     (...(world-trees w)))

;;;;;;;;;;;;;;;;;;;;

;; END Data definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN Implementation / Tests data

;;;;;;;;;;;;;;;;;;;;

;; For Node:
(define NEW_NODE (make-node CANVAS_CENTER_X NODE_RADIUS 0 0 false false empty))
(define CENTER_NODE (make-node CANVAS_CENTER_X CANVAS_CENTER_Y 0 0 false false empty))
(define BOTOM_NODE (make-node CANVAS_CENTER_X
                              (- CANVAS_HEIGHT NODE_RADIUS)
                              0 0 false false empty))

(define MOVABLE_NODE (make-node (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
                                (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS))
                                (* 0 NODE_RADIUS) (* -4 NODE_RADIUS)
                                false true empty))

(define MOVABLE_NODE_UNSELECTED (make-node (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
                                           (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS))
                                           (* 0 NODE_RADIUS) (* -4 NODE_RADIUS)
                                           false false empty))

(define MOVABLE_NODE_AFTER_DRAG (make-node
                                 (+ CANVAS_CENTER_X (* 2 NODE_RADIUS) NODE_RADIUS)
                                 (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS) NODE_RADIUS)
                                 (* 0 NODE_RADIUS) (* -4 NODE_RADIUS)
                                 false true empty))

(define MOVABLE_NODE2 (make-node (+ CANVAS_CENTER_X (* 5 NODE_RADIUS))
                                 (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS))
                                 (* -3 NODE_RADIUS) (* -4 NODE_RADIUS)
                                 false true empty))

(define MOVABLE_NODE2_UNSELECTED (make-node (+ CANVAS_CENTER_X (* 5 NODE_RADIUS))
                                            (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS))
                                            (* -3 NODE_RADIUS) (* -4 NODE_RADIUS)
                                            false false empty))

(define MOVABLE_NODE2_AFTER_DRAG (make-node
                                  (+ CANVAS_CENTER_X (* 5 NODE_RADIUS) NODE_RADIUS)
                                  (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS) NODE_RADIUS)
                                  (* -3 NODE_RADIUS) (* -4 NODE_RADIUS) false true empty))

(define UNSELECTED_TEST_NODE (make-node (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
                                      (- CANVAS_CENTER_Y (* 2 NODE_RADIUS))
                                      0 0 false false
                                      (list MOVABLE_NODE_UNSELECTED MOVABLE_NODE2_UNSELECTED)))

(define SELECTED_TEST_NODE (make-node (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
                                      (- CANVAS_CENTER_Y (* 2 NODE_RADIUS))
                                      0 0 true false
                                      (list MOVABLE_NODE MOVABLE_NODE2)))

(define SELECTED_TEST_NODE_AFTER_DRAG (make-node
                                       (+ CANVAS_CENTER_X (* 2 NODE_RADIUS) NODE_RADIUS)
                                       (+ (- CANVAS_CENTER_Y (* 2 NODE_RADIUS)) NODE_RADIUS)
                                       0 0 true false
                                       (list MOVABLE_NODE_AFTER_DRAG
                                             MOVABLE_NODE2_AFTER_DRAG)))


(define NEW_SON (make-node 100 (+ 100 (* 3 NODE_RADIUS)) 0 -30 false true empty))
(define SECOND_NEW_SON (make-node (+ 100 (* 3 NODE_RADIUS))
                                  (+ 100 (* 3 NODE_RADIUS))
                                  -30 -30 false true empty))

(define SELECTED_ROOT_NO_SONS
  (make-node 100 100 0 0 true false empty))
(define SELECTED_ROOT_WITH_NEW_SON
  (make-node 100 100 0 0 true false (cons NEW_SON empty)))
(define SELECTED_ROOT_WITH_SECOND_NEW_SON
  (make-node 100 100 0 0 true false (list SECOND_NEW_SON NEW_SON)))

(define NODE_WITH_SELECTED_AND_UNSELECTED_SONS
  (make-node 50 50 0 0 false false (list CENTER_NODE SELECTED_ROOT_WITH_SECOND_NEW_SON)))
(define NODE_WITH_UNSELECTED_SON
  (make-node 50 50 0 0 false false (list CENTER_NODE )))

(define NODE_WITH_LEFT_AND_RIGHT_SONS
  (make-node 300 300 0 0 false false
             (list NODE_WITH_SELECTED_AND_UNSELECTED_SONS
                   MOVABLE_NODE)))

(define NODE_WITH_RIGHT_SON
  (make-node 300 300 0 0 false false
             (list MOVABLE_NODE)))

(define CROOKED_TREE
  (make-node 180 130 0 0 true false
   (list
    (make-node 210 160 -30 -30 false true empty)
    (make-node 180 200 0 -70 false true empty))))

(define CROOKED_TREE_WITH_NEW_SON
  (make-node 180 130 0 0 true false
   (list
    (make-node 240 160 -60 -30 false true empty)
    (make-node 210 160 -30 -30 false true empty)
    (make-node 180 200 0 -70 false true empty))))

(define TREE_WITH_HEIRS_OF_HEIRS
  (make-node 180 130 0 0 true false
   (list
    (make-node 210 160 -30 -30 false true
               (list (make-node 260 200 -80 -70 false true empty)))
    (make-node 180 200 0 -70 false true empty))))

(define TREE_WITH_HEIRS_OF_HEIRS_WITH_NEW_SON
  (make-node 180 130 0 0 true false
   (list
    (make-node 240 160 -60 -30 false true empty)
    (make-node 210 160 -30 -30 false true 
               (list (make-node 260 200 -80 -70 false true empty)))
    (make-node 180 200 0 -70 false true empty))))

;;;;;;;;;;;;;;;;;;;;

;; For ListOfTree:
(define NO_TREES empty)
(define ONE_TREE (cons NEW_NODE empty))
(define MANY_TREES (list NEW_NODE CENTER_NODE BOTOM_NODE SELECTED_TEST_NODE))

;;;;;;;;;;;;;;;;;;;;

;; For World:
(define EMPTY_WORLD (make-world NO_TREES))
(define ONE_TREE_WORLD (make-world ONE_TREE))
(define WORLD_WITH_2_NODES (make-world (list SELECTED_ROOT_WITH_NEW_SON)))
(define BIG_TEST_WORLD (make-world MANY_TREES))

;;;;;;;;;;;;;;;;;;;;

;; For Scene:

;;;;;;;;;;;;;;;;;;;;

;; END Implementation / Tests data 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN functions required by problem set

;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world. The given value is ignored.
;; STARTEGY: Combine Simpler Functions
(define (initial-world any)
  (make-world NO_TREES))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (initial-world 666)
   (make-world empty)
   "Initial world is defined by problem set"))

;;;;;;;;;;;;;;;;;;;;

;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-trees w))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (world-to-trees (initial-world 666))
   empty
   "Initial world has no trees")
  (check-equal?
   (world-to-trees BIG_TEST_WORLD)
   (list NEW_NODE CENTER_NODE BOTOM_NODE SELECTED_TEST_NODE)
   "Big test World has 4 children"))

;;;;;;;;;;;;;;;;;;;;

;; tree-to-root : Tree -> Node
;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree, or its sons.
;; STRATEGY: Combine Simpler Functions
(define (tree-to-root t) t)

;; STRATEGY: Use template for Node on t
(define (tree-to-sons t)
  (node-sons t))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (tree-to-root SELECTED_TEST_NODE)
   SELECTED_TEST_NODE
   "Should return the same Node as a given Tree")
  (check-equal?
   (tree-to-sons SELECTED_TEST_NODE)
   (list MOVABLE_NODE MOVABLE_NODE2)
   "Should return 2 sons of a given Tree"))

;;;;;;;;;;;;;;;;;;;;

;; node-to-center : Node -> Posn
;; RETURNS: the center of the given node as it is to be displayed on the scene.
;; Note: this function returns a Posn (an ISL builtin). This is for the
;;       convenience of the testing framework, and you may or may not wish to
;;       represent the center of the node in this way.
;; STRATEGY: Use template for Node on nd
(define (node-to-center nd)
  (make-posn
   (node-pos-x nd)
   (node-pos-y nd)))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (node-to-center SELECTED_TEST_NODE)
   (make-posn (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
              (- CANVAS_CENTER_Y (* 2 NODE_RADIUS)))
   "Expected x= 280, y=180")
  (check-equal?
   (node-to-center MOVABLE_NODE)
   (make-posn (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
              (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS)))
   "Expected x= 280, y=220"))

;;;;;;;;;;;;;;;;;;;;

;; node-to-selected? : Node -> Boolean
;; RETURNS: true iff the given node is selected.
;; STRATEGY: Use template for Node on nd
(define (node-to-selected? nd)
  (node-selected? nd))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (node-to-selected? SELECTED_TEST_NODE)
   true
   "Given node is selected")
  (check-equal?
   (node-to-selected? MOVABLE_NODE)
   false
   "Given node is not selected"))

;;;;;;;;;;;;;;;;;;;;

;; END functions required by problem set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main Function

;;;;;;;;;;;;;;;;;;;;

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
(define (run any)
  (big-bang (initial-world any)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)))

;; TESTS: no tests for big-bang
;; EXAMPLES:
;; (run 99) and (run "Forrest") have the same behavior

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN to-scene functions

;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene 
;; RETURNS: a Scene that portrays the given world.
;; STRATEGY: HOF foldr on w
(define (world-to-scene w)
  (foldr
   node-to-scene EMPTY_CANVAS
   (world-trees w)))

;; EXAMPLES/TESTS: 
(begin-for-test
  (check-equal?
   (world-to-scene WORLD_WITH_2_NODES)
   (place-image SELECTED_NODE
                100 100
                (scene+line
                 (place-image UNSELECTED_NODE 100 130 EMPTY_CANVAS)
                 100 100 100 130 "blue"))
   "Should draw image with one selected Tree and unselected son Node"))

;;;;;;;;;;;;;;;;;;;;

;; node-to-scene : Node Scene -> Scene
;; GIVEN: a Node nd and a Scene scene
;; RETURNS: a Scene that is similar to a given Scene, except that
;;          given node is drawn on it
;; WHERE: selected node is drawn as solid circle and unselected node
;;        is drawn as outline circle
;; STRATEGY: Use Node template on nd
(define (node-to-scene nd scene)
  (place-image 
   (if (node-selected? nd)
       SELECTED_NODE
       UNSELECTED_NODE)
   (node-pos-x nd)
   (node-pos-y nd)
   (if (empty? (node-sons nd))
       scene
       (node-sons-to-scene nd scene))))

;; TESTS/EXAMPLES: in world-to-scene

;;;;;;;;;;;;;;;;;;;;

;; node-sons-to-scene : Node Scene -> Scene
;; GIVEN: Node nd and Scene as scene
;; RETURNS: a Scene that is similar to a given Scene, except that
;;          given node's sons are drawn on it
;; STRATEGY: Use HOF foldr on (node-sons nd)
(define (node-sons-to-scene nd scene)
  (foldr
   ;; Node Scene -> Scene
   (lambda (son accum-scene)
     (line-to-scene son (node-pos-x nd) (node-pos-y nd) accum-scene))
   scene
   (node-sons nd)))

;; EXAMPLES/TESTS: in world-to-scene

;;;;;;;;;;;;;;;;;;;;

;; line-to-scene : Node NonNegInt NonNegInt Scene -> Scene
;; GIVEN: a Node nd, x and y coordinates and a scene
;; RETURNS: a Scene that is similar to a given Scene, except that
;;          a line is drawn from the center of a given node to given coordinates
;; STRATEGY: Use Template for Node on nd
(define (line-to-scene nd x y scene)
  (scene+line
   (node-to-scene nd scene)
   x y
   (node-pos-x nd)
   (node-pos-y nd)
   LINE_COLOR))

;; EXAMPLES/TESTS: in world-to-scene 

;;;;;;;;;;;;;;;;;;;;

;; END to-scene functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN mouse-event functions

;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World NonNegInt NonNegInt MouseEvent -> World
;; GIVEN: world w, mouse coordinates mx and my, and mouse event mev
;; RETURNS: the world that should follow the given world after the given mouse event.
;; STRATEGY: use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   (lot-after-mouse-event (world-trees w) mx my mev)))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event (make-world (list SELECTED_TEST_NODE))
                            (+ CANVAS_CENTER_X (* 2 NODE_RADIUS) NODE_RADIUS)
                            (+ (- CANVAS_CENTER_Y (* 2 NODE_RADIUS)) NODE_RADIUS)
                            "drag")
   (make-world (list SELECTED_TEST_NODE_AFTER_DRAG))
   "All nodes and selected node should move in sync after a drag.")

  (check-equal?
   (world-after-mouse-event (make-world (list SELECTED_TEST_NODE))
                            (+ CANVAS_CENTER_X (* 2 NODE_RADIUS) NODE_RADIUS)
                            (+ (- CANVAS_CENTER_Y (* 2 NODE_RADIUS)) NODE_RADIUS)
                            "button-up")
   (make-world (list UNSELECTED_TEST_NODE))
   "Selected node after mouse click should be unselected.")

  (check-equal?
   (world-after-mouse-event (make-world (list UNSELECTED_TEST_NODE))
                            (+ CANVAS_CENTER_X (* 2 NODE_RADIUS))
                            (+ (- CANVAS_CENTER_Y (* 2 NODE_RADIUS)))
                            "button-down")
   (make-world (list SELECTED_TEST_NODE))
   "Selected node after mouse click should be unselected."))

;;;;;;;;;;;;;;;;;;;;

;; lot-after-mouse-event : ListOfTree NonNegInt NonNegInt MouseEvent -> ListOfTree
;; GIVEN: list of trees lot, x and y coorinates of mouse as mx and my,
;;        mouse event mev
;; RETURNS: the ListOfTree that should follow the given ListOfTree
;;          after the given mouse event.
;; STRATEGY: Use HOF map on lot
(define (lot-after-mouse-event lot mx my mev)
  (map
   ;; Node -> Node
   (lambda (node) (node-after-mouse-event node mx my mev))
   lot))

;; EXAMPLES/TESTS: in world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;

;; node-after-mouse-event : Node NonNegInt NonNegInt MouseEvent -> Node
;; GIVEN: A Node nd, the x and y coordinates of a mouse, and the mouse event
;; WHERE: button-down, drag and button-up are the only supported events
;; RETURNS: a Node that should follow the given Node after
;;          the given mouse event
;; STRATEGY: Cases on mouse event mev
(define (node-after-mouse-event nd mx my mev)
  (cond
    [(mouse=? mev MOUSE_BUTTON_DOWN) (node-after-button-down nd mx my)]
    [(mouse=? mev MOUSE_DRAG) (node-after-drag nd mx my)] 
    [(mouse=? mev MOUSE_BUTTON_UP) (node-after-button-up nd)]
    [else nd]))

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (node-after-mouse-event CENTER_NODE CANVAS_CENTER_X CANVAS_CENTER_Y "leave")
   CENTER_NODE
   "noe-after-mouse event should return same node when mouse-event other than three specified"))

;;;;;;;;;;;;;;;;;;;;

;; node-after-button-down : Node NonNegInt NonNegInt -> Node
;; RETURNS: a Node following a button-down mouse event at the given location
;;          and calculated dist-x dist-y = mouse coordinates - node center.
;; STRATEGY: Use template for Node on nd
(define (node-after-button-down nd mx my)
  (if (in-node? nd mx my)
      (make-node
       (node-pos-x nd)
       (node-pos-y nd)
       (- mx (node-pos-x nd))
       (- my (node-pos-y nd))
       true
       (node-movable? nd)
       (make-sons-movable
        (lot-after-mouse-event (node-sons nd) mx my MOUSE_BUTTON_DOWN) mx my))
      (make-node
       (node-pos-x nd)
       (node-pos-y nd)
       (node-dist-x nd)
       (node-dist-y nd)
       (node-selected? nd)
       (node-movable? nd)
       (lot-after-mouse-event (node-sons nd) mx my MOUSE_BUTTON_DOWN)))) 

;; EXAMPLES/TESTS: in world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;

;; make-sons-movable : ListOfTree -> ListOfTree
;; RETURNS: given ListOfTree, except that all trees and all their successors
;;          have movable = true
;; STRATEGY: Use HOF map on sons
(define (make-sons-movable sons mx my)
  (map
   ;; Node -> Node
   (lambda (nd)
     (make-node
      (node-pos-x nd)
      (node-pos-y nd)
      (- mx (node-pos-x nd))
      (- my (node-pos-y nd))
      (node-selected? nd)
      true
      (make-sons-movable (node-sons nd) mx my)))
   sons))

;; TESTS/EXAMPLES: in world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;

;; in-node? : Node NonNegInt NonNegInt -> Boolean
;; GIVEN: Node nd and x, y coordinate
;; RETURNS: true if the given coordinate is inside the bounding box of
;;          the given node, else false. Calculation is done by comparing Node
;;          radius and a distance from center of a Node to given coordinates,
;           by formula: dist = √(x2 - x1)^2 + (y2-y1)^2
;; STRATEGY: Combine helper functions
(define (in-node? nd x y)
  (>= NODE_RADIUS
      (sqrt
       (+
        (expt
         (- x (node-pos-x nd))
         2)
        (expt
         (- y (node-pos-y nd))
         2)))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (in-node? BOTOM_NODE CANVAS_CENTER_X CANVAS_CENTER_Y)
   false
   "A click in the center should not occur in bottom node")

  (check-equal?
   (in-node? BOTOM_NODE CANVAS_CENTER_X CANVAS_HEIGHT)
   true
   "A click on the edge of BOTOM_NODE should be registered in node"))

;;;;;;;;;;;;;;;;;;;;

;; node-after-drag : Node NonNegInt NonNegInt -> Node
;; GIVEN: Node nd and x, y location coordinates
;; RETURNS: the node following a drag mouse event at the given location,
;;          if node is selected or movable, else - the same node as a given one
;; WHERE: node's center is shifted from mouse location by node-dist-x node-dist-y
;;        using formula = mouse coordinates - node-dist-x or node-dist-y
;; STRATEGY: Use template for Node on nd
(define (node-after-drag nd mx my)
  (make-node
   (if (or (node-selected? nd) (node-movable? nd))
       (- mx (node-dist-x nd))
       (node-pos-x nd))
   (if (or (node-selected? nd) (node-movable? nd))
       (- my (node-dist-y nd))
       (node-pos-y nd))
   (node-dist-x nd)
   (node-dist-y nd)
   (node-selected? nd)
   (node-movable? nd)
   (lot-after-mouse-event (node-sons nd) mx my MOUSE_DRAG)))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (node-after-drag CENTER_NODE CANVAS_WIDTH CANVAS_HEIGHT)
   CENTER_NODE
   "Dragging an unselected node should not affect the position of the node")

  (check-equal?
   (node-after-drag SELECTED_TEST_NODE
                    (+ CANVAS_CENTER_X (* 2 NODE_RADIUS) NODE_RADIUS)
                    (+ (- CANVAS_CENTER_Y (* 2 NODE_RADIUS)) NODE_RADIUS))
   SELECTED_TEST_NODE_AFTER_DRAG
   "Dragging selected_test_node should move it and it's sons"))

;;;;;;;;;;;;;;;;;;;;

;; node-after-button-up : Node -> Node
;; GIVEN: Node nd
;; RETURNS: the node following a button-up mouse event.
;;          selected? and movable? are both set to false
;; STRATEGY: Use template for Node on nd
(define (node-after-button-up nd)
  (make-node
   (node-pos-x nd)
   (node-pos-y nd)
   (node-dist-x nd)
   (node-dist-y nd)
   false
   false
   (lot-after-mouse-event (node-sons nd) 0 0 MOUSE_BUTTON_UP)))

;; TESTS/EXAMPLES: in world-after-mouse-event

;;;;;;;;;;;;;;;;;;;;

;; END mouse-event functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BEGIN key-event functions

;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: World w and KeyEvent kev
;; RETURNS: the World that should follow the given World
;;          after the given keyevent
;; STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev NEW_TREE_KEY) (world-after-new-tree-key w)]
    [else (make-world (trees-after-key-event (world-trees w) kev))]))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (world-after-key-event EMPTY_WORLD NEW_TREE_KEY)
   ONE_TREE_WORLD
   "World should contain new Tree after NEW_TREE_KEY")
  (check-equal?
   (world-after-key-event EMPTY_WORLD "1")
   EMPTY_WORLD
   "Unsupported key will not affect the world"))

;;;;;;;;;;;;;;;;;;;;

;; world-after-new-root-key : World -> World
;; GIVEN: World w
;; RETURNS: given World, except that new tree is added to top center of canvas
;; STRATEGY: Use template for World on w
(define (world-after-new-tree-key w)
  (make-world (cons NEW_NODE (world-trees w))))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (world-after-new-tree-key (make-world empty))
   (make-world (list NEW_NODE))
   "A world with empty TreeList after tree key event should return a list with NEW_NODE"))
;;;;;;;;;;;;;;;;;;;;

;; trees-after-key-event : ListOfTree KeyEvent -> ListOfTree
;; GIVEN: list of trees as trees and a key event kev
;; RETURNS: ListOfTree that should follow the given ListOfTree after
;;          the given KeyEvent
;; STRATEGY: Use HOF foldr on trees
(define (trees-after-key-event trees kev)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   (lambda (nd accum-nodes)
     (remove-deleted-nodes nd accum-nodes kev))
   empty
   trees))

;; TESTS/EXAMPLES: in node-delete-on-pred and node-after-add-son-key

;;;;;;;;;;;;;;;;;;;;

;; remove-deleted-nodes : Tree ListOfTree KeyEvent -> ListOfTree
;; GIVEN: Node as nd to check and Node acumulator as accum-nodes, and
;;        KeyEvent kev
;; RETURNS: Given Node acumulator that follows given KeyEvent, except that
;;          Nodes that had to be deleted are removed
;; STRATEGY: Use simpler functions
(define (remove-deleted-nodes nd accum-nodes kev)
  (if (or (node-delete? nd kev) (node-delete-left? nd kev))
      accum-nodes
      (cons (node-after-key-event nd kev) accum-nodes)))

;; TESTS/EXAMPLES: in trees-after-key-event

;;;;;;;;;;;;;;;;;;;;

;; node-delete? : Node KeyEvent
;; GIVEN: a root node and key Event
;; RETURNS: true if node is selected and DELETE_KEY is pressed
(define (node-delete? nd kev)
  (and (node-selected? nd) (equal? kev DELETE_KEY)))

;; TEST/EXAMPLE: 
(begin-for-test
  (check-equal?
   (node-delete? SELECTED_TEST_NODE DELETE_KEY)
   true
   "True, because given node is selected and given key is DELETE_KEY")
  (check-equal?
   (node-delete? SELECTED_TEST_NODE ADD_SON_KEY)
   false
   "False, because given node is selected but given key is not DELETE_KEY")
  (check-equal?
   (node-delete? NEW_NODE DELETE_KEY)
   false
   "False, because given node is not selected"))

;;;;;;;;;;;;;;;;;;;;

;; node-delete-left? : Node KeyEvent
;; GIVEN: a root node and key Event
;; RETURNS: true if node is on the left side of canvs and DELETE_LEFT_KEY is pressed
(define (node-delete-left? nd kev)
  (and (< (node-pos-x nd) CANVAS_CENTER_X) (equal? kev DELETE_LEFT_KEY)))

;; TESTS/EXAMPLES: in node-delete-on-pred

;;;;;;;;;;;;;;;;;;;;

;; node-after-key-event : Node KeyEvent -> ListOfTree
;; GIVEN: Node nd and KeyEvent kev
;; RETURNS: Node that should follow the given Node after
;;          the given KeyEvent
;; STRATEGY: Cases on key event kev
(define (node-after-key-event node kev)
  (cond
    [(key=? kev ADD_SON_KEY) (node-after-add-son-key node)]
    [(key=? kev DELETE_KEY) (node-after-delete-key node)]
    [(key=? kev DELETE_LEFT_KEY) (node-after-delete-left-key node)]
    [else node]))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (node-after-key-event SELECTED_ROOT_NO_SONS ADD_SON_KEY)
   SELECTED_ROOT_WITH_NEW_SON
   "Should create new son for given node")
  (check-equal?
   (node-after-key-event SELECTED_ROOT_NO_SONS "1")
   SELECTED_ROOT_NO_SONS
   "Should return the same Node, because of unsupported key"))

;;;;;;;;;;;;;;;;;;;;

;; node-after-add-son-key : Tree -> Tree 
;; GIVEN: Node nd
;; RETURNS: given Node, except that new son is added below given node
;;          and to the right of an extreme right son
;; STRATEGY: Use template for Node on nd
(define (node-after-add-son-key nd)
  (make-node (node-pos-x nd)
             (node-pos-y nd)
             (node-dist-x nd)
             (node-dist-y nd)
             (node-selected? nd)
             (node-movable? nd)
             (sons-after-add-son-key nd)))

;; TESTS/EXAMPLES: 
(begin-for-test
  (check-equal?
   (node-after-add-son-key SELECTED_ROOT_NO_SONS)
   SELECTED_ROOT_WITH_NEW_SON
   "The first son of a node should appear 3 radii down and directly beneath the node.")
  (check-equal?
   (node-after-add-son-key SELECTED_ROOT_WITH_NEW_SON)
   SELECTED_ROOT_WITH_SECOND_NEW_SON
   "Second son should be 3 radii to the right of the center of first son")
  (check-equal?
   (node-after-add-son-key CROOKED_TREE)
   CROOKED_TREE_WITH_NEW_SON
   "New son should be added to the right of the most right son,
    regardles of son's order and with dist-x and dist-y relative to mouse")
  (check-equal?
   (node-after-add-son-key TREE_WITH_HEIRS_OF_HEIRS)
   TREE_WITH_HEIRS_OF_HEIRS_WITH_NEW_SON
   "New son should be added to the right of the most right son,
    regardles of sons of their sons"))

;;;;;;;;;;;;;;;;;;;;

;; sons-after-add-son-key : Node -> ListOfTree
;; GIVEN: Node nd
;; RETURNS: same list of trees, in case given Node is not selected
;;          else list of tree with one new tree
(define (sons-after-add-son-key nd)
  (if (node-selected? nd)
      (cons
       (add-node-to-sons nd)
       (trees-after-key-event (node-sons nd) ADD_SON_KEY))
      (trees-after-key-event (node-sons nd) ADD_SON_KEY)))

;; TESTS/EXAMPLES: in node-after-add-son-key

;;;;;;;;;;;;;;;;;;;;

;; node-after-delete-key : Tree -> Tree 
;; GIVEN: Node nd
;; RETURNS: a tree without any selected nodes
;; STRATEGY: Call a more general function
(define (node-after-delete-key nd)
  (node-delete-on-pred
   nd
   ;; Node -> Boolean
   (lambda (cur_node) (not (node-selected? cur_node)))
   DELETE_KEY))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (node-after-delete-key NODE_WITH_SELECTED_AND_UNSELECTED_SONS)
   NODE_WITH_UNSELECTED_SON
   "Only unselected son will be left"))

;;;;;;;;;;;;;;;;;;;;

;; node-after-delete-left-key : Node -> Node 
;; GIVEN: Node nd
;; RETURNS: a tree without any nodes on left side of canvas
;; STRATEGY: Use more general function
(define (node-after-delete-left-key nd)
  (node-delete-on-pred
   nd
   ;; Node -> Boolean
   (lambda (cur_node) (>= (node-pos-x cur_node) CANVAS_CENTER_X))
   DELETE_LEFT_KEY))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (node-after-delete-left-key NODE_WITH_LEFT_AND_RIGHT_SONS)
   NODE_WITH_RIGHT_SON
   "Only unselected sin will be left"))

;;;;;;;;;;;;;;;;;;;;

;; node-delete-on-pred : Node (Node -> Boolean) KeyEvent -> Node 
;; GIVEN: Node nd and a predicate function to delete on, pred, and kev
;; RETURNS: a tree without any selected nodes that satisfy predicate fn
;; STRATEGY: Use template for Node on nd
(define (node-delete-on-pred nd pred kev)
  (make-node (node-pos-x nd)
             (node-pos-y nd)
             (node-dist-x nd)
             (node-dist-y nd)
             (node-selected? nd)
             (node-movable? nd)
             ;; (Node -> Boolean) ListOfTree -> ListOfTree
             (filter pred
                     (trees-after-key-event(node-sons nd) kev))))

;; TESTS/EXAMPLES: in node-after-delete-key and node-after-delete-left-key
  
;;;;;;;;;;;;;;;;;;;;

;; add-node-to-sons : Node -> Node
;; GIVEN: a Node, nd
;; RETURNS: a Node either directly below or 3 radii to right of rightmost son
;;          of a given Node
;; STRATEGY: Use template for Node on nd
(define (add-node-to-sons nd)
  (if (empty? (node-sons nd))
      (make-new-leaf-node
       (node-pos-x nd)
       (pos-y-offset nd)
       (node-dist-x nd)
       (dist-y-offset nd))
      (make-new-leaf-node 
       (pos-x-offset nd)
       (pos-y-offset nd)
       (dist-x-offset nd)
       (dist-y-offset nd))))


;; TESTS: tested in node-after-add-son-key
;; EXAMPLE:
;; (node-after-add-son-key
;;  (make-node 100 100 0 0 true false
;;             (list (make-node 100 130 0 -30 false true empty))) =>
;; (make-node 100 100 0 0 true false
;;            (list (make-node 100 130 0 -30 false true empty))
;;                  (make-node 100 130 0 -30 #false #true empty)))

;;;;;;;;;;;;;;;;;;;;

;; Helper functions
;; pos-x-offset : Node
;; pos-y-offset : Node
;; dist-x-offset : Node
;; dist-y-offset : Node
;; GIVEN: Node nd
;; RETURNS: calculated x and y coordinates, and distance to mouse
;;          for new son of given Node
;; STRATEGY: Use template for Node on nd
(define (pos-x-offset nd)
  (+ (node-pos-x nd) (get-rightmost-offset-from-sons-of nd) NEW-NODE-OFFSET-X))

(define (pos-y-offset nd)
  (+ (node-pos-y nd) NEW-NODE-OFFSET-Y))

(define (dist-x-offset nd)
  (- (node-dist-x nd) (get-rightmost-offset-from-sons-of nd) NEW-NODE-OFFSET-X))

(define (dist-y-offset nd)
  (- (node-dist-y nd) NEW-NODE-OFFSET-Y))

;; TESTS/EXAMPLES: in add-node-to-sons

;;;;;;;;;;;;;;;;;;;;

;; get-rightmost-offset-from-sons-of : Tree -> Integer
;; GIVEN: a Tree, nd
;; WHERE: Tree has sons (not empty node-sons)
;; RETURNS: the x coordinate difference between the rightmost node and given parent Tree
;; STRATEGY: HOF foldr on (node-sons nd)
(define (get-rightmost-offset-from-sons-of nd)
  (foldr
   ;Node NonNegInt -> NonNegInt
   (lambda (cur-node accum-max)
     (max accum-max
          (- (node-pos-x cur-node) (node-pos-x nd))))
   (first-son-offset nd)
   (node-sons nd)))

;; TESTS/EXAMPLES:
(begin-for-test
  (check-equal?
   (get-rightmost-offset-from-sons-of SELECTED_ROOT_WITH_NEW_SON)
   0
   "Given Tree and its son have the same x, so offset is 0")
  (check-equal?
   (get-rightmost-offset-from-sons-of SELECTED_ROOT_WITH_SECOND_NEW_SON)
   30
   "Given Tree and its rightmost son have the x offset = 30"))

;;;;;;;;;;;;;;;;;;;;

;; first-son-offset : Node -> Integer
;; GIVEN: Node nd
;; RETURNS: offset between given Node and its first son
(define (first-son-offset nd)
  (- (node-pos-x (first (node-sons nd)))
     (node-pos-x nd)))

;; TESTS/EXAMPLES: in get-rightmost-offset-from-sons-of

;;;;;;;;;;;;;;;;;;;;

;; make-new-leaf-node : NonNegInt NonNegInt Integer Integer -> Tree
;; GIVEN: an x and y coordinate
;; RETURNS: a new Tree with no sons, centered at x and y
;; STRATEGY: Combine simpler functions
(define (make-new-leaf-node x y dist-x dist-y)
  (make-node x y dist-x dist-y false true empty))

;; TEST/EXAMPLE:
(begin-for-test
  (check-equal?
   (make-new-leaf-node
    (+ CANVAS_CENTER_X (* 5 NODE_RADIUS))
    (+ CANVAS_CENTER_Y (* 2 NODE_RADIUS))
    (* -3 NODE_RADIUS) (* -4 NODE_RADIUS))
   MOVABLE_NODE2
   "New node should be created with center in x=300 and y=220"))
   
;;;;;;;;;;;;;;;;;;;;

;; END key-event functions