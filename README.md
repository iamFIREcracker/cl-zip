# cl-zip

Common Lisp system implementing Huet
[zippers](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

## Synopsis

    (defpackage :example
      (:use :cl :mlutils :zip)
      (:shadowing-import-from :zip :replace :remove))
    (in-package :example)
    (named-readtables:in-readtable :mlutils-syntax)
    
    (defvar *tree* '(defun hello-world ()
                      (format t "Hello, world!")))

Let's _programmatically_ change the function definition behind `*TREE*` to
accept an argument, `name`, and output a custom message based on its value:

    CL-USER> (in-package :example)
    #<PACKAGE "EXAMPLE">
    
    EXAMPLE> *tree*
    (DEFUN HELLO-WORLD () (FORMAT T "Hello, world!"))
    
    EXAMPLE>(~> *tree*
              ;; Create a zipper -- the element under focus (the whole object, initially),
              ;; is wrapped inside a pare of square brackets
              zip                      ; [(defun hello-world () (format t "Hello, World!~%"))]
              ;; change hello-world to just hello
              down                     ; ([defun] hello-world () (format t "Hello, World!~%"))
              right                    ; (defun [hello-world] () (format t "Hello, World!~%"))
              (replace ~ 'hello)       ; (defun [hello] () (format t "Hello, World!~%"))
              ;; change lambda list to include an argument: name
              right                    ; (defun hello [()] (format t "Hello, World!~%"))
              (insert-child ~ 'name)   ; (defun hello [(name)] (format t "Hello, World!~%"))
              ;; change the FORMAT form to hail the specified person
              right                    ; (defun hello (name) [(format t "Hello, World!~%")])
              down                     ; (defun hello (name) ([format] t "Hello, World!~%"))
              rightmost                ; (defun hello (name) (format t ["Hello, World!~%"]))
              (replace ~ "Hello, ~A!") ; (defun hello (name) (format t ["Hello, ~A!~%"]))
              (insert-right ~ 'name)   ; (defun hello (name) (format t ["Hello, ~A!~%"] name))
              ;; Reassembe the tree
              unzip)
    EXAMPLE> (eval *)
    HELLO
    
    EXAMPLE> (funcall * "Matteo")
    Hello, Matteo!

## API

* [structure] **`loc`** -- A zipper's core data structure: a location object.

  It's composed of the following slots:

  - `node` represent the currently focused element.
  - `nav` is a navigation object, allowing a zipper to efficiently move left,
    right, and back up.
  - `meta` is is a META object containing pointers to fns used to implement the
    zipper algorithm.

* [structure] **`nav`** -- A zipper's navigation object, enabling efficient
  movements left, right, and up.

  It's composed of the following slots:

  - `ups` is the list of nodes visible above: the first element of the list
    represents the node immediately above, while the last element represents
    the root.
  - `lefts` is the list of nodes visible the left: the first element of the
    list represents the node immediately to the left, while the last element
    represents the leftmost element to the left.
  - `rights` is the list of nodes visible the right: the first element of the
    list represents the node immediately to the right, while the last element
    represents the rightmost element to the right.
  - `pnav` is a pointer to the NAV object of the parent, while `changed?` is
    a flag indicating whether any mutation has been applied or not.  There are
    mostly used to avoid cons-ing unless strictly required.
* [structure] **`meta`** -- A zipper's metadata object.

  It's composed of the following slots:

  - `branch?` is a fn that, given a LOC, returns T if it can have children,
    even if it currently doesn't.
  - `children` is a fn that, given a LOC, returns a LIST of its children.
  - `make-node` is a fn that, given an existing LOC and a LIST of children,
    returns a new branch LOC with the supplied children.

* [method] **`zip`** `(root list)` -- Creates a zipper and returns a LOC
  focused on `root`

* [function] **`unzip`** `loc` -- Zip all the way up and return the root node,
  reflecting any changes

* [function] **`down`** `loc` -- Returns the LOC of the leftmost child of the
  node at this location, or nil if no children

* [function] **`up`** `loc` -- Returns the LOC of the parent of the node at
  this loc, or nil if at the top

* [function] **`left`** `loc` -- Returns the LOC of the left sibling of the
  node at this loc, or nil

* [function] **`leftmost`** `loc` -- Returns the LOC of the leftmost siblings
  of the node at this loc, or self

* [function] **`right`** `loc` -- Returns the LOC of the right sibling of the
  node at this loc, or nil

* [function] **`rightmost`** `loc` -- Returns the LOC of the rightmost siblings
  of the node at this loc, or self

* [function] **`next`** `loc` -- Moves to the next LOC in the hierarchy,
  depth-first.  When reaching the end, returns nil.

* [function] **`next-that`** `fn` `loc` -- Moves to the next LOC in the
  hierarchy such that (apply fn loc) is T.  If no such LOC exists, returns nil.

* [function] **`prev`** `loc` -- Moves to the previous LOC in the hierarchy,
  depth-first.  When at the root, return nil.

* [function] **`prev-that`** `fn` `loc` -- Moves to the prev LOC in the
  hirerachy such that (apply fn loc) is T.  If no such LOC exists, returns nil.

* [function] **`insert-left`** `loc` `item` -- Inserts the item as the left
  sibling of the node at this loc, without moving

* [function] **`insert-right`** `loc` `item` -- Inserts the item as the right
  sibling of the node at this loc, without moving

* [function] **`replace`** `loc` `item` -- Replaces the node at this loc,
  without moving

* [function] **`edit`** `loc` `fn` `&rest` `args` -- Replaces the node at
  this loc with the result of (apply fn node args)

* [function] **`insert-child`** `loc` `item` -- Inserts `item` as the leftmost
  child of the node at this loc, without moving

* [function] **`append-child`** `loc` `item` -- Inserts `item` as the rightmost
  child of the node at this loc, without moving

* [function] **`remove`** `loc` -- Removes the node at loc and moves to the
  previous loc in the hierarchy, depth-first.
