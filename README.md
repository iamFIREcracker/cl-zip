<a id="x-28ZIP-3A-40ZIP-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# cl-zip

## Table of Contents

- [1 Synopsis][a050]
- [2 Reference][7152]

###### \[in package ZIP\]
Comon lisp system implementing Huet
[zippers](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

<a id="x-28ZIP-3A-40ZIP-SYNOPSIS-20MGL-PAX-3ASECTION-29"></a>

## 1 Synopsis

Let's beging by creating a variable holding the code of a function
printing hello world:

```
(in-package :zip)
=> #<PACKAGE "ZIP">

(defvar *tree* '(defun hello-world ()
                    (format t "Hello, world!")))
=> (DEFUN HELLO-WORLD () (FORMAT T "Hello, world!"))
```

Let's *programmatically* change the function definition behind *TREE* to
accept an argument, `name`, and output a custom message based on its value:

```
(~> *tree*
  ;; Create a zipper -- the element under focus (the whole object, initially),
  ;; is wrapped inside a pare of square brackets
  zip                        ; [(defun hello-world () (format t "Hello, World!~%"))]
  ;; change hello-world to just hello
  down                       ; ([defun] hello-world () (format t "Hello, World!~%"))
  right                      ; (defun [hello-world] () (format t "Hello, World!~%"))
  (replace ~ 'hello)         ; (defun [hello] () (format t "Hello, World!~%"))
  ;; change lambda list to include an argument: name
  right                      ; (defun hello [()] (format t "Hello, World!~%"))
  (insert-child ~ 'name)     ; (defun hello [(name)] (format t "Hello, World!~%"))
  ;; change the FORMAT form to hail the specified person
  right                      ; (defun hello (name) [(format t "Hello, World!~%")])
  down                       ; (defun hello (name) ([format] t "Hello, World!~%"))
  rightmost                  ; (defun hello (name) (format t ["Hello, World!~%"]))
  (replace ~ "Hello, ~A!")   ; (defun hello (name) (format t ["Hello, ~A!~%"]))
  (insert-right ~ 'name)     ; (defun hello (name) (format t ["Hello, ~A!~%"] name))
  ;; Reassembe the tree
  unzip)
=> (DEFUN HELLO-WORLD () (FORMAT T "Hello, world!"))
```

Let's eval the form, run it, et voila\`!

```
(eval *)
=> HELLO

(funcall * "Matteo")
..Hello, Matteo!
=> NIL
```


<a id="x-28ZIP-3A-40ZIP-REFERENCE-20MGL-PAX-3ASECTION-29"></a>

## 2 Reference

<a id="x-28ZIP-3ALOC-20CLASS-29"></a>

- [class] **LOC** *[STRUCTURE-OBJECT][2038]*

    A zipper's core data structure: a location object.
    
    It's composed of the following slots:
    
    - `node` represent the currently focused element.
    
    - [`nav`][beae] is a `NAV` object, allowing a zipper to efficiently move left, right, and
      back up.
    
    - [`meta`][2d25] is is a `META` object containing pointers to fns used to implement the
      zipper algorithm.


<a id="x-28ZIP-3ANAV-20CLASS-29"></a>

- [class] **NAV** *[STRUCTURE-OBJECT][2038]*

    A zipper's navigation object, enabling efficient movements left, right, and
    up.
    
    It's composed of the following slots:
    
    - [`ups`][eb16] is the list of nodes visible above: the first element of the list
      represents the node immediately above, while the last element represents the
      root.
    
    - [`lefts`][4a8b] is the list of nodes visible the left: the first element of the list
      represents the node immediately to the left, while the last element
      represents the leftmost element to the left.
    
    - [`rights`][3ca6] is the list of nodes visible the right: the first element of the
      list represents the node immediately to the right, while the last element
      represents the rightmost element to the right.
    
    - `pnav` is a pointer to the `NAV` object of the parent, while `changed?` is
      a flag indicating whether any mutation has been applied or not.  There are
      mostly used to avoid cons-ing unless strictly required.


<a id="x-28ZIP-3AMETA-20CLASS-29"></a>

- [class] **META** *[STRUCTURE-OBJECT][2038]*

    A zipper's metadata object.
    
    It's composed of the following slots:
    
    - `branch?` is a fn that, given a [`LOC`][3a4a], returns `T` if it can have children, even
      if it currently doesn't.
    
    - `children` is a fn that, given a `LOC`, returns a `LIST`([`0`][79d8] [`1`][6d9f]) of its children.
    
    - `make-node` is a fn that, given an existing `LOC` and a `LIST` of children,
      returns a new branch `LOC` with the supplied children.


<a id="x-28ZIP-3AZIP-20GENERIC-FUNCTION-29"></a>

- [generic-function] **ZIP** *ROOT*

    Creates a zipper and returns a [`LOC`][3a4a] focused on `root`

<a id="x-28ZIP-3AUNZIP-20FUNCTION-29"></a>

- [function] **UNZIP** *LOC*

    zip all the way up and returns the root node, reflecting any changes

<a id="x-28ZIP-3ADOWN-20FUNCTION-29"></a>

- [function] **DOWN** *LOC*

    Returns the `LOC` of the leftmost child of the node at this location, or nil
    if no children

<a id="x-28ZIP-3AUP-20FUNCTION-29"></a>

- [function] **UP** *LOC*

    Returns the `LOC` of the parent of the node at this loc, or nil if at the top

<a id="x-28ZIP-3ALEFT-20FUNCTION-29"></a>

- [function] **LEFT** *LOC*

    Returns the `LOC` of the left sibling of the node at this loc, or nil

<a id="x-28ZIP-3ALEFTMOST-20FUNCTION-29"></a>

- [function] **LEFTMOST** *LOC*

    Returns the `LOC` of the leftmost siblings of the node at this loc, or self

<a id="x-28ZIP-3ARIGHT-20FUNCTION-29"></a>

- [function] **RIGHT** *LOC*

    Returns the `LOC` of the right sibling of the node at this loc, or nil

<a id="x-28ZIP-3ARIGHTMOST-20FUNCTION-29"></a>

- [function] **RIGHTMOST** *LOC*

    Returns the `LOC` of the rightmost siblings of the node at this loc, or self

<a id="x-28ZIP-3ANEXT-20FUNCTION-29"></a>

- [function] **NEXT** *LOC*

    Moves to the next `LOC` in the hierarchy, depth-first.  When reaching the end,
    returns nil.

<a id="x-28ZIP-3ANEXT-THAT-20FUNCTION-29"></a>

- [function] **NEXT-THAT** *FN LOC*

    Moves to the next `LOC` in the hierarchy such that (apply fn loc) is `T`.
    If no such `LOC` exists, returns nil.

<a id="x-28ZIP-3APREV-20FUNCTION-29"></a>

- [function] **PREV** *LOC*

    Moves to the previous `LOC` in the hierarchy, depth-first.  When at the root,
    return nil.

<a id="x-28ZIP-3APREV-THAT-20FUNCTION-29"></a>

- [function] **PREV-THAT** *FN LOC*

    Moves to the prev `LOC` in the hirerachy such that (apply fn loc) is `T`.
    If no such `LOC` exists, returns nil.

<a id="x-28ZIP-3AINSERT-LEFT-20FUNCTION-29"></a>

- [function] **INSERT-LEFT** *LOC ITEM*

    Inserts the item as the left sibling of the node at this loc, without
    moving

<a id="x-28ZIP-3AINSERT-RIGHT-20FUNCTION-29"></a>

- [function] **INSERT-RIGHT** *LOC ITEM*

    Inserts the item as the right sibling of the node at this loc, without
    moving

<a id="x-28ZIP-3AREPLACE-20FUNCTION-29"></a>

- [function] **REPLACE** *LOC ITEM*

    Replaces the node at this loc, without moving

<a id="x-28ZIP-3AEDIT-20FUNCTION-29"></a>

- [function] **EDIT** *LOC FN &REST ARGS*

    Replaces the node at this loc with the result of (apply fn node args)

<a id="x-28ZIP-3AINSERT-CHILD-20FUNCTION-29"></a>

- [function] **INSERT-CHILD** *LOC ITEM*

    Inserts item as the leftmost child of the node at this loc, without moving

<a id="x-28ZIP-3AAPPEND-CHILD-20FUNCTION-29"></a>

- [function] **APPEND-CHILD** *LOC ITEM*

    Inserts item as the rightmost child of the node at this loc, without moving

<a id="x-28ZIP-3AREMOVE-20FUNCTION-29"></a>

- [function] **REMOVE** *LOC*

    Removes the node at loc and moves to the previous loc in the hierarchy,
    depth-first.

  [2038]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_ob.htm "STRUCTURE-OBJECT (MGL-PAX:CLHS CLASS)"
  [2d25]: #x-28ZIP-3AMETA-20CLASS-29 "ZIP:META CLASS"
  [3a4a]: #x-28ZIP-3ALOC-20CLASS-29 "ZIP:LOC CLASS"
  [3ca6]: #x-28ZIP-3ARIGHT-20FUNCTION-29 "ZIP:RIGHT FUNCTION"
  [4a8b]: #x-28ZIP-3ALEFT-20FUNCTION-29 "ZIP:LEFT FUNCTION"
  [6d9f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_list_.htm "LIST (MGL-PAX:CLHS FUNCTION)"
  [7152]: #x-28ZIP-3A-40ZIP-REFERENCE-20MGL-PAX-3ASECTION-29 "Reference"
  [79d8]: http://www.lispworks.com/documentation/HyperSpec/Body/t_list.htm "LIST (MGL-PAX:CLHS CLASS)"
  [a050]: #x-28ZIP-3A-40ZIP-SYNOPSIS-20MGL-PAX-3ASECTION-29 "Synopsis"
  [beae]: #x-28ZIP-3ANAV-20CLASS-29 "ZIP:NAV CLASS"
  [eb16]: #x-28ZIP-3AUP-20FUNCTION-29 "ZIP:UP FUNCTION"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
