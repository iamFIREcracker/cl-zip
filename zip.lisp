(defpackage #:zip
  (:use #:cl #:mlutils #:3am)
  (:shadow :replace :remove)
  (:export
   :make-zipper :zip :unzip
   :down :up :right :rightmost :left :leftmost
   :next :next-that :prev :prev-that
   :insert-left :insert-right :replace :edit :insert-child :append-child :remove))
(in-package #:zip)
(named-readtables:in-readtable :mlutils-syntax)


(defstruct (loc (:constructor %make-loc))
  "A zipper's core data structure: a location object.

`node` represent the currently focused element.

`nav` is a navigation object, allowing a zipper to efficiently move left,
right, and back up.

`meta` is is a META object containing pointers to fns used to implement
the zipper algorithm.  "
  node
  nav
  meta)

(defstruct nav
  "A zipper's navigation object, enabling efficient movements left, right, and
up.

`ups` is the list of nodes visible above: the first element of the list
represents the node immediately above, while the last element represents
the root.

`lefts` is the list of nodes visible the left: the first element of the list
represents the node immediately to the left, while the last element represents
the leftmost element to the left.

`rights` is the list of nodes visible the right: the first element of the list
represents the node immediately to the right, while the last element represents
the rightmost element to the right.

`pnav` is a pointer to the NAV object of the parent, while `changed?` is a
flag indicating whether any mutation has been applied or not.  There are
mostly used to avoid cons-ing unless strictly required.
"
  ups
  lefts
  rights

  pnav
  changed?)

(defstruct meta
  branch?
  children
  make-node)


(defun make-zipper (branch? children make-node root)
  "Creates a new zipper instance.

`branch?` is a fn that, given a LOC, returns T if it can have children,
even if it currently doesn't.

`children` is a fn that, given a LOC, returns a LIST of its children.

`make-node` is a fn that, given an existing LOC and a LIST of children,
returns a new branch LOC with the supplied children.

`root` is the root node."
  (make-loc root nil (make-meta :branch? branch?
                                :children children
                                :make-node make-node)))

(defun make-loc (node nav meta) (%make-loc :node node :nav nav :meta meta))


(defgeneric zip (root)
  (:documentation "Creates a zipper and returns a LOC focused on `root`")
  (:method ((root list))
    (make-zipper #'listp
                 #'identity
                 (fn (node children) (declare (ignore node)) children)
                 root)))


;;; Selectors
(defun node (loc) (loc-node loc))
(defun nav (loc) (loc-nav loc))
(defun meta (loc) (loc-meta loc))
(defun meta/branch? (loc) (meta-branch? (meta loc)))
(defun meta/children (loc) (meta-children (meta loc)))
(defun meta/make-node (loc) (meta-make-node (meta loc)))
(defun branch? (loc) (funcall (meta/branch? loc) (node loc)))
(defun children (loc)
  (if (branch? loc)
    (funcall (meta/children loc) (node loc))
    (error "Called CHILDREN on a leaf node")))
(defun make-node (loc node children)
  (funcall (meta/make-node loc) node children))


(defun ups (loc)
  "Returns a list of nodes leading to this location"
  (nav-ups (nav loc)))

(defun lefts (loc)
  "Returns a list of the left siblings of this location"
  (nav-lefts (pr (nav loc))))

(defun rights (loc)
  "Returns a list of the right siblings of this location"
  (nav-rights (nav loc)))


;;; Basic navigation
(defun down (loc)
  "Returns the location of the leftmost child of the node at this location, or
  nil if no children"
  (when (branch? loc)
    (awhen (children loc)
      (d-b (c . cnext) it
        (w/slots (node nav) loc
          (make-loc c
                    (make-nav :lefts nil
                              :ups (if nav (cons node (ups loc)) (list node))
                              :rights cnext
                              :pnav nav)
                    (meta loc)))))))

(examples down
  (is (equal (~> (zip '(((1)))) down node)
             '((1))))
  (is (equal (~> (zip '(((1)))) down down node)
             '(1)))
  (is (equal (~> (zip '(((1)))) down down down node)
             1)))


(defun changed (nav)
  (bnd1 nav (copy-nav nav)
    (setf (nav-changed? nav) t)
    nav))

(defun up (loc)
  "Returns the loc of the parent of the node at this loc, or nil if at the top"
  (w/slots (node nav) loc
    (when nav
      (w/slots (lefts ups rights pnav changed?) nav
        (when ups
          (bnd* ((pnode (car ups)))
            (if changed?
              (make-loc (make-node loc pnode (append (reverse lefts)
                                                     (cons node rights)))
                        (and pnav (changed pnav))
                        (meta loc))
              (make-loc pnode pnav (meta loc)))))))))

(examples up/down
  (is (equal (~> (zip '(((1)))) down up node)
             '(((1)))))
  (is (equal (~> (zip '(((1)))) down down up up down down down node)
             1)))


(defun all-the-way (fn loc)
  (aif (funcall fn loc)
    (all-the-way fn it)
    loc))

(defun unzip (loc)
  "zip all the way up and returns the root node, reflecting any changes"
  (node (all-the-way #'up loc)))

(examples unzip
  (is (equal (~> (zip '(((1)))) down down down (all-the-way #'up) node)
             '(((1))))))


(defun update-nav (existing &key
                            (lefts nil lefts?)
                            (ups nil ups?)
                            (rights nil rights?)
                            (pnav nil pnav?)
                            (changed? nil changed??))
  "Returns a new NAV instance with slots initialized either via keyword arguments,
or by copying the slot value from `existing`."
  (make-nav :lefts (if lefts? lefts (nav-lefts existing))
            :ups (if ups? ups (nav-ups existing))
            :rights (if rights? rights (nav-rights existing))
            :pnav (if pnav? pnav (nav-pnav existing))
            :changed? (if changed?? changed? (nav-changed? existing))))

(defun left (loc)
  "Returns the loc of the left sibling of the mode at this loc, or nil"
  (w/slots (node nav) loc
    (when nav
      (w/slots (lefts rights) nav
        (when lefts
          (make-loc (car lefts)
                    (update-nav nav
                                :lefts (rest lefts)
                                :rights (cons node rights))
                    (meta loc)))))))

(examples left
  (is (equal (~> (zip '(1 2 3)) down right right left node)
             2))
  (is (equal (~> (zip '(1 2 3)) down right right left left node)
             1)))

(defun leftmost (loc)
  "Returns the loc of the leftmost siblings of the node at this loc, or self"
  (w/slots (node nav) loc
    (when nav
      (w/slots (lefts rights) nav
        (when lefts
          (make-loc (last-elt lefts)
                    (update-nav nav
                                :lefts nil
                                :rights (append (reverse (butlast lefts))
                                                (cons node rights)))
                    (meta loc)))))))

(examples leftmost
  (is (equal (~> (zip '(1 2 3)) down rightmost leftmost node)
             1)))


(defun right (loc)
  "Returns the loc of the right sibling of the mode at this loc, or nil"
  (w/slots (node nav) loc
    (when nav
      (w/slots (lefts rights) nav
        (when rights
          (make-loc (car rights)
                    (update-nav nav
                                :lefts (cons node lefts)
                                :rights (rest rights))
                    (meta loc)))))))

(examples right
  (is (equal (~> (zip '(1 2 3)) down right node)
             2))
  (is (equal (~> (zip '(1 2 3)) down right right node)
             3)))

(defun rightmost (loc)
  "Returns the loc of the rightmost siblings of the node at this loc, or self"
  (w/slots (node nav) loc
    (when nav
      (w/slots (lefts rights) nav
        (when rights
          (make-loc (last-elt rights)
                    (update-nav nav
                                :lefts (append (reverse (butlast rights))
                                               (cons node lefts))
                                :rights nil)
                    (meta loc)))))))

(examples rightmost
  (is (equal (~> (zip '(1 2 3)) down rightmost node)
             3)))


;;; Advanced navigation
(defun next (loc)
  "Moves to the next loc in the hierarchy, depth-first.  When reaching the end,
  returns nil."
  (when loc
    (or
      (and (branch? loc) (down loc))
      (right loc)
      (recursively ((loc loc))
        (aif (up loc)
          (or (right it) (recur it)))))))

(examples next
  (bnd1 z (zip '(* (+ 1 2) (- 3 4)))
    (is (equal (~> z next node)
               '*))
    (is (equal (~> z next next node)
               '(+ 1 2)))
    (is (equal (~> z next next next node)
               '+))
    (is (equal (~> z next next next next node)
               1))
    (is (equal (~> z next next next next next node)
               2))
    (is (equal (~> z next next next next next next node)
               '(- 3 4)))
    (is (equal (~> z next next next next next next next node)
               '-))
    (is (equal (~> z next next next next next next next next node)
               3))
    (is (equal (~> z next next next next next next next next next node)
               4))
    (is (equal (~> z next next next next next next next next next next)
               nil))
    ))

(defun next-that (fn loc)
  "Moves to the next loc in the hierarchy such that (fn loc) is T.
If no such loc exists, returns nil."
  (if-let (nloc (next loc))
    (if (funcall fn nloc) nloc (next-that fn nloc))
    nil))

(examples next-that
  (bnd1 z (zip '(* (+ 1 2) (- 3 4)))
    (setf z (~> z (next-that [numberp (node _)])))
    (is (equal (node z) 1))
    (setf z (~> z (next-that [numberp (node _)])))
    (is (equal (node z) 2))
    (setf z (~> z (next-that [numberp (node _)])))
    (is (equal (node z) 3))
    (setf z (~> z (next-that [numberp (node _)])))
    (is (equal (node z) 4))
    ))


(defun prev (loc)
  "Moves to the previous loc in the hierarchy, depth-first.
When at the root, return nil."
  (declare (optimize (debug 3)))
  (aif (left loc)
    (recursively ((loc it))
      (if-let (child (and (branch? loc) (down loc)))
        (recur (rightmost child))
        loc))
    (up loc)))

(examples prev
  (bnd1 z (~> (zip '(* (+ 1 2) (- 3 4))) (all-the-way #'next))
    (is (equal (node z) 4))
    (is (equal (~> z prev node)
               3))
    (is (equal (~> z prev prev node)
               '-))
    (is (equal (~> z prev prev prev node)
               '(- 3 4)))
    (is (equal (~> z prev prev prev prev node)
               2))
    (is (equal (~> z prev prev prev prev prev node)
               1))
    (is (equal (~> z prev prev prev prev prev prev node)
               '+))
    (is (equal (~> z prev prev prev prev prev prev prev node)
               '(+ 1 2)))
    (is (equal (~> z prev prev prev prev prev prev prev prev node)
               '*))
    (is (equal (~> z prev prev prev prev prev prev prev prev prev node)
                '(* (+ 1 2) (- 3 4))))
    (is (equal (~> z prev prev prev prev prev prev prev prev prev prev)
                nil))
    ))

(defun prev-that (fn loc)
  "Moves to the prev loc in the hirerachy such that (fn loc) is T.
If no such loc exists, returns nil."
  (if-let (ploc (prev loc))
    (if (funcall fn ploc) ploc (prev-that fn ploc))
    nil))

(examples prev-that
  (bnd1 z (~> (zip '(* (+ 1 2) (- 3 4))) (all-the-way #'next))
    (is (equal (node z) 4))
    (setf z (~> z (prev-that [numberp (node _)])))
    (is (equal (node z) 3))
    (setf z (~> z (prev-that [numberp (node _)])))
    (is (equal (node z) 2))
    (setf z (~> z (prev-that [numberp (node _)])))
    (is (equal (node z) 1))
    ))


;;; Mutation
(defun insert-left (loc item)
  "Inserts the item as the left sibling of the node at this loc, without
moving"
  (w/slots (node nav) loc
    (if-not nav
      (error "Insert at top")
      (w/slots (lefts) nav
        (make-loc node
                  (update-nav nav
                              :lefts (cons item lefts)
                              :changed? t)
                  (meta loc))))))

(examples insert-left
  (is (equal (~> (zip '(1 2 3)) down (insert-left ~ 0) node)
             1))
  (is (equal (~> (zip '(1 2 3)) down (insert-left ~ 0) unzip)
             '(0 1 2 3))))

(defun insert-right (loc item)
  "Inserts the item as the right sibling of the node at this loc, without
moving"
  (w/slots (node nav) loc
    (if-not nav
      (error "Insert at top")
      (w/slots (rights) nav
        (make-loc node
                  (update-nav nav
                              :rights (cons item rights)
                              :changed? t)
                  (meta loc))))))

(examples insert-right
  (is (equal (~> (zip '(1 2 3)) down rightmost (insert-right ~ 4) node)
             3))
  (is (equal (~> (zip '(1 2 3)) down rightmost (insert-right ~ 4) unzip)
             '(1 2 3 4))))

(defun replace (loc item)
  "Replaces the node at this loc, without moving"
  (make-loc item (update-nav (nav loc) :changed? t) (meta loc)))

(examples replace
  (is (equal (~> (zip '(1 2 3)) down rightmost (replace ~ 333) node)
             333))
  (is (equal (~> (zip '(1 2 3)) down rightmost (replace ~ 333) unzip)
             '(1 2 333))))

(defun edit (loc fn &rest args)
  "Replaces the node at this loc with the result of (fn node args)"
  (replace loc (apply fn (node loc) args)))

(examples edit
  (is (equal (~> (zip '(1 2 3)) down rightmost (edit ~ [* _ 2]) node)
             6))
  (is (equal (~> (zip '(1 2 3)) down rightmost (edit ~ [* _ 2]) unzip)
             '(1 2 6))))

(defun insert-child (loc item)
  "Inserts `item` as the leftmost child of the node at this loc, without
moving"
  (replace loc (make-node loc (node loc) (cons item (children loc)))))

(examples insert-child
  (is (equal (~> (zip '(1 2 3)) down (edit ~ [list _]) (insert-child ~ 0) node)
             '(0 1)))
  (is (equal (~> (zip '(1 2 3)) down (edit ~ [list _]) (insert-child ~ 0) unzip)
             '((0 1) 2 3))))

(defun append-child (loc item)
  "Inserts `item` as the rightmost child of the node at this loc, without
moving"
  (replace loc (make-node loc (node loc) (append (children loc)
                                                   (list item)))))

(examples append-child
  (is (equal (~> (zip '(1 2 3)) down rightmost (edit ~ [list _]) (append-child ~ 4) node)
             '(3 4)))
  (is (equal (~> (zip '(1 2 3)) down rightmost (edit ~ [list _]) (append-child ~ 4) unzip)
             '(1 2 (3 4)))))

(defun remove (loc)
  "Removes the node at loc and moves to the previous loc in the hierarchy,
depth first."
  (w/slots (nav) loc
    (if-not nav
      (error "Remove at top")
      (w/slots (lefts ups prev rights) nav
        (if (plusp (length lefts))
          (recursively ((loc (make-loc (first lefts)
                                       (update-nav nav
                                                   :lefts (rest lefts)
                                                   :changed? t)
                                       (meta loc))))
            (if-let (child (and (branch? loc) (down loc)))
              (recur (rightmost child))
              loc))
          (make-loc (make-node loc (first ups) rights)
                    (and prev (update-nav prev :changed? t))
                    (meta loc)))))))

(examples remove
  (bnd1 z (zip '(* (+ 1 2) (- 3 4)))
    (is (equal (~> z (next-that [equal (node _) 1]) remove node)
               '+))
    (is (equal (~> z (next-that [equal (node _) 1]) remove unzip)
               '(* (+ 2) (- 3 4))))

    (is (equal (~> z (next-that [equal (node _) '+]) up remove node)
               '*))
    (is (equal (~> z (next-that [equal (node _) '+]) up remove unzip)
               '(* (- 3 4))))
    ))
