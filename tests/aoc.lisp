(defpackage #:zip/tests/aoc
  (:use #:cl #:mlutils #:3am #:zip)
  (:shadowing-import-from #:zip :replace :remove))
(in-package #:zip/tests/aoc)
(named-readtables:in-readtable :mlutils-syntax)

;;; https://adventofcode.com/2021/day/18

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defun input () (merge-pathnames "aoc.txt" *base-pathname*))


(defun parse-snailfish (string)
  (~>> string
    (substitute #\Space #\,)
    (substitute #\( #\[)
    (substitute #\) #\])
    read-from-string))
#+#:excluded (parse-snailfish "[[[[0,7],4],[15,[0,13]]],[1,1]]")
#+#:excluded (parse-snailfish "[[[[[9,8],1],2],3],4]")

(defun leaf? (loc) (numberp (node loc)))
(defun splittable? (loc) (and (leaf? loc) (>= (node loc) 10)))

(defun split (sf)
  (awhen (next-that #'splittable? (zip sf))
    (unzip (replace it (list (floor (node it) 2) (ceiling (node it) 2))))))
#+#:excluded (~> (parse-snailfish "[[[[0,7],4],[15,[0,13]]],[1,1]]")
               split
               split)

(defun pair? (loc)
  (and (listp (node loc))
       (numberp (first (node loc)))
       (numberp (second (node loc)))))

(defun explodable? (loc)
  (and (pair? loc)
       (nav loc)
       (= (length (ups loc)) 4)))

(defun explode (sf)
  (when-let (curr (next-that #'explodable? (zip sf)))
    (flet ((curr? (loc) (eq (node loc) (node curr))))
      (when-let (pleaf (prev-that #'leaf? curr))
        (setf curr (next-that #'curr? (edit pleaf #'+ (first (node curr))))))
      (when-let (nleaf (next-that #'leaf? (next-that #'leaf? (next-that #'leaf? curr))))
        (setf curr (prev-that #'curr? (edit nleaf #'+ (second (node curr)))))))
    (unzip (replace curr 0))))
#+#:excluded (~> (parse-snailfish "[[[[[9,8],1],2],3],4]")
               explode)
#+#:excluded (~> (parse-snailfish "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
               explode)
#+#:excluded (~> (parse-snailfish "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
               explode)
#+#:excluded (~> (parse-snailfish "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
               explode)

(defun reduce-snailfish (sf)
  (aif (or (explode sf) (split sf))
    (reduce-snailfish it)
    sf))
#+#:excluded (equal (reduce-snailfish (list (parse-snailfish "[[[[4,3],4],4],[7,[[8,4],9]]]") (parse-snailfish "[1,1]")))
                    (parse-snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

(defun magnitude (tree)
  (if (atom tree)
    tree
    (+ (* (magnitude (first tree)) 3) (* (magnitude (second tree)) 2))))
#+#:excluded (magnitude (parse-snailfish "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))


(defun part1 (&optional (file (input)))
  (magnitude
    (reduce (fn (sf1 sf2) (reduce-snailfish (list sf1 sf2)))
            (mapcar #'parse-snailfish (uiop:read-file-lines file)))))

(defun part2 (&optional (file (input)))
  (loop for (sf1 . rest) on (mapcar #'parse-snailfish (uiop:read-file-lines file))
        maximize (loop for sf2 in rest
                       maximize (magnitude (reduce-snailfish (list sf1 sf2)))
                       maximize (magnitude (reduce-snailfish (list sf2 sf1))))))
(test aoc
  (is (= (part1) 3987))
  (is (= (part2) 4500)))
