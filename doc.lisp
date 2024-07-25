(in-package #:zip)

(pax:defsection @zip-manual (:title "cl-zip")
  "Comon lisp system implementing Huet
  [zippers](https://en.wikipedia.org/wiki/Zipper_(data_structure))."
  (@zip-synopsis pax:section)
  (@zip-reference pax:section))

(pax:defsection @zip-synopsis (:title "Synopsis")
  "Let's beging by creating a variable holding the code of a function
  printing hello world:

  ```
  (in-package :zip)
  => #<PACKAGE \"ZIP\">

  (defvar *tree* '(defun hello-world ()
                      (format t \"Hello, world!\")))
  => (DEFUN HELLO-WORLD () (FORMAT T \"Hello, world!\"))
  ```

  Let's _programmatically_ change the function definition behind *TREE* to
  accept an argument, `name`, and output a custom message based on its value:

  ```
  (~> *tree*
    ;; Create a zipper -- the element under focus (the whole object, initially),
    ;; is wrapped inside a pare of square brackets
    zip                        ; [(defun hello-world () (format t \"Hello, World!~%\"))]
    ;; change hello-world to just hello
    down                       ; ([defun] hello-world () (format t \"Hello, World!~%\"))
    right                      ; (defun [hello-world] () (format t \"Hello, World!~%\"))
    (replace ~ 'hello)         ; (defun [hello] () (format t \"Hello, World!~%\"))
    ;; change lambda list to include an argument: name
    right                      ; (defun hello [()] (format t \"Hello, World!~%\"))
    (insert-child ~ 'name)     ; (defun hello [(name)] (format t \"Hello, World!~%\"))
    ;; change the FORMAT form to hail the specified person
    right                      ; (defun hello (name) [(format t \"Hello, World!~%\")])
    down                       ; (defun hello (name) ([format] t \"Hello, World!~%\"))
    rightmost                  ; (defun hello (name) (format t [\"Hello, World!~%\"]))
    (replace ~ \"Hello, ~A!\")   ; (defun hello (name) (format t [\"Hello, ~A!~%\"]))
    (insert-right ~ 'name)     ; (defun hello (name) (format t [\"Hello, ~A!~%\"] name))
    ;; Reassembe the tree
    unzip)
  => (DEFUN HELLO-WORLD () (FORMAT T \"Hello, world!\"))
  ```

  Let's eval the form, run it, et voila`!

  ```
  (eval *)
  => HELLO

  (funcall * \"Matteo\")
  ..Hello, Matteo!
  => NIL
  ```
  ")

(pax:defsection @zip-reference (:title "Reference")
  (loc class)
  (nav class)
  (meta class)
  (zip generic-function)
  (unzip function)
  (down function)
  (up function)
  (left function)
  (leftmost function)
  (right function)
  (rightmost function)
  (next function)
  (next-that function)
  (prev function)
  (prev-that function)
  (insert-left function)
  (insert-right function)
  (replace function)
  (edit function)
  (insert-child function)
  (append-child function)
  (remove function))


;;;; Register in PAX World

(defun pax-sections ()
  (list @zip-manual))
(defun pax-pages ()
  `((:objects (, @pax-sections)
     :source-uri-fn ,(pax:make-github-source-uri-fn
                      :zip
                      "https://github.com/iamFIREcracker/cl-zip"))))
(pax:register-doc-in-pax-world :zip 'pax-sections 'pax-pages)

(defun build-doc ()
  (progn
    (pax:update-asdf-system-readmes @zip-manual :net.matteolandi.zip
                                    :formats '(:markdown))
    #+#:excluded (pax:update-asdf-system-html-docs @zip-manual :net.matteolandi.zip
                                                   :pages (pax-pages))))
#+#:excluded (build-doc)
