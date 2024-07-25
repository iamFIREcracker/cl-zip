(pushnew (merge-pathnames (parse-namestring "vendor/ml/")
                          *default-pathname-defaults*)
         asdf:*central-registry*)
(pushnew (merge-pathnames (parse-namestring "vendor/3am/")
                          *default-pathname-defaults*)
         asdf:*central-registry*)


;;; See ZIP::@ZIP-MANUAL for the user guide.
(asdf:defsystem #:net.matteolandi.zip
  :description "Zippers"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:3am
                 #:net.matteolandi.utils

                 #:mgl-pax/document
              )

  :serial t
  :components (

               (:file "zip")
               (:file "doc")

               )

  :in-order-to ((test-op (test-op #:net.matteolandi.zip/tests)))
  )

(asdf:defsystem #:net.matteolandi.zip/doc
  :depends-on (

               #:net.matteolandi.zip

               #:mgl-pax/navigate

               )
  :perform (build-op (o c) (uiop:symbol-call :zip '#:build-doc)))

(asdf:defsystem #:net.matteolandi.zip/tests
  :depends-on (#:net.matteolandi.zip)
  :components (

               (:module "tests"
                :components (
                             (:file "aoc")
                             (:static-file "aoc.txt")
                ))

               )

  :perform (test-op (o c) (uiop:symbol-call :3am '#:run)))
