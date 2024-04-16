(pushnew (merge-pathnames (parse-namestring "vendor/ml/")
                          *default-pathname-defaults*)
         asdf:*central-registry*)
(if (not (asdf:locate-system :3am))
  (pushnew (merge-pathnames (parse-namestring "vendor/3am/")
                            *default-pathname-defaults*)
           asdf:*central-registry*))


(asdf:defsystem #:net.matteolandi.zip
  :description "Zippers"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:3am
                 #:net.matteolandi.utils
              )

  :serial t
  :components (

               (:file "zip")

               )

  :in-order-to ((test-op (test-op #:net.matteolandi.zip/tests)))
  )

(asdf:defsystem #:net.matteolandi.zip/tests
  :depends-on (#:net.matteolandi.zip)
  :perform (test-op (o c) (uiop:symbol-call :3am '#:run)))
