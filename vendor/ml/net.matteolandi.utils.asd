(asdf:defsystem #:net.matteolandi.utils
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :serial t
  :depends-on (#:assoc-utils #:named-readtables)
  :components
  (
   (:file "mlutils-package")
   (:file "mlsyntax")
   (:file "mlutils")
   )
  )
