SBCL_BIN ?= sbclw
SBCL_ARGS ?= --noinform


.PHONY: test
test:
	${SBCL_BIN} ${SBCL_ARGS} \
		--eval "(pushnew '*default-pathname-defaults* asdf:*central-registry*)" \
		--eval "(handler-case (ql:quickload :net.matteolandi.zip/tests) \
			  (error (a) \
			    (format t \"caught error ~s~%~a~%\" a a) \
			    (uiop:quit 17)))" \
		--eval "(handler-case (time (asdf:test-system :net.matteolandi.zip)) \
			  (error (a) \
			    (format T \"caught error ~s~%~a~%\" a a) \
			    (uiop:quit 13)))" \
		--eval "(uiop:quit 0)"


### Boilerplate ##############################################################

.PHONY: vendor
vendor: ml 3am

.PHONY: ml
ml:
	mkdir -p vendor/ml
	cp ~/Workspace/mlutils/mlsyntax.lisp                vendor/ml/
	cp ~/Workspace/mlutils/mlutils-package.lisp         vendor/ml/
	cp ~/Workspace/mlutils/mlutils.lisp                 vendor/ml/
	cp ~/Workspace/mlutils/net.matteolandi.utils.asd    vendor/ml/

.PHONY: 3am
3am:
	mkdir -p vendor/3am
	cp ~/Workspace/3am/3am.asd          vendor/3am/
	cp ~/Workspace/3am/3am.lisp         vendor/3am/
