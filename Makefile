coverage:
	sbcl --load coverage.lisp

test:
	time sbcl --eval "(progn (asdf:test-system :cl-dns) (quit))"

.PHONY: docs
docs:
	sbcl --eval "(ql:quickload :staple-markdown)" --eval "(staple:generate :cl-dns :if-exists :supersede)" --eval "(quit)"
