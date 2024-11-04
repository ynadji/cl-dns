(asdf:defsystem cl-dns
  :serial t
  :description "DNS manipulation library"
  :author "Yacin Nadji <yacin@defmacro.cc>"
  :license "MIT"
  :depends-on ("str" "uiop" "alexandria" "cl-ppcre" "cl-tld" "netaddr" "idna")
  :components ((:file "packages")
               (:file "main"))
  :in-order-to ((test-op (test-op :cl-dns/tests))))

(asdf:defsystem :cl-dns/tests
  :author "Yacin Nadji <yacin@defmacro.cc>"
  :license "MIT"
  :depends-on ("cl-dns" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (uiop:find-symbol* '#:tests
                                                          '#:cl-dns/tests))))
