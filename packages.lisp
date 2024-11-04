(in-package :cl-user)
(defpackage :cl-dns
  (:use :cl)
  (:local-nicknames (:ax :alexandria))
  (:export :valid? :registerable-domain? :make-trie :contains?))
