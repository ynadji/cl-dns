(defpackage cl-dns/tests
  (:use #:cl #:cl-dns)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is)
  (:export #:tests))

(in-package :cl-dns/tests)

(def-suite tests)
(in-suite tests)

(test normalize-domain
  (is (string= "google.com" (normalize-domain "  GoOgLe.com. ")))
  (without-normalization
   (is (not (string= "google.com" (normalize-domain "  GoOgLe.com. "))))))

(test valid-domain?
  (is (valid-domain? "google.com"))
  (is (not (valid-domain? "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-too-long.com")))
  (is (not (valid-domain? "!@#$%^&*().com")))
  (is (valid-domain? "foo.bar.baz.biz.faketld"))
  (is (valid-domain? "cAsEsHoUlNdNoTmAtTeR.com"))
  (is (not (valid-domain? "-cantstartwithahyphentho.com")))
  (is (valid-domain? "underscores_are_fine_chars.com"))
  (is (valid-domain? "_are_fine_chars.com")))

(test registerable-domain?
  (is (registerable-domain? "foo.com"))
  (is (not (registerable-domain? "test")))
  (is (not (registerable-domain? "com")))                  ; Rejects eTLDs
  (is (not (registerable-domain? "foo.test")))             ; Reserved TLDs see https://tools.ietf.org/html/rfc2606#page-2
  (is (not (registerable-domain? "foo.example")))
  (is (not (registerable-domain? "foo.invalid")))
  (is (not (registerable-domain? "foo.localhost")))
  (is (not (registerable-domain? "example")))
  (is (not (registerable-domain? "invalid")))
  (is (not (registerable-domain? "localhost")))
  (is (not (registerable-domain? "foo.baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"))) ; too long, can never be valid TLD
  (is (registerable-domain? "万岁.中国"))                   ; Unicode
  (is (registerable-domain? "xn--chqu66a.xn--fiqs8s"))     ; Above in punycode
  (is (registerable-domain? "ésta.bien.es"))               ; Unicode
  (is (registerable-domain? "xn--sta-9la.bien.es"))        ; Above in punycode
  (is (registerable-domain? "ياسين.كوم"))                    ; Works with reverse directional unicode
  (is (registerable-domain? "xn--mgby9cnc.xn--fhbei"))     ; Above in punycode
  ;; Should fail with leading/trailing whitespace
  (is (not (registerable-domain? "  google.com")))
  (is (not (registerable-domain? "google.com  ")))
  ;; All of the above, but with invalid domain characters
  (is (not (registerable-domain? "!foo.com")))
  (is (not (registerable-domain? "!test")))
  (is (not (registerable-domain? "!com")))
  (is (not (registerable-domain? "!foo.test")))
  (is (not (registerable-domain? "!foo.example")))
  (is (not (registerable-domain? "!foo.invalid")))
  (is (not (registerable-domain? "!foo.localhost")))
  (is (not (registerable-domain? "!example")))
  (is (not (registerable-domain? "!invalid")))
  (is (not (registerable-domain? "!localhost")))
  (is (not (registerable-domain? "!foo.baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar")))
  (is (not (registerable-domain? "!万岁.中国")))
  (is (not (registerable-domain? "!xn--chqu66a.xn--fiqs8s")))
  (is (not (registerable-domain? "¡ésta.bien!.es")))
  (is (not (registerable-domain? "¿xn--sta-9la.bien?.es")))
  (is (not (registerable-domain? "!ياسين.كوم")))
  (is (not (registerable-domain? "!xn--mgby9cnc.xn--fhbei")))
  (is (not (registerable-domain? "!@#$%^&*.com")))
  (is (not (registerable-domain? "dyndns-at-work.com"))) ; Private eTLD
  )

(test get-domain-parts
  (is (equalp '("com" "google" "www") (cl-dns::get-domain-parts "www.google.com")))
  (is (equalp '("com" "google" :wildcard) (cl-dns::get-domain-parts "*.google.com")))
  (is (equalp '("com" "google" :zone-cut) (cl-dns::get-domain-parts "+.google.com")))
  (is (equalp '("com" "google" "+" "nope") (cl-dns::get-domain-parts "nope.+.google.com"))))

(defun random-label ()
  (coerce (loop repeat 41 collect (elt "abcdefghijklmnopqrstuvwxyz" (random 26))) 'simple-string))

(defvar *trie* (make-trie "   www.google.com." "+.NADJI.us   " "  *.org. " "foo.BAR.baz.COM"))
(defvar *unnormalized-trie* (without-normalization (make-trie "   www.google.com." "+.NADJI.us   " "  *.org. " "foo.BAR.baz.COM")))

(test contains-domain?
  (is (contains-domain? *trie* "www.google.com"))
  (is (contains-domain? *trie* "WWW.GOOGLE.COM."))
  (is (not (contains-domain? *trie* "google.com")))
  (is (not (contains-domain? *trie* "foo.google.com")))
  (is (contains-domain? *trie* "yacin.nadji.us"))
  (is (contains-domain? *trie* (format nil "~a.nadji.us" (random-label))))
  (is (contains-domain? *trie* (format nil "~a.~a.nadji.us" (random-label) (random-label))))
  (is (not (contains-domain? *trie* "nadji.us")))
  (is (contains-domain? *trie* "org"))
  (is (contains-domain? *trie* (format nil "~a.org" (random-label))))
  (is (contains-domain? *trie* (format nil "~a.~a.org" (random-label) (random-label))))
  (is (contains-domain? *trie* (format nil "~a.~a.~a.org" (random-label) (random-label) (random-label))))
  (is (contains-domain? *trie* "foo.bar.baz.com"))
  (is (not (contains-domain? *trie* "bar.baz.com")))
  (is (not (contains-domain? *trie* "baz.com")))
  (is (not (contains-domain? *trie* "com.baz.bar.foo")))
  (is (not (contains-domain? *trie* "baz.bar.foo")))
  (is (not (contains-domain? *trie* "foo.baz.bar.com")))
  (is (not (contains-domain? *trie* "bing.com")))

  ;; unnormalized trie
  (is (not (contains-domain? *unnormalized-trie* "www.google.com")))
  (is (not (contains-domain? *unnormalized-trie* "WWW.GOOGLE.COM.")))
  (is (not (contains-domain? *unnormalized-trie* "yacin.nadji.us")))
  (is (not (contains-domain? *unnormalized-trie* (format nil "~a.nadji.us" (random-label)))))
  (is (not (contains-domain? *unnormalized-trie* (format nil "~a.~a.nadji.us" (random-label) (random-label)))))
  (is (not (contains-domain? *unnormalized-trie* "org")))
  (is (not (contains-domain? *unnormalized-trie* (format nil "~a.org" (random-label)))))
  (is (not (contains-domain? *unnormalized-trie* (format nil "~a.~a.org" (random-label) (random-label)))))
  (is (not (contains-domain? *unnormalized-trie* (format nil "~a.~a.~a.org" (random-label) (random-label) (random-label)))))
  (is (not (contains-domain? *unnormalized-trie* "foo.bar.baz.com")))
