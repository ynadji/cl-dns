(in-package :cl-dns)

;; are these a good idea?
(defvar *normalize* t "Normalize all domain names prior to adding to a DOMAIN-TRIE or when checking membership. Normalization removes any whitespace or trailing #\. and lowercases the domain. Defaults to T. You almost certainly want this unless you're doing something weird.")
(defvar *normalize-chars* (append str:*whitespaces* '(#\.)))

(defstruct (domain-trie (:print-object print-domain-trie))
  (trie (make-hash-table :test #'equal) :type hash-table)
  (num-leafs 0 :type integer)
  (num-wildcards 0 :type integer)
  (num-zone-cuts 0 :type integer))

(defun print-domain-trie (dt stream)
  (print-unreadable-object (dt stream :type t)
    (format stream "<TLDs: ~a, FQDNs: ~a, *: ~a, +: ~a>"
            (hash-table-count (domain-trie-trie dt))
            (domain-trie-num-leafs dt)
            (domain-trie-num-wildcards dt)
            (domain-trie-num-zone-cuts dt))))

(defmacro without-normalization (&rest body)
  "Do not use NORMALIZE-DOMAIN for all forms in BODY."
  `(let ((*normalize* nil))
     ,@body))

(defun normalize-domain (domain)
  (if *normalize*
      (str:trim (str:downcase domain) :char-bag *normalize-chars*)
      domain))

(defun valid-domain? (domain)
  "Returns T if DOMAIN is syntactically valid, otherwise NIL."
  (let ((domain (idna:to-ascii domain)))
    (and (not (str:empty? domain))
         (< (length (str:replace-all "." "" domain)) 256)
         (not (ignore-errors (netaddr:make-ip-address domain)))
         (cl-ppcre:scan "^([a-zA-Z0-9_]{1}[a-zA-Z0-9_-]{0,62}){1}(\\.[a-zA-Z0-9_]{1}[a-zA-Z0-9_-]{0,62})*[\\._]?$" domain))))

(defun registerable-domain? (domain)
  "Returns T if the domain is syntactically valid, has an ICANN-listed domain
name (public or private), and is not itself a TLD. E.g., \"foo.google.com\" and
\"google.com\" are both registerable domains, but \"com\" is not. When the
DOMAIN is not registerable, a second value may be returned with the SIMPLE-ERROR
describing the failure condition from CL-TLD."
  (and (valid-domain? domain)
       (ignore-errors
        (multiple-value-bind (e2ld type)
            (cl-tld:get-domain-suffix (idna:to-unicode domain)) ; PSL uses Unicode instead of Punycode.
          (declare (ignore e2ld))
          (not (eq type :unmanaged))))))

(defun get-domain-parts (domain)
  (let ((domain (normalize-domain domain)))
   (let ((parts (str:split #\. domain)))
     (when (string= "*" (first parts))
       (setf (first parts) :wildcard))
     (when (string= "+" (first parts))
       (setf (first parts) :zone-cut))
     (reverse parts))))

(defun %add-domain (trie domain-parts &optional (added? nil))
  (if (null domain-parts)
      added?
      (ax:if-let ((child (gethash (first domain-parts) trie)))
        (%add-domain child (rest domain-parts) added?)
        (let ((child (make-hash-table :test #'equal)))
          (cond ((and (= 1 (length domain-parts))
                      (not (keywordp (first domain-parts))))
                 (setf (gethash :leaf child) :leaf))
                ((and (= 1 (length domain-parts))
                      (keywordp (first domain-parts)))
                 (setf child (first domain-parts))))
          (setf (gethash (first domain-parts) trie) child)
          (%add-domain child (rest domain-parts) t)))))

(defun add-domain (trie domain)
  "Add DOMAIN to the TRIE. Modifies TRIE in-place and returns (VALUES TRIE ADDED?) where ADDED? is NIL if it was already in the TRIE, otherwise returns T."
  (let ((added? (%add-domain (domain-trie-trie trie) (get-domain-parts domain))))
    (when added?
      (case (char domain 0)
        (#\+ (incf (domain-trie-num-wildcards trie)))
        (#\* (incf (domain-trie-num-zone-cuts trie)))
        (otherwise (incf (domain-trie-num-leafs trie)))))
    (values trie added?)))

(defun make-trie (&rest domains)
  "Creates and returns a TRIE that contains all DOMAINS."
  (let ((trie (make-domain-trie)))
    (loop for domain in domains do (add-domain trie domain))
    trie))

(defun contains-domain? (trie domain)
  "Checks if TRIE contains DOMAIN. Returns one of :LEAF, :WILDCARD, or :ZONE-CUT to
flag the type of match that was seen."
  (let* ((domain-parts (get-domain-parts domain))
         (num-parts (length domain-parts))
         (trie (domain-trie-trie trie)))
    (loop for dp in domain-parts for i from 1
          while (setf trie (gethash dp trie)) do
            (ax:if-let ((type (or (gethash :wildcard trie)
                                  (gethash :zone-cut trie))))
              (when (or (eq type :wildcard)
                        (< i num-parts)) ; :ZONE-CUT must have at least one more label.
                (return-from contains-domain? type))))
    (when trie
      (or (gethash :leaf trie)
          (gethash :wildcard trie)))))
