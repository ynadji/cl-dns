(in-package :cl-dns)

;; change trie to a struct

(defun valid-domain? (domain)
  (let ((domain (idna:to-ascii domain)))
   (and (not (str:empty? domain))
        (< (length (str:replace-all "." "" domain)) 256)
        (not (ignore-errors (netaddr:make-ip-address domain)))
        (cl-ppcre:scan "^([a-zA-Z0-9_]{1}[a-zA-Z0-9_-]{0,62}){1}(\\.[a-zA-Z0-9_]{1}[a-zA-Z0-9_-]{0,62})*[\\._]?$" domain))))

(defun registerable-domain? (domain)
  (and (valid-domain? domain)
       (ignore-errors
        (multiple-value-bind (e2ld type)
            (cl-tld:get-domain-suffix (idna:to-unicode domain)) ; PSL uses Unicode instead of Punycode.
          (declare (ignore e2ld))
          (not (eq type :unmanaged))))))

(defun get-domain-parts (domain)
  (let ((parts (str:split #\. domain)))
    (when (string= "*" (first parts))
      (setf (first parts) :wildcard))
    (when (string= "+" (first parts))
      (setf (first parts) :zone-cut))
    (reverse parts)))

(defun make-trie (&rest domains)
  (let ((trie (make-hash-table :test #'equal)))
    (loop for domain in domains do
      (add-domain trie domain))
    trie))

(defun %add-domain (trie domain-parts &optional (original-trie nil) (added? nil))
  (unless original-trie
    (setf original-trie trie))
  (if (null domain-parts)
      (values original-trie added?)
      (ax:if-let ((child (gethash (first domain-parts) trie)))
        (%add-domain child (rest domain-parts) original-trie added?)
        (let ((child (make-hash-table :test #'equal)))
          (cond ((and (= 1 (length domain-parts))
                      (not (keywordp (first domain-parts))))
                 (setf (gethash :leaf child) :leaf))
                ((and (= 1 (length domain-parts))
                      (keywordp (first domain-parts)))
                 (setf child (first domain-parts))))
          (setf (gethash (first domain-parts) trie) child)
          (%add-domain child (rest domain-parts) original-trie t)))))

(defun add-domain (trie domain)
  (%add-domain trie (get-domain-parts domain)))

(defun contains? (trie domain)
  (let* ((domain-parts (get-domain-parts domain))
         (num-parts (length domain-parts)))
    (loop for dp in domain-parts for i from 1
          while (setf trie (gethash dp trie)) do
            (ax:if-let ((type (or (gethash :wildcard trie)
                                  (gethash :zone-cut trie))))
              (when (or (eq type :wildcard)
                        (< i num-parts)) ; :ZONE-CUT must have at least one more label.
               (return-from contains? type))))
    (when trie
      (or (gethash :leaf trie)
          (gethash :wildcard trie)))))
