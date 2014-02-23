;;; tinderbox to org-mode conversion

(require 'xml)
(require 'dash)
(require 's)

(defun tbx-import-file (file &optional dest-buffer)
  "imports tinderbox FILE into DEST-BUFFER (or current buffer, if nil)"
  ;; TODO: use try/catch for badly formatted files
  (condition-case nil
      (let* ((dest-buffer (or dest-buffer (current-buffer)))
          (xml-file    (xml-parse-file file))
          (doc-root    (cdddr (cdddar xml-file)))
          (parent-node (car (xml-get-children doc-root 'item)))
          (leaves      (xml-get-children parent-node 'item)))
     (with-current-buffer dest-buffer (org-mode))
     (mapcar (lambda (node)
               (insert-org-node-from-tbx-node node dest-buffer 1))
             leaves))
    (error
     (message "Couldn't parse Tinderbox file"))))

(defun tbx-get-node-attribute (tbx-node attr)
  "get named 'key attribute' (ATTR) of a tinderbox
node (TBX-NODE). returns NIL if none is found"
  (catch 'found
    (let ((attributes (xml-get-children tbx-node 'attribute)))
      (while attributes
        (let* ((candidate      (car attributes))
               (candidate-name (xml-get-attribute candidate 'name)))
          (if (string= candidate-name attr)
              (throw 'found (caddr candidate)))
        (setq attributes (cdr attributes)))))))

(defun tbx-node-title (tbx-node)
  "returns NAME property of a tinderbox node"
  (tbx-get-node-attribute tbx-node "Name"))

(defun tbx-node-text (tbx-node)
  "returns TEXT content of a tinderbox node"
  (caddar (xml-get-children tbx-node 'text)))

(defun tbx-node-defined-attributes (tbx-node)
  "returns all 'key attributes' defined on TBX-NODE"
  (let* ((prop-string (tbx-get-node-attribute tbx-node "KeyAttributes")))
    (if (not (null prop-string))
      (split-string prop-string ";"))))

(defun tbx-node-named-attributes (tbx-node attrs)
  "returns an alist of property/value pairs of TBX-NODE
given the list of property specified in ATTRS"
  (let* ((values (mapcar (lambda (attr) (tbx-get-node-attribute tbx-node attr))
                        attrs)))
    (-zip attrs values)))

(defun tbx-node-key-attributes (tbx-node)
  "returns all key attributes defined on TBX-NODE
in the form of property/value pairs, if value is not nil"
  ;; TODO: filter out pairs with no value
  (let ((attr-list (tbx-node-defined-attributes tbx-node)))
    (tbx-node-named-attributes tbx-node attr-list)))

;; (defun tbx-set-org-property-from-node (tbx-node attr)
;;   "assumes buffer at org header position"
;;   (org-set-property attr
;;                     (tbx-get-node-attribute tbx-node attr)))

(defun tbx-set-org-property-from-tbx-attribute (attr)
  "set org property at point from attribute/value pair of tinderbox node"
  (let ((prop (car attr))
        (val  (cdr attr)))
    (if (not (eq val nil))
        (org-set-property prop val))))

(defun insert-org-node-from-tbx-node (tbx-node dest-buffer &optional level)
  "write TBX-NODE and descendents into org-mode DEST-BUFFER. Starting at LEVEL"
  (let ((node-title      (tbx-node-title tbx-node))
        (node-properties (tbx-node-key-attributes tbx-node))
        (node-text       (tbx-node-text tbx-node))
        (node-children   (tbx-children tbx-node)))

    (with-current-buffer dest-buffer
      ;; heading
      (let* ((org-prefix (s-repeat level "*"))
             (heading    node-title))
        (insert (concat org-prefix " " heading "\n")))
      ;; properties
      (mapcar (lambda (pair) (tbx-set-org-property-from-tbx-attribute pair))
              node-properties)
      ;; text
      (if (not (null node-text))
        (insert (concat "\n" node-text "\n"))))
      ;; recur for node children
      (mapcar (lambda (child)
                (insert-org-node-from-tbx-node child
                                               dest-buffer
                                               (1+ level)))
              node-children)))

(defun tbx-children (tbx-node)
  "returns children of TBX-NODE in document tree, or NIL if node is a leaf"
  (xml-get-children tbx-node 'item))

(provide 'tbx)
