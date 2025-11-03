(require 'dom)

(defun etml-dom-parse-html (html-string)
  "Parse HTML string into DOM tree (libxml-parse-html-region)."
  (with-temp-buffer
    (insert html-string)
    (libxml-parse-html-region (point-min) (point-max))))

(defun etml-dom-parse-sexp (sexp)
  "Parse SEXP into DOM tree."
  )

(defun etml-text-size (text face)
  "Measure width and height of TEXT in FACE."
  (save-window-excursion
    (let ((buffer (get-buffer-create "*temp-text-size*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (erase-buffer)
        (insert (propertize text 'face face))
        (let* ((width (car (window-text-pixel-size 
                            nil (point-min) (point-max))))
               (height (cdr (window-text-pixel-size 
                             nil (point-min) (point-max)))))
          (list width height))))))


(setq etml-test-node
      '(div ((width . 500) (height . 6))
            (p nil "happy hacking emacs")
            "this is text not in node"
            (div ((color . "red") (display . flex))
                 (button nil "OK")
                 (button nil "Cancel"))))

(dom-tag etml-test-node)
(dom-attr etml-test-node 'width)
(dom-attributes)

(dom-children etml-test-node)
(dom-non-text-children etml-test-node)

dom-text node
Return all the textual elements of the node as a concatenated string.

dom-texts node
Return all the textual elements of the node, as well as the textual elements of all the children of the node, recursively, as a concatenated string. This function also takes an optional separator to be inserted between the textual elements.

dom-parent dom node
Return the parent of node in dom.

dom-remove dom node
Remove node from dom.

The following are functions for altering the DOM.

dom-set-attribute node attribute value
Set the attribute of the node to value.

dom-remove-attribute node attribute
Remove attribute from node.

dom-append-child node child
Append child as the last child of node.

dom-add-child-before node child before
Add child to node’s child list before the before node. If before is nil, make child the first child.

dom-set-attributes node attributes
Replace all the attributes of the node with a new key/value list.

The following are functions for searching for elements in the DOM. They all return lists of matching nodes.

dom-by-tag dom tag
Return all nodes in dom that are of type tag. A typical use would be:

(dom-by-tag dom 'td)
=> '((td ...) (td ...) (td ...))
dom-by-class dom match
Return all nodes in dom that have class names that match match, which is a regular expression.

dom-by-style dom style
Return all nodes in dom that have styles that match match, which is a regular expression.

dom-by-id dom style
Return all nodes in dom that have IDs that match match, which is a regular expression.

dom-search dom predicate
Return all nodes in dom where predicate returns a non-nil value. predicate is called with the node to be tested as its parameter.

dom-strings dom
Return all strings in dom.

Utility functions:

dom-pp dom &optional remove-empty
Pretty-print dom at point. If remove-empty, don’t print textual nodes that just contain white-space.

dom-print dom &optional pretty xml
Print dom at point. If xml is non-nil, print as XML; otherwise, print as HTML. If pretty is non-nil, indent the HTML/XML logically.

(shr-insert-document
 '(body ((width . 200))
        (div ((class . "thing")
              (display . inline))
             (p nil "Foo")
             (div nil "Yes"))))


