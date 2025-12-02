;;; etaf-render-example.el --- Layout tree usage examples (formerly render tree) -*- lexical-binding: t; -*-

;;; Commentary:
;; Examples demonstrating layout tree construction.
;; Note: The render tree has been merged into the layout tree.
;; These examples now demonstrate layout tree operations.

;;; Code:

(require 'etaf-layout)
(require 'etaf-etml)
(require 'etaf-css)

;;; Example 1: Basic layout tree construction

(message "=== Example 1: Basic Layout Tree ===")

(setq example-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style "div { display: block; color: blue; }
                    span { display: inline; color: green; }"))
         (body
          (div "Header")
          (div
           (span "Text 1")
           (span "Text 2"))))))

(setq example-cssom (etaf-css-build-cssom example-dom))
(setq example-layout-tree (etaf-layout-build-tree example-dom example-cssom '(:width 1024 :height 768)))

(message "\nLayout tree structure:")
(message "%s" (etaf-layout-to-string example-layout-tree))

;;; Example 2: Filtering invisible elements

(message "\n=== Example 2: Filtering Invisible Elements ===")

(setq hidden-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style ".hidden { display: none; }
                    div { color: red; }"))
         (body
          (div "Visible 1")
          (div :class "hidden" "This is hidden")
          (div "Visible 2")))))

(setq hidden-cssom (etaf-css-build-cssom hidden-dom))
(setq hidden-layout-tree (etaf-layout-build-tree hidden-dom hidden-cssom '(:width 1024 :height 768)))

(message "\nVisible nodes in layout tree:")
(etaf-layout-walk hidden-layout-tree
  (lambda (node)
    (when (eq (dom-tag node) 'div)
      (message "  <div> display: %s" (etaf-layout-get-display node)))))

;;; Example 3: Querying computed styles

(message "\n=== Example 3: Querying Computed Styles ===")

(setq styled-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style "div { color: blue; font-size: 14px; }
                    #special { color: red; background: yellow; }"))
         (body
          (div "Normal div")
          (div :id "special" "Special div")))))

(setq styled-cssom (etaf-css-build-cssom styled-dom))
(setq styled-layout-tree (etaf-layout-build-tree styled-dom styled-cssom '(:width 1024 :height 768)))

(message "\nComputed styles for each div:")
(dolist (div-node (etaf-layout-find-by-tag styled-layout-tree 'div))
  (message "  <div>:")
  (message "    color: %s" (etaf-layout-get-style div-node 'color))
  (message "    font-size: %s" (etaf-layout-get-style div-node 'font-size))
  (when-let ((bg (etaf-layout-get-style div-node 'background)))
    (message "    background: %s" bg)))

;;; Example 4: Layout tree statistics

(message "\n=== Example 4: Render Tree Statistics ===")

(setq stats-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style "div { display: block; }
                    span { display: inline; }
                    p { display: block; }"))
         (body
          (div
           (span "Text")
           (div
            (p "Paragraph")
            (span "More text")))))))

(setq stats-cssom (etaf-css-build-cssom stats-dom))
(setq stats-layout-tree (etaf-layout-build-tree stats-dom stats-cssom '(:width 1024 :height 768)))
(setq stats (etaf-layout-stats stats-layout-tree))

(message "\nLayout tree statistics:")
(message "  Total nodes: %d" (plist-get stats :node-count))
(message "  Max depth: %d" (plist-get stats :max-depth))
(message "  Display types:")
(dolist (type-count (plist-get stats :display-types))
  (message "    %s: %d" (car type-count) (cdr type-count)))

;;; Example 5: Finding nodes by criteria

(message "\n=== Example 5: Finding Nodes ===")

(setq search-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style "div { display: block; }
                    .inline { display: inline; }"))
         (body
          (div "Block 1")
          (div :class "inline" "Inline div")
          (span "Span")
          (div "Block 2")))))

(setq search-cssom (etaf-css-build-cssom search-dom))
(setq search-layout-tree (etaf-layout-build-tree search-dom search-cssom '(:width 1024 :height 768)))

(message "\nAll block-level elements:")
(dolist (node (etaf-layout-find-by-display search-layout-tree "block"))
  (message "  <%s>" (dom-tag node)))

(message "\nAll inline elements:")
(dolist (node (etaf-layout-find-by-display search-layout-tree "inline"))
  (message "  <%s>" (dom-tag node)))

(message "\n=== Examples Complete ===")

(provide 'etaf-layout-example)
;;; etaf-layout-example.el ends here
