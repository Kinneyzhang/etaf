;;; etaf-render-example.el --- Render tree usage examples -*- lexical-binding: t; -*-

;;; Commentary:
;; Examples demonstrating render tree construction and usage

;;; Code:

(require 'etaf-render)
(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-ecss)

;;; Example 1: Basic render tree construction

(message "=== Example 1: Basic Render Tree ===")

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
(setq example-render-tree (etaf-render-build-tree example-dom example-cssom))

(message "\nRender tree structure:")
(message "%s" (etaf-render-to-string example-render-tree))

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
(setq hidden-render-tree (etaf-render-build-tree hidden-dom hidden-cssom))

(message "\nVisible nodes in render tree:")
(etaf-render-walk hidden-render-tree
  (lambda (node)
    (when (eq (dom-tag node) 'div)
      (message "  <div> display: %s" (etaf-render-get-display node)))))

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
(setq styled-render-tree (etaf-render-build-tree styled-dom styled-cssom))

(message "\nComputed styles for each div:")
(dolist (div-node (etaf-render-find-by-tag styled-render-tree 'div))
  (message "  <div>:")
  (message "    color: %s" (etaf-render-get-style div-node 'color))
  (message "    font-size: %s" (etaf-render-get-style div-node 'font-size))
  (when-let ((bg (etaf-render-get-style div-node 'background)))
    (message "    background: %s" bg)))

;;; Example 4: Render tree statistics

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
(setq stats-render-tree (etaf-render-build-tree stats-dom stats-cssom))
(setq stats (etaf-render-stats stats-render-tree))

(message "\nRender tree statistics:")
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
(setq search-render-tree (etaf-render-build-tree search-dom search-cssom))

(message "\nAll block-level elements:")
(dolist (node (etaf-render-find-by-display search-render-tree "block"))
  (message "  <%s>" (dom-tag node)))

(message "\nAll inline elements:")
(dolist (node (etaf-render-find-by-display search-render-tree "inline"))
  (message "  <%s>" (dom-tag node)))

(message "\n=== Examples Complete ===")

(provide 'etaf-render-example)
;;; etaf-render-example.el ends here
