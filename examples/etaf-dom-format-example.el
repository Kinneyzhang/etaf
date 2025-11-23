;;; etaf-dom-format-example.el --- DOM format usage examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;;; Commentary:
;;
;; Examples demonstrating the DOM format representation of CSSOM,
;; render tree, and layout tree structures.
;;
;; DOM format allows these structures to be manipulated like DOM trees,
;; with a standardized interface: (tag ((attr . val) ...) children...)

;;; Code:

(require 'etaf-tml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)

;;; Example 1: CSSOM in DOM format

(defun etaf-dom-format-example-cssom ()
  "Demonstrate CSSOM conversion to DOM format."
  (interactive)
  (message "\n=== Example 1: CSSOM in DOM Format ===\n")
  
  ;; Create a simple DOM with styles
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { margin-left: 0px; padding-left: 20px; }
                    .container { width: 800px; background: #f0f0f0; }
                    .box { 
                      width: 200px; 
                      height: 100px; 
                      padding-left: 10px;
                      margin-left: 5px;
                      border-top-width: 2px;
                    }
                    #special { color: red !important; }
                  "))
                 (body
                  (div :class "container"
                   (div :class "box" "Box 1")
                   (div :class "box" :id "special" "Box 2"))))))
         
         ;; Build CSSOM (plist format)
         (cssom (etaf-css-build-cssom dom))
         
         ;; Convert to DOM format
         (cssom-dom (etaf-css-cssom-to-dom cssom)))
    
    (message "CSSOM structure type: %s" (car cssom-dom))
    (message "CSSOM is in DOM format: %s" 
             (and (listp cssom-dom) 
                  (eq (car cssom-dom) 'cssom)
                  (listp (cadr cssom-dom))))
    
    ;; Access attributes using DOM-like interface
    (let ((attrs (cadr cssom-dom)))
      (message "\nCSSOM attributes:")
      (message "  - inline-rules: %d rules" 
               (length (cdr (assq 'inline-rules attrs))))
      (message "  - style-rules: %d rules" 
               (length (cdr (assq 'style-rules attrs))))
      (message "  - all-rules: %d total rules" 
               (length (cdr (assq 'all-rules attrs))))
      
      ;; Show a sample rule
      (let ((first-rule (car (cdr (assq 'style-rules attrs)))))
        (when first-rule
          (message "\nSample style rule:")
          (message "  selector: %s" (plist-get first-rule :selector))
          (message "  declarations: %S" (plist-get first-rule :declarations))
          (message "  specificity: %S" (plist-get first-rule :specificity)))))
    
    ;; Convert back to plist format
    (let ((cssom-restored (etaf-css-cssom-from-dom cssom-dom)))
      (message "\nRound-trip conversion successful: %s"
               (equal (plist-get cssom :all-rules)
                      (plist-get cssom-restored :all-rules))))))

;;; Example 2: Render tree in DOM format

(defun etaf-dom-format-example-render ()
  "Demonstrate render tree conversion to DOM format."
  (interactive)
  (message "\n=== Example 2: Render Tree in DOM Format ===\n")
  
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    div { display: block; color: blue; }
                    span { display: inline; color: green; }
                    .hidden { display: none; }
                  "))
                 (body
                  (div "Parent"
                   (span "Child 1")
                   (div :class "hidden" "Hidden")
                   (span "Child 2"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         
         ;; Convert to DOM format
         (render-dom (etaf-render-to-dom render-tree)))
    
    (message "Render tree root: %s" (car render-dom))
    (message "Render tree is in DOM format: %s"
             (and (listp render-dom)
                  (eq (car render-dom) 'render-node)
                  (listp (cadr render-dom))))
    
    ;; Access attributes using DOM-like interface
    (let ((attrs (cadr render-dom)))
      (message "\nRoot render node:")
      (message "  tag: %s" (cdr (assq 'tag attrs)))
      (message "  display: %s" (cdr (assq 'display attrs)))
      (message "  computed-style entries: %d"
               (length (cdr (assq 'computed-style attrs)))))
    
    ;; Walk the tree in DOM format
    (message "\nWalking render tree (DOM format):")
    (cl-labels ((walk-dom (node depth)
                  (when (and node (eq (car node) 'render-node))
                    (let* ((attrs (cadr node))
                           (tag (cdr (assq 'tag attrs)))
                           (display (cdr (assq 'display attrs)))
                           (children (cddr node))
                           (indent (make-string (* depth 2) ?\s)))
                      (message "%s<%s> display=%s, children=%d"
                               indent tag display (length children))
                      ;; Recursively process children
                      (dolist (child children)
                        (walk-dom child (1+ depth)))))))
      (walk-dom render-dom 0))
    
    ;; Convert back to plist format
    (let ((render-restored (etaf-render-from-dom render-dom)))
      (message "\nRound-trip conversion preserves structure: %s"
               (eq (plist-get render-tree :tag)
                   (plist-get render-restored :tag))))))

;;; Example 3: Layout tree in DOM format

(defun etaf-dom-format-example-layout ()
  "Demonstrate layout tree conversion to DOM format."
  (interactive)
  (message "\n=== Example 3: Layout Tree in DOM Format ===\n")
  
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { 
                      margin-left: 0px; 
                      padding-left: 20px; 
                      width: 960px; 
                    }
                    .container { 
                      width: 800px; 
                      padding-left: 10px;
                      border-top-width: 2px;
                      margin-bottom: 10px;
                    }
                    .box { 
                      width: 200px; 
                      height: 100px; 
                      padding-left: 10px;
                      margin-left: 5px;
                    }
                  "))
                 (body
                  (div :class "container"
                   (div :class "box" "Box 1")
                   (div :class "box" "Box 2"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         
         ;; Convert to DOM format
         (layout-dom (etaf-layout-to-dom layout-tree)))
    
    (message "Layout tree root: %s" (car layout-dom))
    (message "Layout tree is in DOM format: %s"
             (and (listp layout-dom)
                  (eq (car layout-dom) 'layout-node)
                  (listp (cadr layout-dom))))
    
    ;; Access attributes using DOM-like interface
    (let* ((attrs (cadr layout-dom))
           (position (cdr (assq 'position attrs)))
           (box-model (cdr (assq 'box-model attrs)))
           (render-node (cdr (assq 'render-node attrs))))
      (message "\nRoot layout node:")
      (message "  position: x=%d, y=%d"
               (plist-get position :x)
               (plist-get position :y))
      (message "  box-model content: %dx%d"
               (plist-get (plist-get box-model :content) :width)
               (plist-get (plist-get box-model :content) :height))
      (message "  render node tag: %s" (plist-get render-node :tag)))
    
    ;; Walk the tree and display box model information
    (message "\nWalking layout tree (DOM format):")
    (cl-labels ((walk-layout (node depth)
                  (when (and node (eq (car node) 'layout-node))
                    (let* ((attrs (cadr node))
                           (render-node (cdr (assq 'render-node attrs)))
                           (tag (plist-get render-node :tag))
                           (position (cdr (assq 'position attrs)))
                           (box-model (cdr (assq 'box-model attrs)))
                           (content (plist-get box-model :content))
                           (padding (plist-get box-model :padding))
                           (children (cddr node))
                           (indent (make-string (* depth 2) ?\s)))
                      (message "%s<%s> pos=(%d,%d) size=%dx%d padding-left=%d"
                               indent
                               tag
                               (plist-get position :x)
                               (plist-get position :y)
                               (plist-get content :width)
                               (plist-get content :height)
                               (plist-get padding :left))
                      ;; Recursively process children
                      (dolist (child children)
                        (walk-layout child (1+ depth)))))))
      (walk-layout layout-dom 0))
    
    ;; Convert back to plist format
    (let ((layout-restored (etaf-layout-from-dom layout-dom)))
      (message "\nRound-trip conversion preserves box model: %s"
               (equal (plist-get layout-tree :box-model)
                      (plist-get layout-restored :box-model))))))

;;; Example 4: Complete pipeline with DOM format

(defun etaf-dom-format-example-pipeline ()
  "Demonstrate complete pipeline using DOM format for all structures."
  (interactive)
  (message "\n=== Example 4: Complete Pipeline with DOM Format ===\n")
  
  (let* (;; 1. Start with TML
         (tml '(html
                (head
                 (style "
                   .card { 
                     width: 300px; 
                     padding-left: 20px;
                     padding-top: 10px;
                     border-top-width: 1px;
                     margin-bottom: 15px;
                   }
                   .title { color: #333; font-weight: bold; }
                 "))
                (body
                 (div :class "card"
                  (h2 :class "title" "Card Title")
                  (p "Card content goes here.")))))
         
         ;; 2. Convert to DOM
         (dom (etaf-tml-to-dom tml))
         
         ;; 3. Build CSSOM and convert to DOM format
         (cssom (etaf-css-build-cssom dom))
         (cssom-dom (etaf-css-cssom-to-dom cssom))
         
         ;; 4. Build render tree and convert to DOM format
         (render-tree (etaf-render-build-tree dom cssom))
         (render-dom (etaf-render-to-dom render-tree))
         
         ;; 5. Build layout tree and convert to DOM format
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (layout-dom (etaf-layout-to-dom layout-tree)))
    
    (message "Pipeline stages in DOM format:")
    (message "  1. TML → DOM: (html ...)")
    (message "  2. DOM → CSSOM (DOM format): %s" (car cssom-dom))
    (message "  3. DOM + CSSOM → Render (DOM format): %s" (car render-dom))
    (message "  4. Render → Layout (DOM format): %s" (car layout-dom))
    
    (message "\nAll structures use consistent DOM format (tag attrs children):")
    (message "  CSSOM: %s" (and (eq (car cssom-dom) 'cssom) (listp (cadr cssom-dom))))
    (message "  Render: %s" (and (eq (car render-dom) 'render-node) (listp (cadr render-dom))))
    (message "  Layout: %s" (and (eq (car layout-dom) 'layout-node) (listp (cadr layout-dom))))
    
    ;; Demonstrate unified tree traversal
    (message "\nUnified tree traversal (all structures use same pattern):")
    (cl-labels ((count-nodes (tree)
                  (if (listp tree)
                      (+ 1 (apply #'+ (mapcar #'count-nodes (cddr tree))))
                    0)))
      (message "  CSSOM nodes: %d" (count-nodes cssom-dom))
      (message "  Render nodes: %d" (count-nodes render-dom))
      (message "  Layout nodes: %d" (count-nodes layout-dom)))))

;;; Example 5: Manipulating structures in DOM format

(defun etaf-dom-format-example-manipulation ()
  "Demonstrate manipulating structures in DOM format."
  (interactive)
  (message "\n=== Example 5: Manipulating DOM Format Structures ===\n")
  
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "div { width: 100px; }"))
                 (body
                  (div "Box 1")
                  (div "Box 2")
                  (div "Box 3")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (render-dom (etaf-render-to-dom render-tree)))
    
    ;; Example: Filter nodes by tag using DOM format
    (message "Finding all <div> nodes in render tree:")
    (cl-labels ((find-by-tag (tree tag-name)
                  (let ((result '()))
                    (when (and (listp tree) (eq (car tree) 'render-node))
                      (let* ((attrs (cadr tree))
                             (tag (cdr (assq 'tag attrs)))
                             (children (cddr tree)))
                        (when (eq tag tag-name)
                          (push tree result))
                        (dolist (child children)
                          (setq result (append result (find-by-tag child tag-name))))))
                    result)))
      (let ((divs (find-by-tag render-dom 'div)))
        (message "  Found %d <div> nodes" (length divs))
        (dolist (div divs)
          (let* ((attrs (cadr div))
                 (display (cdr (assq 'display attrs))))
            (message "    - display: %s" display)))))
    
    ;; Example: Extract all computed styles using DOM format
    (message "\nExtracting computed styles:")
    (cl-labels ((collect-styles (tree)
                  (let ((result '()))
                    (when (and (listp tree) (eq (car tree) 'render-node))
                      (let* ((attrs (cadr tree))
                             (style (cdr (assq 'computed-style attrs)))
                             (children (cddr tree)))
                        (when style
                          (push style result))
                        (dolist (child children)
                          (setq result (append result (collect-styles child))))))
                    result)))
      (let ((all-styles (collect-styles render-dom)))
        (message "  Total computed styles: %d" (length all-styles))
        (message "  Sample style: %S" (car all-styles))))
    
    (message "\nDOM format enables standard tree operations on all structures!")))

;;; Run all examples

(defun etaf-dom-format-run-all-examples ()
  "Run all DOM format examples."
  (interactive)
  (etaf-dom-format-example-cssom)
  (etaf-dom-format-example-render)
  (etaf-dom-format-example-layout)
  (etaf-dom-format-example-pipeline)
  (etaf-dom-format-example-manipulation)
  (message "\n\n=== All DOM Format Examples Complete ==="))

;; Run examples when file is loaded
(when (and (boundp 'load-file-name) load-file-name)
  (etaf-dom-format-run-all-examples))

(provide 'etaf-dom-format-example)
;;; etaf-dom-format-example.el ends here
