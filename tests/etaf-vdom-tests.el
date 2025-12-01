(require 'etaf-ert)
(require 'etaf-etml)
(require 'etaf-vdom)
(require 'etaf-css-selector)
(require 'etaf-dom)
(require 'dom)

(setq-local lisp-indent-offset 2)

;;; ============================================================================
;;; Virtual DOM Tests
;;; ============================================================================

;;; Test VNode creation

(let ((vnode (etaf-vdom-element 'div :props '(:class "container"))))
  (should (etaf-vdom-vnode-p vnode))
  (should (etaf-vdom-element-p vnode))
  (should (eq (etaf-vdom-get-tag vnode) 'div))
  (should-equal (etaf-vdom-get-props vnode) '(:class "container")))

;;; Test text VNode creation

(let ((text-node (etaf-vdom-text "Hello World")))
  (should (etaf-vdom-vnode-p text-node))
  (should (etaf-vdom-text-p text-node))
  (should-equal (etaf-vdom-get-dom text-node) "Hello World"))

;;; Test etaf-etml-to-dom-with-vdom basic functionality

(let* ((result (etaf-etml-to-dom-with-vdom '(div :class "test" "Hello")))
       (vtree (etaf-vdom-result-get-vtree result))
       (dom (etaf-vdom-result-get-dom result)))
  ;; DOM should be clean (standard structure)
  (should-equal (car dom) 'div)
  (should-equal (cdr (assq 'class (cadr dom))) "test")
  ;; VTree should be a VNode
  (should (etaf-vdom-vnode-p vtree))
  (should (eq (etaf-vdom-get-tag vtree) 'div)))

;;; Test that tag-instance is in VTree, not DOM - for anchor tag

(let* ((result (etaf-etml-to-dom-with-vdom '(a :href "/test" "Link")))
       (vtree (etaf-vdom-result-get-vtree result))
       (dom (etaf-vdom-result-get-dom result))
       (tag-instance (etaf-vdom-get-tag-instance vtree)))
  ;; DOM should NOT have etaf-tag-instance
  (should-not (assq 'etaf-tag-instance (cadr dom)))
  ;; DOM should have href attribute
  (should (assq 'href (cadr dom)))
  ;; VTree should have tag-instance (because `a` tag has on-click handler)
  (should tag-instance)
  (should-equal (plist-get tag-instance :tag-name) 'a))

;;; Test that tag-instance is in VTree, not DOM - for button tag

(let* ((result (etaf-etml-to-dom-with-vdom '(button "Click Me")))
       (vtree (etaf-vdom-result-get-vtree result))
       (dom (etaf-vdom-result-get-dom result))
       (tag-instance (etaf-vdom-get-tag-instance vtree)))
  ;; DOM should NOT have etaf-tag-instance
  (should-not (assq 'etaf-tag-instance (cadr dom)))
  ;; VTree should have tag-instance
  (should tag-instance)
  (should-equal (plist-get tag-instance :tag-name) 'button)
  (should-equal (plist-get tag-instance :children) '("Click Me")))

;;; Test that div (no events) does NOT have tag-instance

(let* ((result (etaf-etml-to-dom-with-vdom '(div "No events")))
       (vtree (etaf-vdom-result-get-vtree result))
       (tag-instance (etaf-vdom-get-tag-instance vtree)))
  ;; div has no event handlers, so no tag-instance
  (should-not tag-instance))

;;; Test nested structure - tag-instance only in VTree

(let* ((result (etaf-etml-to-dom-with-vdom 
                '(div :class "container"
                      (p "Some text")
                      (a :href "/link" "Link text"))))
       (vtree (etaf-vdom-result-get-vtree result))
       (dom (etaf-vdom-result-get-dom result)))
  ;; DOM should be clean structure
  (should-equal (car dom) 'div)
  ;; Find the `a` element in DOM - should NOT have etaf-tag-instance
  (let ((a-elem (cl-find-if (lambda (child)
                              (and (listp child)
                                   (eq (car child) 'a)))
                            (cddr dom))))
    (should a-elem)
    (should-not (assq 'etaf-tag-instance (cadr a-elem))))
  ;; VTree children should include the `a` vnode with tag-instance
  (let* ((children (etaf-vdom-get-children vtree))
         (a-vnode (cl-find-if (lambda (child)
                                (and (etaf-vdom-element-p child)
                                     (eq (etaf-vdom-get-tag child) 'a)))
                              children)))
    (should a-vnode)
    (should (etaf-vdom-get-tag-instance a-vnode))))

;;; Test CSS selector returns clean DOM (the main use case from problem statement)

(let* ((dom (etaf-etml-to-dom
             '(div :class "rounded-xl bg-white p-10" :id "test-id"
                   (div :class "space-y-6"
                        (p "Text 1")
                        (p "Text 2"))
                   (p :class "mb-3" "Text 3")
                   (p :class "font-semibold"
                      (a :href "https://tailwindcss.com/docs"
                         :class "text-gray-950" "Read the docs →")))))
       (query-result (etaf-css-selector-query dom "div > p")))
  ;; Query result should return clean DOM nodes
  (dolist (node query-result)
    ;; Each node should be a DOM node, not contain etaf-tag-instance
    (when (eq (car node) 'p)
      (should-not (assq 'etaf-tag-instance (cadr node))))
    ;; Check nested `a` tag doesn't have etaf-tag-instance
    (dolist (child (cddr node))
      (when (and (listp child) (eq (car child) 'a))
        (should-not (assq 'etaf-tag-instance (cadr child)))))))

;;; Test VNode walking

(let* ((result (etaf-etml-to-dom-with-vdom 
                '(div (span "a") (span "b") (span "c"))))
       (vtree (etaf-vdom-result-get-vtree result))
       (count 0))
  ;; Walk all nodes and count
  (etaf-vdom-walk vtree (lambda (_) (setq count (1+ count))))
  ;; Should have 4 nodes: 1 div + 3 span
  ;; Plus 3 text nodes = 7 total
  (should (>= count 4)))

;;; Test VNode find

(let* ((result (etaf-etml-to-dom-with-vdom 
                '(div :class "outer"
                      (div :class "inner"
                           (button "Click")))))
       (vtree (etaf-vdom-result-get-vtree result))
       (button-vnode (etaf-vdom-find vtree
                                     (lambda (v)
                                       (and (etaf-vdom-element-p v)
                                            (eq (etaf-vdom-get-tag v) 'button))))))
  (should button-vnode)
  (should (etaf-vdom-get-tag-instance button-vnode)))

;;; Test VNode find-all

(let* ((result (etaf-etml-to-dom-with-vdom 
                '(ul
                  (li "Item 1")
                  (li "Item 2")
                  (li "Item 3"))))
       (vtree (etaf-vdom-result-get-vtree result))
       (li-vnodes (etaf-vdom-find-all vtree
                                       (lambda (v)
                                         (and (etaf-vdom-element-p v)
                                              (eq (etaf-vdom-get-tag v) 'li))))))
  (should-equal (length li-vnodes) 3))

(provide 'etaf-vdom-tests)
