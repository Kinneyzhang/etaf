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

;;; ============================================================================
;;; Vue 3 Inspired Features Tests
;;; ============================================================================

;;; Test fragment VNode creation

(let ((fragment (etaf-vdom-fragment
                 (etaf-vdom-element 'div)
                 (etaf-vdom-element 'span)
                 (etaf-vdom-element 'p))))
  (should (etaf-vdom-vnode-p fragment))
  (should (etaf-vdom-fragment-p fragment))
  (should-equal (length (etaf-vdom-get-children fragment)) 3))

;;; Test comment VNode creation

(let ((comment (etaf-vdom-comment "This is a comment")))
  (should (etaf-vdom-vnode-p comment))
  (should (etaf-vdom-comment-p comment))
  (should-equal (etaf-vdom-get-content comment) "This is a comment"))

;;; Test key extraction from props

(let ((vnode (etaf-vdom-element 'div :props '(:key "unique-1" :class "box"))))
  (should-equal (etaf-vdom-get-key vnode) "unique-1"))

(let ((vnode (etaf-vdom-element 'div :props '(:id "my-id" :class "box"))))
  (should-equal (etaf-vdom-get-key vnode) "my-id"))

;;; Test same-type-p predicate

(let ((vnode1 (etaf-vdom-element 'div :props '(:class "a")))
      (vnode2 (etaf-vdom-element 'div :props '(:class "b"))))
  (should (etaf-vdom-same-type-p vnode1 vnode2)))

(let ((vnode1 (etaf-vdom-element 'div :props '(:key "1")))
      (vnode2 (etaf-vdom-element 'div :props '(:key "2"))))
  (should-not (etaf-vdom-same-type-p vnode1 vnode2)))

(let ((vnode1 (etaf-vdom-element 'div))
      (vnode2 (etaf-vdom-element 'span)))
  (should-not (etaf-vdom-same-type-p vnode1 vnode2)))

;;; Test lifecycle hooks

(let ((vnode (etaf-vdom-element 'div))
      (mounted-called nil)
      (updated-called nil))
  (etaf-vdom-add-hook vnode :mounted
                      (lambda (node) (setq mounted-called t)))
  (etaf-vdom-add-hook vnode :updated
                      (lambda (node old) (setq updated-called t)))
  
  ;; Test mount
  (etaf-vdom-mount vnode)
  (should mounted-called)
  (should (etaf-vdom-get-mounted-p vnode))
  
  ;; Test update
  (let ((new-vnode (etaf-vdom-element 'div :props '(:class "updated"))))
    (etaf-vdom-add-hook new-vnode :updated
                        (lambda (node old) (setq updated-called t)))
    (etaf-vdom-update vnode new-vnode)
    (should updated-called)))

;;; Test unmount

(let ((vnode (etaf-vdom-element 'div))
      (unmounted-called nil))
  (etaf-vdom-add-hook vnode :unmounted
                      (lambda (node) (setq unmounted-called t)))
  (etaf-vdom-mount vnode)
  (should (etaf-vdom-get-mounted-p vnode))
  (etaf-vdom-unmount vnode)
  (should unmounted-called)
  (should-not (etaf-vdom-get-mounted-p vnode)))

;;; Test diff: CREATE patch

(let* ((old-vnode nil)
       (new-vnode (etaf-vdom-element 'div))
       (patches (etaf-vdom-diff old-vnode new-vnode)))
  (should-equal (length patches) 1)
  (should-equal (etaf-vdom-patch-type (car patches)) 'create)
  (should-equal (etaf-vdom-patch-new-vnode (car patches)) new-vnode))

;;; Test diff: REMOVE patch

(let* ((old-vnode (etaf-vdom-element 'div))
       (new-vnode nil)
       (patches (etaf-vdom-diff old-vnode new-vnode)))
  (should-equal (length patches) 1)
  (should-equal (etaf-vdom-patch-type (car patches)) 'remove)
  (should-equal (etaf-vdom-patch-old-vnode (car patches)) old-vnode))

;;; Test diff: REPLACE patch (different types)

(let* ((old-vnode (etaf-vdom-element 'div))
       (new-vnode (etaf-vdom-element 'span))
       (patches (etaf-vdom-diff old-vnode new-vnode)))
  (should-equal (length patches) 1)
  (should-equal (etaf-vdom-patch-type (car patches)) 'replace))

;;; Test diff: UPDATE patch (same type, different props)

(let* ((old-vnode (etaf-vdom-element 'div :props '(:class "old")))
       (new-vnode (etaf-vdom-element 'div :props '(:class "new")))
       (patches (etaf-vdom-diff old-vnode new-vnode)))
  ;; Should have at least an update patch for the prop change
  (should (> (length patches) 0))
  (let ((update-patch (cl-find-if
                       (lambda (p) (eq (etaf-vdom-patch-type p) 'update))
                       patches)))
    (should update-patch)
    (should (plist-get (etaf-vdom-patch-props update-patch) :class))))

;;; Test diff: No changes

(let* ((old-vnode (etaf-vdom-element 'div :props '(:class "same")))
       (new-vnode (etaf-vdom-element 'div :props '(:class "same")))
       (patches (etaf-vdom-diff old-vnode new-vnode)))
  ;; No property changes means no update patch
  (should-equal (length patches) 0))

;;; Test diff-props: Detect changes

(let* ((old-vnode (etaf-vdom-element 'div :props '(:class "a" :id "x")))
       (new-vnode (etaf-vdom-element 'div :props '(:class "b" :id "x")))
       (changes (etaf-vdom-diff-props old-vnode new-vnode)))
  (should-equal (plist-get changes :class) "b")
  (should-not (plist-member changes :id)))

;;; Test diff-props: Detect removals

(let* ((old-vnode (etaf-vdom-element 'div :props '(:class "a" :id "x")))
       (new-vnode (etaf-vdom-element 'div :props '(:class "a")))
       (changes (etaf-vdom-diff-props old-vnode new-vnode)))
  (should (plist-member changes :id))
  (should-equal (plist-get changes :id) nil))

;;; Test diff-children: Ordered diff (no keys)

(let* ((old-vnode (etaf-vdom-element 'ul))
       (new-vnode (etaf-vdom-element 'ul))
       (old-children (list (etaf-vdom-element 'li :props '(:class "a"))
                          (etaf-vdom-element 'li :props '(:class "b"))))
       (new-children (list (etaf-vdom-element 'li :props '(:class "a"))
                          (etaf-vdom-element 'li :props '(:class "c"))
                          (etaf-vdom-element 'li :props '(:class "d")))))
  (etaf-vdom-set-children old-vnode old-children)
  (etaf-vdom-set-children new-vnode new-children)
  (let ((patches (etaf-vdom-diff old-vnode new-vnode)))
    ;; Should have patches for changed and added children
    (should (> (length patches) 0))))

;;; Test diff-children: Keyed diff

(let* ((old-vnode (etaf-vdom-element 'ul))
       (new-vnode (etaf-vdom-element 'ul))
       (old-children (list (etaf-vdom-element 'li :props '(:key "1" :class "a"))
                          (etaf-vdom-element 'li :props '(:key "2" :class "b"))
                          (etaf-vdom-element 'li :props '(:key "3" :class "c"))))
       (new-children (list (etaf-vdom-element 'li :props '(:key "3" :class "c"))
                          (etaf-vdom-element 'li :props '(:key "1" :class "a"))
                          (etaf-vdom-element 'li :props '(:key "4" :class "d")))))
  (etaf-vdom-set-children old-vnode old-children)
  (etaf-vdom-set-children new-vnode new-children)
  (let ((patches (etaf-vdom-diff old-vnode new-vnode)))
    ;; Should have patches including reorder and create
    (should (> (length patches) 0))
    ;; Should have a create patch for key "4"
    (should (cl-some (lambda (p) (eq (etaf-vdom-patch-type p) 'create)) patches))
    ;; Should have a remove patch for key "2"
    (should (cl-some (lambda (p) (eq (etaf-vdom-patch-type p) 'remove)) patches))))

;;; Test mount with children

(let* ((parent (etaf-vdom-element 'div))
       (child1 (etaf-vdom-element 'span))
       (child2 (etaf-vdom-element 'p))
       (mounted-count 0))
  (etaf-vdom-set-children parent (list child1 child2))
  (etaf-vdom-add-hook parent :mounted (lambda (n) (cl-incf mounted-count)))
  (etaf-vdom-add-hook child1 :mounted (lambda (n) (cl-incf mounted-count)))
  (etaf-vdom-add-hook child2 :mounted (lambda (n) (cl-incf mounted-count)))
  (etaf-vdom-mount parent)
  ;; All three nodes should be mounted
  (should-equal mounted-count 3)
  (should (etaf-vdom-get-mounted-p parent))
  (should (etaf-vdom-get-mounted-p child1))
  (should (etaf-vdom-get-mounted-p child2)))

;;; Test unmount with children

(let* ((parent (etaf-vdom-element 'div))
       (child1 (etaf-vdom-element 'span))
       (child2 (etaf-vdom-element 'p))
       (unmounted-count 0))
  (etaf-vdom-set-children parent (list child1 child2))
  (etaf-vdom-add-hook parent :unmounted (lambda (n) (cl-incf unmounted-count)))
  (etaf-vdom-add-hook child1 :unmounted (lambda (n) (cl-incf unmounted-count)))
  (etaf-vdom-add-hook child2 :unmounted (lambda (n) (cl-incf unmounted-count)))
  (etaf-vdom-mount parent)
  (etaf-vdom-unmount parent)
  ;; All three nodes should be unmounted
  (should-equal unmounted-count 3)
  (should-not (etaf-vdom-get-mounted-p parent))
  (should-not (etaf-vdom-get-mounted-p child1))
  (should-not (etaf-vdom-get-mounted-p child2)))

(provide 'etaf-vdom-tests)
