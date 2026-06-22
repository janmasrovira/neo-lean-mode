;;; neo-lean-render-test.el --- Tests for neo-lean-render and neo-lean-rpc  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch ERT tests for the pure parts of neo-lean-mode: TaggedText flattening,
;; goal rendering, and RPC wire-format detection.  No Lean server required.
;;
;; Run with:
;;   emacs -Q --batch -L . -l test/neo-lean-render-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'neo-lean-render)
(require 'neo-lean-rpc)
(require 'neo-lean-goal)

;;;; TaggedText flattening

(ert-deftest neo-lean-render-tagged-text-leaf ()
  (should (equal (neo-lean-render-tagged-text '(:text "hello")) "hello")))

(ert-deftest neo-lean-render-tagged-text-nil ()
  (should (equal (neo-lean-render-tagged-text nil) "")))

(ert-deftest neo-lean-render-tagged-text-append ()
  ;; arrays come back from the JSON parser as vectors
  (should (equal (neo-lean-render-tagged-text
                  (list :append (vector '(:text "a") '(:text "b") '(:text "c"))))
                 "abc")))

(ert-deftest neo-lean-render-tagged-text-tag-renders-inner ()
  ;; (:tag [SUBEXPR INNER]) -- inner text rendered (info kept as a property).
  (should (equal (neo-lean-render-tagged-text
                  (list :tag (vector '(:info "ignored") '(:text "Nat"))))
                 "Nat")))

(ert-deftest neo-lean-render-tagged-text-tag-attaches-info ()
  ;; The subexpression's `info' becomes the `neo-lean-info' text property.
  (let* ((info '(:p 7))
         (tt (list :tag (vector (list :info info :subexprPos "0")
                                '(:text "Nat"))))
         (s (neo-lean-render-tagged-text tt)))
    (should (equal s "Nat"))
    (should (eq (get-text-property 0 'neo-lean-info s) info))
    (should (equal (get-text-property 0 'neo-lean-subexpr-pos s) "0"))))

(ert-deftest neo-lean-render-tagged-text-innermost-info-wins ()
  ;; Outer tag wraps "f x"; an inner tag covers just "x".  The inner (more
  ;; specific) info must win on "x" while the outer fills "f ".
  (let* ((outer '(:p 1))
         (inner '(:p 2))
         (inner-node (list :tag (vector (list :info inner :subexprPos "1")
                                        '(:text "x"))))
         (tt (list :tag (vector (list :info outer :subexprPos "0")
                                (list :append (vector '(:text "f ") inner-node)))))
         (s (neo-lean-render-tagged-text tt)))
    (should (equal s "f x"))
    (should (eq (get-text-property 0 'neo-lean-info s) outer))   ; "f"
    (should (eq (get-text-property 2 'neo-lean-info s) inner)))) ; "x"

(ert-deftest neo-lean-render-goal-keeps-info-in-type ()
  ;; The subexpression info survives goal rendering (concat, not format).
  (let* ((info '(:p 9))
         (goal (list :hyps (vector)
                     :type (list :tag (vector (list :info info :subexprPos "0")
                                              '(:text "Nat")))))
         (s (neo-lean-render-goal goal)))
    (should (equal s "⊢ Nat"))
    ;; The "Nat" starts right after the goal prefix "⊢ ".
    (should (eq (get-text-property (string-match "Nat" s) 'neo-lean-info s)
                info))))

(ert-deftest neo-lean-render-tagged-text-nested ()
  (let ((tt (list :append
                  (vector
                   '(:text "f ")
                   (list :tag (vector '(:info "x") '(:text "x")))
                   '(:text " ")
                   (list :append (vector '(:text "(") '(:text "y") '(:text ")")))))))
    (should (equal (neo-lean-render-tagged-text tt) "f x (y)"))))

;;;; Goal rendering

(ert-deftest neo-lean-render-goal-simple ()
  (let ((goal (list :hyps (vector (list :names (vector "a" "b")
                                        :type '(:text "Nat")))
                    :type '(:text "a = b"))))
    (should (equal (neo-lean-render-goal goal)
                   "a b : Nat\n⊢ a = b"))))

(ert-deftest neo-lean-render-goal-case-and-let ()
  (let ((goal (list :userName "succ"
                    :goalPrefix "⊢ "
                    :hyps (vector (list :names (vector "n")
                                        :type '(:text "Nat")
                                        :val '(:text "0")))
                    :type '(:text "P n"))))
    (should (equal (neo-lean-render-goal goal)
                   "case succ\nn : Nat := 0\n⊢ P n"))))

(ert-deftest neo-lean-render-goals-empty ()
  (should (equal (neo-lean-render-goals (vector)) "No goals."))
  (should (equal (neo-lean-render-goals nil) "No goals.")))

(ert-deftest neo-lean-render-goals-multiple ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A"))
                       (list :hyps (vector) :type '(:text "B")))))
    (should (equal (neo-lean-render-goals goals) "⊢ A\n\n⊢ B"))))

;;;; Structural faces

(defun neo-lean-render-test--face-includes-p (face expected)
  "Return non-nil when FACE includes EXPECTED."
  (if (listp face)
      (memq expected face)
    (eq face expected)))

(ert-deftest neo-lean-render-goal-faces ()
  (let* ((goal (list :hyps (vector (list :names (vector "a" "h✝¹")
                                         :type '(:text "Nat")))
                     :type '(:text "a = b")))
         (s (neo-lean-render-goal goal)))
    ;; the inaccessible marker (and its superscript) is stripped: "h✝¹" -> "h"
    (should (equal s "a h : Nat\n⊢ a = b"))
    ;; accessible hyp name "a"
    (should (eq (get-text-property 0 'face s) 'neo-lean-goal-hypothesis-name))
    ;; inaccessible hyp name "h" (was "h✝¹") is dimmed
    (should (eq (get-text-property 2 'face s) 'neo-lean-goal-inaccessible-name))
    ;; the turnstile carries the prefix face
    (should (eq (get-text-property (string-match "⊢" s) 'face s)
                'neo-lean-goal-prefix))))

(ert-deftest neo-lean-render-goal-case-face ()
  (let* ((goal (list :userName "succ" :hyps (vector) :type '(:text "P")))
         (s (neo-lean-render-goal goal)))
    (should (eq (get-text-property 0 'face s) 'neo-lean-goal-case))))

(ert-deftest neo-lean-render-goal-diff-faces ()
  (let* ((inserted (neo-lean-render-goal
                    (list :isInserted t :hyps (vector) :type '(:text "A"))))
         (removed (neo-lean-render-goal
                   (list :isRemoved t :hyps (vector) :type '(:text "A"))))
         (unchanged (neo-lean-render-goal
                     (list :isInserted :json-false
                           :hyps (vector)
                           :type '(:text "A")))))
    (should (eq (get-text-property (string-match "⊢" inserted) 'face inserted)
                'neo-lean-goal-inserted))
    (should (eq (get-text-property (string-match "⊢" removed) 'face removed)
                'neo-lean-goal-removed))
    (should (eq (get-text-property (string-match "⊢" unchanged) 'face unchanged)
                'neo-lean-goal-prefix))))

(ert-deftest neo-lean-render-hypothesis-diff-faces ()
  (let* ((inserted (neo-lean-render-goal
                    (list :hyps (vector (list :isInserted t
                                              :names (vector "h")
                                              :type '(:text "Nat")))
                          :type '(:text "Nat"))))
         (removed (neo-lean-render-goal
                   (list :hyps (vector (list :isRemoved t
                                             :names (vector "h")
                                             :type '(:text "Nat")))
                         :type '(:text "Nat"))))
         (unchanged (neo-lean-render-goal
                     (list :hyps (vector (list :isInserted :json-false
                                               :names (vector "h")
                                               :type '(:text "Nat")))
                           :type '(:text "Nat")))))
    (should (neo-lean-render-test--face-includes-p
             (get-text-property 0 'face inserted)
             'neo-lean-goal-inserted))
    (should (neo-lean-render-test--face-includes-p
             (get-text-property 0 'face removed)
             'neo-lean-goal-removed))
    (should-not (neo-lean-render-test--face-includes-p
                 (get-text-property 0 'face unchanged)
                 'neo-lean-goal-inserted))
    (should (eq (get-text-property 0 'face unchanged)
                'neo-lean-goal-hypothesis-name))))

(ert-deftest neo-lean-render-tagged-text-diff-status-faces ()
  (let* ((info '(:p 1))
         (inserted (neo-lean-render-tagged-text
                    (list :tag (vector (list :info info
                                             :subexprPos "/"
                                             :diffStatus "wasInserted")
                                       '(:text "x")))))
         (removed (neo-lean-render-tagged-text
                   (list :tag (vector (list :diffStatus "willDelete")
                                      '(:text "x")))))
         (changed (neo-lean-render-tagged-text
                   (list :tag (vector (list :diffStatus "wasChanged")
                                      '(:text "x"))))))
    (should (eq (get-text-property 0 'face inserted)
                'neo-lean-goal-inserted))
    (should (eq (get-text-property 0 'neo-lean-info inserted) info))
    (should (equal (get-text-property 0 'neo-lean-subexpr-pos inserted) "/"))
    (should (eq (get-text-property 0 'face removed)
                'neo-lean-goal-removed))
    (should (eq (get-text-property 0 'face changed)
                'neo-lean-goal-changed))))

;;;; Term goal / combined state

(ert-deftest neo-lean-render-term-goal-nil ()
  (should (null (neo-lean-render-term-goal nil))))

(ert-deftest neo-lean-render-term-goal-simple ()
  (let ((term (list :hyps (vector (list :names (vector "n")
                                        :type '(:text "Nat")))
                    :type '(:text "Nat"))))
    (should (equal (neo-lean-render-term-goal term)
                   "Expected type:\nn : Nat\n⊢ Nat"))))

(ert-deftest neo-lean-render-state-goals-only ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A")))))
    (should (equal (neo-lean-render-state goals nil) "⊢ A"))))

(ert-deftest neo-lean-render-state-term-only ()
  ;; In term mode there are no tactic goals; only the expected type shows.
  (let ((term (list :hyps (vector) :type '(:text "Nat"))))
    (should (equal (neo-lean-render-state (vector) term)
                   "Expected type:\n⊢ Nat"))))

(ert-deftest neo-lean-render-state-both ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A"))))
        (term (list :hyps (vector) :type '(:text "Nat"))))
    (should (equal (neo-lean-render-state goals term)
                   "⊢ A\n\nExpected type:\n⊢ Nat"))))

(ert-deftest neo-lean-render-messages-empty ()
  (should (null (neo-lean-render-messages nil)))
  (should (null (neo-lean-render-messages '("")))))

(defun neo-lean-render-test--text-diagnostic (text)
  "Return a simple interactive diagnostic with message TEXT."
  (list :source "Lean 4"
        :severity 3
        :message (list :text text)))

(ert-deftest neo-lean-goal-filter-interactive-diagnostics ()
  (let* ((visible (neo-lean-render-test--text-diagnostic "trace output"))
         (unsolved (plist-put (neo-lean-render-test--text-diagnostic
                               "unsolved goals")
                              :leanTags (vector 1)))
         (accomplished (plist-put (neo-lean-render-test--text-diagnostic
                                   "goals accomplished")
                                  :leanTags (vector 2)))
         (silent (plist-put (neo-lean-render-test--text-diagnostic
                             "silent")
                            :isSilent t)))
    (should (equal (neo-lean--goal-filter-interactive-diagnostics
                    (list visible unsolved accomplished silent))
                   (list visible)))))

(ert-deftest neo-lean-render-messages ()
  (let ((s (neo-lean-render-messages
            (list (neo-lean-render-test--text-diagnostic
                   "trace.profiler: elaboration 12ms")
                  (neo-lean-render-test--text-diagnostic
                   "other message")))))
    (should (equal s (concat "Messages:\n"
                             "▼ Lean 4 Information\n"
                             "trace.profiler: elaboration 12ms\n\n"
                             "▼ Lean 4 Information\n"
                             "other message")))
    (should (eq (get-text-property 0 'face s) 'neo-lean-goal-messages))))

(defun neo-lean-render-test--trace-diagnostic (&optional collapsed children)
  "Return a small interactive trace diagnostic for renderer tests."
  (let* ((child (list :tag
                      (vector
                       (list :trace
                             (list :indent 2
                                   :cls "Elab.step"
                                   :msg '(:text "child")
                                   :collapsed nil
                                   :children (list :strict (vector))))
                       '(:text ""))))
         (trace (list :indent 0
                      :cls "Elab.command"
                      :msg '(:text "parent")
                      :collapsed collapsed
                      :children (or children (list :strict (vector child))))))
    (list :source "Lean 4"
          :severity 3
          :message (list :tag (vector (list :trace trace) '(:text ""))))))

(ert-deftest neo-lean-render-interactive-diagnostic-trace-folds ()
  (let* ((diag (neo-lean-render-test--trace-diagnostic))
         (s (neo-lean-render-messages (list diag))))
    (should (string-match-p "▼ Lean 4 Information" s))
    (should (string-match-p "▼ \\[Elab\\.command\\] parent" s))
    (should (string-match-p "\\[Elab\\.step\\] child" s))
    (should (equal (get-text-property (string-match "Elab\\.command" s)
                                      'neo-lean-fold-id s)
                   "diagnostic/0/message"))))

(ert-deftest neo-lean-render-interactive-diagnostic-trace-collapsed-state ()
  (let ((state (make-hash-table :test #'equal)))
    (puthash "diagnostic/0/message" '(:collapsed t) state)
    (let* ((neo-lean-render-fold-state state)
           (diag (neo-lean-render-test--trace-diagnostic))
           (s (neo-lean-render-messages (list diag))))
      (should (string-match-p "▶ \\[Elab\\.command\\] parent" s))
      (should (eq (get-text-property (string-match "child" s) 'invisible s)
                  'neo-lean-fold)))))

(ert-deftest neo-lean-render-interactive-diagnostic-lazy-trace-keeps-ref ()
  (let* ((lazy '(:p 7))
         (diag (neo-lean-render-test--trace-diagnostic
                t (list :lazy lazy)))
         (s (neo-lean-render-messages (list diag)))
         (pos (string-match "Elab\\.command" s)))
    (should (string-match-p "▶ \\[Elab\\.command\\] parent" s))
    (should (equal (get-text-property pos 'neo-lean-fold-lazy s) lazy))))

(ert-deftest neo-lean-render-state-with-messages ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A")))))
    (should (equal (neo-lean-render-state
                    goals nil
                    (list (neo-lean-render-test--text-diagnostic
                           "trace.profiler: 12ms")))
                   (concat "⊢ A\n\nMessages:\n"
                           "▼ Lean 4 Information\n"
                           "trace.profiler: 12ms")))))

(ert-deftest neo-lean-render-state-messages-only ()
  (should (equal (neo-lean-render-state
                  nil nil
                  (list (neo-lean-render-test--text-diagnostic
                         "trace.profiler: 12ms")))
                 (concat "Messages:\n"
                         "▼ Lean 4 Information\n"
                         "trace.profiler: 12ms"))))

(ert-deftest neo-lean-render-state-empty ()
  (should (equal (neo-lean-render-state (vector) nil) "No goals."))
  (should (equal (neo-lean-render-state nil nil) "No goals.")))

;;;; Info popup (hover)

(ert-deftest neo-lean-render-info-popup-nil ()
  (should (null (neo-lean-render-info-popup nil)))
  (should (null (neo-lean-render-info-popup '()))))

(ert-deftest neo-lean-render-info-popup-type-only ()
  (should (equal (neo-lean-render-info-popup '(:type (:text "Nat")))
                 "Nat")))

(ert-deftest neo-lean-render-info-popup-expr-and-type ()
  (should (equal (neo-lean-render-info-popup
                  '(:exprExplicit (:text "n") :type (:text "Nat")))
                 "n : Nat")))

(ert-deftest neo-lean-render-info-popup-doc ()
  (should (equal (neo-lean-render-info-popup
                  '(:exprExplicit (:text "n") :type (:text "Nat")
                    :doc "A natural number."))
                 "n : Nat\n\nA natural number."))
  ;; An empty (or absent) docstring contributes nothing.
  (should (equal (neo-lean-render-info-popup '(:type (:text "Nat") :doc ""))
                 "Nat")))

(ert-deftest neo-lean-render-info-popup-keeps-info ()
  ;; The popup's tagged text keeps the subexpression info as a property,
  ;; so the hover text is itself interactive.
  (let* ((info '(:p 3))
         (popup (list :type (list :tag (vector (list :info info :subexprPos "0")
                                               '(:text "Nat")))))
         (s (neo-lean-render-info-popup popup)))
    (should (equal s "Nat"))
    (should (eq (get-text-property 0 'neo-lean-info s) info))))

;;;; Hover highlight span

(ert-deftest neo-lean-subexpr-ancestor-p ()
  ;; Path-prefix containment on slash-separated coordinate paths.
  (should (neo-lean--subexpr-ancestor-p "/" "/"))          ; root = root
  (should (neo-lean--subexpr-ancestor-p "/" "/1/0"))       ; root is everyone's
  (should (neo-lean--subexpr-ancestor-p "/1" "/1/0"))
  (should (neo-lean--subexpr-ancestor-p "/1/0" "/1/0"))    ; reflexive
  (should-not (neo-lean--subexpr-ancestor-p "/1/0" "/1"))  ; deeper is not ancestor
  (should-not (neo-lean--subexpr-ancestor-p "/1" "/0"))    ; siblings
  ;; Compared per coordinate, not as raw strings (guards naive `string-prefix-p').
  (should-not (neo-lean--subexpr-ancestor-p "/1" "/10"))
  ;; A missing path is not a descendant -- crucially, not even of the root,
  ;; whose coordinates are also empty.
  (should-not (neo-lean--subexpr-ancestor-p "/" nil))
  (should-not (neo-lean--subexpr-ancestor-p nil "/1")))

(ert-deftest neo-lean-goal-span-stops-at-unpropertized ()
  ;; A root-path expression must not bleed into surrounding text with no
  ;; subexpression path (separators, the turnstile, hypothesis names, other
  ;; goals): root \"/\" is an ancestor of every path, but not of plain text.
  (let* ((tt (list :tag (vector (list :info '(:p 1) :subexprPos "/")
                                '(:text "a=b"))))
         (s (neo-lean-render-tagged-text tt)))
    (with-temp-buffer
      (insert "x : ")                 ; unpropertised separator
      (let ((start (point)))
        (insert s)                    ; "a=b" at the root path
        (insert "\n no")              ; trailing unpropertised text
        ;; Hovering anywhere in "a=b" highlights exactly "a=b".
        (should (equal (neo-lean--goal-span-at start)
                       (cons start (+ start 3))))
        (should (equal (neo-lean--goal-span-at (1+ start))  ; the "="
                       (cons start (+ start 3))))))))

(ert-deftest neo-lean-goal-span-at ()
  ;; "f x": the application is the root "/"; "x" is the argument at "/1".
  (let* ((outer '(:p 1))
         (inner '(:p 2))
         (inner-node (list :tag (vector (list :info inner :subexprPos "/1")
                                        '(:text "x"))))
         (tt (list :tag (vector (list :info outer :subexprPos "/")
                                (list :append (vector '(:text "f ") inner-node)))))
         (s (neo-lean-render-tagged-text tt)))
    (with-temp-buffer
      (insert s)
      ;; buffer positions are 1-based: 1='f' 2=' ' 3='x'
      ;; On the root (the "f" or the space), the whole expression is the extent.
      (should (equal (neo-lean--goal-span-at 1) '(1 . 4)))  ; whole "f x"
      (should (equal (neo-lean--goal-span-at 2) '(1 . 4)))  ; the space, too
      (should (equal (neo-lean--goal-span-at 3) '(3 . 4)))  ; just the child "x"
      ;; Text with no subexpression path has no span.
      (insert "  no info")
      (should (null (neo-lean--goal-span-at (point)))))))

(ert-deftest neo-lean-goal-span-at-delimiter ()
  ;; "( x )": the parens belong to the parent expression "/"; "x" is the child
  ;; "/1".  Hovering a delimiter highlights the whole parent extent (paren to
  ;; paren, contiguously), while hovering the child highlights just the child.
  (let* ((parent '(:p 1))
         (child '(:p 2))
         (child-node (list :tag (vector (list :info child :subexprPos "/1")
                                        '(:text "x"))))
         (tt (list :tag (vector (list :info parent :subexprPos "/")
                                (list :append (vector '(:text "( ")
                                                      child-node
                                                      '(:text " )"))))))
         (s (neo-lean-render-tagged-text tt)))
    (with-temp-buffer
      (insert s)
      ;; positions: 1='(' 2=' ' 3='x' 4=' ' 5=')'
      (should (equal (neo-lean--goal-span-at 1) '(1 . 6)))  ; "(" -> whole "( x )"
      (should (equal (neo-lean--goal-span-at 5) '(1 . 6)))  ; ")" -> whole "( x )"
      (should (equal (neo-lean--goal-span-at 3) '(3 . 4))))))  ; child "x" only

;;;; RPC wire-format / dead-code detection

(ert-deftest neo-lean-rpc-dead-code-p ()
  (should (neo-lean-rpc--dead-code-p -32900))
  (should (neo-lean-rpc--dead-code-p -32801))
  (should (neo-lean-rpc--dead-code-p -32901))
  (should (neo-lean-rpc--dead-code-p -32902))
  (should-not (neo-lean-rpc--dead-code-p -32602))
  (should-not (neo-lean-rpc--dead-code-p nil)))

;; `neo-lean-rpc--ref-key' reads from an eglot connection's capabilities.  We
;; fake that accessor to test the wire-format mapping in isolation.
(ert-deftest neo-lean-rpc-ref-key-v1 ()
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_) '(:experimental (:rpcProvider (:rpcWireFormat "v1"))))))
    (should (equal (neo-lean-rpc--ref-key 'fake) "__rpcref"))))

(ert-deftest neo-lean-rpc-ref-key-v0-default ()
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_) '(:experimental (:rpcProvider (:rpcWireFormat "v0"))))))
    (should (equal (neo-lean-rpc--ref-key 'fake) "p")))
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_) '())))
    (should (equal (neo-lean-rpc--ref-key 'fake) "p"))))

(provide 'neo-lean-render-test)
;;; neo-lean-render-test.el ends here
