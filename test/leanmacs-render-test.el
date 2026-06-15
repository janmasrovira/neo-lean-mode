;;; leanmacs-render-test.el --- Tests for leanmacs-render and leanmacs-rpc  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch ERT tests for the pure parts of lean-emacs: TaggedText flattening,
;; goal rendering, and RPC wire-format detection.  No Lean server required.
;;
;; Run with:
;;   emacs -Q --batch -L . -l test/leanmacs-render-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'leanmacs-render)
(require 'leanmacs-rpc)

;;;; TaggedText flattening

(ert-deftest leanmacs-render-tagged-text-leaf ()
  (should (equal (leanmacs-render-tagged-text '(:text "hello")) "hello")))

(ert-deftest leanmacs-render-tagged-text-nil ()
  (should (equal (leanmacs-render-tagged-text nil) "")))

(ert-deftest leanmacs-render-tagged-text-append ()
  ;; arrays come back from the JSON parser as vectors
  (should (equal (leanmacs-render-tagged-text
                  (list :append (vector '(:text "a") '(:text "b") '(:text "c"))))
                 "abc")))

(ert-deftest leanmacs-render-tagged-text-tag-renders-inner ()
  ;; (:tag [SUBEXPR INNER]) -- inner text rendered (info kept as a property).
  (should (equal (leanmacs-render-tagged-text
                  (list :tag (vector '(:info "ignored") '(:text "Nat"))))
                 "Nat")))

(ert-deftest leanmacs-render-tagged-text-tag-attaches-info ()
  ;; The subexpression's `info' becomes the `leanmacs-info' text property.
  (let* ((info '(:p 7))
         (tt (list :tag (vector (list :info info :subexprPos "0")
                                '(:text "Nat"))))
         (s (leanmacs-render-tagged-text tt)))
    (should (equal s "Nat"))
    (should (eq (get-text-property 0 'leanmacs-info s) info))
    (should (equal (get-text-property 0 'leanmacs-subexpr-pos s) "0"))))

(ert-deftest leanmacs-render-tagged-text-innermost-info-wins ()
  ;; Outer tag wraps "f x"; an inner tag covers just "x".  The inner (more
  ;; specific) info must win on "x" while the outer fills "f ".
  (let* ((outer '(:p 1))
         (inner '(:p 2))
         (inner-node (list :tag (vector (list :info inner :subexprPos "1")
                                        '(:text "x"))))
         (tt (list :tag (vector (list :info outer :subexprPos "0")
                                (list :append (vector '(:text "f ") inner-node)))))
         (s (leanmacs-render-tagged-text tt)))
    (should (equal s "f x"))
    (should (eq (get-text-property 0 'leanmacs-info s) outer))   ; "f"
    (should (eq (get-text-property 2 'leanmacs-info s) inner)))) ; "x"

(ert-deftest leanmacs-render-goal-keeps-info-in-type ()
  ;; The subexpression info survives goal rendering (concat, not format).
  (let* ((info '(:p 9))
         (goal (list :hyps (vector)
                     :type (list :tag (vector (list :info info :subexprPos "0")
                                              '(:text "Nat")))))
         (s (leanmacs-render-goal goal)))
    (should (equal s "⊢ Nat"))
    ;; The "Nat" starts right after the goal prefix "⊢ ".
    (should (eq (get-text-property (string-match "Nat" s) 'leanmacs-info s)
                info))))

(ert-deftest leanmacs-render-tagged-text-nested ()
  (let ((tt (list :append
                  (vector
                   '(:text "f ")
                   (list :tag (vector '(:info "x") '(:text "x")))
                   '(:text " ")
                   (list :append (vector '(:text "(") '(:text "y") '(:text ")")))))))
    (should (equal (leanmacs-render-tagged-text tt) "f x (y)"))))

;;;; Goal rendering

(ert-deftest leanmacs-render-goal-simple ()
  (let ((goal (list :hyps (vector (list :names (vector "a" "b")
                                        :type '(:text "Nat")))
                    :type '(:text "a = b"))))
    (should (equal (leanmacs-render-goal goal)
                   "a b : Nat\n⊢ a = b"))))

(ert-deftest leanmacs-render-goal-case-and-let ()
  (let ((goal (list :userName "succ"
                    :goalPrefix "⊢ "
                    :hyps (vector (list :names (vector "n")
                                        :type '(:text "Nat")
                                        :val '(:text "0")))
                    :type '(:text "P n"))))
    (should (equal (leanmacs-render-goal goal)
                   "case succ\nn : Nat := 0\n⊢ P n"))))

(ert-deftest leanmacs-render-goals-empty ()
  (should (equal (leanmacs-render-goals (vector)) "No goals."))
  (should (equal (leanmacs-render-goals nil) "No goals.")))

(ert-deftest leanmacs-render-goals-multiple ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A"))
                       (list :hyps (vector) :type '(:text "B")))))
    (should (equal (leanmacs-render-goals goals) "⊢ A\n\n⊢ B"))))

;;;; Structural faces

(ert-deftest leanmacs-render-goal-faces ()
  (let* ((goal (list :hyps (vector (list :names (vector "a" "h✝¹")
                                         :type '(:text "Nat")))
                     :type '(:text "a = b")))
         (s (leanmacs-render-goal goal)))
    ;; the inaccessible marker (and its superscript) is stripped: "h✝¹" -> "h"
    (should (equal s "a h : Nat\n⊢ a = b"))
    ;; accessible hyp name "a"
    (should (eq (get-text-property 0 'face s) 'leanmacs-goal-hypothesis-name))
    ;; inaccessible hyp name "h" (was "h✝¹") is dimmed
    (should (eq (get-text-property 2 'face s) 'leanmacs-goal-inaccessible-name))
    ;; the turnstile carries the prefix face
    (should (eq (get-text-property (string-match "⊢" s) 'face s)
                'leanmacs-goal-prefix))))

(ert-deftest leanmacs-render-goal-case-face ()
  (let* ((goal (list :userName "succ" :hyps (vector) :type '(:text "P")))
         (s (leanmacs-render-goal goal)))
    (should (eq (get-text-property 0 'face s) 'leanmacs-goal-case))))

;;;; Term goal / combined state

(ert-deftest leanmacs-render-term-goal-nil ()
  (should (null (leanmacs-render-term-goal nil))))

(ert-deftest leanmacs-render-term-goal-simple ()
  (let ((term (list :hyps (vector (list :names (vector "n")
                                        :type '(:text "Nat")))
                    :type '(:text "Nat"))))
    (should (equal (leanmacs-render-term-goal term)
                   "Expected type:\nn : Nat\n⊢ Nat"))))

(ert-deftest leanmacs-render-state-goals-only ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A")))))
    (should (equal (leanmacs-render-state goals nil) "⊢ A"))))

(ert-deftest leanmacs-render-state-term-only ()
  ;; In term mode there are no tactic goals; only the expected type shows.
  (let ((term (list :hyps (vector) :type '(:text "Nat"))))
    (should (equal (leanmacs-render-state (vector) term)
                   "Expected type:\n⊢ Nat"))))

(ert-deftest leanmacs-render-state-both ()
  (let ((goals (vector (list :hyps (vector) :type '(:text "A"))))
        (term (list :hyps (vector) :type '(:text "Nat"))))
    (should (equal (leanmacs-render-state goals term)
                   "⊢ A\n\nExpected type:\n⊢ Nat"))))

(ert-deftest leanmacs-render-state-empty ()
  (should (equal (leanmacs-render-state (vector) nil) "No goals."))
  (should (equal (leanmacs-render-state nil nil) "No goals.")))

;;;; RPC wire-format / dead-code detection

(ert-deftest leanmacs-rpc-dead-code-p ()
  (should (leanmacs-rpc--dead-code-p -32900))
  (should (leanmacs-rpc--dead-code-p -32801))
  (should (leanmacs-rpc--dead-code-p -32901))
  (should (leanmacs-rpc--dead-code-p -32902))
  (should-not (leanmacs-rpc--dead-code-p -32602))
  (should-not (leanmacs-rpc--dead-code-p nil)))

;; `leanmacs-rpc--ref-key' reads from an eglot connection's capabilities.  We
;; fake that accessor to test the wire-format mapping in isolation.
(ert-deftest leanmacs-rpc-ref-key-v1 ()
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_) '(:experimental (:rpcProvider (:rpcWireFormat "v1"))))))
    (should (equal (leanmacs-rpc--ref-key 'fake) "__rpcref"))))

(ert-deftest leanmacs-rpc-ref-key-v0-default ()
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_) '(:experimental (:rpcProvider (:rpcWireFormat "v0"))))))
    (should (equal (leanmacs-rpc--ref-key 'fake) "p")))
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_) '())))
    (should (equal (leanmacs-rpc--ref-key 'fake) "p"))))

(provide 'leanmacs-render-test)
;;; leanmacs-render-test.el ends here
