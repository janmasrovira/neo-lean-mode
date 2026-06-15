;;; leanmacs-render.el --- Render interactive goals to text  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure rendering of Lean's interactive goal data structures to plain text.
;; These functions take the plists/vectors that Eglot's JSON parser produces
;; (objects -> keyword plists, arrays -> vectors) and have no side effects, so
;; they are unit-testable without a running server.
;;
;; The `tag' payloads of `TaggedText'/`CodeWithInfos' carry per-subexpression
;; info (an `InfoWithCtx' handle the server uses to answer questions about that
;; exact subterm, plus its `subexprPos').  We render the text leaves as before
;; but attach that info as text properties (`leanmacs-info' /
;; `leanmacs-subexpr-pos') on the spans they cover, which is what makes the goal
;; interactive: commands like go-to-definition read the info under point and
;; call the server.  Because `equal' ignores text properties on strings, the
;; plain-text rendering (and its tests) is unchanged.

;;; Code:

(require 'seq)

(defconst leanmacs-render-default-goal-prefix "⊢ "
  "Symbol shown before a goal's target type when none is provided.")

;;;; Faces
;;
;; Structural highlighting only (mirroring lean.nvim's infoview): the goal
;; expressions themselves are not re-tokenised, but the turnstile, case labels,
;; hypothesis names and the expected-type header are coloured.  Each inherits a
;; standard font-lock face so it follows the user's theme; rebind to taste.

(defface leanmacs-goal-prefix
  '((t :inherit font-lock-keyword-face))
  "Face for a goal's prefix (the `⊢' turnstile)."
  :group 'leanmacs)

(defface leanmacs-goal-case
  '((t :inherit font-lock-keyword-face))
  "Face for a goal's `case' label."
  :group 'leanmacs)

(defface leanmacs-goal-hypothesis-name
  '((t :inherit font-lock-variable-name-face))
  "Face for accessible hypothesis names."
  :group 'leanmacs)

(defface leanmacs-goal-inaccessible-name
  '((t :inherit shadow))
  "Face for inaccessible hypothesis names (those marked with `✝')."
  :group 'leanmacs)

(defface leanmacs-goal-expected-type
  '((t :inherit font-lock-keyword-face))
  "Face for the `Expected type:' header above the term goal."
  :group 'leanmacs)

(defun leanmacs-render--hyp-name (name)
  "Return hypothesis NAME with the appropriate name face.
Names carrying the inaccessible marker `✝' (optionally with a superscript
index, e.g. `n✝¹') are dimmed, and the marker is stripped -- the shadow face
alone conveys inaccessibility."
  (if (string-search "✝" name)
      (propertize (replace-regexp-in-string "✝[⁰¹²³⁴⁵⁶⁷⁸⁹]*" "" name)
                  'face 'leanmacs-goal-inaccessible-name)
    (propertize name 'face 'leanmacs-goal-hypothesis-name)))

(defun leanmacs-render--apply-info (string info pos)
  "Return STRING with INFO/POS attached where no `leanmacs-info' is set yet.
Nested tags render innermost-first, so the smallest (most specific)
subexpression's info is already in place; this fills only the gaps with the
enclosing tag's INFO, leaving the more specific inner info to win."
  (let ((s (copy-sequence string))
        (i 0)
        (n (length string)))
    (while (< i n)
      (let ((next (or (next-single-property-change i 'leanmacs-info s) n)))
        (unless (get-text-property i 'leanmacs-info s)
          (put-text-property i next 'leanmacs-info info s)
          (when pos
            (put-text-property i next 'leanmacs-subexpr-pos pos s)))
        (setq i next)))
    s))

(defun leanmacs-render-tagged-text (tt)
  "Flatten a `TaggedText' (or `CodeWithInfos') TT to a string.
TT is one of: (:text STRING), (:append [TT...]) or (:tag [SUBEXPR TT]).
Tag nodes attach their `SUBEXPR' info as text properties on the rendered
span (see `leanmacs-render--apply-info'); only text leaves contribute text."
  (cond
   ((null tt) "")
   ((stringp tt) tt)
   ((plist-member tt :text)
    (or (plist-get tt :text) ""))
   ((plist-member tt :append)
    (mapconcat #'leanmacs-render-tagged-text (plist-get tt :append) ""))
   ((plist-member tt :tag)
    ;; (:tag [SUBEXPR INNER]) -- render INNER and tag it with SUBEXPR's info.
    (let* ((tag (plist-get tt :tag))
           (subexpr (seq-elt tag 0))
           (inner (leanmacs-render-tagged-text (seq-elt tag 1)))
           (info (plist-get subexpr :info)))
      (if info
          (leanmacs-render--apply-info inner info (plist-get subexpr :subexprPos))
        inner)))
   (t "")))

(defun leanmacs-render--hypothesis (hyp)
  "Render one `InteractiveHypothesisBundle' HYP to a string.
Bundled names share a type, e.g. \"a b : Nat\"; let-binders also show
their value after \":=\"."
  (let* ((names (mapconcat #'leanmacs-render--hyp-name
                           (append (plist-get hyp :names) nil) " "))
         (type (leanmacs-render-tagged-text (plist-get hyp :type)))
         (val (plist-get hyp :val)))
    ;; `concat' preserves the type's text properties (the subexpression info),
    ;; which `format' with %s does not reliably do.
    (concat names " : " type
            (when val
              (concat " := " (leanmacs-render-tagged-text val))))))

(defun leanmacs-render-goal (goal)
  "Render one interactive GOAL to a string.
Shows an optional case name, the hypotheses, and the target type prefixed
by the goal's `goalPrefix' (default `leanmacs-render-default-goal-prefix')."
  (let ((lines '())
        (user-name (plist-get goal :userName))
        (prefix (propertize (or (plist-get goal :goalPrefix)
                                leanmacs-render-default-goal-prefix)
                            'face 'leanmacs-goal-prefix)))
    (when (and user-name (not (string-empty-p user-name)))
      (push (propertize (format "case %s" user-name)
                        'face 'leanmacs-goal-case)
            lines))
    (seq-doseq (hyp (plist-get goal :hyps))
      (push (leanmacs-render--hypothesis hyp) lines))
    (push (concat prefix (leanmacs-render-tagged-text (plist-get goal :type))) lines)
    (string-join (nreverse lines) "\n")))

(defun leanmacs-render-goals (goals)
  "Render the vector of interactive GOALS to a string.
Returns \"No goals.\" when GOALS is empty."
  (if (seq-empty-p goals)
      "No goals."
    (mapconcat #'leanmacs-render-goal goals "\n\n")))

(defconst leanmacs-render-term-goal-header "Expected type:"
  "Header shown above the term goal (the expected type at point).")

(defun leanmacs-render-term-goal (term-goal)
  "Render an `InteractiveTermGoal' TERM-GOAL, or nil when there is none.
A term goal carries hypotheses and a target type just like a tactic goal,
so it is rendered the same way under `leanmacs-render-term-goal-header'."
  (when term-goal
    (concat (propertize leanmacs-render-term-goal-header
                        'face 'leanmacs-goal-expected-type)
            "\n"
            (leanmacs-render-goal term-goal))))

(defun leanmacs-render-state (goals term-goal)
  "Render the proof state at point to a string.
GOALS is the vector of interactive tactic goals; TERM-GOAL is the optional
`InteractiveTermGoal' (the expected type), or nil.  Shows the tactic goals,
then an `Expected type' section when a term goal is present, and
\"No goals.\" when there is neither."
  (let ((sections '()))
    (unless (seq-empty-p goals)
      (push (mapconcat #'leanmacs-render-goal goals "\n\n") sections))
    (when-let* ((term (leanmacs-render-term-goal term-goal)))
      (push term sections))
    (if sections
        (string-join (nreverse sections) "\n\n")
      "No goals.")))

(provide 'leanmacs-render)
;;; leanmacs-render.el ends here
