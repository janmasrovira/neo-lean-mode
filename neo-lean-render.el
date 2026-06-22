;;; neo-lean-render.el --- Render interactive goals to text  -*- lexical-binding: t; -*-

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
;; but attach that info as text properties (`neo-lean-info' /
;; `neo-lean-subexpr-pos') on the spans they cover, which is what makes the goal
;; interactive: commands like go-to-definition read the info under point and
;; call the server.  Because `equal' ignores text properties on strings, the
;; plain-text rendering (and its tests) is unchanged.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'seq)

(defconst neo-lean-render-default-goal-prefix "⊢ "
  "Symbol shown before a goal's target type when none is provided.")

;;;; Faces
;;
;; Structural highlighting only: the goal
;; expressions themselves are not re-tokenised, but the turnstile, case labels,
;; hypothesis names and the expected-type header are coloured.  Each inherits a
;; standard font-lock face so it follows the user's theme; rebind to taste.

(defface neo-lean-goal-prefix
  '((t :inherit font-lock-keyword-face))
  "Face for a goal's prefix (the `⊢' turnstile)."
  :group 'neo-lean)

(defface neo-lean-goal-case
  '((t :inherit font-lock-keyword-face))
  "Face for a goal's `case' label."
  :group 'neo-lean)

(defface neo-lean-goal-hypothesis-name
  '((t :inherit font-lock-variable-name-face))
  "Face for accessible hypothesis names."
  :group 'neo-lean)

(defface neo-lean-goal-inaccessible-name
  '((t :inherit shadow))
  "Face for inaccessible hypothesis names (those marked with `✝')."
  :group 'neo-lean)

(defface neo-lean-goal-expected-type
  '((t :inherit font-lock-keyword-face))
  "Face for the `Expected type:' header above the term goal."
  :group 'neo-lean)

(defface neo-lean-goal-messages
  '((t :inherit font-lock-keyword-face))
  "Face for the `Messages:' header above Lean diagnostics."
  :group 'neo-lean)

(defface neo-lean-goal-status
  '((t :inherit success))
  "Face for status lines in the infoview."
  :group 'neo-lean)

(defface neo-lean-goal-fold
  '((t :inherit font-lock-keyword-face))
  "Face for clickable infoview fold chevrons."
  :group 'neo-lean)

(defface neo-lean-goal-hover
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the subexpression under point in the goal buffer.
Changes the text colour and weight rather than the background, which reads
better over the goal's own highlighting."
  :group 'neo-lean)

(defface neo-lean-goal-inserted
  '((t :inherit diff-added))
  "Face for proof-state fragments Lean marks as inserted."
  :group 'neo-lean)

(defface neo-lean-goal-removed
  '((t :inherit diff-removed))
  "Face for proof-state fragments Lean marks as removed."
  :group 'neo-lean)

(defface neo-lean-goal-changed
  '((t :inherit diff-refine-changed))
  "Face for proof-state fragments Lean marks as changed."
  :group 'neo-lean)

(defvar neo-lean-render-fold-state nil
  "Hash table consulted while rendering foldable infoview nodes.
Each key is a fold id and each value is a plist.  Recognized keys are
`:collapsed', `:children', and `:loading'.")

(defconst neo-lean-render-fold-open "▼")
(defconst neo-lean-render-fold-closed "▶")

(defun neo-lean-render--json-true-p (value)
  "Return non-nil when VALUE is JSON true, not JSON false."
  (eq value t))

(defun neo-lean-render--insert-remove-face (object)
  "Return the insertion/removal face requested by Lean for OBJECT."
  (cond
   ((neo-lean-render--json-true-p (plist-get object :isInserted))
    'neo-lean-goal-inserted)
   ((neo-lean-render--json-true-p (plist-get object :isRemoved))
    'neo-lean-goal-removed)))

(defun neo-lean-render--diff-status-face (status)
  "Return the face corresponding to Lean's diff STATUS."
  (pcase (if (symbolp status) (symbol-name status) status)
    ((or "wasInserted" "willInsert") 'neo-lean-goal-inserted)
    ((or "wasDeleted" "willDelete" "willChange") 'neo-lean-goal-removed)
    ("wasChanged" 'neo-lean-goal-changed)))

(defun neo-lean-render--add-face (string face)
  "Return STRING with FACE added across its full extent."
  (let ((s (copy-sequence string)))
    (when (and face (> (length s) 0))
      (add-face-text-property 0 (length s) face nil s))
    s))

(defun neo-lean-render--fold-entry (id)
  "Return the fold-state entry for ID, or nil."
  (and (hash-table-p neo-lean-render-fold-state)
       (gethash id neo-lean-render-fold-state)))

(defun neo-lean-render--fold-value (id key)
  "Return fold-state KEY for ID, or nil when it is absent."
  (let ((entry (neo-lean-render--fold-entry id)))
    (and (plist-member entry key)
         (plist-get entry key))))

(defun neo-lean-render--fold-has-value-p (id key)
  "Return non-nil when fold-state KEY is present for ID."
  (plist-member (neo-lean-render--fold-entry id) key))

(defun neo-lean-render--fold-propertize-header (string id collapsed lazy)
  "Attach fold metadata to STRING for fold ID.
COLLAPSED is the rendered fold state.  LAZY is an optional lazy trace-child
RPC reference."
  (add-text-properties
   0 (length string)
   (list 'neo-lean-fold-id id
         'neo-lean-fold-collapsed collapsed
         'neo-lean-fold-lazy lazy
         'mouse-face 'highlight
         'help-echo "mouse-1, TAB: toggle fold")
   string)
  string)

(cl-defun neo-lean-render--foldable
    (id title body &key collapsed lazy (indent 0) loading)
  "Render foldable TITLE with BODY under stable fold ID.
When COLLAPSED is non-nil, BODY is rendered invisible.  LAZY is attached to the
header so the caller can fetch children on expansion.  INDENT spaces are placed
before the chevron.  LOADING renders a temporary loading body."
  (let* ((entry (neo-lean-render--fold-entry id))
         (state-collapsed (plist-get entry :collapsed))
         (has-collapsed (plist-member entry :collapsed))
         (children-loaded (plist-member entry :children))
         (state-loading (or loading (plist-get entry :loading)))
         (body (cond
                (state-loading "Loading trace children...")
                (t body)))
         (has-body (and (stringp body) (not (string-empty-p body))))
         (collapsed (if has-collapsed state-collapsed collapsed))
         (prefix (make-string indent ?\s)))
    (if (or has-body lazy state-loading)
        (let* ((arrow (propertize (if collapsed
                                      neo-lean-render-fold-closed
                                    neo-lean-render-fold-open)
                                  'face 'neo-lean-goal-fold))
               (header (concat prefix arrow " " title))
               (header (neo-lean-render--fold-propertize-header
                        header id collapsed (unless children-loaded lazy)))
               (body-block (and has-body (concat "\n" body))))
          (when (and collapsed body-block)
            (add-text-properties 0 (length body-block)
                                 '(invisible neo-lean-fold)
                                 body-block))
          (concat header body-block))
      (concat prefix "  " title))))

(defun neo-lean-render--hyp-name (name)
  "Return hypothesis NAME with the appropriate name face.
Names carrying the inaccessible marker `✝' (optionally with a superscript
index, e.g. `n✝¹') are dimmed, and the marker is stripped -- the shadow face
alone conveys inaccessibility."
  (if (string-search "✝" name)
      (propertize (replace-regexp-in-string "✝[⁰¹²³⁴⁵⁶⁷⁸⁹]*" "" name)
                  'face 'neo-lean-goal-inaccessible-name)
    (propertize name 'face 'neo-lean-goal-hypothesis-name)))

(defun neo-lean-render--apply-info (string info pos)
  "Return STRING with INFO/POS attached where no `neo-lean-info' is set yet.
Nested tags render innermost-first, so the smallest (most specific)
subexpression's info is already in place; this fills only the gaps with the
enclosing tag's INFO, leaving the more specific inner info to win."
  (let ((s (copy-sequence string))
        (i 0)
        (n (length string)))
    (while (< i n)
      (let ((next (or (next-single-property-change i 'neo-lean-info s) n)))
        (unless (get-text-property i 'neo-lean-info s)
          (put-text-property i next 'neo-lean-info info s)
          (when pos
            (put-text-property i next 'neo-lean-subexpr-pos pos s)))
        (setq i next)))
    s))

(defun neo-lean-render-tagged-text (tt)
  "Flatten a `TaggedText' (or `CodeWithInfos') TT to a string.
TT is one of: (:text STRING), (:append [TT...]) or (:tag [SUBEXPR TT]).
Tag nodes attach their `SUBEXPR' info as text properties on the rendered
span (see `neo-lean-render--apply-info'); only text leaves contribute text."
  (cond
   ((null tt) "")
   ((stringp tt) tt)
   ((plist-member tt :text)
    (or (plist-get tt :text) ""))
   ((plist-member tt :append)
    (mapconcat #'neo-lean-render-tagged-text (plist-get tt :append) ""))
   ((plist-member tt :tag)
    ;; (:tag [SUBEXPR INNER]) -- render INNER and tag it with SUBEXPR's info.
    (let* ((tag (plist-get tt :tag))
           (subexpr (seq-elt tag 0))
           (inner (neo-lean-render-tagged-text (seq-elt tag 1)))
           (info (plist-get subexpr :info))
           (rendered (if info
                         (neo-lean-render--apply-info
                          inner info (plist-get subexpr :subexprPos))
                       inner))
           (diff-face
            (neo-lean-render--diff-status-face
             (plist-get subexpr :diffStatus))))
      (if diff-face
          (neo-lean-render--add-face rendered diff-face)
        rendered)))
   (t "")))

(defun neo-lean-render--hypothesis (hyp)
  "Render one `InteractiveHypothesisBundle' HYP to a string.
Bundled names share a type, e.g. \"a b : Nat\"; let-binders also show
their value after \":=\"."
  (let* ((names (mapconcat #'neo-lean-render--hyp-name
                           (append (plist-get hyp :names) nil) " "))
         (names (neo-lean-render--add-face
                 names (neo-lean-render--insert-remove-face hyp)))
         (type (neo-lean-render-tagged-text (plist-get hyp :type)))
         (val (plist-get hyp :val)))
    ;; `concat' preserves the type's text properties (the subexpression info),
    ;; which `format' with %s does not reliably do.
    (concat names " : " type
            (when val
              (concat " := " (neo-lean-render-tagged-text val))))))

(defun neo-lean-render-goal (goal)
  "Render one interactive GOAL to a string.
Shows an optional case name, the hypotheses, and the target type prefixed
by the goal's `goalPrefix' (default `neo-lean-render-default-goal-prefix')."
  (let ((lines '())
        (user-name (plist-get goal :userName))
        (prefix (propertize
                 (or (plist-get goal :goalPrefix)
                     neo-lean-render-default-goal-prefix)
                 'face (or (neo-lean-render--insert-remove-face goal)
                           'neo-lean-goal-prefix))))
    (when (and user-name (not (string-empty-p user-name)))
      (push (propertize (format "case %s" user-name)
                        'face 'neo-lean-goal-case)
            lines))
    (seq-doseq (hyp (plist-get goal :hyps))
      (push (neo-lean-render--hypothesis hyp) lines))
    (push (concat prefix (neo-lean-render-tagged-text (plist-get goal :type))) lines)
    (string-join (nreverse lines) "\n")))

(defun neo-lean-render-goals (goals)
  "Render the vector of interactive GOALS to a string.
Returns \"No goals.\" when GOALS is empty."
  (if (seq-empty-p goals)
      "No goals."
    (mapconcat #'neo-lean-render-goal goals "\n\n")))

(defun neo-lean-render-goals-accomplished (message)
  "Render the completed-proof MESSAGE."
  (propertize message 'face 'neo-lean-goal-status))

(defconst neo-lean-render-term-goal-header "Expected type:"
  "Header shown above the term goal (the expected type at point).")

(defun neo-lean-render-term-goal (term-goal)
  "Render an `InteractiveTermGoal' TERM-GOAL, or nil when there is none.
A term goal carries hypotheses and a target type just like a tactic goal,
so it is rendered the same way under `neo-lean-render-term-goal-header'."
  (when term-goal
    (concat (propertize neo-lean-render-term-goal-header
                        'face 'neo-lean-goal-expected-type)
            "\n"
            (neo-lean-render-goal term-goal))))

(defun neo-lean-render-info-popup (popup)
  "Render an `InfoPopup' POPUP to a string, or nil when there is nothing to show.
POPUP is what `Lean.Widget.InteractiveDiagnostics.infoToInteractive' returns
for a subexpression: an optional :exprExplicit and :type (both
`CodeWithInfos') and an optional :doc string.  Renders the explicit
expression and its type on one line as \"EXPR : TYPE\", then the docstring
after a blank line."
  (when popup
    (let* ((expr (plist-get popup :exprExplicit))
           (type (plist-get popup :type))
           (doc (plist-get popup :doc))
           (parts '()))
      (cond
       ((and expr type)
        (push (concat (neo-lean-render-tagged-text expr) " : "
                      (neo-lean-render-tagged-text type))
              parts))
       (type (push (neo-lean-render-tagged-text type) parts))
       (expr (push (neo-lean-render-tagged-text expr) parts)))
      (when (and (stringp doc) (not (string-empty-p doc)))
        (push doc parts))
      (when parts
        (string-join (nreverse parts) "\n\n")))))

(defconst neo-lean-render-messages-header "Messages:"
  "Header shown above Lean diagnostics and trace output.")

(defun neo-lean-render--variant-value (value variant)
  "Return VALUE's VARIANT payload, or nil when absent."
  (and (listp value)
       (plist-member value variant)
       (plist-get value variant)))

(defun neo-lean-render--trace-children (children)
  "Return (KIND . VALUE) for trace CHILDREN."
  (cond
   ((and (listp children) (plist-member children :strict))
    (cons 'strict (plist-get children :strict)))
   ((and (listp children) (plist-member children :lazy))
    (cons 'lazy (plist-get children :lazy)))
   (t nil)))

(defun neo-lean-render--interactive-message-seq (items path parent-cls)
  "Render interactive message ITEMS under PATH and PARENT-CLS."
  (let ((i 0)
        (parts '()))
    (seq-doseq (item items)
      (push (neo-lean-render-interactive-message
             item (format "%s/%d" path i) parent-cls)
            parts)
      (setq i (1+ i)))
    (string-join (nreverse parts) "")))

(defun neo-lean-render--trace (trace path &optional parent-cls)
  "Render TRACE embed under PATH.
PARENT-CLS is used to abbreviate repeated trace class prefixes."
  (let* ((cls (or (plist-get trace :cls) ""))
         (abbr-cls (if (and parent-cls
                            (string-prefix-p (concat parent-cls ".") cls))
                       (substring cls (1+ (length parent-cls)))
                     cls))
         (msg (neo-lean-render-interactive-message
               (plist-get trace :msg) (concat path "/msg") cls))
         (title (concat "[" abbr-cls "] " msg))
         (children (neo-lean-render--trace-children (plist-get trace :children)))
         (lazy (and (eq (car-safe children) 'lazy) (cdr children)))
         (strict (and (eq (car-safe children) 'strict) (cdr children)))
         (state-children-loaded (neo-lean-render--fold-has-value-p path :children))
         (state-children (neo-lean-render--fold-value path :children))
         (rendered-children
          (cond
           (state-children-loaded
            (neo-lean-render--interactive-message-seq
             state-children (concat path "/loaded") cls))
           (strict
            (neo-lean-render--interactive-message-seq
             strict (concat path "/children") cls)))))
    (neo-lean-render--foldable
     path title rendered-children
     :collapsed (plist-get trace :collapsed)
     :lazy lazy
     :indent (or (plist-get trace :indent) 0)
     :loading (neo-lean-render--fold-value path :loading))))

(defun neo-lean-render--msg-embed (embed path inner &optional parent-cls)
  "Render diagnostic message EMBED under PATH.
INNER is the tagged text inside the embedding tag."
  (cond
   ((neo-lean-render--variant-value embed :trace)
    (neo-lean-render--trace
     (neo-lean-render--variant-value embed :trace) path parent-cls))
   ((neo-lean-render--variant-value embed :expr)
    (neo-lean-render-tagged-text (neo-lean-render--variant-value embed :expr)))
   ((neo-lean-render--variant-value embed :goal)
    (neo-lean-render-goal (neo-lean-render--variant-value embed :goal)))
   ((neo-lean-render--variant-value embed :widget)
    (neo-lean-render-interactive-message
     (plist-get (neo-lean-render--variant-value embed :widget) :alt)
     (concat path "/widget")
     parent-cls))
   (t (neo-lean-render-interactive-message inner (concat path "/inner") parent-cls))))

(defun neo-lean-render-interactive-message (message path &optional parent-cls)
  "Render an interactive diagnostic MESSAGE.
PATH is a stable path used to identify foldable trace nodes.  PARENT-CLS is the
parent trace class, used only for display."
  (cond
   ((null message) "")
   ((stringp message) message)
   ((plist-member message :text)
    (or (plist-get message :text) ""))
   ((plist-member message :append)
    (neo-lean-render--interactive-message-seq
     (plist-get message :append) (concat path "/append") parent-cls))
   ((plist-member message :tag)
    (let* ((tag (plist-get message :tag))
           (embed (seq-elt tag 0))
           (inner (seq-elt tag 1)))
      (neo-lean-render--msg-embed embed path inner parent-cls)))
   (t "")))

(defun neo-lean-render--severity-name (severity)
  "Return display text for LSP diagnostic SEVERITY."
  (pcase severity
    (1 "Error")
    (2 "Warning")
    (3 "Information")
    (4 "Hint")
    (_ "Message")))

(defun neo-lean-render-interactive-diagnostic (diagnostic index)
  "Render an interactive DIAGNOSTIC at INDEX."
  (let* ((path (format "diagnostic/%d" index))
         (source (or (plist-get diagnostic :source) "Lean"))
         (title (format "%s %s" source
                        (neo-lean-render--severity-name
                         (plist-get diagnostic :severity))))
         (body (neo-lean-render-interactive-message
                (plist-get diagnostic :message) (concat path "/message"))))
    (neo-lean-render--foldable path title body :collapsed nil)))

(defun neo-lean-render-messages (messages)
  "Render Lean interactive diagnostic MESSAGES, or nil when there are none."
  (when-let* ((messages (seq-filter #'listp messages)))
    (concat (propertize neo-lean-render-messages-header
                        'face 'neo-lean-goal-messages)
            "\n"
            (let ((i 0)
                  (parts '()))
              (dolist (message messages)
                (push (neo-lean-render-interactive-diagnostic message i) parts)
                (setq i (1+ i)))
              (string-join (nreverse parts) "\n\n")))))

(defun neo-lean-render-state
    (goals term-goal &optional messages goals-accomplished)
  "Render the proof state at point to a string.
GOALS is the vector of interactive tactic goals; TERM-GOAL is the optional
`InteractiveTermGoal' (the expected type), or nil.  MESSAGES is an optional
list of Lean interactive diagnostics, including trace output.
GOALS-ACCOMPLISHED is an optional completed-proof status string.  Shows the
completed-proof status first when present, tactic goals when present, then an
`Expected type' section when a term goal is present, then a `Messages' section
when diagnostics are present, and \"No goals.\" when there is no content."
  (let ((sections '()))
    (when goals-accomplished
      (push (neo-lean-render-goals-accomplished goals-accomplished) sections))
    (unless (seq-empty-p goals)
      (push (neo-lean-render-goals goals) sections))
    (when-let* ((term (neo-lean-render-term-goal term-goal)))
      (push term sections))
    (when-let* ((msgs (neo-lean-render-messages messages)))
      (push msgs sections))
    (if sections
        (string-join (nreverse sections) "\n\n")
      "No goals.")))

(provide 'neo-lean-render)
;;; neo-lean-render.el ends here
