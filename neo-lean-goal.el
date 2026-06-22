;;; neo-lean-goal.el --- Show and track the interactive goal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch the interactive goals at point over the Lean RPC session and show
;; them in the goal buffer.  `neo-lean-goal' opens the buffer on demand; while
;; it is visible it follows the cursor, refreshing after a short debounce.
;;
;; Cursor-follow is gated on the goal buffer being visible -- so moving around
;; with the infoview closed costs nothing -- and each request carries a
;; monotonic id so a slow response cannot overwrite a newer one.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'eldoc)
(require 'neo-lean-rpc)
(require 'neo-lean-render)
(require 'neo-lean-infoview)

(defun neo-lean--error-message (err)
  "Best-effort human-readable string for an RPC ERR plist (or value)."
  (or (and (listp err) (plist-get err :message)) err))

(defcustom neo-lean-goal-auto-update t
  "When non-nil, refresh the goal buffer as the cursor moves.
Tracking only happens while the goal buffer is visible; open it with
`neo-lean-goal'."
  :type 'boolean
  :group 'neo-lean)

(defcustom neo-lean-goal-update-delay 0.05
  "Seconds to wait after the cursor stops before refreshing the goal.
Coalesces rapid movement into a single request."
  :type 'number
  :group 'neo-lean)

(defvar-local neo-lean--goal-timer nil
  "Pending debounce timer for the goal refresh in this buffer.")
(defvar-local neo-lean--goal-request-id 0
  "Monotonic id of the latest goal request; guards against stale responses.")
(defvar-local neo-lean--goal-last-point nil
  "Buffer position of the last scheduled refresh.")

(defun neo-lean--goal-line-range (pos)
  "Return the one-line LSP line range containing POS."
  (when-let* ((line (plist-get (plist-get pos :position) :line)))
    (list :start line :end (1+ line))))

(defun neo-lean--goal-diagnostic-tag-p (diagnostic tag)
  "Return non-nil when DIAGNOSTIC has Lean diagnostic TAG."
  (seq-contains-p (plist-get diagnostic :leanTags) tag))

(defun neo-lean--goal-diagnostic-visible-p (diagnostic)
  "Return non-nil when interactive DIAGNOSTIC should appear in the infoview."
  (and (not (plist-get diagnostic :isSilent))
       (not (neo-lean--goal-diagnostic-tag-p
             diagnostic neo-lean--diagnostic-tag-unsolved-goals))
       (not (neo-lean--goal-diagnostic-tag-p
             diagnostic neo-lean--diagnostic-tag-goals-accomplished))))

(defun neo-lean--goal-filter-interactive-diagnostics (diagnostics)
  "Filter Lean interactive DIAGNOSTICS for infoview display."
  (seq-filter #'neo-lean--goal-diagnostic-visible-p diagnostics))

(defun neo-lean--goal-source-buffer ()
  "Return the live source buffer for the visible infoview, or nil."
  (when-let* (((neo-lean-infoview-visible-p))
              (buffer (get-buffer neo-lean-infoview-buffer-name))
              (src (buffer-local-value 'neo-lean-infoview--source-buffer buffer))
              ((buffer-live-p src)))
    src))

(defun neo-lean--goal-buffer-uri-p (uri)
  "Return non-nil if URI names the current buffer's file."
  (when-let* ((path (and uri (neo-lean-uri-to-path uri)))
              (file (buffer-file-name)))
    (equal (file-truename path) (file-truename file))))

(defun neo-lean--goal-schedule-update ()
  "Schedule a debounced infoview refresh for the current source buffer."
  (when (timerp neo-lean--goal-timer)
    (cancel-timer neo-lean--goal-timer))
  (let ((buf (current-buffer)))
    (setq neo-lean--goal-timer
          (run-with-timer
           neo-lean-goal-update-delay nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq neo-lean--goal-timer nil)
                 (neo-lean--goal-update nil))))))))

(defun neo-lean--infoview-fold-state ()
  "Return the current infoview buffer's fold-state table."
  (unless (hash-table-p neo-lean-infoview--fold-state)
    (setq neo-lean-infoview--fold-state (make-hash-table :test #'equal)))
  neo-lean-infoview--fold-state)

(defun neo-lean--infoview-fold-entry (id)
  "Return the fold state entry for ID in the current infoview buffer."
  (gethash id (neo-lean--infoview-fold-state)))

(defun neo-lean--infoview-fold-put (id key value)
  "Set fold state KEY to VALUE for fold ID in the current infoview buffer."
  (let* ((state (neo-lean--infoview-fold-state))
         (entry (copy-sequence (or (gethash id state) '()))))
    (puthash id (plist-put entry key value) state)))

(defun neo-lean--infoview-render-data (data)
  "Render structured infoview DATA using current fold state."
  (let ((neo-lean-render-fold-state (neo-lean--infoview-fold-state)))
    (neo-lean-render-state (plist-get data :goals)
                           (plist-get data :term-goal)
                           (plist-get data :messages))))

(defun neo-lean--infoview-set-render-data (data)
  "Store structured infoview DATA in the shared goal buffer."
  (with-current-buffer (neo-lean-infoview--buffer)
    (neo-lean--infoview-fold-state)
    (setq neo-lean-infoview--render-data data)))

(defun neo-lean--infoview-goto-fold (id)
  "Move point to fold ID in the current infoview buffer."
  (when (stringp id)
    (let ((pos (point-min))
          (end (point-max))
          found)
      (while (and (not found) (< pos end))
        (if (equal (get-text-property pos 'neo-lean-fold-id) id)
            (setq found pos)
          (setq pos (or (next-single-property-change
                         pos 'neo-lean-fold-id nil end)
                        end))))
      (when found
        (goto-char found)))))

(defun neo-lean--infoview-rerender (&optional preserve-fold-id)
  "Rerender the shared goal buffer from its last structured payload.
When PRESERVE-FOLD-ID is non-nil, leave point on that fold header after
rerendering."
  (when-let* ((buffer (get-buffer neo-lean-infoview-buffer-name)))
    (let ((window-starts
           (mapcar (lambda (window)
                     (cons window (window-start window)))
                   (get-buffer-window-list buffer nil t))))
      (with-current-buffer buffer
        (when neo-lean-infoview--render-data
          (neo-lean-infoview-update
           (neo-lean--infoview-render-data neo-lean-infoview--render-data)
           t)
          (when preserve-fold-id
            (neo-lean--infoview-goto-fold preserve-fold-id))
          (dolist (entry window-starts)
            (when (window-live-p (car entry))
              (set-window-start (car entry)
                                (min (cdr entry) (point-max))
                                t))))))))

(defun neo-lean--infoview-fold-property-at-line (property)
  "Return fold PROPERTY at point or on the current line."
  (or (get-text-property (point) property)
      (let ((pos (line-beginning-position))
            (end (line-end-position))
            value)
        (while (and (not value) (< pos end))
          (setq value (get-text-property pos property))
          (setq pos (or (next-single-property-change pos property nil end)
                        end)))
        value)))

(defun neo-lean--infoview-load-lazy-fold (id lazy)
  "Load lazy trace children LAZY for fold ID."
  (let* ((infoview (current-buffer))
         (src neo-lean-infoview--source-buffer)
         (pos neo-lean-infoview--source-pos))
    (unless (buffer-live-p src)
      (user-error "No live Lean source buffer for this goal"))
    (with-current-buffer src
      (unless (eglot-current-server)
        (user-error "No Lean language server connected"))
      (let ((subsession (neo-lean-rpc-open pos)))
        (neo-lean-rpc-lazy-trace-children-to-interactive
         subsession lazy
         (lambda (children)
           (when (buffer-live-p infoview)
             (with-current-buffer infoview
               (neo-lean--infoview-fold-put id :loading nil)
               (neo-lean--infoview-fold-put id :children children)
               (neo-lean--infoview-rerender id))))
         (lambda (err)
           (when (buffer-live-p infoview)
             (with-current-buffer infoview
               (neo-lean--infoview-fold-put id :loading nil)
               (neo-lean--infoview-fold-put
                id :children
                (vector (list :text
                              (format "Error loading trace children: %s"
                                      (neo-lean--error-message err)))))
               (neo-lean--infoview-rerender id)))))))))

(defun neo-lean--goal-update (&optional display)
  "Fetch the proof state at point and render it into the infoview.
Requests the interactive tactic goals and the term goal (the expected
type) and shows both.  With DISPLAY non-nil, pop up the infoview window
once they arrive; otherwise refresh its contents in place."
  (when (eglot-current-server)
    (let* ((id (cl-incf neo-lean--goal-request-id))
           (src (current-buffer))
           (pos (neo-lean-rpc-position-params))
           (line-range (neo-lean--goal-line-range pos))
           (subsession (neo-lean-rpc-open pos)))
      (cl-labels ((fresh-p ()
                    (and (buffer-live-p src)
                         (= id (buffer-local-value 'neo-lean--goal-request-id src))))
                  (show (goals term-goal diagnostics)
                    (let* ((data (list :goals goals
                                       :term-goal term-goal
                                       :messages diagnostics)))
                      ;; Remember where this goal came from so interactive
                      ;; commands in the goal buffer can query the server.
                      (neo-lean-infoview-set-source src pos)
                      (neo-lean--infoview-set-render-data data)
                      (let ((text (with-current-buffer (neo-lean-infoview--buffer)
                                    (neo-lean--infoview-render-data data))))
                        (if display
                            (neo-lean-infoview-display text)
                          (neo-lean-infoview-update text)))))
                  (show-with-diagnostics (goals term-goal)
                    (neo-lean-rpc-get-interactive-diagnostics
                     subsession line-range
                     (lambda (diagnostics)
                       (when (fresh-p)
                         (show goals term-goal
                               (neo-lean--goal-filter-interactive-diagnostics
                                diagnostics))))
                     (lambda (_err)
                       (when (fresh-p)
                         (show goals term-goal nil))))))
        (neo-lean-rpc-get-interactive-goals
         subsession
         (lambda (result)
           (when (fresh-p)
             ;; Chain the term goal so both render together; a missing or
             ;; failed term goal just leaves the tactic goals showing.
             (let ((goals (plist-get result :goals)))
               (neo-lean-rpc-get-interactive-term-goal
                subsession
                (lambda (term-goal)
                  (when (fresh-p)
                    (show-with-diagnostics goals term-goal)))
                (lambda (_err)
                  (when (fresh-p)
                    (show-with-diagnostics goals nil)))))))
         (lambda (err)
           ;; Ignore transient errors during movement; only report on an
           ;; explicit request.
           (when (and (fresh-p) display)
             (message "neo-lean-goal: %s" (neo-lean--error-message err)))))))))

;;;###autoload
(defun neo-lean-goal ()
  "Display the interactive tactic state at point in the goal buffer.
Opens the goal buffer; while it stays visible it follows the cursor (see
`neo-lean-goal-auto-update').  Requires an active Eglot connection to the
Lean server in this buffer."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No Lean language server connected (try `eglot')"))
  (neo-lean--goal-update t))

;;;###autoload
(defun neo-lean-infoview-toggle ()
  "Toggle the Lean infoview for the current buffer.
When the goal buffer is visible, hide it.  Otherwise fetch and display the
interactive tactic state at point, like `neo-lean-goal'."
  (interactive)
  (if (neo-lean-infoview-visible-p)
      (neo-lean-infoview-hide)
    (neo-lean-goal)))

;;;###autoload
(defun neo-lean-infoview-toggle-fold (&optional event)
  "Toggle the fold at point in the Lean infoview.
When invoked by mouse EVENT, toggle the fold under the click."
  (interactive)
  (when (and (consp event) (eventp event))
    (let ((posn (event-end event)))
      (when (windowp (posn-window posn))
        (select-window (posn-window posn)))
      (when (integer-or-marker-p (posn-point posn))
        (goto-char (posn-point posn)))))
  (let ((id (neo-lean--infoview-fold-property-at-line 'neo-lean-fold-id))
        (collapsed (neo-lean--infoview-fold-property-at-line
                    'neo-lean-fold-collapsed))
        (lazy (neo-lean--infoview-fold-property-at-line 'neo-lean-fold-lazy)))
    (unless id
      (user-error "No fold at point"))
    (if (and collapsed lazy
             (not (plist-member (neo-lean--infoview-fold-entry id) :children)))
        (progn
          (neo-lean--infoview-fold-put id :collapsed nil)
          (neo-lean--infoview-fold-put id :loading t)
          (neo-lean--infoview-rerender id)
          (neo-lean--infoview-load-lazy-fold id lazy))
      (neo-lean--infoview-fold-put id :collapsed (not collapsed))
      (neo-lean--infoview-rerender id))))

(defun neo-lean-infoview-mouse-toggle-fold (event)
  "Toggle the infoview fold clicked by mouse EVENT.
Clicks outside fold headers simply move point inside the infoview."
  (interactive "e")
  (let* ((posn (event-end event))
         (window (posn-window posn))
         (pos (posn-point posn)))
    (when (windowp window)
      (select-window window))
    (when (integer-or-marker-p pos)
      (goto-char pos)
      (when (neo-lean--infoview-fold-property-at-line 'neo-lean-fold-id)
        (neo-lean-infoview-toggle-fold)))))

(defun neo-lean--goal-post-command ()
  "Schedule a goal refresh after cursor movement, while the infoview is visible."
  (when (and neo-lean-goal-auto-update
             (eglot-current-server)
             (neo-lean-infoview-visible-p)
             (not (eql (point) neo-lean--goal-last-point)))
    (setq neo-lean--goal-last-point (point))
    (neo-lean--goal-schedule-update)))

(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql textDocument/publishDiagnostics))
           &key uri &allow-other-keys)
  "Refresh a visible infoview when Lean publishes diagnostics for its source."
  (when-let* (((bound-and-true-p neo-lean-goal-auto-update))
              (src (neo-lean--goal-source-buffer)))
    (with-current-buffer src
      (when (and (eglot-current-server)
                 (neo-lean--goal-buffer-uri-p uri))
        (neo-lean--goal-schedule-update)))))

(defun neo-lean--goal-setup ()
  "Install buffer-local cursor-follow tracking for the goal buffer."
  (add-hook 'post-command-hook #'neo-lean--goal-post-command nil t))

(add-hook 'neo-lean-mode-hook #'neo-lean--goal-setup)

;;;; Interactive subexpressions: go to definition/declaration/type

(define-key neo-lean-infoview-mode-map (kbd "TAB") #'neo-lean-infoview-toggle-fold)
(define-key neo-lean-infoview-mode-map [mouse-1] #'neo-lean-infoview-mouse-toggle-fold)
(define-key neo-lean-infoview-mode-map [mouse-2] #'ignore)

(defun neo-lean--goal-jump (link)
  "Jump to LSP LINK, an LSP `Location' or `LocationLink' plist."
  (let* ((uri (or (plist-get link :targetUri) (plist-get link :uri)))
         (range (or (plist-get link :targetSelectionRange)
                    (plist-get link :targetRange)
                    (plist-get link :range)))
         (path (and uri (neo-lean-uri-to-path uri))))
    (unless path
      (user-error "Location has no file URI"))
    (pop-to-buffer (find-file-noselect path))
    (when range
      (goto-char (eglot--lsp-position-to-point (plist-get range :start)))
      (recenter))))

(defun neo-lean--goal-go-to (kind)
  "Resolve the subexpression under point and go to its KIND location.
KIND is \"definition\", \"declaration\" or \"type\".  Reads the
`neo-lean-info' text property at point in the goal buffer and asks the Lean
server (on the source buffer's session) for the location.

Returns non-nil once the request is dispatched.  The actual jump happens
asynchronously in the callback, so this is also a well-behaved Doom
`+lookup' handler: Doom can only judge success by the return value (point
has not moved yet when we return), so we must report success here."
  (let ((info (get-text-property (point) 'neo-lean-info))
        (src neo-lean-infoview--source-buffer)
        (pos neo-lean-infoview--source-pos))
    (unless info
      (user-error "No subexpression under point"))
    (unless (buffer-live-p src)
      (user-error "No live Lean source buffer for this goal"))
    (with-current-buffer src
      (unless (eglot-current-server)
        (user-error "No Lean language server connected"))
      (let ((subsession (neo-lean-rpc-open pos)))
        (neo-lean-rpc-get-go-to-location
         subsession kind info
         (lambda (links)
           (if (seq-empty-p links)
               (message "neo-lean: no %s location" kind)
             (neo-lean--goal-jump (seq-elt links 0))))
         (lambda (err)
           (message "neo-lean: %s" (neo-lean--error-message err))))))
    t))

(defun neo-lean-goal-go-to-definition ()
  "Go to the definition of the subexpression under point in the goal buffer."
  (interactive)
  (neo-lean--goal-go-to "definition"))

(defun neo-lean-goal-go-to-declaration ()
  "Go to the declaration of the subexpression under point in the goal buffer."
  (interactive)
  (neo-lean--goal-go-to "declaration"))

(defun neo-lean-goal-go-to-type ()
  "Go to the type of the subexpression under point in the goal buffer."
  (interactive)
  (neo-lean--goal-go-to "type"))

(define-key neo-lean-infoview-mode-map (kbd "RET") #'neo-lean-goal-go-to-definition)

;;;; Hover: type and docs of the subexpression under point (via ElDoc)

(defun neo-lean-goal-eldoc-function (callback &rest _)
  "ElDoc function for the goal buffer: the type and docs of the subexpr at point.
Reads the `neo-lean-info' text property under point, asks the Lean server (on
the source buffer's session) via `infoToInteractive', and reports the result
asynchronously through CALLBACK.  Returns non-nil when a request is dispatched
so ElDoc waits for the async answer.

The response is dropped if point has since moved, so a slow reply cannot show
the wrong subexpression's type."
  (when-let* ((info (get-text-property (point) 'neo-lean-info))
              (src neo-lean-infoview--source-buffer)
              ((buffer-live-p src))
              (pos neo-lean-infoview--source-pos))
    (let ((buf (current-buffer))
          (pt (point)))
      (with-current-buffer src
        (when (eglot-current-server)
          (let ((subsession (neo-lean-rpc-open pos)))
            (neo-lean-rpc-info-to-interactive
             subsession info
             (lambda (popup)
               (when (and (buffer-live-p buf)
                          (eq (with-current-buffer buf (point)) pt))
                 (when-let* ((text (neo-lean-render-info-popup popup)))
                   (funcall callback text))))
             #'ignore))
          t)))))

(defvar-local neo-lean--goal-hover-overlay nil
  "Overlay highlighting the subexpression under point in the goal buffer.")

(defun neo-lean--subexpr-coords (path)
  "Return the coordinate list of subexpression PATH, or nil.
PATH is Lean's `SubExpr.Pos' serialization: a slash-separated path of
coordinates such as \"/1/0\", with the root being \"/\" (the empty path)."
  (and (stringp path)
       (seq-remove #'string-empty-p (split-string path "/"))))

(defun neo-lean--subexpr-ancestor-p (ancestor descendant)
  "Non-nil if subexpression ANCESTOR is an ancestor of, or equal to, DESCENDANT.
Ancestry is prefix containment on the coordinate paths (compared per
coordinate, not as raw strings), so the root \"/\" is an ancestor of every
subexpression."
  ;; Both must be real paths.  The root \"/\" and a missing path both have
  ;; empty coordinates, so without the `stringp' guard the root would count as
  ;; an ancestor of every unpropertised character (separators, the turnstile,
  ;; hypothesis names), and the span would engulf the whole buffer.
  (and (stringp ancestor) (stringp descendant)
       (let ((a (neo-lean--subexpr-coords ancestor))
             (d (neo-lean--subexpr-coords descendant)))
         (and (<= (length a) (length d))
              (cl-every #'equal a d)))))

(defun neo-lean--goal-span-at (pos)
  "Return (START . END) of the subexpression under POS, or nil.
The span is the full, contiguous extent of the innermost subexpression
covering POS: the maximal run whose `neo-lean-subexpr-pos' is that
subexpression's path or a descendant of it.  Because a subexpression always
renders as one contiguous block, this yields its complete extent -- nested
children and the subexpression's own delimiters alike -- so hovering a
binder's brace highlights the whole binder rather than a stray fragment."
  (let ((path (get-text-property pos 'neo-lean-subexpr-pos)))
    (when path
      (let ((start pos)
            (end pos)
            (min (point-min))
            (max (point-max)))
        (while (and (> start min)
                    (neo-lean--subexpr-ancestor-p
                     path (get-text-property (1- start) 'neo-lean-subexpr-pos)))
          (setq start (1- start)))
        (while (and (< end max)
                    (neo-lean--subexpr-ancestor-p
                     path (get-text-property end 'neo-lean-subexpr-pos)))
          (setq end (1+ end)))
        (cons start end)))))

(defun neo-lean--goal-update-hover ()
  "Move the hover highlight to the subexpression under point.
Hides it when point is not on an interactive subexpression."
  (let ((span (neo-lean--goal-span-at (point))))
    (cond
     (span
      (unless (overlayp neo-lean--goal-hover-overlay)
        (setq neo-lean--goal-hover-overlay (make-overlay 1 1))
        (overlay-put neo-lean--goal-hover-overlay 'face 'neo-lean-goal-hover))
      (move-overlay neo-lean--goal-hover-overlay (car span) (cdr span)
                    (current-buffer)))
     ((overlayp neo-lean--goal-hover-overlay)
      (delete-overlay neo-lean--goal-hover-overlay)))))

;; Optional: show the hover in a childframe tooltip at point when the
;; `eldoc-box' package is installed.  Declared so the byte-compiler doesn't
;; warn when the package is absent.
(declare-function eldoc-box-hover-at-point-mode "ext:eldoc-box")

(defcustom neo-lean-goal-hover-tooltip t
  "When non-nil, show goal-buffer hovers in a childframe tooltip at point.
This needs the optional `eldoc-box' package and a graphical frame; without
either, the hover falls back to ElDoc's echo-area display."
  :type 'boolean
  :group 'neo-lean)

(defun neo-lean--infoview-eldoc-setup ()
  "Enable ElDoc hover and the hover highlight in the goal buffer.
If `neo-lean-goal-hover-tooltip' is set and the optional `eldoc-box' package
is available, the hover is shown in a childframe tooltip at point; otherwise
ElDoc falls back to the echo area."
  (add-hook 'eldoc-documentation-functions
            #'neo-lean-goal-eldoc-function nil t)
  (eldoc-mode 1)
  (when (and neo-lean-goal-hover-tooltip
             (display-graphic-p)
             (require 'eldoc-box nil t))
    (eldoc-box-hover-at-point-mode 1))
  (add-hook 'post-command-hook #'neo-lean--goal-update-hover nil t))

(add-hook 'neo-lean-infoview-mode-hook #'neo-lean--infoview-eldoc-setup)

(provide 'neo-lean-goal)
;;; neo-lean-goal.el ends here
