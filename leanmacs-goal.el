;;; leanmacs-goal.el --- Show and track the interactive goal  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch the interactive goals at point over the Lean RPC session and show
;; them in the goal buffer.  `leanmacs-goal' opens the buffer on demand; while
;; it is visible it follows the cursor, refreshing after a short debounce
;; (like lean.nvim's `update_cooldown').
;;
;; Cursor-follow is gated on the goal buffer being visible -- so moving around
;; with the infoview closed costs nothing -- and each request carries a
;; monotonic id so a slow response cannot overwrite a newer one.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'eldoc)
(require 'leanmacs-rpc)
(require 'leanmacs-render)
(require 'leanmacs-infoview)

;; `eglot-uri-to-path' is the public name on Emacs 30+; on 29.x only the
;; internal `eglot--uri-to-path' exists.  Pick whichever is present.
(defalias 'leanmacs--goal-uri-to-path
  (if (fboundp 'eglot-uri-to-path) 'eglot-uri-to-path 'eglot--uri-to-path)
  "Convert an LSP document URI to a local file path.")

(defcustom leanmacs-goal-auto-update t
  "When non-nil, refresh the goal buffer as the cursor moves.
Tracking only happens while the goal buffer is visible; open it with
`leanmacs-goal'."
  :type 'boolean
  :group 'leanmacs)

(defcustom leanmacs-goal-update-delay 0.05
  "Seconds to wait after the cursor stops before refreshing the goal.
Coalesces rapid movement into a single request."
  :type 'number
  :group 'leanmacs)

(defvar-local leanmacs--goal-timer nil
  "Pending debounce timer for the goal refresh in this buffer.")
(defvar-local leanmacs--goal-request-id 0
  "Monotonic id of the latest goal request; guards against stale responses.")
(defvar-local leanmacs--goal-last-point nil
  "Buffer position of the last scheduled refresh.")

(defun leanmacs--goal-update (&optional display)
  "Fetch the proof state at point and render it into the infoview.
Requests the interactive tactic goals and the term goal (the expected
type) and shows both.  With DISPLAY non-nil, pop up the infoview window
once they arrive; otherwise refresh its contents in place."
  (when (eglot-current-server)
    (let* ((id (cl-incf leanmacs--goal-request-id))
           (src (current-buffer))
           (pos (leanmacs-rpc-position-params))
           (subsession (leanmacs-rpc-open pos)))
      (cl-flet ((fresh-p ()
                  (and (buffer-live-p src)
                       (= id (buffer-local-value 'leanmacs--goal-request-id src))))
                (show (goals term-goal)
                  (let ((text (leanmacs-render-state goals term-goal)))
                    ;; Remember where this goal came from so interactive
                    ;; commands in the goal buffer can query the server.
                    (leanmacs-infoview-set-source src pos)
                    (if display
                        (leanmacs-infoview-display text)
                      (leanmacs-infoview-update text)))))
        (leanmacs-rpc-get-interactive-goals
         subsession
         (lambda (result)
           (when (fresh-p)
             ;; Chain the term goal so both render together; a missing or
             ;; failed term goal just leaves the tactic goals showing.
             (let ((goals (plist-get result :goals)))
               (leanmacs-rpc-get-interactive-term-goal
                subsession
                (lambda (term-goal) (when (fresh-p) (show goals term-goal)))
                (lambda (_err) (when (fresh-p) (show goals nil)))))))
         (lambda (err)
           ;; Ignore transient errors during movement; only report on an
           ;; explicit request.
           (when (and (fresh-p) display)
             (message "leanmacs-goal: %s"
                      (or (and (listp err) (plist-get err :message)) err)))))))))

;;;###autoload
(defun leanmacs-goal ()
  "Display the interactive tactic state at point in the goal buffer.
Opens the goal buffer; while it stays visible it follows the cursor (see
`leanmacs-goal-auto-update').  Requires an active Eglot connection to the
Lean server in this buffer."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No Lean language server connected (try `eglot')"))
  (leanmacs--goal-update t))

(defun leanmacs--goal-post-command ()
  "Schedule a goal refresh after cursor movement, while the infoview is visible."
  (when (and leanmacs-goal-auto-update
             (eglot-current-server)
             (leanmacs-infoview-visible-p)
             (not (eql (point) leanmacs--goal-last-point)))
    (setq leanmacs--goal-last-point (point))
    (when (timerp leanmacs--goal-timer)
      (cancel-timer leanmacs--goal-timer))
    (let ((buf (current-buffer)))
      (setq leanmacs--goal-timer
            (run-with-timer
             leanmacs-goal-update-delay nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq leanmacs--goal-timer nil)
                   (leanmacs--goal-update nil)))))))))

(defun leanmacs--goal-setup ()
  "Install buffer-local cursor-follow tracking for the goal buffer."
  (add-hook 'post-command-hook #'leanmacs--goal-post-command nil t))

(add-hook 'leanmacs-mode-hook #'leanmacs--goal-setup)

;;;; Interactive subexpressions: go to definition/declaration/type

(defun leanmacs--goal-jump (link)
  "Jump to LSP LINK, an LSP `Location' or `LocationLink' plist."
  (let* ((uri (or (plist-get link :targetUri) (plist-get link :uri)))
         (range (or (plist-get link :targetSelectionRange)
                    (plist-get link :targetRange)
                    (plist-get link :range)))
         (path (and uri (leanmacs--goal-uri-to-path uri))))
    (unless path
      (user-error "Location has no file URI"))
    (pop-to-buffer (find-file-noselect path))
    (when range
      (goto-char (eglot--lsp-position-to-point (plist-get range :start)))
      (recenter))))

(defun leanmacs--goal-go-to (kind)
  "Resolve the subexpression under point and go to its KIND location.
KIND is \"definition\", \"declaration\" or \"type\".  Reads the
`leanmacs-info' text property at point in the goal buffer and asks the Lean
server (on the source buffer's session) for the location.

Returns non-nil once the request is dispatched.  The actual jump happens
asynchronously in the callback, so this is also a well-behaved Doom
`+lookup' handler: Doom can only judge success by the return value (point
has not moved yet when we return), so we must report success here."
  (let ((info (get-text-property (point) 'leanmacs-info))
        (src leanmacs-infoview--source-buffer)
        (pos leanmacs-infoview--source-pos))
    (unless info
      (user-error "No subexpression under point"))
    (unless (buffer-live-p src)
      (user-error "No live Lean source buffer for this goal"))
    (with-current-buffer src
      (unless (eglot-current-server)
        (user-error "No Lean language server connected"))
      (let ((subsession (leanmacs-rpc-open pos)))
        (leanmacs-rpc-get-go-to-location
         subsession kind info
         (lambda (links)
           (if (seq-empty-p links)
               (message "leanmacs: no %s location" kind)
             (leanmacs--goal-jump (seq-elt links 0))))
         (lambda (err)
           (message "leanmacs: %s"
                    (or (and (listp err) (plist-get err :message)) err))))))
    t))

(defun leanmacs-goal-go-to-definition ()
  "Go to the definition of the subexpression under point in the goal buffer."
  (interactive)
  (leanmacs--goal-go-to "definition"))

(defun leanmacs-goal-go-to-declaration ()
  "Go to the declaration of the subexpression under point in the goal buffer."
  (interactive)
  (leanmacs--goal-go-to "declaration"))

(defun leanmacs-goal-go-to-type ()
  "Go to the type of the subexpression under point in the goal buffer."
  (interactive)
  (leanmacs--goal-go-to "type"))

(define-key leanmacs-infoview-mode-map (kbd "RET") #'leanmacs-goal-go-to-definition)

;;;; Hover: type and docs of the subexpression under point (via ElDoc)

(defun leanmacs-goal-eldoc-function (callback &rest _)
  "ElDoc function for the goal buffer: the type and docs of the subexpr at point.
Reads the `leanmacs-info' text property under point, asks the Lean server (on
the source buffer's session) via `infoToInteractive', and reports the result
asynchronously through CALLBACK.  Returns non-nil when a request is dispatched
so ElDoc waits for the async answer.

The response is dropped if point has since moved, so a slow reply cannot show
the wrong subexpression's type."
  (when-let* ((info (get-text-property (point) 'leanmacs-info))
              (src leanmacs-infoview--source-buffer)
              ((buffer-live-p src))
              (pos leanmacs-infoview--source-pos))
    (let ((buf (current-buffer))
          (pt (point)))
      (with-current-buffer src
        (when (eglot-current-server)
          (let ((subsession (leanmacs-rpc-open pos)))
            (leanmacs-rpc-info-to-interactive
             subsession info
             (lambda (popup)
               (when (and (buffer-live-p buf)
                          (eq (with-current-buffer buf (point)) pt))
                 (when-let* ((text (leanmacs-render-info-popup popup)))
                   (funcall callback text))))
             #'ignore))
          t)))))

(defvar-local leanmacs--goal-hover-overlay nil
  "Overlay highlighting the subexpression under point in the goal buffer.")

(defun leanmacs--subexpr-coords (path)
  "Return the coordinate list of subexpression PATH, or nil.
PATH is Lean's `SubExpr.Pos' serialization: a slash-separated path of
coordinates such as \"/1/0\", with the root being \"/\" (the empty path)."
  (and (stringp path)
       (seq-remove #'string-empty-p (split-string path "/"))))

(defun leanmacs--subexpr-ancestor-p (ancestor descendant)
  "Non-nil if subexpression ANCESTOR is an ancestor of, or equal to, DESCENDANT.
Ancestry is prefix containment on the coordinate paths (compared per
coordinate, not as raw strings), so the root \"/\" is an ancestor of every
subexpression."
  ;; Both must be real paths.  The root \"/\" and a missing path both have
  ;; empty coordinates, so without the `stringp' guard the root would count as
  ;; an ancestor of every unpropertised character (separators, the turnstile,
  ;; hypothesis names), and the span would engulf the whole buffer.
  (and (stringp ancestor) (stringp descendant)
       (let ((a (leanmacs--subexpr-coords ancestor))
             (d (leanmacs--subexpr-coords descendant)))
         (and (<= (length a) (length d))
              (cl-every #'equal a d)))))

(defun leanmacs--goal-span-at (pos)
  "Return (START . END) of the subexpression under POS, or nil.
The span is the full, contiguous extent of the innermost subexpression
covering POS: the maximal run whose `leanmacs-subexpr-pos' is that
subexpression's path or a descendant of it.  Because a subexpression always
renders as one contiguous block, this yields its complete extent -- nested
children and the subexpression's own delimiters alike -- so hovering a
binder's brace highlights the whole binder rather than a stray fragment."
  (let ((path (get-text-property pos 'leanmacs-subexpr-pos)))
    (when path
      (let ((start pos)
            (end pos)
            (min (point-min))
            (max (point-max)))
        (while (and (> start min)
                    (leanmacs--subexpr-ancestor-p
                     path (get-text-property (1- start) 'leanmacs-subexpr-pos)))
          (setq start (1- start)))
        (while (and (< end max)
                    (leanmacs--subexpr-ancestor-p
                     path (get-text-property end 'leanmacs-subexpr-pos)))
          (setq end (1+ end)))
        (cons start end)))))

(defun leanmacs--goal-update-hover ()
  "Move the hover highlight to the subexpression under point.
Hides it when point is not on an interactive subexpression."
  (let ((span (leanmacs--goal-span-at (point))))
    (cond
     (span
      (unless (overlayp leanmacs--goal-hover-overlay)
        (setq leanmacs--goal-hover-overlay (make-overlay 1 1))
        (overlay-put leanmacs--goal-hover-overlay 'face 'leanmacs-goal-hover))
      (move-overlay leanmacs--goal-hover-overlay (car span) (cdr span)
                    (current-buffer)))
     ((overlayp leanmacs--goal-hover-overlay)
      (delete-overlay leanmacs--goal-hover-overlay)))))

(defun leanmacs--infoview-eldoc-setup ()
  "Enable ElDoc hover and the hover highlight in the goal buffer."
  (add-hook 'eldoc-documentation-functions
            #'leanmacs-goal-eldoc-function nil t)
  (eldoc-mode 1)
  (add-hook 'post-command-hook #'leanmacs--goal-update-hover nil t))

(add-hook 'leanmacs-infoview-mode-hook #'leanmacs--infoview-eldoc-setup)

(provide 'leanmacs-goal)
;;; leanmacs-goal.el ends here
