;;; neo-lean-mode.el --- Major mode for Lean 4  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/janmasrovira/neo-lean-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A major mode for editing Lean 4 source.  This file defines `neo-lean-mode'
;; and configures Eglot to talk to the Lean language server (`lake serve').
;;
;; This milestone keeps the mode itself minimal -- just enough for Eglot
;; to attach.  Full font-lock, indentation and imenu come later.  The point
;; of the current milestone is the interactive RPC layer (see `neo-lean-rpc.el')
;; and the goal display (`neo-lean-goal').

;;; Code:

(require 'eglot)
(require 'neo-lean-indent)
(require 'neo-lean-input)

(defgroup neo-lean nil
  "Major mode for the Lean 4 theorem prover."
  :group 'languages
  :prefix "neo-lean-")

(defcustom neo-lean-server-command '("lake" "serve")
  "Command used to start the Lean language server.
A list whose first element is the program and the rest are arguments.
Eglot launches this within the current project."
  :type '(repeat string)
  :group 'neo-lean)

(defcustom neo-lean-initialization-options '(:hasWidgets t)
  "Initialization options sent to the Lean language server.
`hasWidgets' asks Lean to return structured interactive diagnostics, including
collapsible trace nodes, from its widget RPC endpoints."
  :type 'plist
  :group 'neo-lean)

;;;; Syntax

(defvar neo-lean-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; `--' line comments and `/- ... -/' (nestable) block comments.
    ;; In Emacs' two-character comment syntax, `-' and `/' share the
    ;; comment-start/-end slots: `--' opens a line comment, `/-' a block.
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?- ". 123" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; Treat common identifier characters as symbol constituents so that
    ;; navigation and `symbol-at-point' behave sensibly on Lean names.
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?' "_" table)
    (modify-syntax-entry ?. "_" table)
    ;; Brackets.
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table)
  "Syntax table for `neo-lean-mode'.")

;;;; Keymap

(defvar-keymap neo-lean-mode-map
  :doc "Keymap for `neo-lean-mode'."
  "C-c C-g" #'neo-lean-goal
  "C-c TAB" #'neo-lean-infoview-toggle
  "C-c C-r" #'neo-lean-restart-file)

;;;; Eglot integration

;; Register the Lean server.  `lake serve' prints nothing on startup, so we
;; tell Eglot not to wait synchronously for output (see
;; `neo-lean--eglot-managed-setup').
(add-to-list 'eglot-server-programs
             (cons 'neo-lean-mode
                   (lambda (&optional _interactive _project)
                     (append neo-lean-server-command
                             (list :initializationOptions
                                   neo-lean-initialization-options)))))

(defun neo-lean--eglot-managed-setup ()
  "Buffer-local Eglot tweaks for Lean, run from `eglot-managed-mode-hook'."
  (when (derived-mode-p 'neo-lean-mode)
    ;; `lake serve' is silent on startup; don't block waiting for a banner.
    (setq-local eglot-sync-connect nil)))

(add-hook 'eglot-managed-mode-hook #'neo-lean--eglot-managed-setup)

;;;; Semantic-tokens refresh (LSP coloring)

;; Lean colors the buffer through Eglot's `eglot-semantic-tokens-mode', but it
;; elaborates a file incrementally and reports semantic tokens only for the part
;; it has processed so far.  Eglot fires a single `textDocument/semanticTokens/
;; full' request -- often while elaboration is still mid-flight -- caches that
;; partial answer, and never asks again: the document text does not change so its
;; cache stays "current", and Lean (unlike the LSP spec's intent) does not send
;; `workspace/semanticTokens/refresh' when more tokens become ready.  The result
;; is the "sometimes only half the file is highlighted" symptom -- whatever had
;; elaborated when that one request landed is all you ever get.
;;
;; Lean does, however, stream `$/lean/fileProgress' as elaboration advances, so
;; we use it as the "more tokens may be ready" signal: re-request tokens
;; (debounced) on progress, which also lands a final refresh once the file goes
;; quiet and the token set is complete.  The spec-correct
;; `workspace/semanticTokens/refresh' is handled too, as insurance for server
;; versions that do send it.

;; Eglot internal: the buffer-local semantic-token cache.  Declared so the
;; byte-compiler is happy on Eglot versions that predate it; we only touch it
;; when `eglot-semantic-tokens-mode' is actually on.
(defvar eglot--semtok-state)
(declare-function neo-lean-uri-to-path "neo-lean-rpc" (uri))

(defcustom neo-lean-semantic-tokens-refresh-delay 0.3
  "Seconds to wait after a file-progress update before refreshing LSP coloring.
Coalesces the burst of `$/lean/fileProgress' notifications Lean sends while
elaborating into occasional semantic-token re-requests."
  :type 'number
  :group 'neo-lean)

(defvar-local neo-lean--semtok-refresh-timer nil
  "Pending debounce timer for a semantic-tokens refresh in this buffer.")

(defun neo-lean--semtok-refresh ()
  "Make Eglot re-request semantic tokens for the current buffer.
With the document text unchanged Eglot treats its cached tokens as current, so
clear the cached version (`:docver') to force `eglot--semtok-font-lock' to issue
a fresh request, then flush font-lock to trigger it."
  (when (and (bound-and-true-p eglot-semantic-tokens-mode)
             (boundp 'eglot--semtok-state))
    (setf (cl-getf eglot--semtok-state :docver) nil)
    (eglot--widening (font-lock-flush))))

(defun neo-lean--semtok-schedule-refresh (uri)
  "Schedule a debounced semantic-tokens refresh for URI's buffer."
  (when-let* ((path (ignore-errors (neo-lean-uri-to-path uri)))
              (buffer (find-buffer-visiting path)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'neo-lean-mode)
                 (bound-and-true-p eglot-semantic-tokens-mode)
                 (not (timerp neo-lean--semtok-refresh-timer)))
        (setq neo-lean--semtok-refresh-timer
              (run-with-timer
               neo-lean-semantic-tokens-refresh-delay nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq neo-lean--semtok-refresh-timer nil)
                     (neo-lean--semtok-refresh))))))))))

;; Re-request tokens as elaboration advances.  An `:after' method augments the
;; progress module's own handling of this notification without replacing it.
(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql $/lean/fileProgress))
           &key textDocument &allow-other-keys)
  (neo-lean--semtok-schedule-refresh (plist-get textDocument :uri)))

;; Insurance: honour `workspace/semanticTokens/refresh' for servers that send it
;; (Eglot's own handler is a deliberate no-op).
(cl-defmethod eglot-handle-request
  (server (_method (eql workspace/semanticTokens/refresh)))
  "Refresh semantic-token coloring in Lean buffers when the server asks."
  (dolist (buffer (eglot--managed-buffers server))
    (eglot--when-live-buffer buffer
      (when (derived-mode-p 'neo-lean-mode)
        (neo-lean--semtok-refresh)))))

;;;; Mode

;;;###autoload
(define-derived-mode neo-lean-mode prog-mode "Neo-Lean"
  "Major mode for editing Lean 4 files.

\\{neo-lean-mode-map}"
  :syntax-table neo-lean-mode-syntax-table
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "[ \t]*--+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'neo-lean-indent-line)
  (setq-local electric-indent-inhibit t)
  ;; Suggest `lake build' when compiling.
  (setq-local compile-command "lake build")
  ;; Unicode abbreviation input (\alpha -> α, \<> -> ⟨⟩, ...).
  (when neo-lean-input-enable
    (activate-input-method neo-lean-input-method-name)))

;; Only claim `.lean' if nothing else already has (e.g. lean4-mode), so the
;; two can coexist and the user's chosen default wins.  Standalone installs
;; still get `neo-lean-mode' automatically.
;;;###autoload
(unless (assoc "\\.lean\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist '("\\.lean\\'" . neo-lean-mode)))

(require 'neo-lean-goal)
(require 'neo-lean-progress)
(require 'neo-lean-restart)
;; Optional editor integrations; each self-disables when its host is absent.
(require 'neo-lean-doom)

(provide 'neo-lean-mode)
;;; neo-lean-mode.el ends here
