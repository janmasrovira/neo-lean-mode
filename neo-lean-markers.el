;;; neo-lean-markers.el --- Source-buffer markers from Lean diagnostics  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Lean can publish silent diagnostics for editor-only markers.  In particular,
;; `leanTags = [2]' means "goals accomplished": the proof at that range is
;; complete.  This module filters those silent diagnostics out of Eglot's normal
;; diagnostics and renders a small marker in the source buffer margin, matching
;; lean.nvim's sign-column behaviour.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'seq)
(require 'subr-x)
(require 'neo-lean-rpc)

(defcustom neo-lean-goals-accomplished-enable t
  "When non-nil, show a marker next to completed Lean proofs."
  :type 'boolean
  :group 'neo-lean)

(defcustom neo-lean-goals-accomplished-character "🎉"
  "Character shown in the margin for completed Lean proofs.
Set this to an empty string to disable the marker without disabling silent
diagnostic filtering."
  :type 'string
  :group 'neo-lean)

(defface neo-lean-goals-accomplished
  '((t :inherit success))
  "Face for the completed-proof marker."
  :group 'neo-lean)

(defvar-local neo-lean-goals-accomplished--overlays nil
  "Overlays currently showing completed-proof markers in this buffer.")
(defvar-local neo-lean-goals-accomplished--saved-left-margin nil
  "Previous left margin width before completed-proof markers widened it.")

(defun neo-lean-goals-accomplished--lean-capabilities ()
  "Return Lean-specific client capabilities for completed-proof markers."
  (let ((lean (list :rpcWireFormat "v1")))
    (when neo-lean-goals-accomplished-enable
      (setq lean (plist-put lean :silentDiagnosticSupport t)))
    lean))

(defun neo-lean-goals-accomplished-diagnostic-p (diagnostic)
  "Return non-nil when DIAGNOSTIC is Lean's completed-proof marker."
  (seq-contains-p (plist-get diagnostic :leanTags)
                  neo-lean--diagnostic-tag-goals-accomplished))

(defun neo-lean--silent-diagnostic-p (diagnostic)
  "Return non-nil when DIAGNOSTIC is marked silent by Lean."
  (eq (plist-get diagnostic :isSilent) t))

(defun neo-lean--filter-silent-diagnostics (diagnostics)
  "Return DIAGNOSTICS without Lean silent diagnostics, as a vector."
  (vconcat (seq-remove #'neo-lean--silent-diagnostic-p diagnostics)))

(defun neo-lean-goals-accomplished--clear ()
  "Remove completed-proof marker overlays from the current buffer."
  (mapc #'delete-overlay neo-lean-goals-accomplished--overlays)
  (setq neo-lean-goals-accomplished--overlays nil)
  (when neo-lean-goals-accomplished--saved-left-margin
    (setq left-margin-width neo-lean-goals-accomplished--saved-left-margin
          neo-lean-goals-accomplished--saved-left-margin nil)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-margins window left-margin-width right-margin-width))))

(defun neo-lean-goals-accomplished--ensure-margin ()
  "Ensure the left margin can display the completed-proof marker."
  (let ((width (max 1 (string-width neo-lean-goals-accomplished-character))))
    (when (< (or left-margin-width 0) width)
      (unless neo-lean-goals-accomplished--saved-left-margin
        (setq neo-lean-goals-accomplished--saved-left-margin
              (or left-margin-width 0)))
      (setq left-margin-width width)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (set-window-margins window left-margin-width right-margin-width)))))

(defun neo-lean-goals-accomplished--before-string ()
  "Return the marker `before-string' for a completed proof."
  (propertize
   " "
   'display
   `((margin left-margin)
     ,(propertize neo-lean-goals-accomplished-character
                  'face 'neo-lean-goals-accomplished))
   'help-echo "Goals accomplished"))

(defun neo-lean-goals-accomplished--position-point (position)
  "Return the buffer point corresponding to LSP POSITION, or nil."
  (when (and (plist-get position :line)
             (plist-get position :character))
    (ignore-errors
      (eglot--lsp-position-to-point position))))

(defun neo-lean-goals-accomplished--range (diagnostic)
  "Return DIAGNOSTIC's semantic range."
  (or (plist-get diagnostic :fullRange)
      (plist-get diagnostic :range)))

(defun neo-lean-goals-accomplished--add (diagnostic)
  "Add a completed-proof marker for DIAGNOSTIC in the current buffer."
  (when-let* ((range (neo-lean-goals-accomplished--range diagnostic))
              (start (neo-lean-goals-accomplished--position-point
                      (plist-get range :start))))
    (let* ((end (or (neo-lean-goals-accomplished--position-point
                     (plist-get range :end))
                    start))
           (range-end (if (> end start) end (min (point-max) (1+ start))))
           (range-overlay (make-overlay start range-end))
           (marker-overlay (make-overlay start range-end)))
      (overlay-put range-overlay 'neo-lean-goals-accomplished t)
      (overlay-put range-overlay 'evaporate t)
      (overlay-put marker-overlay 'neo-lean-goals-accomplished-marker t)
      (overlay-put marker-overlay 'before-string
                   (neo-lean-goals-accomplished--before-string))
      (push range-overlay neo-lean-goals-accomplished--overlays)
      (push marker-overlay neo-lean-goals-accomplished--overlays))))

(defun neo-lean-goals-accomplished--redraw (diagnostics)
  "Redraw completed-proof markers for DIAGNOSTICS in the current buffer."
  (neo-lean-goals-accomplished--clear)
  (when (and neo-lean-goals-accomplished-enable
             (not (string-empty-p neo-lean-goals-accomplished-character)))
    (let ((markers (seq-filter #'neo-lean-goals-accomplished-diagnostic-p
                               diagnostics)))
      (when markers
        (neo-lean-goals-accomplished--ensure-margin)
        (dolist (diagnostic markers)
          (neo-lean-goals-accomplished--add diagnostic))))))

(defun neo-lean-goals-accomplished-at-point-p (&optional point)
  "Return non-nil when POINT is inside a completed-proof range."
  (seq-some
   (lambda (overlay)
     (overlay-get overlay 'neo-lean-goals-accomplished))
   (overlays-at (or point (point)))))

(defun neo-lean-goals-accomplished-at-line-p (&optional point)
  "Return non-nil when POINT's line overlaps a completed-proof range."
  (save-excursion
    (goto-char (or point (point)))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (seq-some
       (lambda (overlay)
         (overlay-get overlay 'neo-lean-goals-accomplished))
       (overlays-in beg end)))))

(defun neo-lean-goals-accomplished--buffer-for-uri (uri)
  "Return the live Lean buffer visiting URI, or nil."
  (when (stringp uri)
    (let ((path (ignore-errors (neo-lean-uri-to-path uri))))
      (when-let* ((buffer (and path (find-buffer-visiting path))))
        (with-current-buffer buffer
          (and (derived-mode-p 'neo-lean-mode) buffer))))))

(cl-defmethod eglot-client-capabilities :around (_server)
  "Advertise Lean-specific client capabilities to Lean."
  (let ((capabilities (cl-call-next-method)))
    (if (derived-mode-p 'neo-lean-mode)
        (plist-put capabilities
                   :lean
                   (neo-lean-goals-accomplished--lean-capabilities))
      capabilities)))

(cl-defmethod eglot-handle-notification :around
  (server (_method (eql textDocument/publishDiagnostics))
          &rest args
          &key uri diagnostics &allow-other-keys)
  "Render Lean silent markers and hide silent diagnostics from Eglot."
  (if-let* ((buffer (neo-lean-goals-accomplished--buffer-for-uri uri)))
      (let ((filtered (neo-lean--filter-silent-diagnostics diagnostics)))
        (with-current-buffer buffer
          (neo-lean-goals-accomplished--redraw diagnostics))
        (prog1
            (if (plist-member args :version)
                (cl-call-next-method
                 server 'textDocument/publishDiagnostics
                 :uri uri
                 :version (plist-get args :version)
                 :diagnostics filtered)
              (cl-call-next-method
               server 'textDocument/publishDiagnostics
               :uri uri
               :diagnostics filtered))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (run-hook-with-args
               'neo-lean-diagnostics-updated-functions
               uri diagnostics filtered)))))
    (cl-call-next-method)))

(defun neo-lean-goals-accomplished-clear ()
  "Remove completed-proof markers from the current buffer."
  (interactive)
  (neo-lean-goals-accomplished--clear))

(provide 'neo-lean-markers)
;;; neo-lean-markers.el ends here
