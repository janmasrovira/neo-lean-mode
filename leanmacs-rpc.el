;;; leanmacs-rpc.el --- Interactive RPC with the Lean server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira

;; Author: Jan Mas Rovira <janmasrovira@gmail.com>
;; Keywords: languages, lean
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Low-level interactive RPC with the Lean language server, the layer that
;; powers a real infoview (clickable goals, widgets, hovers).  It is a port
;; of lean.nvim's `lua/lean/rpc.lua' onto Emacs' `jsonrpc.el' (the library
;; Eglot is built on).
;;
;; The model: for each open document we establish one RPC *session* via
;; `$/lean/rpc/connect', keep it alive with periodic `$/lean/rpc/keepAlive'
;; notifications, and issue requests with `$/lean/rpc/call'.  A session is
;; bound to a document URI; individual calls additionally carry a text
;; position.  `leanmacs-rpc-open' returns a position-bound handle and
;; `leanmacs-rpc-subsession-call' dispatches a method on it.
;;
;; This milestone deliberately omits two things from the Lua original, noted
;; inline below: releasing `RpcRef's via finalizers (not doing so only leaks
;; a little server memory) and the multi-attempt reconnect wrapper.  A single
;; transparent reconnect on a dead session is implemented.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'eglot)

;;;; Error codes

;; Lean LSP/JSON-RPC error codes that mean the RPC session is dead and a new
;; `$/lean/rpc/connect' is required.  See Lean/Data/JsonRpc.lean.
(defconst leanmacs-rpc--needs-reconnect -32900)
(defconst leanmacs-rpc--content-modified -32801)
(defconst leanmacs-rpc--worker-exited -32901)
(defconst leanmacs-rpc--worker-crashed -32902)

(defun leanmacs-rpc--dead-code-p (code)
  "Return non-nil if CODE indicates the RPC session is dead."
  (and (integerp code)
       (memq code (list leanmacs-rpc--needs-reconnect
                        leanmacs-rpc--content-modified
                        leanmacs-rpc--worker-exited
                        leanmacs-rpc--worker-crashed))))

;;;; Keep-alive period

;; `Lean.Server.FileWorker.Utils' expects a keep-alive at least every 30s, so
;; we send one a comfortable margin under that.
(defconst leanmacs-rpc--keepalive-seconds 20)

;;;; Session

(cl-defstruct (leanmacs-rpc-session (:constructor leanmacs-rpc--session-create))
  "An interactive RPC session bound to a document URI."
  connection                  ; the jsonrpc/eglot connection
  uri                         ; the document URI string
  ref-key                     ; "p" (wire v0) or "__rpcref" (wire v1)
  session-id                  ; integer session id from the server
  (connected nil)             ; t once the connect handshake has resolved
  connect-error               ; non-nil error if the handshake failed
  keepalive-timer
  (pending nil)               ; thunks queued until `connected'
  (closed nil))               ; t once the session must not be used

;; URI string -> `leanmacs-rpc-session'.
(defvar leanmacs-rpc--sessions (make-hash-table :test #'equal)
  "Registry of live RPC sessions, keyed by document URI.")

(defun leanmacs-rpc--ref-key (connection)
  "Determine the JSON key used for RPC references on CONNECTION.
Wire format v1 uses \"__rpcref\"; v0 (the default) uses \"p\".  This is
read from the server's `experimental.rpcProvider.rpcWireFormat'
capability."
  (let* ((caps (eglot--capabilities connection))
         (provider (plist-get (plist-get caps :experimental) :rpcProvider))
         (wire (plist-get provider :rpcWireFormat)))
    (if (equal wire "v1") "__rpcref" "p")))

(defun leanmacs-rpc--flush-pending (session)
  "Run and clear the queued thunks on SESSION."
  (let ((thunks (nreverse (leanmacs-rpc-session-pending session))))
    (setf (leanmacs-rpc-session-pending session) nil)
    (dolist (thunk thunks)
      (funcall thunk))))

(defun leanmacs-rpc--start-keepalive (session)
  "Start the periodic keep-alive notifications for SESSION."
  (setf (leanmacs-rpc-session-keepalive-timer session)
        (run-with-timer
         leanmacs-rpc--keepalive-seconds leanmacs-rpc--keepalive-seconds
         (lambda ()
           (unless (or (leanmacs-rpc-session-closed session)
                       (null (leanmacs-rpc-session-session-id session)))
             (ignore-errors
               (jsonrpc-notify
                (leanmacs-rpc-session-connection session)
                :$/lean/rpc/keepAlive
                (list :uri (leanmacs-rpc-session-uri session)
                      :sessionId (leanmacs-rpc-session-session-id session)))))))))

(defun leanmacs-rpc--close (session)
  "Mark SESSION closed and stop its keep-alive timer.
Does not release outstanding `RpcRef's (a later milestone)."
  (setf (leanmacs-rpc-session-closed session) t)
  (when (timerp (leanmacs-rpc-session-keepalive-timer session))
    (cancel-timer (leanmacs-rpc-session-keepalive-timer session))
    (setf (leanmacs-rpc-session-keepalive-timer session) nil)))

(defun leanmacs-rpc--connect (connection uri)
  "Open a new RPC session on CONNECTION for document URI.
Register it immediately (so callers can queue work) and resolve the
handshake asynchronously."
  (let ((session (leanmacs-rpc--session-create
                  :connection connection
                  :uri uri
                  :ref-key (leanmacs-rpc--ref-key connection))))
    (puthash uri session leanmacs-rpc--sessions)
    (jsonrpc-async-request
     connection :$/lean/rpc/connect (list :uri uri)
     :success-fn
     (lambda (result)
       (setf (leanmacs-rpc-session-session-id session) (plist-get result :sessionId)
             (leanmacs-rpc-session-connected session) t)
       (leanmacs-rpc--start-keepalive session)
       (leanmacs-rpc--flush-pending session))
     :error-fn
     (lambda (err)
       (setf (leanmacs-rpc-session-connect-error session) err
             (leanmacs-rpc-session-connected session) t)
       (leanmacs-rpc--close session)
       (leanmacs-rpc--flush-pending session)))
    session))

(defun leanmacs-rpc--session-for (connection uri)
  "Return a live session for URI on CONNECTION, connecting if needed."
  (let ((existing (gethash uri leanmacs-rpc--sessions)))
    (if (and existing
             (not (leanmacs-rpc-session-closed existing))
             (not (leanmacs-rpc-session-connect-error existing)))
        existing
      (leanmacs-rpc--connect connection uri))))

(defun leanmacs-rpc--do-call (session pos method params success error)
  "Issue `$/lean/rpc/call' on SESSION; the connect handshake has resolved.
POS is an LSP text-document-position plist.  METHOD/PARAMS are the Lean
RPC method name and its parameters.  SUCCESS and ERROR are callbacks."
  (cond
   ((leanmacs-rpc-session-connect-error session)
    (funcall error (leanmacs-rpc-session-connect-error session)))
   ((leanmacs-rpc-session-closed session)
    (funcall error (list :code leanmacs-rpc--needs-reconnect
                         :message "RPC session is closed")))
   (t
    (jsonrpc-async-request
     (leanmacs-rpc-session-connection session)
     :$/lean/rpc/call
     (append pos (list :sessionId (leanmacs-rpc-session-session-id session)
                       :method method
                       :params params))
     :success-fn success
     :error-fn
     (lambda (err)
       (when (leanmacs-rpc--dead-code-p (plist-get err :code))
         (leanmacs-rpc--close session))
       (funcall error err))))))

;;;; Public API

;;;###autoload
(defun leanmacs-rpc-position-params ()
  "Return the LSP text-document-position params at point.
A plist of the form (:textDocument (:uri URI) :position (:line L
:character C)), where C is the correct UTF-16 offset."
  (eglot--TextDocumentPositionParams))

(cl-defstruct (leanmacs-rpc-subsession (:constructor leanmacs-rpc--subsession-create))
  "A position-bound handle for issuing RPC calls."
  session                     ; the underlying `leanmacs-rpc-session'
  pos)                        ; the bound text-document-position plist

;;;###autoload
(defun leanmacs-rpc-open (pos)
  "Open (or reuse) an RPC session for POS and return a subsession handle.
POS is an LSP text-document-position plist, e.g. from
`leanmacs-rpc-position-params'.  Requires an active Eglot server in the
current buffer."
  (let* ((connection (eglot--current-server-or-lose))
         (uri (plist-get (plist-get pos :textDocument) :uri))
         (session (leanmacs-rpc--session-for connection uri)))
    (leanmacs-rpc--subsession-create :session session :pos pos)))

(defun leanmacs-rpc-subsession-call (subsession method params success error)
  "Call Lean RPC METHOD with PARAMS on SUBSESSION.
SUCCESS and ERROR are callbacks receiving the result plist or an error
plist respectively.  If the session is mid-handshake the call is queued
until it resolves.  On a dead session, reconnect once transparently."
  (let* ((session (leanmacs-rpc-subsession-session subsession))
         (pos (leanmacs-rpc-subsession-pos subsession))
         (retried nil)
         (on-error nil)
         (run nil))
    (setq on-error
          (lambda (err)
            ;; One transparent reconnect on a dead session.
            (if (and (not retried)
                     (leanmacs-rpc--dead-code-p (plist-get err :code)))
                (let* ((connection (leanmacs-rpc-session-connection session))
                       (uri (leanmacs-rpc-session-uri session)))
                  (setq retried t)
                  (remhash uri leanmacs-rpc--sessions)
                  (setq session (leanmacs-rpc--session-for connection uri))
                  (setf (leanmacs-rpc-subsession-session subsession) session)
                  (funcall run))
              (funcall error err))))
    (setq run
          (lambda ()
            (if (leanmacs-rpc-session-connected session)
                (leanmacs-rpc--do-call session pos method params success on-error)
              (push (lambda ()
                      (leanmacs-rpc--do-call session pos method params success on-error))
                    (leanmacs-rpc-session-pending session)))))
    (funcall run)))

;;;; Convenience wrappers for specific Lean RPC methods

(defun leanmacs-rpc-get-interactive-goals (subsession success error)
  "Request the interactive goals on SUBSESSION.
Calls `Lean.Widget.getInteractiveGoals'."
  (leanmacs-rpc-subsession-call
   subsession "Lean.Widget.getInteractiveGoals"
   (leanmacs-rpc-subsession-pos subsession) success error))

(defun leanmacs-rpc-get-interactive-term-goal (subsession success error)
  "Request the interactive term goal on SUBSESSION.
Calls `Lean.Widget.getInteractiveTermGoal'."
  (leanmacs-rpc-subsession-call
   subsession "Lean.Widget.getInteractiveTermGoal"
   (leanmacs-rpc-subsession-pos subsession) success error))

(defun leanmacs-rpc-get-go-to-location (subsession kind info success error)
  "Request go-to locations for INFO on SUBSESSION.
KIND is one of the strings \"definition\", \"declaration\" or \"type\".
INFO is the `InfoWithCtx' handle taken from a subexpression's tag (the
`leanmacs-info' text property).  Calls `Lean.Widget.getGoToLocation' and
yields a vector of LSP `LocationLink's."
  (leanmacs-rpc-subsession-call
   subsession "Lean.Widget.getGoToLocation"
   (list :kind kind :info info) success error))

(defun leanmacs-rpc-info-to-interactive (subsession info success error)
  "Request the info popup for INFO on SUBSESSION.
INFO is the `InfoWithCtx' handle taken from a subexpression's tag (the
`leanmacs-info' text property).  Calls
`Lean.Widget.InteractiveDiagnostics.infoToInteractive' and yields an
`InfoPopup' plist with optional :exprExplicit, :type and :doc."
  (leanmacs-rpc-subsession-call
   subsession "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
   info success error))

(provide 'leanmacs-rpc)
;;; leanmacs-rpc.el ends here
