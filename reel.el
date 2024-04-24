;;; reel.el --- Rust-based HTTP library for Emacs Lisp. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: January 25, 2023
;; Modified: January 25, 2023
;; Version: 0.1.0
;; Keywords: lisp
;; Homepage: https://github.com/rfaulhaber/reel
;; Package-Requires: ((emacs "25.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Rust-based HTTP library for Emacs Lisp.
;;
;;; Code:

(require 'cl-lib)

(unless (functionp 'module-load)
  (error "Dynamic module feature not available, please compile Emacs --with-modules option turned on"))

;; (eval-and-compile
;;   (require 'reel-dyn))

(eval-when-compile
  (let* ((env (if (string= (getenv "REEL_ENV") "release") "release" "debug"))
         (system (pcase system-type
                   ('gnu/linux "so")
                   ('darwin "dylib")
                   ('windows-nt "dll")))
         (path (format "./target/%s/libreel.%s" env system)))
    (message "loading %s" path)
    (module-load (expand-file-name path))
    (message "feature? %s" (featurep 'reel-dyn))))

(defconst reel-http-methods '(GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE PATCH)
  "Valid HTTP methods.")

;; (cl-defstruct (reel-request
;;                (:constructor reel-make-request)
;;                (:copier nil))
;;   "An HTTP request."
;;   (method nil :read-only t)
;;   (headers nil :read-only t)
;;   (mode nil :read-only t)
;;   (cache nil :read-only t)
;;   (credentials nil :read-only t)
;;   (redirect nil :read-only t)
;;   (referrer-policy nil :read-only t)
;;   (body nil :read-only t))

(cl-defstruct (reel-response
               (:constructor reel-make-response)
               (:copier nil))
  "An HTTP response."
  (status nil :read-only t)
  (headers nil :read-only t)
  (body nil :read-only t))

(cl-defstruct (reel-client
               (:constructor reel--make-client)
               (:copier nil))
  "Client object for making multiple requests."
  user-agent use-cookies default-headers timeout connect-timeout proxy --internal-ref)

;; TODO create FormData

;;;###autoload
(cl-defun reel (url &key method headers body mode credentials cache redirect referrer-policy integrity keepalive)
  "Make an HTTP request with URL.
The key arguments will adjust the behavior of the request.

METHOD is a string and one of: GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE
PATCH

HEADERS is an alist of header/value pairs. E.g. `\'((Content-Type .
application/json))'. Keys and values are treated as strings.

BODY is the string representation of the request body.

`reel' is asynchronous, and will not block under any circumstance. This function
also does not accept callbacks. In order to block this request, wrap the call in
`reel-await'.

"
  (reel-dyn-execute-request url (if (null method)
                                    "GET"
                                  (symbol-name method))
                            headers body))

(cl-defun reel-make-client (&key user-agent use-cookies default-headers timeout connect-timeout proxy &allow-other-keys)
  )

(defun reel-format-query-parameters (url query-params)
  ;; TODO
  )

;; (reel-server
;;  :port 8000
;;  :endpoints '(("/foo" . (lambda (request) "hello"))))

(cl-defun reel-server (&key port responses)
  ;; TODO
  )

(provide 'reel)
;;; reel.el ends here
