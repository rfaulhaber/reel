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
(require 'eieio)

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

(defconst reel-default-user-agent (format "reel/GNU Emacs/%s %s" emacs-version system-type)
  "Default user-agent header.")

(cl-defstruct (reel-request
               (:constructor reel-make-request)
               (:copier nil))
  "An HTTP request."
  (url nil :read-only t)
  (method nil :read-only t)
  (headers nil :read-only t)
  (body nil :read-only t))

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
  "A reusable reel client."
  (ptr nil :read-only t))

;; TODO create FormData

;;;###autoload
(cl-defun reel (url-or-request &key method headers body)
  "Make an HTTP request with URL-OR-REQUEST.
The key arguments will adjust the behavior of the request.

URL-OR-REQUEST can be either a string URL or an instance of a reel-request.

METHOD is a string and one of: GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE
PATCH

HEADERS is an alist of header/value pairs. E.g. `\'((\"Content-Type\" .
\"application/json\"))'. Keys and values must be strings.

BODY is the string representation of the request body.

`reel' is synchronous."
  (let ((client (reel-dyn-make-client)))
    (if (reel-request-p url-or-request)
        (with-slots (url method headers body) url-or-request
          (reel--build-response
           (reel-dyn-make-client-request client url method (reel--make-header-map headers) body)))
      (reel--build-response
       (reel-dyn-make-client-request client url-or-request (if (null method)
                                                               "GET"
                                                             method)
                                     (reel--make-header-map headers) body)))))

(defun reel-make-client ()
  "Returns an instance of a reel client."
  (reel--make-client
   :ptr (reel-dyn-make-client)))

(cl-defun reel-make-client-request (client &key url method headers body)
  "Makes a request using a reel CLIENT."
  (let ((headers (if (assoc "user-agent" headers) headers (push `("user-agent" . ,reel-default-user-agent) headers))))
    (reel--build-response
     (reel-dyn-make-client-request (reel-client-ptr client) url method (reel--make-header-map headers) body))))

(defun reel--make-header-map (headers)
  "Given an alist of HEADERS, converts them into a header map."
  (let ((header-map (reel-dyn-make-header-map)))
    (mapc (lambda (header)
            (reel-dyn-insert-header header-map (car header) (cdr header)))
          headers)
    header-map))

(defun reel--build-response (resp-pointer)
  "Builds a reel-response struct out of RESP-POINTER."
  (reel-make-response
   :status (reel-dyn-get-response-status resp-pointer)
   :headers (reel--get-headers (reel-dyn-get-response-headers resp-pointer))
   :body (reel-dyn-get-response-body resp-pointer)))

(defun reel--get-headers (headers)
  "Pulls header keys and values out of HEADERS and into an alist."
  (let ((keys (reel-dyn-get-header-keys headers)))
    (mapcar (lambda (key)
              (cons key (reel-dyn-get-header headers key)))
            keys)))

(provide 'reel)
;;; reel.el ends here
