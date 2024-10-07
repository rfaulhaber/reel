;;; reel.el --- Rust-based HTTP library for Emacs Lisp. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: January 25, 2023
;; Modified: October 04, 2023
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

(eval-and-compile
  (defvar reel-dyn--version)
  (let* ((ext (pcase system-type
                ('gnu/linux "so")
                ('darwin "dylib")
                ('windows-nt "dll")))
         (path (format "libreel.%s" ext)))
    ;; we load the rust output depending on the context
    ;; if the module exists as a sibling to this file, we immediately load that
    (if (file-exists-p (expand-file-name path))
        (module-load (expand-file-name path))
      ;; by default, we check the load path. this is how we get eask install to work
      (load path))
    (unless (or reel-dyn--version (featurep 'reel-dyn))
      (error "Dynamic module was not loaded correctly"))))

(require 'reel-dyn)

(defconst reel-http-methods '(GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE PATCH)
  "Valid HTTP methods.")

(defconst reel-default-user-agent (format "reel (GNU Emacs %s %s)/%s" emacs-version system-type reel-dyn--version)
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

;;;###autoload
(cl-defun reel (url-or-request &key method headers body)
  "Make an HTTP request with URL-OR-REQUEST.
The key arguments will adjust the behavior of the request.

URL-OR-REQUEST can be either a string URL or an instance of a reel-request.

METHOD is a string and one of: GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE
PATCH

HEADERS is an alist of header/value pairs. E.g. `\'((\"Content-Type\" .
\"application/json\"))'. Keys and values must be strings.

BODY is either the string representation of the request body or an alist of
key-value pairs for form submission.

`reel' is synchronous."
  (let ((client (reel-dyn-make-client)))
    (if (reel-request-p url-or-request)
        (with-slots (url method headers body) url-or-request
          (reel--make-request client url method headers body))
      (reel--make-request client url-or-request method headers body))))

(defun reel-make-client ()
  "Returns an instance of a reel client."
  (reel--make-client
   :ptr (reel-dyn-make-client)))

(cl-defun reel-make-client-request (client &key url method headers body)
  "Makes a request using a reel CLIENT."
  (reel--make-request client url method headers body))

(defun reel-url-search-parameters (parameters)
  "Given an alist PARAMETERS, converts the alist to a search query string."
  (concat "?"
          (string-join
           (seq-map (lambda (param)
                      (concat (car param) "=" (cdr param)))
                    parameters)
           "&")))

(defun reel--make-request (client url method headers body)
  "Makes a request via reel-dyn using defaults."
  (let* ((client (if (reel-client-p client) (reel-client-ptr client) client))
         (method (or method "GET"))
         (default-headers (if (assoc "user-agent" headers) headers (push `("user-agent" . ,reel-default-user-agent) headers)))
         (headers (reel--make-header-map default-headers)))
    (reel--build-response
     (reel-dyn-make-client-request client url method headers body))))

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
