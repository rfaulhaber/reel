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
;; Homepage: https://github.com/ryan/reel
;; Package-Requires: ((emacs "25.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Rust-based HTTP library for Emacs Lisp.
;;
;;; Code:

(eval-when-compile
  (let* ((env (if (string= (getenv "REEL_ENV") "release") "release" "debug"))
         (system (pcase system-type
                   ('gnu/linux "so")
                   ('darwin "dylib")
                   ('windows-nt "dll")))
         (path (format "./target/%s/libreel.%s" env system)))
    (message "loading %s" path)
    (module-load path)))

(cl-defstruct reel-response
  "An HTTP response."
  (status nil :read-only t)
  (headers nil :read-only t)
  (body nil :read-only t))

;;;###autoload
(cl-defun reel (url &key method headers body)
  ;; TODO
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
