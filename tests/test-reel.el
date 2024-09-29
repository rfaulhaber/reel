;;; test-reel.el --- Buttercup tests for reel  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'reel)

(defconst test-endpoint "http://127.0.0.1:8080/anything")

(describe "reel"
  (before-all
    (reel-test-setup))

  (after-all
    (reel-test-teardown))

  (describe "reel fn"
    (it "makes basic calls with default arguments"
      (let ((result (reel (format "%s/foo/bar" test-endpoint))))
        (expect result :to-be-truthy)
        (with-slots (status body) result
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (message "json body %s" (hash-table-keys json-body))
            (expect (gethash "url" json-body) :to-equal (format "%s/foo/bar" test-endpoint))))))))

(defun reel-test-setup ()
  (message "starting docker")
  (make-process
   :name "reel test"
   :buffer (get-buffer-create "*reel test*")
   :command '("docker" "run" "-it" "--rm" "--name reel-test" "-p 8080:80" "kennethreitz/httpbin")))

(defun reel-test-teardown()
  (message "stopping docker")
  (call-process "docker" nil t nil "stop" "reel-test"))

;;; test-reel.el ends here
