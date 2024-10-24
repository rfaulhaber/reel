;;; test-reel.el --- Buttercup tests for reel  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'reel)

(defconst reel-test-endpoint "http://127.0.0.1:8080")
(defconst reel-test-container-name "reel-test")

(describe "reel"
  (describe "reel fn"
    (it "makes basic calls with default arguments"
      (let* ((path "/anything/foo/bar")
             (url (concat reel-test-endpoint path))
             (result (reel url)))
        (expect result :to-be-truthy)
        (let ((status (reel-response-status result))
              (body (reel-response-body result)))
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "url" json-body) :to-equal path)
            (expect (gethash "method" json-body) :to-equal "GET")))))
    (it "makes basic POST calls"
      (let* ((url (concat reel-test-endpoint "/anything"))
             (result
              (reel url
                    :method "POST"
                    :body "hello world")))
        (expect result :to-be-truthy)
        (let ((status (reel-response-status result))
              (body (reel-response-body result)))
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "body" json-body) :to-equal "hello world")
            (expect (gethash "method" json-body) :to-equal "POST")))))
    (it "makes basic calls with headers"
      (let* ((url (concat reel-test-endpoint "/anything"))
             (result
              (reel url
                    :headers '(("header-1" . "value")))))
        (expect result :to-be-truthy)
        (let ((status (reel-response-status result))
              (body (reel-response-body result)))
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "method" json-body) :to-equal "GET")
            (let ((headers (gethash "headers" json-body)))
              (expect (gethash "header-1" headers) :to-equal "value"))))))
    (xit "makes basic calls with form data"
      (let* ((url (concat reel-test-endpoint "/anything"))
             (result
              (reel url
                    :method "POST"
                    :body '(("field-1" . "value")))))
        (expect result :to-be-truthy)
        (let ((status (reel-response-status result))
              (body (reel-response-body result)))
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "method" json-body) :to-equal "POST")
            (let ((form (gethash "form" json-body)))
              (expect (gethash "field-1" form) :to-equal "value"))))))))

;;; test-reel.el ends here
