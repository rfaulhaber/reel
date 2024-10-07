;;; test-reel.el --- Buttercup tests for reel  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'reel)

;; NOTE these tests expect a local instance of httpbin to be running
;; run:
;; docker run -it --rm --name reel-test -p 8080:80 kennethreitz/httpbin
;; to have these tests run properly!

(defconst reel-test-endpoint "http://127.0.0.1:8080/anything")
(defconst reel-test-container-name "reel-test")

(describe "reel"
  (describe "reel fn"
    (it "makes basic calls with default arguments"
      (let* ((url (format "%s/foo/bar" reel-test-endpoint))
             (result (reel url)))
        (expect result :to-be-truthy)
        (with-slots (status body) result
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "url" json-body) :to-equal url)
            (expect (gethash "method" json-body) :to-equal "GET")))))
    (it "makes basic POST calls"
      (let* ((url (concat reel-test-endpoint "/resource"))
             (result
              (reel url
                    :method "POST"
                    :body "hello world")))
        (expect result :to-be-truthy)
        (with-slots (status body) result
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "data" json-body) :to-equal "hello world")
            (expect (gethash "method" json-body) :to-equal "POST")))))
    (it "makes basic calls with headers"
      (let* ((url (concat reel-test-endpoint "/resource"))
             (result
              (reel url
                    :headers '(("header-1" . "value")))))
        (expect result :to-be-truthy)
        (with-slots (status body) result
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "method" json-body) :to-equal "GET")
            (let ((headers (gethash "headers" json-body)))
              (expect (gethash "Header-1" headers) :to-equal "value"))))))
    (it "makes basic calls with form data"
      (let* ((url (concat reel-test-endpoint "/resource"))
             (result
              (reel url
                    :method "POST"
                    :body '(("field-1" . "value")))))
        (expect result :to-be-truthy)
        (with-slots (status body) result
          (expect status :to-equal 200)
          (let ((json-body (json-parse-string body)))
            (expect (gethash "method" json-body) :to-equal "POST")
            (let ((form (gethash "form" json-body)))
              (expect (gethash "field-1" form) :to-equal "value"))))))))

;;; test-reel.el ends here
