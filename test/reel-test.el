;;; reel-test.el --- Tests for reel

(module-load "./target/debug/libreel.so")

(ert-deftest module-loads ()
  (should (featurep 'reel-dyn))
  (fboundp 'reel-dyn-execute-request))

(ert-deftest basic-callouts-work()
  (reel-dyn-execute-request '("https://httpbin.org/anything" "GET" nil nil)))

;;; reel-test.el ends here
