;;; parallel-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'parallel)

(ert-deftest parallel ()
  (bydi-match-expansion
   (parallel some-fun other-fun)
   '(defun some-fun||other-fun (&optional arg)
     "Call `some-fun' or `other-fun' depending on prefix argument.\nNo argument means: call the prior. Numeric prefix `0' means: call the latter.\n\nFor all other prefix values: numeric prefixes call the latter,\n`universal-argument' prefixes call the prior."
     (interactive "P")
     (cond
      ((not arg)
       (call-interactively 'some-fun))
      ((equal 0 arg)
       (setq current-prefix-arg nil)
       (prefix-command-update)
       (call-interactively 'other-fun))
      ((equal
        (prefix-numeric-value arg)
        arg)
       (call-interactively 'other-fun))
      (t
       (call-interactively 'some-fun))))))

(ert-deftest parallel--universalized ()
  (bydi-match-expansion
   (parallel some-fun other-fun :universalize t)
   '(defun some-fun||other-fun (&optional arg)
     "Call `some-fun' or `other-fun' depending on prefix argument.\nNo argument means: call the prior. Numeric prefix `0' means: call the latter.\n\nFor all other prefix values: numeric prefixes call the latter,\n`universal-argument' prefixes call the prior.\n\nThis function is universalized."
     (interactive "P")
     (cond
      ((not arg)
       (call-interactively 'some-fun))
      ((equal 0 arg)
       (setq current-prefix-arg nil)
       (prefix-command-update)
       (call-interactively 'other-fun))
      ((equal
        (prefix-numeric-value arg)
        arg)
       (progn
         (setq current-prefix-arg
               (list arg))
         (prefix-command-update)
         (call-interactively 'other-fun)))
      (t
       (call-interactively 'some-fun))))))


;;; parallel-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
