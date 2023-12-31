;;; parallel-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'parallel)

(ert-deftest parallel ()
  (bydi-match-expansion
   (parallel--parallelize some-fun other-fun)
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

(ert-deftest parallel--names ()
  (bydi-match-expansion
   (parallel--parallelize some-fun some-other-fun :name given-name-fun)
   '(defun given-name-fun (&optional arg)
      "Call `some-fun' or `some-other-fun' depending on prefix argument.\nNo argument means: call the prior. Numeric prefix `0' means: call the latter.\n\nFor all other prefix values: numeric prefixes call the latter,\n`universal-argument' prefixes call the prior."
      (interactive "P")
      (cond
       ((not arg)
        (call-interactively 'some-fun))
       ((equal 0 arg)
        (setq current-prefix-arg nil)
        (prefix-command-update)
        (call-interactively 'some-other-fun))
       ((equal
         (prefix-numeric-value arg)
         arg)
        (call-interactively 'some-other-fun))
       (t
        (call-interactively 'some-fun))))))

(ert-deftest parallel--normalizes ()
  (let ((parallel-naming-function 'parallel--normalize))

    (bydi-match-expansion
     (parallel--parallelize some-fun some-other-fun)
     '(defun some-fun||other-fun (&optional arg)
        "Call `some-fun' or `some-other-fun' depending on prefix argument.\nNo argument means: call the prior. Numeric prefix `0' means: call the latter.\n\nFor all other prefix values: numeric prefixes call the latter,\n`universal-argument' prefixes call the prior."
        (interactive "P")
        (cond
         ((not arg)
          (call-interactively 'some-fun))
         ((equal 0 arg)
          (setq current-prefix-arg nil)
          (prefix-command-update)
          (call-interactively 'some-other-fun))
         ((equal
           (prefix-numeric-value arg)
           arg)
          (call-interactively 'some-other-fun))
         (t
          (call-interactively 'some-fun)))))))

(ert-deftest parallel--universalized ()
  (bydi-match-expansion
   (parallel--parallelize some-fun other-fun :universalize t)
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

(ert-deftest parallel--name--truncates ()
  (let ((parallel-custom-namespace "test-"))

    (should (string= "helper" (parallel--name 'test-helper)))))

(ert-deftest parallel--normalize--no-empty-b ()
  (let ((parallel-separator "||"))

    (should (eq 'test-helper-friend||helper
                (parallel--normalize 'test-helper-friend 'test-helper)))))

;;; -- Mirroring.

(ert-deftest parallel-mirror--boolean ()
  (defun mirror-test-flat ()
    "Tests nothing."
    t)

  (bydi-match-expansion
   (parallel-mirror--mirror mirror-test-flat :type boolean)
   '(defun parallel-mirror-mirror-test-flat ()
      "Inverts function `mirror-test-flat'."
      (not (mirror-test-flat))))

  (defun mirror-test (a b)
    "Tests A and B."
    (equal a b))

  (bydi-match-expansion
   (parallel-mirror--mirror mirror-test :type boolean)
   '(defun parallel-mirror-mirror-test (a b)
      "Inverts function `mirror-test'."
      (not (mirror-test a b))))

  (defun mirror-test-optional (a b &optional c)
    "Test A and B and optionally B."
    (or c
        (equal a b)))

  (bydi-match-expansion
   (parallel-mirror--mirror mirror-test-optional :type boolean)
   '(defun parallel-mirror-mirror-test-optional (a b &optional c)
      "Inverts function `mirror-test-optional'."
      (not (mirror-test-optional a b c))))

  (defun mirror-test-many (a &optional b &rest c)
    "Test A and B and optionally B."
    (or c
        (equal a b)))

  (bydi-match-expansion
   (parallel-mirror--mirror mirror-test-many :type boolean)
   '(defun parallel-mirror-mirror-test-many (a &rest r)
      "Inverts function `mirror-test-many'."
      (not (mirror-test-many a r)))))

;;; -- API

(ert-deftest parallel--public ()
  (bydi-match-expansion
   (parallel some-fun other-fun :universalize t)
   '(progn
      (parallel--parallelize some-fun other-fun :universalize t))))

(ert-deftest parallel-mirror--public ()
  (bydi-match-expansion
   (parallel-mirror some-fun :type boolean)
   '(progn
      (parallel-mirror--mirror some-fun :type boolean))))

;;; parallel-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
