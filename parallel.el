;;; parallel.el --- Parallel commands -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/parallel
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; `parallel' allows you to create a command comprising two commands.
;; The first function, the default, consumes normal prefix arguments,
;; the second requires a numeric argument to activate and consumes
;; numeric prefix arguments.


;;; Code:

(require 'cl-lib)

;;; -- Customization

(defcustom parallel--separator "||"
  "Separator between the two `parallel' function names."
  :group 'parallel
  :type 'string)

;;; -- Private

(cl-defmacro parallel--parallelize (a b &key universalize)
  "Define a function composing A and B.

Both functions are called interactively.

By default, A is called. B will be called if the prefix argument
is numeric. This allows both commands to consume the prefix.

If UNIVERSALIZE is t, the prefix argument is set to mimic the
`universal-argument' for B."
  (declare (indent defun))

  (let ((a-name (symbol-name a))
        (b-name (symbol-name b)))

    `(defun ,(intern (concat a-name parallel--separator b-name)) (&optional arg)
       ,(concat (format "Call `%s' or `%s' depending on prefix argument."
                        a-name
                        b-name)
                "\n"
                "No argument means: call the prior. "
                "Numeric prefix `0' means: call the latter."
                "\n\n"
                "For all other prefix values: numeric prefixes call the latter,\n"
                "`universal-argument' prefixes call the prior"
                (if universalize ".\n\nThis function is universalized." "."))
       (interactive "P")

       (cond
        ((not arg)
         (call-interactively ',a))
        ((equal 0 arg)
         (setq current-prefix-arg nil)
         (prefix-command-update)
         (call-interactively ',b))
        ((equal (prefix-numeric-value arg) arg)
         ,(if (not universalize)
              `(call-interactively ',b)
            `(progn
               (setq current-prefix-arg (list arg))
               (prefix-command-update)
               (call-interactively ',b))))
        (t
         (call-interactively ',a))))))

;;; -- API

;;;###autoload
(cl-defmacro parallel (a b &rest args)
  "Define a function composing A and B.

Both functions are called interactively.

By default, A is called. B will be called if the prefix argument
is numeric. This allows both commands to consume the prefix.

See `parallel--parallelize' for the options in ARGS."
  (declare (indent defun))
  `(progn
     (parallel--parallelize ,a ,b ,@args)))

(provide 'parallel)

;;; parallel.el ends here
