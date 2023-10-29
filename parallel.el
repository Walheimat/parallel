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

(defcustom parallel-separator "||"
  "Separator between the two `parallel' function names."
  :group 'parallel
  :type 'string)

(defcustom parallel-naming-function 'parallel--normalize
  "The naming function to use."
  :group 'parallel
  :type '(choice (const :tag "Normalize" parallel--normalize)
                 (const :tag "Concatenate" parallel--concatenate)))

(defcustom parallel-custom-namespace nil
  "The namespace used when defining your own functions.

If this is set, the custom namespace prefix is ignored."
  :group 'parallel
  :type 'string)

;;; -- Naming

(defun parallel--maybe-truncate-custom-namespace (str)
  "If STR is prefixed by the custom namespace, truncate left."
  (if (and parallel-custom-namespace
           (string-prefix-p parallel-custom-namespace str))
      (string-trim str parallel-custom-namespace)
    str))

(defun parallel--common-prefix (s1 s2)
  "Return the common prefix for S1 and S2."
  (let ((index 0)
        (s1-length (length s1))
        (s2-length (length s2)))

    (while (and (< index s1-length)
                (< index s2-length)
                (eq (aref s1 index)
                    (aref s2 index)))
      (setq index (1+ index)))

    (when (or (eq index s1-length)
              (eq index s2-length))
      (setq index (1- index))

      (while (and (> index 0)
                  (not (eq ?- (aref s1 index))))
        (setq index (1- index)))

      (setq index (1+ index)))

    index))

(defun parallel--name (fun)
  "Get the name of FUN."
  (parallel--maybe-truncate-custom-namespace
   (symbol-name fun)))

(defun parallel--normalize (a b)
  "Normalize functions A and B.

This will make sure that prefixes used for both functions aren't
repeated. If the functions don't share a prefix, this defers to
`partial-recall--concatenate'."
  (let* ((name-a (parallel--name a))
         (name-b (parallel--name b))
         (prefix (parallel--common-prefix name-a name-b)))

    (if (> prefix 0)
        (intern (concat name-a parallel-separator (substring name-b prefix)))
      (parallel--concatenate a b))))

(defun parallel--concatenate (a b)
  "Concatenate functions A and B."
  (intern (concat (parallel--name a) parallel-separator (parallel--name b))))

;;; -- Core

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

    `(defun ,(funcall parallel-naming-function a b) (&optional arg)
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
