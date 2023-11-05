;;; parallel.el --- Parallel commands -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/parallel
;; Version: 0.2.0
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

(cl-defmacro parallel--parallelize (a b &key universalize name)
  "Define a function composing A and B.

Both functions are called interactively.

By default, A is called. B will be called if the prefix argument
is numeric. This allows both commands to consume the prefix.

If UNIVERSALIZE is t, the prefix argument is set to mimic the
`universal-argument' for B.

NAME can be a symbol to use for defining the function if you
don't want it to be automatically named."
  (declare (indent defun))

  (let ((a-name (symbol-name a))
        (b-name (symbol-name b)))

    `(defun ,(or name (funcall parallel-naming-function a b)) (&optional arg)
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

;; TODO: It might be easier just to check if it accepts any args and
;;       then use `apply'.
(defun parallel-mirror--derive-signature (a)
  "Derive signature and arguments for A."
  (let* ((arity (func-arity a))
         (min (car-safe arity))
         (max (cdr-safe arity))

         (inc 96)
         (get-arg-name (lambda () (setq inc (1+ inc)) (intern (char-to-string inc))))
         (args)
         (sig))

    (cond
     ((and (eq min max)
           (> min 0))

      (dotimes (_ min)
        (push (funcall get-arg-name) args))

      (setq args (reverse args)
            sig args))

     ((and (numberp max)
           (> max min))

      (let ((required nil)
            (optional nil))

        (dotimes (_ min)
          (let ((new-arg (funcall get-arg-name)))
            (push new-arg required)
            (push new-arg args)))

        (setq required (reverse required))

        (dotimes (_ (- max min))
          (let ((new-arg (funcall get-arg-name)))
            (push new-arg optional)
            (push new-arg args)))

        (setq optional (reverse optional))

        (setq args (reverse args))
        (setq sig `(,@required &optional ,@optional))))

     ((equal max 'many)

      (dotimes (_ min)
        (push (funcall get-arg-name) args))

      (setq sig `(,@(reverse args) &rest r)
            args `(,@args r))))

    (cons sig args)))

(cl-defmacro parallel-mirror--mirror (a &key type)
  "Mirror function A.

TYPE is the way mirroring should be done. Currently this needs to
be `boolean' to invert."
  (pcase type
    ('boolean
     (let ((args (parallel-mirror--derive-signature a)))

       `(defun ,(intern (format "parallel-mirror-%s" a)) (,@(car args))
          ,(format "Inverts function `%s'." a)
          (not (,a ,@(cdr args))))))))

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

;;;###autoload
(cl-defmacro parallel-mirror (a &rest args)
  "Mirror function A by reversing what it does.

See `parallel-mirror--mirror' for the options in ARGS."
  (declare (indent defun))
  `(progn
     (parallel-mirror--mirror ,a ,@args)))

(provide 'parallel)

;;; parallel.el ends here
