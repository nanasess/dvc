;;; dvc-lisp.el --- DVC lisp helper functions

;; Copyright (C) 2003-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>
;;    Michael Olson <mwolson@gnu.org>

;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Helper functions unrelated from GNU Arch.

;;; History:
;;
;; Created in May 2005 by Matthieu Moy
;;

(eval-and-compile
  (defvar dvc-gensym-counter 0)

  (defun dvc-gensym (&optional arg)
    "Generate a new uninterned symbol.
    The name is made by appending a number to PREFIX, default
\"dvc\"."
    (let* ((prefix (if (stringp arg) arg "dvc-gensym-uniq-"))
           (num (if (integerp arg) arg
                  (prog1
                      dvc-gensym-counter
                    (setq dvc-gensym-counter (1+
                                               dvc-gensym-counter)))))
           (symbol (make-symbol (format "%s%d" prefix num))))
      (eval `(defvar ,symbol nil "lint trap"))
      symbol))


  (defun dvc-capturing-lambda-helper (l)
    (cond ((atom l) l)
          ((eq (car l) 'capture)
           (let ((sym (cadr l)))
             (unless (symbolp sym)
               (error "Expected a symbol in capture statement: %S" sym))
             (let ((g (car (rassq sym captured-values))))
               (unless g
                 (setq g (dvc-gensym))
                 (push (cons g sym) captured-values))
               g)))
          (t (mapcar 'dvc-capturing-lambda-helper l))))

  (defmacro dvc-capturing-lambda (args &rest body)
    "A `lambda' macro which is capable of capturing symbol values from its
defining environment.
    Symbols to be captured should be surrounded by (capture ...).
    For example:

      (let* ((x 'lexical-x)
             (y 'lexical-y)
             (l (dvc-capturing-lambda (arg)
                  (list x (capture y) arg))))
        (let ((y 'dynamic-y)
              (x 'dynamic-x))
          (funcall l 'dummy-arg)))

    => (dynamic-x lexical-y dummy-arg)"
    (let* ((captured-values nil)
           (body (dvc-capturing-lambda-helper body)))
      `(list 'lambda ',args
             (list 'let (list
                         ,@(mapcar (lambda (var)
                                     `(list ',(car var)
                                            (list 'quote ,(cdr var))))
                                   captured-values))
                   (list 'funcall (lambda () . ,body))))))
  (put 'dvc-capturing-lambda 'lisp-indent-function 1)
  (put 'dvc-capturing-lambda 'edebug-form-spec '(sexp body))

  )

(defun dvc-lexical-let-perform-replacement-in-source ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "`(lambda" nil t)
    (search-backward "(")
    (save-excursion (forward-sexp 1) (insert ")"))
    (backward-delete-char 1)
    (insert "(lexical-let ")
    (search-backward "(lex")
    (let ((beginning (point))
          (letlist "")
          (namelist nil))
      (forward-sexp 1)
      (save-restriction
        (narrow-to-region beginning (point))
        (goto-char (point-min))
        (while (search-forward "," nil t)
          (backward-delete-char 1)
          (let* ((beg (point))
                 (end (progn (forward-sexp 1) (point)))
                 (name (buffer-substring-no-properties beg end))
                 (var (concat (replace-regexp-in-string "[^a-zA-Z\\-]" "-"
                                                        name) "-lex")))
            (when (not (member name namelist))
              (push name namelist)
              (setq letlist (concat
                             letlist (when (not (string= letlist ""))
                                       " ")
                             "(" var " "
                             name
                             ")")))
            (delete-region beg end)
            (goto-char beg)
            (insert var)
            ))
        (goto-char (point-min))
        (search-forward "(lexical-let ")
        (insert "(" letlist ")")
        (newline-and-indent)))))

(defun dvc-capturing-lambda-perform-replacement-in-source ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "`(lambda" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "(dvc-capturing-lambda")
    (search-backward "(")
    (let ((beginning (point)))
      (forward-sexp 1)
      (save-restriction
        (narrow-to-region beginning (point))
        (goto-char (point-min))
        (while (search-forward "," nil t)
          (backward-delete-char 1)
          (insert "(capture ")
          (forward-sexp 1)
          (insert ")"))))))

(provide 'dvc-lisp)
