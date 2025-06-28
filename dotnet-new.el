;;; dotnet-new.el --- Completion for the dotnet cli "new" command -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Austin Kearns
;;
;; Author: Austin Kearns <aus@dark>
;; Maintainer: Austin Kearns <aus@dark>
;; Created: January 25, 2025
;; Modified: January 25, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lillenne/dotnet-new
;; Package-Requires: ((emacs "29.1") (s "1.10") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Completion for the dotnet cli "new" command
;;
;;; Code:

;; https://github.com/positron-solutions/transient-showcase?tab=readme-ov-file#Prefixes-and-Suffixes

(require 'transient)
(require 's)
(require 'dash)

(defvar dotnet-new-max-chars 160 "Max chars for argument descriptions.")
(defvar dotnet-new-extra-line-padding 10
  "The number of spaces to insert for extra description lines.
Used when arguments descriptions are longer than `dotnet-new-max-chars'.")

(defun dotnet-new--get-candidates ()
  (--filter (not (s-blank? it))
            (mapcar (lambda (line) (let ((part (nth 1 (split-string line "\\s-\\{2,\\}"))))
                                     (if (and part (s-contains? "," part))
                                         (car (s-split "," part))
                                       part)))
                    (cddddr (s-lines (shell-command-to-string "dotnet new list"))))))

(defun dotnet-new--get-help-command (selected)
  (shell-command-to-string  (concat (s-concat "dotnet new " selected) " --help")))

(cl-defstruct (dotnet-arg (:constructor dotnet-arg-create)
                          (:copier dotnet-arg-copy))
  arg short long desc type default choices choice-descriptions)

(defvar dotnet-new--selected "The last selected dotnet template.")

(defun dotnet-arg--get-shortcut (arg &optional count)
  (let ((shortcut (car (s-match (concat "-[^-]\\{" (if count (if (stringp count) count (number-to-string count)) "1") "\\}") (dotnet-arg-short arg)))))
    (if shortcut
        shortcut
      (dotnet-arg-long arg))))

(defun dotnet-new--parse-help (STRING)
  (unless (s-contains? "No options" STRING t)
    (with-temp-buffer (insert STRING)
                      (goto-char (point-min))
                      (search-forward "Template options:")
                      (let ((matches '())
                            (current (dotnet-arg-create)))
                        (while-let ((point (re-search-forward "^[[:blank:]]*\\([[:blank:]]-+[^,^[:blank:]]+\\)" nil t)))
                          (setf (dotnet-arg-short current) (s-trim-left (match-string 1)))
                          (when-let ((long-match (s-match "\\(--+[^,^[:blank:]]+\\)" (thing-at-point 'line))))
                            (setf (dotnet-arg-long current) (nth 1 long-match)))
                          (re-search-forward "[[:space:]][[:space:]]+") ; columns are separated by 2+ spaces
                          (let* ((begin (point))
                                 (type-pos (search-forward "Type: "))
                                 (type (buffer-substring-no-properties (point) (line-end-position)))
                                 (end (line-beginning-position))
                                 (desc (buffer-substring-no-properties begin end))
                                 (trimmed (s-join " " (--map (s-trim it) (--filter (not (s-blank? it)) (s-lines desc)))))
                                 (trimmed (if (> (length trimmed) dotnet-new-max-chars)
                                              (let ((words (split-string trimmed))
                                                    (lines '())
                                                    (current-line ""))
                                                (dolist (word words)
                                                  (if (> (+ (length current-line) (length word) 1) dotnet-new-max-chars)
                                                      (progn
                                                        (push current-line lines)
                                                        (setq current-line word))
                                                    (setq current-line (if (string-empty-p current-line)
                                                                           word
                                                                         (concat current-line " " word)))))
                                                (when (not (string-empty-p current-line))
                                                  (push (concat (make-string dotnet-new-extra-line-padding ?\s) current-line) lines))
                                                (string-join (reverse lines) "\n"))
                                            trimmed))
                                 (default-pos (search-forward "Default: "))
                                 (default-line-begin-pos (line-beginning-position))
                                 (default (buffer-substring-no-properties default-pos (line-end-position))))
                            (setf (dotnet-arg-desc current) trimmed)
                            (setf (dotnet-arg-type current) type)
                            (setf (dotnet-arg-default current) default)
                            (when (string= "choice" type)
                              (goto-char type-pos)
                              (forward-line)
                              (let ((choices '())
                                    (descriptions '()))
                                (while (< (point) default-line-begin-pos)
                                  (re-search-forward "[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]+\\(.*\\)")
                                  (unless (> (point) default-line-begin-pos)
                                    (push (match-string 1) choices)
                                    (push (match-string 2) descriptions)))
                                (goto-char default-pos)
                                (setf (dotnet-arg-choices current) choices)
                                (setf (dotnet-arg-choice-descriptions current) descriptions))))
                          (push current matches)
                          (setq current (dotnet-arg-create)))
                        matches))))

(defun dotnet-arg--create-transient-arg (arg)
  (let ((long-arg (concat (dotnet-arg-long arg) (unless (string= "bool" (dotnet-arg-type arg)) "="))))
    (if (dotnet-arg-choices arg)
        (transient-parse-suffix
         'transient--prefix
         `(,(dotnet-arg-arg arg) ,(dotnet-arg-desc arg) ,long-arg :choices ,(dotnet-arg-choices arg)))
      (transient-parse-suffix
       'transient--prefix
       `(,(dotnet-arg-arg arg) ,(dotnet-arg-desc arg) ,long-arg)))))

(defun dotnet-arg--create-transient-args (dotnet-args)
  (dolist (arg dotnet-args)
    (setf (dotnet-arg-arg arg) (dotnet-arg--get-shortcut arg)))
  (dotimes (i (length dotnet-args))
    (dotimes (j (length dotnet-args))
      (let ((ia (nth i dotnet-args))
            (ja (nth j dotnet-args)))
        (unless (eql i j)
          (while (or (s-contains? (dotnet-arg-arg ia) (dotnet-arg-arg ja))
                     (s-contains? (dotnet-arg-arg ja) (dotnet-arg-arg ia)))
            (let ((l (+ 1 (max (length (dotnet-arg-arg ja)) (length (dotnet-arg-arg ia))))))
              (setf (dotnet-arg-arg ia) (dotnet-arg--get-shortcut ia l) (dotnet-arg-arg ja) (dotnet-arg--get-shortcut ja l))))))))
  (mapcar #'dotnet-arg--create-transient-arg dotnet-args))

(defun dotnet-new--invoke (&optional transient-params)
  "Run command with `TRANSIENT-PARAMS' from the last `dotnet-new-transient'."
  (interactive
   (list (transient-args 'dotnet-new-transient)))
  (shell-command (shell-quote-argument (s-concat "dotnet new " dotnet-new--selected " " (s-join " " transient-params)))))

(transient-define-prefix dotnet-new-transient ()
  "Transient CLI dispatcher for the last template selected with `dotnet-new'."
  [])

;;;###autoload
(defun dotnet-new-dispatch ()
  "Select a ~dotnet new~ template and invoke a transient interface for it."
  (interactive)
  (when-let* ((candidates (dotnet-new--get-candidates))
              (selected (completing-read "Which template? " candidates nil t))
              (continue (not (s-blank? selected))))
    (let* ((cmd-help (dotnet-new--get-help-command selected))
           (dotnet-args (dotnet-new--parse-help cmd-help)))
      (setq dotnet-new--selected selected)
      (transient-define-prefix dotnet-new-transient ()
        "Transient CLI dispatcher for the last template selected with M-x `dotnet-new'"
        ["Selected Template:"
         (:info (lambda () (upcase selected)))]
        ["Common Arguments"
         ("n" "Name" "--name=")
         ("o" "Output" "--output=")
         ("d" "Dry run" "--dry-run")
         ("f" "Force" "--force")
         ("u" "No update check" "--no-update-check")
         ("p" "Project" "--project=")
         ("l" "Language" "--language" :choices '("C#" "F#" "VB"))
         ("t" "Type" "--type=")]
        ["Template Arguments"
         :class transient-column
         :setup-children (lambda (_) (dotnet-arg--create-transient-args dotnet-args))]
        ["Finalize"
         ("e" "Execute" dotnet-new--invoke)
         ("q" "Quit" transient-quit-all)]))
    (dotnet-new-transient)))

(provide 'dotnet-new)
;;; dotnet-new.el ends here
