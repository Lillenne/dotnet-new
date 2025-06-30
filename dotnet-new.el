;;; dotnet-new.el --- Completion for the dotnet cli "new" command -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Austin Kearns
;;
;; Author: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Maintainer: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Created: January 25, 2025
;; Modified: January 25, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lillenne/dotnet-new
;; Package-Requires: ((emacs "29.1") (s "1.10") (transient "0.2.0") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Completion for the dotnet cli "new" command
;;
;;; Code:

(require 'transient)
(require 's)
(require 'dash)

(defvar dotnet-new-max-chars 160 "Max chars for argument descriptions.")
(defvar dotnet-new-extra-line-padding 10
  "The number of spaces to insert for extra description lines.
Used when arguments descriptions are longer than `dotnet-new-max-chars'.")

(defvar dotnet-new--candidates-cache nil "Cached list of dotnet template candidates.")
(defvar dotnet-new--help-cache (make-hash-table :test 'equal) "Cache for parsed help data by template name.")

(defun dotnet-new--get-candidates ()
  "Get list of dotnet template candidates, with caching."
  (or dotnet-new--candidates-cache
      (setq dotnet-new--candidates-cache
            (--filter (not (s-blank? it))
                      (mapcar (lambda (line) (let ((part (nth 1 (split-string line "\\s-\\{2,\\}"))))
                                               (if (and part (s-contains? "," part))
                                                   (car (s-split "," part))
                                                 part)))
                              (cddddr (s-lines (shell-command-to-string "dotnet new list"))))))))

(defun dotnet-new--clear-candidates-cache ()
  "Clear the candidates cache. Useful when templates are added/removed."
  (interactive)
  (setq dotnet-new--candidates-cache nil)
  (message "Dotnet template candidates cache cleared"))

(defun dotnet-new--get-help-command (selected)
  (when (string-blank-p selected) (error "Selected command is blank!"))
  (let ((help-cmd (shell-command-to-string (concat "dotnet new " selected " --help"))))
    (if (string-blank-p help-cmd)
        (error "Failed to get help! (%s)" help-cmd))
    help-cmd))

(cl-defstruct (dotnet-arg (:constructor dotnet-arg-create)
                          (:copier dotnet-arg-copy))
  arg short long desc type default choices choice-descriptions)

(defvar dotnet-new--selected "The last selected dotnet template.")
(defvar dotnet-new--current-args nil "Current parsed arguments for the selected template.")

(defun dotnet-arg--get-shortcut (arg &optional count)
  (let ((shortcut (car (s-match (concat "-[^-]\\{" (if count (if (stringp count) count (number-to-string count)) "1") "\\}") (dotnet-arg-short arg)))))
    (if shortcut
        shortcut
      (dotnet-arg-long arg))))

(defun dotnet-new--parse-help (STRING)
  "Parse help string into dotnet-arg structures."
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
                                 (before-default-pos (point))
                                 (default-pos (search-forward "Default: " (point-max) t))
                                 (default-line-begin-pos (when default-pos (line-beginning-position)))
                                 (default (when default-pos (buffer-substring-no-properties default-pos (line-end-position)))))
                            (unless default-pos (goto-char before-default-pos))
                            (setf (dotnet-arg-desc current) trimmed)
                            (setf (dotnet-arg-type current) type)
                            (when default (setf (dotnet-arg-default current) default))
                            (when (string= "choice" type)
                              (goto-char type-pos)
                              (forward-line)
                              (let ((choices '())
                                    (descriptions '()))
                                (while (and default-line-begin-pos (< (point) default-line-begin-pos))
                                  (re-search-forward "[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]+\\(.*\\)")
                                  (unless (> (point) default-line-begin-pos)
                                    (push (match-string 1) choices)
                                    (push (match-string 2) descriptions)))
                                (goto-char (or default-pos before-default-pos))
                                (setf (dotnet-arg-choices current) choices)
                                (setf (dotnet-arg-choice-descriptions current) descriptions))))
                          (push current matches)
                          (setq current (dotnet-arg-create)))
                        matches))))

(defun dotnet-new--get-parsed-help-cached (template)
  "Get parsed help for TEMPLATE, with caching."
  (or (gethash template dotnet-new--help-cache)
      (when-let ((cmd-help (dotnet-new--get-help-command template)))
        (let ((parsed-args (dotnet-new--parse-help cmd-help)))
          (puthash template parsed-args dotnet-new--help-cache)
          parsed-args))))

(defun dotnet-new--clear-help-cache ()
  "Clear the help cache. Useful when template help might have changed."
  (interactive)
  (clrhash dotnet-new--help-cache)
  (message "Dotnet template help cache cleared"))

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

(defun dotnet-new--update-transient-layout ()
  "Update the transient layout with current template arguments."
  (let ((template-args (when dotnet-new--current-args
                         (dotnet-arg--create-transient-args dotnet-new--current-args))))
    (transient-define-prefix dotnet-new-transient ()
      "Transient CLI dispatcher for dotnet new templates."
      ["Selected Template:"
       (:info (lambda () (upcase (or dotnet-new--selected "No template selected"))))]
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
       :setup-children (lambda (_) template-args)]
      ["Actions"
       ("s" "Select template" dotnet-new--select-template)
       ("e" "Execute" dotnet-new--invoke)
       ("q" "Quit" transient-quit-all)])))

(defun dotnet-new--select-template ()
  "Select a new template and update the transient."
  (interactive)
  (when-let* ((candidates (dotnet-new--get-candidates))
              (selected (completing-read "Which template? " candidates nil t))
              (continue (not (s-blank? selected))))
    (setq dotnet-new--selected selected)
    (setq dotnet-new--current-args (dotnet-new--get-parsed-help-cached selected))
    (dotnet-new--update-transient-layout)
    (transient-setup 'dotnet-new-transient)))

(transient-define-prefix dotnet-new-transient ()
  "Transient CLI dispatcher for dotnet new templates."
  ["Selected Template:"
   (:info "No template selected")]
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
   :class transient-column]
  ["Actions"
   ("s" "Select template" dotnet-new--select-template)
   ("e" "Execute" dotnet-new--invoke)
   ("q" "Quit" transient-quit-all)])

;;;###autoload
(defun dotnet-new-dispatch ()
  "Select a ~dotnet new~ template and invoke a transient interface for it."
  (interactive)
  (dotnet-new-transient))

(provide 'dotnet-new)
;;; dotnet-new.el ends here
