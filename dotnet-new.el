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

(defvar dotnet-new-max-chars 80 "Max chars for argument descriptions before wrap.")
(defvar dotnet-new-extra-line-padding 10
  "The number of spaces to insert for extra description lines.
Used when arguments descriptions are longer than `dotnet-new-max-chars'.")

(defvar dotnet-new--candidates-cache nil "Cached list of dotnet template candidates.")
(defvar dotnet-new--help-cache (make-hash-table :test 'equal) "Cache for parsed help data by template name.")

(defun dotnet-new--get-candidates ()
  "Get list of dotnet template candidates, with caching."
  (or dotnet-new--candidates-cache
      (setq dotnet-new--candidates-cache
            (--filter (and it (not (s-blank? (cdr it))) (not (s-blank? (car it))))
                      (mapcar (lambda (line) (let* ((split (split-string line "\\s-\\{2,\\}"))
                                                    (part (nth 1 split))
                                                    (description (concat (car split) (propertize (concat " (" part ")") 'face 'shadow))))
                                               (cons description (if (and part (s-contains? "," part))
                                                   (car (s-split "," part))
                                                 part))))
                              (cddddr (s-lines (shell-command-to-string "dotnet new list"))))))))

(defun dotnet-new--clear-candidates-cache ()
  "Clear the candidates cache. Useful when templates are added/removed."
  (interactive)
  (setq dotnet-new--candidates-cache nil)
  (message "Dotnet template candidates cache cleared"))

(defun dotnet-new--get-help-command (selected)
  ;; TODO return buffer, not string
  (when (string-blank-p selected) (error "Selected command is blank!"))
  (let ((help-cmd (shell-command-to-string (concat "dotnet new " selected " --help"))))
    (if (string-blank-p help-cmd)
        (error "Failed to get help! (%s)" help-cmd))
    help-cmd))

(cl-defstruct (dotnet-arg (:constructor dotnet-arg-create)
                          (:copier dotnet-arg-copy))
  arg short long desc type default choices choice-descriptions)

(defvar dotnet-new--selected nil "The last selected template.")
(defvar dotnet-new--current-args nil "Current parsed arguments for the selected template.")

(defun dotnet-arg--get-shortcut (arg &optional count)
  (let ((shortcut (car (s-match (concat "-[^-]\\{" (if count
                                                       (if (stringp count)
                                                           count
                                                         (number-to-string count))
                                                     "1")
                                        "\\}")
                                (dotnet-arg-short arg)))))
    (or shortcut (dotnet-arg-long arg))))

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
                              (beginning-of-line)
                              (when-let* ((numspace (length (car (s-match "[[:blank:]]+" (thing-at-point 'line)))))
                                          (regex (format "[[:blank:]]\\{%d\\}\\([^[:blank:]]+\\)[[:blank:]]+\\([^\n]*\\)" numspace)))
                                (let ((choices '())
                                      (descriptions '()))
                                  (while (and default-line-begin-pos
                                              (< (point) default-line-begin-pos)
                                              (re-search-forward regex nil t))
                                    (push (match-string-no-properties 1) choices)
                                    (push (match-string-no-properties 2) descriptions))
                                  (goto-char (or default-pos before-default-pos))
                                  (setf (dotnet-arg-choices current) choices)
                                  (setf (dotnet-arg-choice-descriptions current) descriptions))))
                            (push current matches)
                            (setq current (dotnet-arg-create))))
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
  (setq dotnet-new--selected nil)
  (clrhash dotnet-new--help-cache)
  (message "Dotnet template help cache cleared"))

(defun dotnet-new-clear-caches ()
  "Clear both the candidates and help caches."
  (interactive)
  (dotnet-new--clear-candidates-cache)
  (dotnet-new--clear-help-cache)
  (message "Dotnet new caches cleared"))

(defun dotnet-arg--create-transient-arg (arg &optional padding)
  (when (and (numberp padding) (> padding 0))
    (let ((trimmed (dotnet-arg-desc arg)))
      (when (> (length trimmed) dotnet-new-max-chars)
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
            (push (concat (s-trim-left current-line)) lines))
          ;; todo calculate padding based on longest arg
          ;; (push (concat (make-string dotnet-new-extra-line-padding ?\s) current-line) lines))
          ;; (setf (dotnet-arg-desc arg) (string-join (reverse lines) "\n"))
          (setf (dotnet-arg-desc arg) (string-join (reverse lines) " "))))))
  (let ((long-arg (concat (dotnet-arg-long arg) (unless (string= "bool" (dotnet-arg-type arg)) "="))))
    (if (dotnet-arg-choices arg)
        (transient-parse-suffix
         'transient--prefix
         `(,(dotnet-arg-arg arg) ,(dotnet-arg-desc arg) ,long-arg :choices ,(dotnet-arg-choices arg)))
      (transient-parse-suffix
       'transient--prefix
       `(,(dotnet-arg-arg arg) ,(dotnet-arg-desc arg) ,long-arg)))))

(defun dotnet-arg--create-transient-args (dotnet-args)
  ;; happening in blazorwasm template. TODO look into root cause
  (setq dotnet-args (cl-remove-duplicates dotnet-args :test
                                          (lambda (a b)
                                            (or (string= (dotnet-arg-short a) (dotnet-arg-short b))
                                                (string= (dotnet-arg-long a) (dotnet-arg-long b))))))
  ;; trim prefixes from args
  (dolist (arg dotnet-args)
    (setf (dotnet-arg-arg arg)
          (s-chop-prefixes '("--" "-") (dotnet-arg-long arg))))

  ;; identify flags which are not unique from the start of other prefixes
  ;; e.g., --auth and --authority as separate flags
  (let ((unique 0)) ; unique prefix counter
    (dolist (arg dotnet-args)
      (let ((flag (dotnet-arg-arg arg))
            (collision nil)
            (found nil))
        (while (not found)
          (dolist (arg2 dotnet-args)
            (unless (eq arg arg2)
              (let ((f2 (dotnet-arg-arg arg2)))
                ;; no collision yet
                (when (and (not collision)
                           ;; and if the flag is a prefix of another arg or vice versa
                           (or (s-starts-with? flag f2)
                               (s-starts-with? f2 flag)))
                  ;; then adjust the flag to be unique
                  (setq flag (if (<= 1 (length flag))
                                 ;; right shift the transient chars by 1 if possible
                                 (substring flag 1 (length flag))
                               (if (s-uppercase? flag)
                                   ;; if it is already upcased, add a unique prefix
                                   (concat (number-to-string (cl-incf unique)) flag)
                                 ;; otherwise, upcase the flag to make it unique and try again
                                 (upcase (dotnet-arg-arg arg))))
                        collision t)))))
          ;; after iterating through all args, if we found a collision we need to try again
          (if collision
              (setq collision nil)
            ;; otherwise, we found a unique flag
            (setq found t)
            (setf (dotnet-arg-arg arg) flag))))))

  ;; Try to shorten each arg name to shortest unique prefix
  (dolist (arg dotnet-args)
    (let* ((orig-flag (dotnet-arg-arg arg))
           (shortest-flag orig-flag))

      ;; Try all possible lengths from shortest to longest
      (let ((done nil))
        (dotimes (len (length orig-flag))
          (unless done
            (let* ((candidate (substring orig-flag 0 (1+ len)))
                   (is-unique t))

              ;; Check if this length is unique against all other args
              (dolist (other-arg dotnet-args)
                (unless (eq arg other-arg)
                  (when (string-prefix-p candidate (dotnet-arg-arg other-arg))
                    (setq is-unique nil))))

              ;; If unique, this is our new shortest candidate
              (when is-unique
                (setq shortest-flag candidate)
                ;; We found the shortest unique prefix, can stop checking
                (setq done t))))))

      ;; Update to the shortest unique version found
      (setf (dotnet-arg-arg arg) shortest-flag)))

  (let ((max-prefix-len 0))
    (dolist (arg dotnet-args)
      (unless (s-starts-with? "-" (dotnet-arg-arg arg))
        (setf (dotnet-arg-arg arg) (concat "-" (dotnet-arg-arg arg))))
      (setq max-prefix-len
            (max max-prefix-len (length (dotnet-arg-arg arg)))))
    ;; Create transient args for each dotnet-arg
    (mapcar (lambda (arg) (dotnet-arg--create-transient-arg
                           arg
                           (max dotnet-new-extra-line-padding)))
            dotnet-args)))

(defun dotnet-new--invoke (&optional transient-params)
  "Run command with `TRANSIENT-PARAMS' from the last `dotnet-new-transient'."
  (interactive
   (list (transient-args 'dotnet-new-transient)))
  (unless dotnet-new--selected
    (error "No template selected! Please select a template first."))
  (shell-command (shell-quote-argument (s-concat "dotnet new " (cdr dotnet-new--selected) " " (s-join " " transient-params)))))

(defun dotnet-new--select-template ()
  "Select a new template and update the transient."
  (interactive)
  (when-let* ((candidates (dotnet-new--get-candidates))
              (selected (assoc (completing-read "Which template? " (mapcar #'car candidates) nil t) candidates))
              (continue (and (cdr selected) (not (s-blank? (cdr selected))))))
    (setq dotnet-new--selected selected)
    (setq dotnet-new--current-args (dotnet-new--get-parsed-help-cached (cdr dotnet-new--selected)))
    (dotnet-new--update-transient-layout)
    (transient-setup 'dotnet-new-transient)))

(defun dotnet-new--update-transient-layout ()
  "Update the transient layout with current template arguments."
  (transient-define-prefix dotnet-new-transient ()
    "Transient CLI dispatcher for dotnet new templates."
    :refresh-suffixes t
    ["Selected Template:"
     (:info (lambda () (or (car dotnet-new--selected) "No template selected")))]
    ["Common Arguments"
     :pad-keys t
     ("n" "Name" "--name=")
     ("o" "Output" "--output=")
     ("d" "Dry run" "--dry-run")
     ("f" "Force" "--force")
     ("u" "No update check" "--no-update-check")
     ("p" "Project" "--project=")
     ("l" "Language" "--language" :choices '("C#" "F#" "VB"))
     ("t" "Type" "--type=")]
    ["Template Arguments"
     :pad-keys t
     :class transient-column
     :setup-children (lambda (_) (when dotnet-new--current-args
                                   (dotnet-arg--create-transient-args dotnet-new--current-args)))]
    ["Actions"
     :pad-keys t
     ("s" "Select template" dotnet-new--select-template)
     ("e" "Execute" dotnet-new--invoke)
     ("c" "Clear cache" dotnet-new-clear-caches)
     ("q" "Quit" transient-quit-all)]))

;;;###autoload
(transient-define-prefix dotnet-new-transient ()
  "Transient CLI dispatcher for dotnet new templates."
  :refresh-suffixes t
  ["Selected Template:"
   (:info (lambda () (or (car dotnet-new--selected) "No template selected")))]
  ["Common Arguments"
   :pad-keys t
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
   :pad-keys t
   ("s" "Select template" dotnet-new--select-template :transient t)
   ("e" "Execute" dotnet-new--invoke)
   ("c" "Clear cache" dotnet-new-clear-caches)
   ("q" "Quit" transient-quit-all)])

 ;;;###autoload
(defun dotnet-new-dispatch ()
  "Select a ~dotnet new~ template and invoke a transient interface for it."
  (interactive)
  (dotnet-new-transient))

(provide 'dotnet-new)
;;; dotnet-new.el ends here
