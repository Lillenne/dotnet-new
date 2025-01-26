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
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Completion for the dotnet cli "new" command
;;
;;; Code:

;; https://github.com/positron-solutions/transient-showcase?tab=readme-ov-file#Prefixes-and-Suffixes

(defvar dotnet-new-test-to "Template options:
  -f, --framework <choice>   The target framework for the project.
                             Type: choice
                               net9.0  Target net9.0
                               net8.0  Target net8.0
                             Default: net9.0
  --exclude-launch-settings  Whether to exclude launchSettings.json from the
                             generated template.
                             Type: bool
                             Default: false
  --no-restore               If specified, skips the automatic restore of the
                             project on create.
                             Type: bool
                             Default: false
  --use-program-main         Whether to generate an explicit Program class and
                             Main method instead of top-level statements.
                             Type: bool
                             Default: false
  --aot                      Whether to enable the project for publishing as
                             native AOT.
                             Type: bool
                             Default: false")
(defvar dotnet-new-test-help
  "
ASP.NET Core gRPC Service (C#)
Author: Microsoft
Description: A project template for creating a gRPC service using ASP.NET Core, with optional support for publishing as native AOT.

Usage:
  dotnet new grpc [options] [template options]

Options:
  -n, --name <name>       The name for the output being created. If no name is
                          specified, the name of the output directory is used.
  -o, --output <output>   Location to place the generated output.
  --dry-run               Displays a summary of what would happen if the given
                          command line were run if it would result in a
                          template creation.
  --force                 Forces content to be generated even if it would
                          change existing files.
  --no-update-check       Disables checking for the template package updates
                          when instantiating a template.
  --project <project>     The project that should be used for context
                          evaluation.
  -lang, --language <C#>  Specifies the template language to instantiate.
  --type <project>        Specifies the template type to instantiate.

Template options:
  -f, --framework <choice>   The target framework for the project.
                             Type: choice
                               net9.0  Target net9.0
                               net8.0  Target net8.0
                             Default: net9.0
  --exclude-launch-settings  Whether to exclude launchSettings.json from the
                             generated template.
                             Type: bool
                             Default: false
  --no-restore               If specified, skips the automatic restore of the
                             project on create.
                             Type: bool
                             Default: false
  --use-program-main         Whether to generate an explicit Program class and
                             Main method instead of top-level statements.
                             Type: bool
                             Default: false
  --aot                      Whether to enable the project for publishing as
                             native AOT.
                             Type: bool
                             Default: false

")

(cl-defstruct (dotnet-arg (:constructor dotnet-arg-create)
                          (:copier dotnet-arg-copy))
  short long desc type default choices choice-descriptions)

;; (defun dotnet-arg-has-choice-p (arg)
;;   (dotnet-arg-choices arg))

(defun dotnet-arg--get-shortcut (arg)
  (car (s-match "-[^-]" (dotnet-arg-short arg))))

(ert-deftest dotnet-arg--get-shortcut ()
  (should (string= "s" (dotnet-arg--get-shortcut dotnet-new-arg-test))))

(defvar dotnet-new-arg-test (dotnet-arg-create :short "-s" :long "--slong" :desc "Description" :type "choice" :default "net9.0" :choices '("net8.0" "net9.0") :choice-descriptions '("target net8" "target net9")))
(ert-deftest dotnet-new--test ()
  (let ((args `(,dotnet-new-arg-test ,dotnet-new-arg-test)))
    (mapcar (lambda (arg) (transient-parse-suffix
                           'transient--prefix
                           `(,(dotnet-arg--get-shortcut arg) ,(dotnet-arg-desc arg) (lambda ()
                                                                                      (interactive)
                                                                                      (message "okay!"))))) args)
    )
  )

;; TODO not working for all templates, but for some
(defun dotnet-new--parse-help (STRING)
  (with-temp-buffer (insert STRING)
                    (goto-char (point-min))
                    (search-forward "Template options:")
                    (let ((matches '())
                          (current (dotnet-arg-create)))
                      (while-let ((point (re-search-forward "\\([[:blank:]]-+[^,^[:blank:]]+\\)" nil t)))
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
                      matches)))

(defun dotnet-arg--create-transient-arg (arg)
  (let ((long-arg (concat (dotnet-arg-long arg) (unless (string= "bool" (dotnet-arg-type arg)) "="))))
    (if (dotnet-arg-choices arg)
        (transient-parse-suffix
         'transient--prefix
         `(,(dotnet-arg--get-shortcut arg) ,(dotnet-arg-desc arg) ,long-arg :choices ,(dotnet-arg-choices arg)))
      (transient-parse-suffix
       'transient--prefix
       `(,(dotnet-arg--get-shortcut arg) ,(dotnet-arg-desc arg) ,long-arg)))))


(defvar dotnet-new--selected)
;; invoke command ;; TODO don't need to dynamically generate new maps -> can do first completion and setup sub menu dynamically? How to get all args?
(defun dotnet-new--invoke (&optional transient-params)
  (interactive
   (list (transient-args 'dotnet-new-transient4)))
  ;; (message transient-params) (sleep-for 10)
  ;; (message (s-concat "dotnet new " dotnet-new--selected (s-join " " transient-params))) (sleep-for 10)
  (shell-command (s-concat "dotnet new " dotnet-new--selected " " (s-join " " transient-params)))
  )

(when-let* (
            (candidates (--filter (not (s-blank? it)) (cddddr (s-lines (shell-command-to-string "dotnet new list | awk -F '[[:space:]][[:space:]]+' '{print $2}'")))))
            (selected (completing-read "Which template? " candidates nil t))
            (continue (not (s-blank? selected)))
            (prefix-name (intern (concat "dotnet-new-" selected)))
            )
  (setq dotnet-new--selected selected)
  (unless (transient-prefix-p prefix-name)
    (let* ((cmd-base (s-concat "dotnet new " selected))
           (cmd-help (shell-command-to-string (concat cmd-base " --help")))
           (dotnet-args (dotnet-new--parse-help cmd-help)))
      ;; TODO need the prefix name to be unique per template
      (transient-define-prefix dotnet-new-transient4 ()
        "dotnet new"
        ["Common Arguments"
         ("n" "Name" "--name=")
         ("o" "Output" "--output=")
         ("e" "Execute" dotnet-new--invoke)
         ]
        [:description (lambda () selected)
         :setup-children
         (lambda (_)
           (mapcar #'dotnet-arg--create-transient-arg dotnet-args))
         ]
        ["Misc"
         ("q" "quit" transient-quit-all)])
      )
    )
  ;; (prefix-name)
  )
;; standard options -n name -o output --force --project

(provide 'dotnet-new)
;;; dotnet-new.el ends here
