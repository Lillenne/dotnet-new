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


;; infix defined with a macro
(transient-define-argument tsc--exclusive-switches ()
  "This is a specialized infix for only selecting one of several values."
  :class 'transient-switches
  :argument-format "--%s-snowcone"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix tsc-basic-infixes ()
  "Prefix that just shows off many typical infix types."
  ["Infixes"

   ;; from macro
   ("-e" "exclusive switches" tsc--exclusive-switches)

   ;; shorthand definitions
   ("-b" "switch with shortarg" ("-w" "--switch-short"))
   ;; note :short-arg != :key

   ("-s" "switch" "--switch")
   ( "n" "no dash switch" "still works")
   ("-a" "argument" "--argument=" :prompt "Let's argue because: ")

   ;; a bit of inline EIEIO in our shorthand
   ("-n" "never empty" "--non-null=" :always-read t  :allow-empty nil
    :init-value (lambda (obj) (oset obj value "better-than-nothing")))

   ("-c" "choices" "--choice=" :choices (foo bar baz))]

  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])
(tsc-basic-infixes)

;; (tsc-basic-infixes)



(transient-define-prefix dotnet-new-transient ()
  "Placeholder"
  [])

;; (transient-parse-suffix)
;; (transient-define-prefix dotnet-new-transient ()
;;   "dotnet new"
;;   ["Common Arguments"
;;    ("n" "Name" "--name=")
;;    ("o" "Output" "--output=")
;;    ]
;;   ["Section"
;;    ()]
;;   ["Misc"
;;    ("q" "quit" transient-quit-all)])
;;
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

(substring dotnet-new-test-help (s-index-of "Template options:" dotnet-new-test-help))
;; types: bool, choice, text
;; desc, type, default
;; choice fmt:
;; choice desc
;; choice desc
(let* ((topts (substring dotnet-new-test-help (s-index-of "Template options:" dotnet-new-test-help)))
       (lines (s-lines topts))
       (opt '()))
  ;; (s-match-strings-all "-" topts)
  ;; (s-match-strings-all "\\(-[alpha]\\),*[[space]]*\" topts)
  ;; (s-match-strings-all "\\(-[alpha]\\)?,?[[space]]*\\(--\\)?" topts)
  (s-match-strings-all "\\(-^[space]+\\)" topts)
  )

(s-match "\\(-[[:blank:]]+\\)" " - f")
(s-match "[[:blank:]]\\(-[^,^[:blank:]]+\\),?[[:space:]]+\\(--[^[:blank:]]+\\)[[:space:]]*\\(.*\\)?" " -f, --framework description\nmoar")
(s-match-strings-all "\\([[:blank:]]-[^,^[:blank:]]+\\),?[[:space:]]?\\(--[^[:blank:]]+\\)[[:space:]]*\\(.*\\)?" (s-replace "\n" " " dotnet-new-test-help))
;; (s-match "\\([[:blank:]]-[^,^[:blank:]]+\\),?[[:space:]]?\\(--[^[:blank:]]+\\)[[:space:]]*\\(.*\\)?" " -f, --framework description\nmoar") ; works until newline
;; (s-match "\\([[:blank:]]-[^,^[:blank:]]+\\),?[[:space:]]?\\(--[^[:blank:]]+\\)[[:space:]]*\\(^#*\\)" " -f, --framework")
;; (s-match "\\([[:blank]]-[^,^[:blank:]]+\\),?[[:space:]]?\\(--[^[:blank:]]\\)[[:space:]]*\\(^\n\\)" " -f, --framework")
;; ^[^#\r\n].*
;; (while-let ((point (re-search-forward "\\([[:blank:]]-[^,^[:blank:]]+\\)?,?[[:space:]]*\\(--[^[:blank:]]+\\)?" nil t)))

(cl-defstruct (dotnet-arg (:constructor dotnet-arg-create)
                          (:copier dotnet-arg-copy))
  short long desc type default choices choice-descriptions)

(defun dotnet-arg--get-shortcut (arg)
  (car (s-match "[^-]" (dotnet-arg-short arg))))

(defun dotnet-arg--)

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

(with-temp-buffer (insert dotnet-new-test-help)
                  (search-backward "Template options:")
                  (let ((matches '())
                        (current (dotnet-arg-create)))
                    (while-let ((point (re-search-forward "\\([[:blank:]]-+[^,^[:blank:]]+\\)" nil t)))
                      (setf (dotnet-arg-short current) (s-trim-left (match-string 1)))
                      ;; (setq current (plist-put current :short (s-trim-left (match-string 1))))
                      (when-let ((long-match (s-match "\\(--+[^,^[:blank:]]+\\)" (thing-at-point 'line))))
                        (setf (dotnet-arg-long current) (nth 1 long-match)))
                      ;; (setq current (plist-put current :long (nth 1 long-match))))
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
                        ;; (setq current (plist-put current :desc trimmed))
                        ;; (setq current (plist-put current :type type))
                        ;; (setq current (plist-put current :default default))
                        (when (string= "choice" type)
                          (goto-char type-pos)
                          (forward-line)
                          (let ((choices '())
                                (descriptions '()))
                            (while (< (point) default-line-begin-pos)
                              (re-search-forward "[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]+\\(.*\\)")
                              (unless (> (point) default-line-begin-pos)
                                (push (match-string 1) choices)
                                (push (match-string 2) descriptions))
                              )
                            (goto-char default-pos)
                            (setf (dotnet-arg-choices current) choices)
                            (setf (dotnet-arg-choice-descriptions current) descriptions)
                            ;; (setq current (plist-put current :choices choices))
                            ;; (setq current (plist-put current :choice-descriptions descriptions))
                            )
                          )
                        ;; todo choices, default & val
                        )
                      ;; (push (or (match-string 1) (match-string 2)) matches)
                      (push current matches)
                      (setq current (dotnet-arg-create))
                      )
                    matches))

"Whether to exclude launchSettings.json from the
                             generated template. "

(s-match "\\([[:blank:]]-[^,^[:blank:]]+\\)?,?[[:space:]]+\\(--[^[:blank:]]+\\)?[[:space:]]*\\(.*\\)?"
         "-f, --framework <choice>   The target framework for the project.")

(s-match "[[:blank:]]\\(-[^,^[:blank:]]+\\),?[[:space:]]+\\(--[^[:blank:]]+\\)[[:space:]]*\\(.*\\)?" " -f, --framework description\nmoar")
(let ((opts '()))
  (when (re-search-forward "-+[[]]+"))

  )
()
)
(while)
;; Template options:
;;   -f, --framework <choice>   The target framework for the project.
;;                              Type: choice
;;                                net9.0  Target net9.0
;;                                net8.0  Target net8.0
;;                              Default: net9.0
;;   --exclude-launch-settings  Whether to exclude launchSettings.json from the
;;                              generated template.
;;                              Type: bool
;;                              Default: false
;;   --no-restore               If specified, skips the automatic restore of the
;;                              project on create.
;;                              Type: bool
;;                              Default: false
;;   --use-program-main         Whether to generate an explicit Program class and
;;                              Main method instead of top-level statements.
;;                              Type: bool
;;                              Default: false
;;   --aot                      Whether to enable the project for publishing as
;;                              native AOT.
;;                              Type: bool
;;                              Default: false




(when-let* (
            (candidates (--filter (not (s-blank? it)) (cddddr (s-lines (shell-command-to-string "dotnet new list | awk -F '[[:space:]][[:space:]]+' '{print $2}'")))))
            (selected (completing-read "Which template? " candidates nil t))
            (continue (not (s-blank? selected)))
            (cmd-base (s-concat "dotnet new " selected))
            (cmd-help (shell-command-to-string (concat cmd-base " --help")))
            )
  cmd-help

  (transient-define-prefix dotnet-new-transient ()
    "dotnet new"
    ["Common Arguments"
     ("n" "Name" "--name=")
     ("o" "Output" "--output=")
     ]
    [:description (lambda () selected)
     :setup-children
     (lambda (_) ; we don't care about the suffix

       ;; remember to return a list
       (list (transient-parse-suffix
              'transient--prefix
              '("r" "replacement" (lambda ()
                                    (interactive)
                                    (message "okay!"))))))
     ]
    ["Misc"
     ("q" "quit" transient-quit-all)])
  (dotnet-new-transient)
  )
;; standard options -n name -o output --force --project

(provide 'dotnet-new)
;;; dotnet-new.el ends here

(transient-define-prefix dotnet-new-transient ()
  "dotnet new"
  ["Common Arguments"
   ("n" "Name" "--name=")
   ("o" "Output" "--output=")
   ]
  [
   :description "desc"
   ;; :description (lambda () selected)
   :setup-children
   (lambda (_)
     (let ((args `(,dotnet-new-arg-test ,dotnet-new-arg-test)))
       (mapcar (lambda (arg) (transient-parse-suffix
                              'transient--prefix
                              `(,(dotnet-arg--get-shortcut arg) ,(dotnet-arg-desc arg) (lambda ()
                                                                                         (interactive)
                                                                                         (message "okay!"))))) args)
       ))

   ]
  ["Misc"
   ("q" "quit" transient-quit-all)])
(dotnet-new-transient)
