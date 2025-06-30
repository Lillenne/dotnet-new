;;; tests.el --- Tests for dotnet-new-transient -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Austin Kearns
;;
;; Author: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Maintainer: Austin Kearns <59812315+Lillenne@users.noreply.github.com>
;; Created: January 29, 2025
;; Modified: January 29, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lillenne/dotnet-new
;; Package-Requires: ((emacs "29.1") (s "1.10") (transient "0.2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ert)
(require 'dotnet-new)

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

(defvar dotnet-new-gitignore-help
  "dotnet gitignore file
Author: Microsoft
Description: Creates a gitignore file for a dotnet project.

Usage:
  dotnet new gitignore [options] [template options]
  dotnet new .gitignore [options] [template options]

Options:
  -n, --name <name>      The name for the output being created. If no name is
                         specified, the name of the output directory is used.
  -o, --output <output>  Location to place the generated output.
  --dry-run              Displays a summary of what would happen if the given
                         command line were run if it would result in a template
                         creation.
  --force                Forces content to be generated even if it would change
                         existing files.
  --no-update-check      Disables checking for the template package updates
                         when instantiating a template.
  --project <project>    The project that should be used for context evaluation.
  --type <item>          Specifies the template type to instantiate.

Template options:
   (No options)
")

(defvar dotnet-new-sln-help
  "Solution File
Author: Microsoft
Description: Create an empty solution containing no projects

Usage:
  dotnet new sln [options] [template options]
  dotnet new solution [options] [template options]

Options:
  -n, --name <name>      The name for the output being created. If no name is
                         specified, the name of the output directory is used.
  -o, --output <output>  Location to place the generated output.
  --dry-run              Displays a summary of what would happen if the given
                         command line were run if it would result in a template
                         creation.
  --force                Forces content to be generated even if it would change
                         existing files.
  --no-update-check      Disables checking for the template package updates
                         when instantiating a template.
  --project <project>    The project that should be used for context evaluation.

Template options:
   (No options)
")

(defvar dotnet-new-test-help
  "ASP.NET Core gRPC Service (C#)
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

(defvar dotnet-new-test-helpers
  `(("grpc" . ,dotnet-new-test-help)
    ("gitignore" . ,dotnet-new-gitignore-help) 
    ("sln" . ,dotnet-new-sln-help)
    ("console" . "Template options: (No options)")
    ("classlib" . "Template options:\n  -f, --framework <NET_VERSION>  Target framework\nType: string\nDefault: net9.0"))
  "Alist of (template-name . help-output) for testing.")

(defun dotnet-new-test--mock-get-candidates ()
  (mapcar #'car dotnet-new-test-helpers))

(defun dotnet-new-test--mock-get-help (template)
  (cdr (assoc template dotnet-new-test-helpers)))

(defvar dotnet-new-arg-test (dotnet-arg-create :short "-s" :long "--slong" :desc "Description" :type "choice" :default "net9.0" :choices '("net8.0" "net9.0") :choice-descriptions '("target net8" "target net9")))

(ert-deftest dotnet-new--test ()
  (let ((args `(,dotnet-new-arg-test ,dotnet-new-arg-test)))
    (mapcar (lambda (arg) (transient-parse-suffix
                           'transient--prefix
                           `(,(dotnet-arg--get-shortcut arg) ,(dotnet-arg-desc arg) (lambda ()
                                                                                      (interactive)
                                                                                      (message "okay!"))))) args)))

(ert-deftest dotnet-arg--get-shortcut ()
  (should (string= "-s" (dotnet-arg--get-shortcut dotnet-new-arg-test))))

(ert-deftest dotnet-new-test-all-templates ()
  (cl-letf* (
             ;; uncomment for only the supplied mocks, else run all installed templates.
             ;; ((symbol-function 'dotnet-new--get-candidates)
             ;;  #'dotnet-new-test--mock-get-candidates)
             ;; ((symbol-function 'dotnet-new--get-help-command)
             ;;  (lambda (template) (dotnet-new-test--mock-get-help template)))
             (templates (dotnet-new--get-candidates)))
    
    (dolist (template templates)
      (let* ((_ (message "Testing template: %s" template))
             (help-text (dotnet-new--get-help-command template))

             (parsed-args (dotnet-new--parse-help help-text))
             (dn-args (when parsed-args
                               (dotnet-arg--create-transient-args parsed-args))))
        
        (should (stringp help-text))
        
        (if (s-contains? "(No options)" help-text)
            (should (null parsed-args))
          (should (consp parsed-args))
          (dolist (arg parsed-args)
            (should (dotnet-arg-p arg))
            (should (stringp (dotnet-arg-desc arg)))
            (should (stringp (dotnet-arg-type arg)))

            (when (string= "choice" (dotnet-arg-type arg))
              (should (consp (dotnet-arg-choices arg)))
              (should (consp (dotnet-arg-choice-descriptions arg)))
              (should (= (length (dotnet-arg-choices arg))
                         (length (dotnet-arg-choice-descriptions arg)))))))

        (when parsed-args
          (should (consp dn-args))
          (dolist (targ dn-args)
            (should targ)
            ;; (should (transient-argument-p targ)) ; not valid due to the way the transient args are created/parsed for setup_children.
            ))
        ))))


(provide 'tests)
;;; tests.el ends here
