;;; tests.el --- Tests for dotnet-new-transient -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Austin Kearns
;;
;; Author: Austin Kearns <aus@dark>
;; Maintainer: Austin Kearns <aus@dark>
;; Created: January 29, 2025
;; Modified: January 29, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/lillenne/tests
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'ert)

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

(defvar dotnet-new-arg-test (dotnet-arg-create :short "-s" :long "--slong" :desc "Description" :type "choice" :default "net9.0" :choices '("net8.0" "net9.0") :choice-descriptions '("target net8" "target net9")))

(ert-deftest dotnet-new--test ()
  (let ((args `(,dotnet-new-arg-test ,dotnet-new-arg-test)))
    (mapcar (lambda (arg) (transient-parse-suffix
                           'transient--prefix
                           `(,(dotnet-arg--get-shortcut arg) ,(dotnet-arg-desc arg) (lambda ()
                                                                                      (interactive)
                                                                                      (message "okay!"))))) args)))

(ert-deftest dotnet-arg--get-shortcut ()
  (should (string= "s" (dotnet-arg--get-shortcut dotnet-new-arg-test))))

(provide 'tests)
;;; tests.el ends here
