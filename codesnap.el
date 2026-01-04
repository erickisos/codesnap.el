;;; codesnap.el -- The beautiful screenshot tooling for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Erick G. Islas Osuna
;;
;; Author: Erick G. Islas Osuna <erickisos653@gmail.com>
;; Created: October 28, 2025
;; Modified: October 28, 2025
;; Version: 0.0.1
;; Keywords: convenience docs languages multimedia tools
;; Homepage: https://github.com/erickisos/codesnap
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides Emacs integration for the Codesnap-rs CLI tool.
;;
;;; Code:


(defgroup codesnap nil
  "Emacs integration for codesnap."
  :group  'tools
  :prefix "codesnap-")

(defcustom codesnap-binary "codesnap"
  "Path to the codesnap binary."
  :type  'string
  :group 'codesnap)

(defcustom codesnap-log-buffer "*CodeSnap Messages*"
  "Name of the buffer used for logging codesnap operations."
  :type  'string
  :group 'codesnap)

(defcustom codesnap-watermark "CodeSnap.el"
  "Watermark that will be used within the generated screensot."
  :type  '(choice
           (string :tag "Watermark text")
           (const :tag "No Watermark" nil))
  :group 'codesnap)

(defcustom codesnap-breadcrumbs t
  "Enables the breadcrumbs to be displayed in the snapshot."
  :type 'boolean
  :group 'codesnap)

(defcustom codesnap-theme nil
  "Code theme for syntax higlighting in the screenshot.

When nil, will try to load the theme from the config file.
See codesnap-rs documentation for available themes."
  :type '(choice
          (string :tag "Theme name")
          (const :tag "Use config file theme" nil))
  :group 'codesnap)

(defun codesnap--build-args (start-range end-range &rest kw-args)
  "Build command-line arguments for codesnap.

KW-ARGS should contain one of the following: FILENAME, SOURCE;
this will be used to render the final output.

FILENAME is the file to snapshot, START-RANGE and END-RANGE define
the code range."
  (let ((filename    (plist-get kw-args :filename))
        (source-code (plist-get kw-args :source)))
    (append (if filename
                (list "--from-file" filename)
              (list "--from-code" source-code))
            (list "--range"     (format "%d:%d" start-range end-range)
                  "--has-line-number")
            (when codesnap-watermark
              (list "--watermark" codesnap-watermark))
            (when codesnap-breadcrumbs
              (list "--has-breadcrumbs" "true"))
            (list "--output" "clipboard"))))

;;;###autoload
(defun codesnap-selection ()
  "Screenshot selected code using codesnap."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((filename    (buffer-file-name))
         (start-range (line-number-at-pos (region-beginning)))
         (end-range   (line-number-at-pos (region-end)))
         (line-start  (save-excursion (goto-char (point-min))
                                      (forward-line (1- start-range))
                                      (line-beginning-position)))
         (line-end    (save-excursion (goto-char (point-min))
                                      (forward-line (1- end-range))
                                      (line-end-position)))
         (code        (buffer-substring-no-properties line-start line-end))
         (log-buffer  (get-buffer-create codesnap-log-buffer))
         (args        (codesnap--build-args start-range end-range :filename filename :source code))
         (result      (apply #'call-process codesnap-binary nil log-buffer t args)))
    (message "CodeSnap: code - %s" code)
    (if (zerop result)
        (message "CodeSnap: screenshot copied to clipboard")
      (message "CodeSnap: failed - check %s buffer" log-buffer)
      (pop-to-buffer log-buffer))))

;;;###autoload
(defun codesnap-show-log ()
  "Show the CodeSnap Messages buffer."
  (interactive)
  (let ((log-buffer (get-buffer codesnap-log-buffer)))
    (if log-buffer
        (pop-to-buffer log-buffer)
      (message "No CodeSnap log buffer found. Run codesnap-selection first."))))

(provide 'codesnap)
;;; codesnap.el ends here
