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
  :type  'string
  :group 'codesnap)

;;;###autoload
(defun codesnap-selection ()
  "Screenshot selected code using codesnap."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((filename    (buffer-file-name))
         (start-range (line-number-at-pos (region-beginning)))
         (end-range   (line-number-at-pos (region-end)))
         (log-buffer  (get-buffer-create codesnap-log-buffer))
         (result      (call-process codesnap-binary nil log-buffer t
                                    "--from-file" filename
                                    "--output"    "clipboard"
                                    "--range"     (format "%d:%d" start-range end-range)
                                    "--watermark" codesnap-watermark
                                    "--has-line-number")))
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
