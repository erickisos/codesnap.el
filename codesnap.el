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
  :group 'tools
  :prefix "codesnap-")

(defcustom codesnap-binary "codesnap"
  "Path to the codesnap binary."
  :type 'string
  :group 'codesnap)

(defcustom codesnap-log-buffer "*CodeSnap Messages*"
  "Name of the buffer used for logging codesnap operations."
  :type 'string
  :group 'codesnap)

(defun codesnap--get-file-extension ()
  "Get the file extension for the current buffer."
  (when (buffer-file-name)
    (file-name-extension (buffer-file-name))))

(defun codesnap--create-temp-file (content)
  "Create a temporary file with CONTENT and appropriate extension."
  (let* ((file-ext (codesnap--get-file-extension))
         (temp-file (make-temp-file "codesnap-" nil (if file-ext (concat "." file-ext) ".txt"))))
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun codesnap--log-execution (temp-file content)
  "Log the codesnap execution details."
  (let ((log-buffer (get-buffer-create codesnap-log-buffer))
        (command (format "%s --from-file %s --output clipboard --has-line-number" codesnap-binary temp-file))
        (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
        (file-ext (codesnap--get-file-extension)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "[%s] Executing: %s\n" timestamp command))
      (insert (format "File: %s | Extension: %s | Lines: %d\n"
                      (or (buffer-file-name) "untitled")
                      (or file-ext "txt")
                      (count-lines (point-min) (point-max))))
      (insert "--- Selected Code ---\n")
      (insert content)
      (insert "\n--- End Code ---\n"))
    log-buffer))

(defun codesnap--execute-and-log (temp-file log-buffer)
  "Execute codesnap command and log the result."
  (let ((result (call-process codesnap-binary nil log-buffer t
                              "--from-file" temp-file
                              "--output" "clipboard"
                              "--has-line-number")))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "Exit code: %s\n" result))
      (if (zerop result)
          (insert "✓ Screenshot copied to clipboard successfully\n")
        (insert "✗ Error occurred during screenshot generation\n"))
      (insert "==========================================\n\n"))
    result))

;;;###autoload
(defun codesnap-selection ()
  "Screenshot selected code using codesnap."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((content (buffer-substring-no-properties (region-beginning) (region-end)))
         (temp-file (codesnap--create-temp-file content))
         (log-buffer (codesnap--log-execution temp-file content)))
    (unwind-protect
        (let ((result (codesnap--execute-and-log temp-file log-buffer)))
          (if (zerop result)
              (message "Code screenshot copied to clipboard")
            (message "CodeSnap failed - check %s buffer" codesnap-log-buffer)
            (pop-to-buffer log-buffer)))
      (delete-file temp-file))))

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
