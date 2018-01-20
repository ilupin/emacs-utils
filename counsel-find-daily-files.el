;;; counsel-find-daily-files.el  -*- lexical-binding: t; -*-

;; Access files in frequently-used directories easily
;; 1. provide `counsel-find-daily-file' (C-u M-x forces updating cache)
;; 2. bind C-M-f to this function for `ivy-switch-buffer' as a fallback


;;; Code:

(require 'ivy)
(require 'counsel)

(defvar counsel-daily-directory-list nil)

;; '("ag" "--nocolor" "-l" "-g" "") only files
;; '("find" "-type" "f")
(defvar counsel-daily-file-cmd '("fd" "-p" "-c" "never" ""))

(defvar counsel--daily-file-cache nil)

(defun counsel--generate-daily-file-cache ()
  (let ((program (car counsel-daily-file-cmd))
        (dirs (mapcar #'expand-file-name counsel-daily-directory-list))
        args)
    (counsel-require-program program)
    (if (string-equal program "find")
        (setq args (append dirs (cdr counsel-daily-file-cmd)))
      (setq args (append (cdr counsel-daily-file-cmd) dirs)))
    (with-temp-buffer
      (apply #'call-process program nil t nil args)
      (setq counsel--daily-file-cache
            (split-string
             (buffer-string)
             counsel-async-split-string-re
             t)))
    nil))

(defun counsel-find-daily-file (&optional update initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive "p")
  (when (or update (not counsel--daily-file-cache))
    (counsel--generate-daily-file-cache)
    (message "Updating daily file cache ... done."))
  (ivy-read "Find file: " counsel--daily-file-cache
            :initial-input initial-input
            :re-builder #'ivy--regex-plus
            :action #'find-file
            :unwind #'counsel-delete-process
            :caller 'counsel-find-daily-file))

(define-key ivy-switch-buffer-map (kbd "C-M-f")
  (lambda ()
    (interactive)
    (ivy-quit-and-run
     (counsel-find-daily-file nil ivy-text))))

(provide 'counsel-find-daily-files)
