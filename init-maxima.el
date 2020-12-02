;; -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))

(defvar imaxima-use-maxima-mode-flag t)
(defvar imaxima-fnt-size "large")
(defvar imaxima-scale-factor 1.5)
(defvar maxima-save-input-history t)

(defvar maxima-mode-map)
(defvar inferior-maxima-mode-map)
(defvar inferior-maxima-process)

(declare-function maxima-send-region "maxima")
(declare-function maxima-send-line "maxima")
(declare-function imaxima "imaxima")

(with-eval-after-load "maxima"

  (define-key maxima-mode-map "\M-;" 'comment-dwim-line)
  ;; (define-key maxima-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key inferior-maxima-mode-map (kbd "TAB") 'inferior-maxima-complete)

  (define-key maxima-mode-map "\C-\M-f" 'forward-sexp)
  (define-key maxima-mode-map "\C-\M-b" 'backward-sexp)

  (defun my-maxima-send-line-or-region (beg end)
    (interactive "r")
    (let ((use-empty-active-region nil))
      (if (use-region-p)
          (maxima-send-region beg end)
        (maxima-send-line))))
  (define-key maxima-mode-map "\C-c\C-c" 'my-maxima-send-line-or-region)

  (defun my-maxima-switch-to-process-buffer ()
    "Display the inferior-maxima-process buffer so the recent output is visible."
    (interactive)
    (if (not (processp inferior-maxima-process))
        (imaxima))
    (pop-to-buffer (process-buffer inferior-maxima-process))
    (goto-char (point-max)))

  (define-key maxima-mode-map "\C-c\C-z" 'my-maxima-switch-to-process-buffer)
  )

(provide 'init-maxima)
