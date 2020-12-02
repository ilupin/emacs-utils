;; show buffers grouped by bookmarked directories in the buffer menu
;; also available via control+click1

(defvar msb-menu-cond)

(defun create-msb-menu-from-bookmark (&optional _)
  (setq
   msb-menu-cond
   (append
    (delq
     nil
     (seq-map-indexed
      (lambda (it idx)
        (let ((dir (assoc-default 'filename (cdr it))))
          (when (string-suffix-p "/" dir)
            `((and (string-prefix-p
                    ,(expand-file-name dir)
                    (or (buffer-file-name) ""))
                   'no-multi)
              ,(+ 1000 idx)
              ,(format "%s (%%d)" (car it))))))
      (bookmark-maybe-sort-alist)))
    '(((and (file-remote-p (or (buffer-file-name) "")) 'no-multi)
       2010
       "Remote (%d)")
      ((and (buffer-file-name) 'no-multi) 2020 "Misc files (%d)")
      ('no-multi 3000 "Other buffers (%d)")))))

(advice-add 'msb-menu-bar-update-buffers :before 'create-msb-menu-from-bookmark)

(msb-mode 1)
