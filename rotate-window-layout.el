;;; rotate-window-layout.el --- rotate/flip window layout -*- lexical-binding: t; -*-

;; Copyright 2017 Liu Hui

;; Emacs 26+

;; Examples:
;;
;;        +-----+--------+----+         +---------+---------+
;;        |     |        |    |         |         |         |
;;        |     |        |    |         |         +---------+
;;        +-----+---+----+----+  ---->  +---------+         |
;;        |         |         |         |         +---------+
;;        |         |         |         |         |         |
;;        +---------+---------+         +---------+---------+


;;; Commentary:
;;

;;; Code:

(defvar rotate-window-layout-order '(row column))

(defvar rotate-window-layout-rotate-direction 'clockwise)

(defun rotate-window-layout--merge (intervals)
  "Merge successive INTERVALS (e.g. [[0 2], [1 3]] -> [[0 3]])."
  (let ((sorted (cl-sort
                 intervals
                 (lambda (x y)
                   (or (< (car x) (car y))
                       (< (cdr x) (cdr y))))))
        result inv beg end)
    (setq inv (pop sorted))
    (setq beg (car inv) end (cdr inv))
    (while sorted
      (setq inv (pop sorted))
      (if (> end (car inv))             ; strictly larger
          (setq end (max end (cdr inv)))
        (push (cons beg end) result)
        (setq beg (car inv) end (cdr inv))))
    (push (cons beg end) result)
    (reverse result)))

(defun rotate-window-layout--convert (alist)
  "Return the shallow layout according to ALIST."
  (let ((order rotate-window-layout-order)
        op intervals row-or-col sub-alists)
    (while (and order (not row-or-col))
      (setq op (if (eq (car order) 'row) 'cadr 'cl-caddr)
            intervals (rotate-window-layout--merge (mapcar op alist)))
      (if (> (length intervals) 1)
          (setq row-or-col (car order))
        (pop order)))
    (unless row-or-col
      (error "Only one window"))
    (push row-or-col sub-alists)
    (dolist (inv intervals)
      (push
       (cons (- (cdr inv) (car inv))
             (cl-remove-if
              (list 'lambda '(e)
                    `(let ((v ,(if (eq row-or-col 'row) '(cadr e) '(cl-caddr e))))
                       (or (< (car v) ,(car inv))
                           (> (cdr v) ,(cdr inv)))))
              alist))
       sub-alists))
    (reverse sub-alists)))

(defvar rotate-window-layout-getting-fn
  (lambda (w) (list 'buffer (window-buffer w) 'window w))
  "This function returns a property list describing a window.
Properties `buffer' and `window' should be included.")

(defun rotate-window-layout--get (alist)
  "Return the layout according to ALIST."
  (let* ((sub-alists (rotate-window-layout--convert alist))
         (layout (list (pop sub-alists))) size sub-alist element)
    (dolist (x sub-alists)
      (setq size (car x) sub-alist (cdr x))
      (setq element
            (if (= (length sub-alist) 1)
                (append (list 'size size)
                        (funcall rotate-window-layout-getting-fn
                                 (caar sub-alist)))
              (list 'size size 'layout (rotate-window-layout--get sub-alist))))
      (setq layout (append layout (list element))))
    layout))

(defvar rotate-window-layout-excluding-fn
  (lambda (w) (or (not w) (window-parameter w 'window-side))))

(defun rotate-window-layout-get ()
  "Return the layout of selected frame."
  (let (alist)
    (walk-windows
     (lambda (w)
       (when (not (funcall rotate-window-layout-excluding-fn w))
         (let* ((top  (window-top-line w))
                (left (window-left-column w))
                (bottom (+ top (window-height w)))
                (right  (+ left (window-total-width w))))
           (push (list w (cons top bottom) (cons left right)) alist))))
     'nomini)
    (if (= (length alist) 1)
        (error "Only one window")
      (rotate-window-layout--get alist))))

(defvar rotate-window-layout-setting-fn
  (lambda (plist)
    (rotate-window-layout--set-buffer (plist-get plist 'buffer)))
  "This function sets a window according to the property list.")

(defun rotate-window-layout--set-buffer (buf)
  "Set the buffer of selected window to BUF."
  (set-window-buffer
   (selected-window)
   (or (get-buffer buf) (get-buffer-create "*scratch*"))))

(defun rotate-window-layout-set (layout &optional arg override)
  "Arrange sub-windows according to LAYOUT in selected window.
Override the row/column setting of LAYOUT with ARG if OVERRIDE is
non-nil."
  (if (atom (car layout))
      (if (and arg override)
          (pop layout)
        (setq arg (pop layout)))
    (or arg (error "Unspecified layout: row or column?")))
  (let (split-fn length-fn len orig-len child-arg)
    (if (eq arg 'row)
        (setq split-fn  'split-window-below
              length-fn 'window-height
              child-arg 'column)
      (setq split-fn  'split-window-right
            length-fn 'window-total-width
            child-arg 'row))
    (setq len (funcall length-fn)
          orig-len (apply '+ (mapcar (lambda (x) (plist-get x 'size)) layout)))
    (dolist (x layout)
      (plist-put x 'size (round (* (/ (plist-get x 'size) (float orig-len)) len))))
    (while layout
      (let (x next sub-layout)
        (setq x (pop layout))
        (and layout (setq next (funcall split-fn (plist-get x 'size))))
        (setq sub-layout (plist-get x 'layout))
        (if (not sub-layout)
            (funcall rotate-window-layout-setting-fn x)
          (rotate-window-layout-set sub-layout child-arg override))
        (and next (select-window next t))))))

(defun rotate-window-layout--delete-windows ()
  "Delete all windows except the selected one."
  (let ((ws (cl-remove-if
             rotate-window-layout-excluding-fn
             (window-list (selected-frame) 'no-mini (selected-window))))
        (config (current-window-configuration)))
    (condition-case err
        (progn
          (dolist (w (cdr ws)) (delete-window w))
          (select-window (car ws) t))
      (error
       (set-window-configuration config)
       (signal (car err) (cdr err))))))

(defun rotate-window-layout--swap-row-and-column (layout)
  "Swap row and column in LAYOUT."
  (mapcar
   (lambda (x)
     (if (atom x)
         x
       (if (not (member (car x) '(row column)))
	   (rotate-window-layout--swap-row-and-column x)
	 (rotate-window-layout--swap-row-and-column
          (cons (if (eq (car x) 'row) 'column 'row) (cdr x))))))
   layout))

(defun rotate-window-layout--rotate (layout &optional n direction)
  "Rotate LAYOUT N times by DIRECTION at root."
  (let ((n (if (numberp n) (% n 4) 1)) lo)
    (or direction (setq direction rotate-window-layout-rotate-direction))
    (cond
     ((or (= n 1) (= n 3))
      (setq lo (rotate-window-layout--swap-row-and-column (cdr layout)))
      (if (or (and (= n 1) (eq direction 'clockwise))
              (and (= n 3) (eq direction 'anti-clockwise)))
          (if (eq (car layout) 'row)
              (cons 'column (reverse lo))
            (cons 'row lo))
        (if (eq (car layout) 'row)
            (cons 'column lo)
          (cons 'row (reverse lo)))))
     ((= n 2)
      (rotate-window-layout--flip layout))
     (t
      layout))))

(defun rotate-window-layout--flip (layout)
  "Flip LAYOUT at root."
  (cons (car layout) (reverse (cdr layout))))

;; (defun rotate-window-layout--rotate-at (layout window &optional n direction)
;;   (mapcar
;;    (lambda (x)
;;      (if (atom x)
;;          x
;;        (if (cl-some
;;             (lambda (e) (eq (plist-get e 'window) window))
;;             x)
;;            (rotate-window-layout--rotate x n direction)
;;          (rotate-window-layout--rotate-at layout window n direction))))
;;    layout))

;;;###autoload
(defun rotate-window-layout (arg)
  "Rotate window layout at root.
If ARG is a number, layout is rotated ARG times."
  (interactive "p")
  (let* ((order rotate-window-layout-order)
         (rotate-window-layout-order (if (< arg 0) (reverse order) order))
         (layout (rotate-window-layout-get))
         (win (selected-window)))
    (rotate-window-layout--delete-windows)
    (rotate-window-layout-set
     (rotate-window-layout--rotate layout (abs arg)))
    (select-window win t)))

;;;###autoload
(defun flip-window-layout (arg)
  "Flip window layout at root. If ARG is negative."
  (interactive "p")
  (let* ((order rotate-window-layout-order)
         (rotate-window-layout-order (if (< arg 0) (reverse order) order))
         (layout (rotate-window-layout-get))
         (win (selected-window)))
    (rotate-window-layout--delete-windows)
    (rotate-window-layout-set
     (rotate-window-layout--flip layout))
    (select-window win t)))

(provide 'rotate-window-layout)

;;; rotate-window-layout.el ends here
