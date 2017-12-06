;;; buffer-sidebar.el --- list and group buffers in a sidebar

;; Inspired by imenu-tree.el and tabbar.el

;;; Code:

(provide 'buffer-sidebar)

(require 'cl-seq)
(require 'tree-widget)
(require 'imenu)
(require 'dash)

(declare-function hl-line-highlight 'hl-line)
(declare-function projectile-project-name 'projectile)

(defgroup buffer-sidebar nil
  "Display imenu using tree-widget"
  :group 'convenience)

(defcustom bufbar-mode-name "*bufbar*"
  "Buffer name of buffer sidebars."
  :group 'buffer-sidebar
  :type 'string)

(defcustom bufbar-name '(or (buffer-name) "<NIL>")
  "*Tree imenu root name. "
  :group 'buffer-sidebar
  :type 'sexp)

(defcustom bufbar-icons
  '(("Types" . "function")
    ("Variables" . "variable"))
  "*A list to search icon for the button in the tree.
The key is a regexp match the tree node name.  The value is the icon
name for the children of the tree node."
  :group 'buffer-sidebar
  :type '(alist :keytype regexp :value-type string))

(defcustom bufbar-update-flag t
  "Non-nil means to automatically update the sidebars."
  :type 'symbol
  :group 'buffer-sidebar)

(defcustom bufbar-update-interval 2
  "Time in seconds needed before buffer sidebars will update themselves."
  :type 'integer
  :group 'buffer-sidebar)

;; (defcustom bufbar-use-tool-tips-flag (fboundp 'tooltip-mode)
;;   "Non-nil means to use tool tips if they are available.
;; When tooltips are not available, mouse-tracking and minibuffer
;; display is used instead."
;;   :group 'buffer-sidebar
;;   :type 'boolean)

;; (defcustom bufbar-track-mouse-flag (not bufbar-use-tool-tips-flag)
;;   "Non-nil means to display info about the line under the mouse."
;;   :group 'buffer-sidebar
;;   :type 'boolean)

;; (defcustom bufbar-indentation-width 1
;;   "When sub-nodes are expanded, the number of spaces used for indentation."
;;   :group 'buffer-sidebar
;;   :type 'integer)

(defface bufbar-group-face
  '((((class color) (background light))
     :weight bold :height 1.2 :foreground "gray20" :background "gray80")
    (((class color) (background dark))
     :weight bold :height 1.2 :foreground "gray80" :background "gray20"))
  "Face used for group names."
  :group 'buffer-sidebar)

(defface bufbar-buffer-name-face
  '((((class color) (background light))
     :weight bold :foreground "gray20")
    (((class color) (background dark))
     :weight bold :foreground "gray80"))
  "Face used for buffer names."
  :group 'buffer-sidebar)

(defface bufbar-entry-1
  '((t :height 1.0 :weight normal :inherit font-lock-function-name-face))
  "Face used for entries of depth 1."
  :group 'buffer-sidebar)

(defface bufbar-entry-2
  '((t :height 1.0 :weight normal :inherit font-lock-variable-name-face))
  "Face used for entries of depth 2."
  :group 'buffer-sidebar)

(defface bufbar-entry-3
  '((t :height 1.0 :weight normal :inherit font-lock-keyword-face))
  "Face used for entries of depth 3."
  :group 'buffer-sidebar)

(defface bufbar-entry-4
  '((t :height 1.0 :weight normal :inherit font-lock-comment-face))
  "Face used for entries of depth 4."
  :group 'buffer-sidebar)

(defconst bufbar-entry-faces '(bufbar-entry-1 bufbar-entry-2
                               bufbar-entry-3 bufbar-entry-4)
  "Lists of faces.")



(defvar bufbar-display-action
  '(display-buffer-in-side-window
    (side . right)
    (window-width . 33))
  "Refer to `display-buffer' to specify how buffer-sidebar displays.")

(defvar bufbar-mode-hook '(bufbar-set-text-size)
  "Hook run after entering Bufbar mode.")

(defun bufbar-set-text-size ()
  "Use small font size in buffer sidebars."
  (text-scale-set -1))

(defvar bufbar-updating-functions '(
                                    ;; bufbar-fold-all
                                    bufbar-goto-buffer
                                    ;; bufbar-expand-1
                                    bufbar-goto-entry)
  "Functions run after the update of sidebar content is finished.
Execution will be stopped if any function returns nil.")

(defvar bufbar-after-update-hook nil
  "Hook run after updating all buffer sidebars. An interactive
call of `imenu-tree' also cause the hook run.")

(defconst bufbar-default-config
  '((inhibit . bufbar-inhibit-default)
    (group . bufbar-group-default)
    (sort . bufbar-sort-function)
    (face . bufbar-entry-face-default)))

(defvar bufbar-config bufbar-default-config)

(defvar bufbar-inhibit-functions '(bufbar-inhibit-default)
  "List of functions used to prevent specific buffers from showing in sidebars.
A buffer is excluded if any function returns non-nil.")

(defun bufbar-inhibit-default (buf)
  (with-current-buffer buf
    (or (and (not (buffer-file-name))
             (not (apply #'derived-mode-p '(text-mode prog-mode))))
        (memq major-mode '(bufbar-mode image-mode))
        (string-equal " " (substring (or (buffer-name) " ") 0 1)))))

(defvar bufbar-group-function 'bufbar-group-default
  "Grouping function of buffers.")

(defun bufbar-group-default (buf)
  (let ((project-name
         (and (fboundp 'projectile-project-name)
              (with-current-buffer buf (projectile-project-name)))))
    (if (string-equal project-name "-")
        (setq project-name nil))
    (if project-name
        (setq project-name (format "[P] %s" project-name)))
    (or project-name
        (format-mode-line (buffer-local-value 'mode-name buf))
        "Default")))

(defvar bufbar-sort-function '(string-lessp . bufbar-buffer-lessp)
  "A pair of functions used to sort groups and buffers.")

(defun bufbar-buffer-lessp (buf1 buf2)
  (string-lessp (buffer-name buf1) (buffer-name buf2)))

(defvar bufbar-entry-face-function 'bufbar-entry-face-default
  "The function returning faces of imenu entries.
Arguments include entry depth, entry listp, and entry string.")

(defun bufbar-entry-face-default (depth _listp _str)
  "Assign faces by attributes of entries."
  (cond
   ((<= depth (length bufbar-entry-faces))
    (nth (1- depth) bufbar-entry-faces))
   (t
    (car (last bufbar-entry-faces)))))



(defvar bufbar-current-buffer nil)
(defvar bufbar-current-point nil)

(defvar bufbar-on nil)

(defvar-local bufbar--state nil
  "Buffer-local imenu-tree state. nil means the buffer is not
  managed by any imenu-tree buffers.")

(defvar bufbar--use-separate-buffer nil)
(defvar bufbar--group nil)
(defvar bufbar-timer nil)

(defvar bufbar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "r" 'bufbar-refresh-at-point)
    (define-key map "d" 'bufbar-delete-at-point)
    (define-key map "e" 'bufbar-expand-at-point)
    (define-key map "u" 'bufbar-up-level)
    ;; (define-key map "N" 'bufbar-next-same-level)
    ;; (define-key map "P" 'bufbar-previous-same-level)
    (define-key map "f" 'bufbar-next-tree)
    (define-key map "b" 'bufbar-previous-tree)
    ;; (define-key map "TAB" 'bufbar-next-group)
    ;; (define-key map "" 'bufbar-fold-all)
    ;; (define-key map "!" 'bufbar-fold-other)
    (dotimes (i 10)
      (define-key map `[,(+ ?0 i)] 'digit-argument))
    map)
  "Keymap for bufbar mode.")

;; TODO
(define-derived-mode bufbar-mode nil "Bufbar"
  "A sidebar to display buffers"
  ;; (tree-widget-set-theme "imenu")
  (widget-setup)
  :keymap bufbar-mode-map)

(defun bufbar-turn-on ()
  "docstring"
  (interactive)
  (unless bufbar-on
    (add-hook 'delete-frame-functions
              (lambda (f)
                   (let ((b (frame-parameter f 'bufbar-buffer-name)))
                     (and b (get-buffer b) (kill-buffer b)))))
    (run-with-idle-timer 1 nil
                         (lambda ()
                           (bufbar--update-state)
                           (message "%s" "bufbar initialized.")))
    (setq bufbar-timer (run-with-timer
                            1
                            bufbar-update-interval
                            #'bufbar-update))
    ;; at last
    (setq bufbar-on t)))

(defun bufbar-turn-off ()
  (interactive)
  (unless (not bufbar-on)
    (cancel-timer bufbar-timer)
    (remove-hook 'delete-frame-functions
                 (lambda (f)
                   (let ((b (frame-parameter f 'bufbar-buffer-name)))
                     (and b (get-buffer b) (kill-buffer b)))))
    (mapc
     (lambda (win)
       (if (eq 'bufbar-mode
               (buffer-local-value 'major-mode (window-buffer win)))
           (delete-window win)))
     (-flatten
      (mapcar #'window-list (frame-list))))
    (dolist (buf (buffer-list))
      (if (eq 'bufbar-mode (buffer-local-value 'major-mode buf))
          (kill-buffer buf)
        (with-current-buffer buf
          (remove-hook 'after-change-functions #'bufbar-after-change t)
          (setq bufbar--state nil))))
    (setq bufbar-on nil)))

(defun bufbar--inhibit-internal (buf &optional relax)
  (run-hook-with-args-until-success
   'bufbar-inhibit-functions buf)
  ;; (or
  ;;  (if relax ; allow possible imenu-ed buffers (e.g. empty scratch buffer) during auto-updating
  ;;      nil
  ;;    (or (and (eq bufbar-exclude-empty-buffers 'all)
  ;;             (eq (cdr (buffer-local-value 'imenu--index-alist buf)) nil))
  ;;        (and (eq bufbar-exclude-empty-buffers t)
  ;;             (eq (buffer-local-value 'imenu--index-alist buf) nil))))
  ;;  )
  )

(defun bufbar--create-groups ()
  (mapcar
   (lambda (elt)
     (cons (car elt)
           (-sort (cdr bufbar-sort-function) (cdr elt))))
   (-sort
    (lambda (e1 e2)
      (funcall (car bufbar-sort-function) (car e1) (car e2)))
    (-group-by
     bufbar-group-function
     (cl-delete-if
      #'bufbar--inhibit-internal
      (buffer-list))))))

(defun bufbar--insert-group-heading (tag)
  (widget-create
   'push-button
   :tag tag
   ;; :button-icon "bucket"
   ;; :notify
   :format "%[%t%]\n"
   :button-face 'bufbar-group-face
   :heading t))

(defun bufbar-heading-widget-p (w)
  "Non-nil if W is a group heading widget."
  ;; better rule?
  (widget-get w :heading))

(defun bufbar--insert (buf)
  "Insert tree widget of buffer BUF."
  (widget-create (bufbar-widget buf))
  (bufbar--set-state 0 buf))

(defun bufbar-widget (buf)
  "Widget for buffer BUF."
  `(tree-widget
    :node (push-button
           :tag ,(with-current-buffer buf (or (buffer-name) "Untitled"))
           :format "%[%t%]\n"
           :button-face bufbar-buffer-name-face
           :notify bufbar-jump-to-buffer)
    :expander bufbar-expander
    :has-children t
    :buffer ,buf  ; add a buffer name attribute?
    :open nil))

(defun bufbar-buffer-node-p (w)
  "Non-nil if W is the root of a buffer widget."
  ;; better rule?
  (eq (widget-get w :notify) 'bufbar-jump-to-buffer))

(defun bufbar--get-marker (item buf)
  "Get marker of imenu entry ITEM in buffer BUF."
  (and (listp (cdr item)) (setq item (car item)))
  (with-current-buffer buf
    (cond
     ((eq 'org-mode major-mode)
      (get-text-property 0 'org-imenu-marker (or (car-safe item) item)))
     (t
      (when (cdr-safe item)
        (let ((pos (cdr item)))
          (cond
           ((markerp pos) pos)
           ((numberp pos)
            (set-marker (make-marker) pos buf))
           ((overlayp pos)
            (set-marker (make-marker) (overlay-start pos) buf))
           (t nil))))))))

(defun bufbar-item (item buf icon depth)
  (if (listp (cdr item))
      `(tree-widget
        :node (push-button
               :tag ,(if (consp (car item)) (caar item) (car item))
               :imenu-marker ,(bufbar--get-marker item buf)
               ;; :button-icon "bucket"
               :depth ,depth
               :alist t
               :notify bufbar-jump-to-entry
               :format "%[%t%]\n"
               :button-face ,(funcall bufbar-entry-face-function
                              depth t (if (consp (car item)) (caar item) (car item))))
        :expander bufbar-sub-expander
        :has-children t)
    `(push-button
      :tag ,(car item)
      :imenu-marker ,(bufbar--get-marker item buf)
      :button-icon ,icon
      :depth ,depth
      :format "%[%t%]\n"
      :button-face ,(funcall bufbar-entry-face-function
                     depth nil (car item))
      :notify bufbar-jump-to-entry)))

(defun bufbar-expander (tree)
  (or (widget-get tree :args)
      (let ((buf (widget-get tree :buffer))
            index)
        (setq index
              (delq nil (cdr (buffer-local-value 'bufbar--state buf))))
        (mapcar (lambda (item)
                  ;; depth is 1
                  (bufbar-item item buf "function" 1))
                index))))

(defun bufbar-sub-expander (bucket)
  (let ((tree bucket) path buf index name (depth 1))
    (while (and (tree-widget-p tree)
                (widget-get tree :parent))
      (push (widget-get (widget-get tree :node) :tag) path)
      (setq tree (widget-get tree :parent)
            depth (1+ depth)))
    (setq buf (widget-get tree :buffer)
          name (car (last path)))
    (setq index (buffer-local-value 'imenu--index-alist buf))
    ;; TODO (setq index (cdr (buffer-local-value 'bufbar--state buf)))
    (while path
      (setq index (assoc-default
                   (car path) index #'equal-including-properties))
      (setq path (cdr path)))
    (and index
         (mapcar (lambda (item)
                   (bufbar-item
                    item
                    buf
                    (or (assoc-default name bufbar-icons 'string-match)
                        "function")
                    depth))
                 index))
    ))

(defun bufbar--create-index (buf)
  "Transform `imenu--index-alist' in buffer BUF."
  (with-current-buffer buf
    (cond
     ((eq major-mode 'org-mode)
      (-tree-map
       (lambda (elt)
         (let (a b)
           (if (consp elt)
               (setq a (car elt)
                     b (copy-marker (cdr elt)))
             (setq a elt
                   b (copy-marker (get-text-property 0 'org-imenu-marker elt))))
           (set-text-properties 0 (length a) `(org-imenu-marker ,b) a)
           (cons a b)))
       imenu--index-alist))
     (t
      imenu--index-alist))))

(defun bufbar--set-state (n &optional buf)
  "Set the car of `imenu-tree--state' to N (0 or 2) in buffer BUF."
  (with-current-buffer (or buf (current-buffer))
    (setq bufbar--state
          (cons n (bufbar--create-index (current-buffer))))))

(defun bufbar--update-state ()
  ""
  (and bufbar-on
       (mapc
        (lambda (buf)
          (with-current-buffer buf
            (cond
             ;; initialization
             ((eq bufbar--state nil)
              (add-hook 'after-change-functions #'bufbar-after-change nil t)
              (ignore-errors (imenu--make-index-alist t))
              (bufbar--set-state 0))
             ;; even numbers means buffer is not changed
             ((cl-evenp (car bufbar--state)) nil)
             ;; 1 --> 0 if imenu entries remain the same or 2 otherwise
             ((eq (car bufbar--state) 1)
              (ignore-errors (imenu--make-index-alist t))
              (if (equal (bufbar--create-index buf)
                         (cdr bufbar--state))
                  (setcar bufbar--state 0)
                (bufbar--set-state 2)))
             ;; any odd numbers larger than 2
             (t
              (ignore-errors (imenu--make-index-alist t))
              (bufbar--set-state 2)))))
        ;; get buffers whose states are to be updated
        (cl-delete-if
         (lambda (buf)
           ;; updating all possible buffers
           (bufbar--inhibit-internal buf t))
         (buffer-list)))))

(defun bufbar-after-change (&rest ignore)
  "Mark `imenu-tree--state' when buffer changes.
Odd number means buffer is changed and imenu entries need to be
regenerated."
  (when bufbar--state
    (if (cl-evenp (car bufbar--state))
        (cl-incf (car bufbar--state)))))

(defun bufbar--refresh (tree)
  "Refresh TREE. Does not keep original structure (TODO)."
  (let ((path (tree-mode-opened-tree tree)))
    (widget-put tree :args nil)
    (widget-value-set tree (widget-value tree))
    (tree-mode-open-tree tree path)))

;;;###autoload
(defun bufbar (&optional new-name)
  "Create & display imenu-tree buffer in current frame."
  (interactive "P")
  (or bufbar-on (error "Forget to `M-x bufbar-turn-on'?"))
  (if new-name
      (or (stringp new-name)
          (error "Wrong type of argument `new-name'.")))
  (setq bufbar-current-buffer (current-buffer)
        bufbar-current-point  (point))
  (let ((old-name (frame-parameter nil 'bufbar-buffer-name))
        bufbar-buffer win)
    (cond
     (new-name (setq bufbar-buffer (get-buffer-create new-name)))
     (old-name (setq bufbar-buffer (get-buffer-create old-name)))
     (t
      (setq bufbar-buffer (generate-new-buffer "*bufbar*"))
      (set-frame-parameter nil
                           'bufbar-buffer-name
                           (buffer-name bufbar-buffer))))
    (with-current-buffer bufbar-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (or (eq major-mode 'bufbar-mode)
          (bufbar-mode))
      ;; record current settings as buffer-local variables
      (and new-name
           (set (make-local-variable 'bufbar--use-separate-buffer) new-name))
      (set (make-local-variable 'bufbar-update-flag) bufbar-update-flag)
      ;; The following variables should remain UNCHANGED once they are
      ;; initialized in the buffer locally, to make sure buffer
      ;; contents can be updated reliably. In fact, if they are
      ;; changed, a full refresh by `bufbar' is necessary.
      (dolist (var '(bufbar-inhibit-functions
                     bufbar-group-function
                     bufbar-sort-function
                     bufbar-entry-face-function))
        (set (make-local-variable var) (symbol-value var)))
      ;; update imenu-entries & insert tree widgets
      (bufbar--update-state)
      (set (make-local-variable 'bufbar--group) (bufbar--create-groups))
      (dolist (elt bufbar--group)
        (bufbar--insert-group-heading (car elt))
        (dolist (buf (cdr elt))
          (bufbar--insert buf)))
      ;; (run-hooks 'bufbar-init-hook)
      )
    ;; if `bufbar' is called with `new-name' in lisp code, just
    ;; return the buffer created; otherwise, display the buffer ...
    (if new-name
        bufbar-buffer
      (setq win (display-buffer
                 bufbar-buffer
                 bufbar-display-action))
      (if (not win)
          (message "Imenu tree failed to create window.")
        (set-window-dedicated-p win t)
        (with-selected-window win
          (unless (run-hook-with-args-until-success
                   'bufbar-inhibit-functions bufbar-current-buffer)
            (run-hook-with-args-until-failure
             'bufbar-updating-functions))
          (bufbar-highlight-line)))
      (run-hooks 'bufbar-after-update-hook))))

(defun bufbar-highlight-line ()
  (and (boundp 'hl-line-mode)
       (or hl-line-mode (hl-line-mode 1))
       (hl-line-highlight)))

(defun bufbar--add (buf &optional group)
  (or group (setq group (funcall bufbar-group-function buf)))
  (goto-char (point-min))
  (let (g created b w wp stop)
    (while (and (not stop)
                (or (not (eobp))
                    (string-equal g group)))
      (setq w  (widget-at)
            wp (widget-get w :parent))
      (cond
       ;; group headings
       ((bufbar-heading-widget-p w)
        (setq g (widget-get w :tag))
        (if (string-equal g group)
            (and (setq created t)
                 (forward-line))
          (if (funcall (car bufbar-sort-function) group g)
              (progn
                (or created (bufbar--insert-group-heading group))
                (bufbar--insert buf)
                (setq stop t))
            (forward-line))))
       ;; buffer nodes
       ((and (tree-widget-p wp) (widget-get wp :buffer))
        (if (not (string-equal g group))
            (goto-char (widget-get wp :to))
          (setq b (widget-get wp :buffer))
          (if (not (buffer-live-p b))
              (widget-delete wp)
            (if (funcall (cdr bufbar-sort-function) buf b)
                (progn
                  (bufbar--insert buf)
                  (setq stop t))
              (forward-line)))))
       (t
        (if (not (eobp))
            (forward-line)
          (when (string-equal g group)
            (bufbar--insert buf)
            (setq stop t))))))))

(defun bufbar--update-1 ()
  (or (eq major-mode 'bufbar-mode)
      (error "Not in bufbar-mode buffers"))
  ;; update buffers *managed* by current buffer
  (bufbar--update-state)
  (let* ((old-group bufbar--group)
         (new-group (bufbar--create-groups))
         (group-list (mapcar #'car new-group))
         (buffer-list (-flatten (mapcar #'cdr new-group)))
         old-agl ; old alist of (group headings . position)
         old-bl
         ;; other temp variables
         b  ; the buffer associated with widget
         w  ; widget at point
         wp ; the parent widget of w
         ng ; nearest group heading
         )
    ;; step 1. delete nodes & scan for a list of remaining
    ;; groups/buffers
    (goto-char (point-min))
    (while (not (eobp))
      ;; always at the line beginning
      (setq w  (widget-at)
            wp (widget-get w :parent))
      (cond
       ;; buffer nodes
       ((and (tree-widget-p wp) (widget-get wp :buffer))
        (setq b (widget-get wp :buffer))
        (if (and (buffer-live-p b)
                 (memq b buffer-list)
                 ;; delete node if its group changes
                 (string-equal
                  (car (cl-rassoc b new-group :test #'memq))
                  (car (cl-rassoc b old-group :test #'memq)))
                 ;; delete node if buffer name changes
                 (string-equal (widget-get (widget-get wp :node) :tag)
                               (buffer-name b)))
            (progn
              ;; update entries
              (when (not (zerop (car (buffer-local-value 'bufbar--state b))))
                (bufbar--set-state 0 b)
                (bufbar--refresh wp)
                ;; (message "imenu tree in %s updated" (buffer-name b))
                )
              (push b old-bl)
              (goto-char (widget-get wp :to)))
          (widget-delete wp)))
       ;; group headings
       ((bufbar-heading-widget-p w)
        (if (member (widget-get w :tag) group-list)
            (progn
              (push (cons (widget-get w :tag) (point)) old-agl)
              (forward-line))
          (widget-delete w)))
       (t
        (forward-line))))
    (and old-agl (setq old-agl (reverse old-agl)))
    ;; step 2. insert new group headings in reverse order
    (dolist (g (reverse group-list))
      (when (not (assoc-default g old-agl))
        ;; find the first elt s.t. sort(g, elt) => t
        (setq ng (--first
                  (not (funcall (car bufbar-sort-function) it g))
                  (mapcar #'car old-agl)))
        (if ng
            (goto-char (assoc-default ng old-agl))
          (goto-char (point-max)))
        (add-to-list 'old-agl (cons g (point)) t)
        (bufbar--insert-group-heading g)))
    ;; step 3. add buffer nodes successively in reverse order
    (dolist (b (reverse buffer-list))
      (when (not (memq b old-bl))
        (bufbar--add b (car (cl-rassoc b new-group :test #'memq)))))
    ;; step 4. save current groups
    (setq bufbar--group new-group)
    ))

(defmacro bufbar-inhibit-by-project (proj)
  `(lambda (buf)
     (not
      (string-equal
       ,proj
       (with-current-buffer buf (projectile-project-name))))))

(defun bufbar-display-project-buffers (&optional buf)
  "Display a sidebar of buffers in the same project with buffer BUF."
  (interactive)
  (let ((proj-name (with-current-buffer (or buf (current-buffer))
                     (projectile-project-name))))
    (if (string-equal proj-name "-")
        (message "The buffer has no project.")
      (let ((bufbar--use-separate-buffer
             (concat "*bufbar: " proj-name "*"))
            (bufbar-inhibit-functions
             `(bufbar-inhibit-default
               ,(bufbar-inhibit-by-project proj-name)))
            ;; (bufbar-display-action nil)
            )
        (display-buffer
         (or (get-buffer bufbar--use-separate-buffer)
             (bufbar bufbar--use-separate-buffer))
         bufbar-display-action)))))

(defun bufbar-update ()
  "Update visible `bufbar-mode' buffers."
  (setq bufbar-current-buffer (current-buffer)
        bufbar-current-point  (point))
  (mapc
     (lambda (buf)
       ;; if the sidebar is in special use, do not update
       (unless (and (eq buf (window-buffer
                             (ignore-errors
                               (window-at
                                (cadr (mouse-position))
                                (cddr (mouse-position))
                                (car (mouse-position))))))
                    (memq last-command '(mwheel-scroll)))
         (with-current-buffer buf
           (when bufbar-update-flag
             ;; (run-hook-with-args 'bufbar-before-update-hook
             ;;                     src-buf src-pos)
             (save-mark-and-excursion
               (bufbar--update-1))))
         (with-selected-window (get-buffer-window buf)
           (unless (run-hook-with-args-until-success
                    'bufbar-inhibit-functions bufbar-current-buffer)
             (run-hook-with-args-until-failure
              'bufbar-updating-functions))
           (bufbar-highlight-line))))
     ;; get a list of visible sidebars
     (cl-delete-duplicates
      (cl-delete-if-not
       (lambda (buf)
         (with-current-buffer buf
           (eq major-mode 'bufbar-mode)))
       (-flatten
        (mapcar (lambda (f)
                  (and (frame-visible-p f)
                       (mapcar (lambda (w) (window-buffer w))
                               (window-list f 'omit))))
                (if (boundp 'bufbar-update-all-frames)
                    (frame-list)
                  (list (selected-frame))))))))
  (run-hooks 'bufbar-after-update-hook))

(defun bufbar-toggle-updating (arg)
  "Toggle auto-updating of current buffer when no argument is
passed. Otherwise, turn on auto-updating when ARG is 1, and any
other value turns off auto-updating."
  (interactive "P")
  (or (eq major-mode 'bufbar-mode)
      (error "Not bufbar buffer"))
  (if (null arg)
      (setq bufbar-update-flag
            (not bufbar-update-flag))
    (setq arg (prefix-numeric-value arg))
    (if (eq arg 1)
        (setq bufbar-update-flag t)
      (setq bufbar-update-flag nil)))
  ;; updating mode line
  )

(defun bufbar-show-or-hide (&optional name)
  "Toggle displaying of the default bufbar buffer of current frame.
Does not update the content."
  (interactive "P")
  (let* ((b (or name (frame-parameter nil 'bufbar-buffer-name)))
         (buf (get-buffer b))
         win)
    (if (not buf)
        (if name
            (message "No bufbar buffer named %s." b)
          (bufbar))
      (setq win (get-buffer-window buf))
      (if win
          (delete-window win)
        (display-buffer buf bufbar-display-action)))))

(defun bufbar-jump-to-buffer (node &rest ignore)
  "Display buffer. If current buffer is *bufbar*, display in
a pop-up buffer."
  (interactive)
  (let* ((target-buf (widget-get (widget-get node :parent) :buffer))
         src-buf win)
    (and global-mark-ring
         (setq src-buf (marker-buffer (car global-mark-ring)))
         (setq win (get-buffer-window src-buf)))
    (if (buffer-live-p target-buf)
        (if (eq src-buf (current-buffer))
            (pop-to-buffer target-buf)
          (if win
              (with-selected-window win
                (switch-to-buffer target-buf)
                (push-mark))
            (pop-to-buffer target-buf)))
      (message "Buffer has been killed."))))

(defun bufbar-jump-to-entry (node &rest ignore)
  (let ((marker (widget-get node :imenu-marker)))
    (and marker
         (with-selected-window (display-buffer (marker-buffer marker))
           (goto-char marker)
           (bufbar-highlight-line)))))

(defun bufbar-jump ()
  (interactive)
  (let ((w (bufbar-widget-current-line)))
    (if (widget-get (widget-get w :parent) :buffer)
        (bufbar-jump-to-buffer w)
      (bufbar-jump-to-entry w))))

(define-key bufbar-mode-map "\C-o" 'bufbar-display)

(defun bufbar-refresh-at-point ()
  "Refresh current tree by calling `imenu--make-index-alist'.
It is the only way to retrieve latest imenu entries when
auto-updating is turned off, and also useful when entries cannot
be updated automatically due to `imenu-auto-rescan' or
`imenu-auto-rescan-maxout'."
  (interactive)
  (let ((tree (bufbar-root-widget-at-point)) (p (point)) buf)
    (if (or (not tree) (not (tree-widget-p tree)))
        (message "No tree at point!")
      (setq buf (widget-get tree :buffer))
      (if (not (buffer-live-p buf))
          (message "Buffer %s has been killed." buf)
        (with-current-buffer buf
          (setq imenu--index-alist nil)
          (ignore-errors (imenu--make-index-alist t))
          (bufbar--set-state 0))
        (bufbar--refresh tree)
        (goto-char p)))))

(defun bufbar-delete-at-point ()
  "Delete current tree and kill the associated buffer."
  (interactive)
  (let ((tree (bufbar-root-widget-at-point)) buf name)
    (if (or (not tree) (not (tree-widget-p tree)))
        (message "No tree at point!")
      (setq buf (widget-get tree :buffer))
      (widget-delete tree)
      (and (buffer-live-p buf)
           (setq name (buffer-name buf))
           (kill-buffer buf)
           (message "Buffer %s has been killed." name)))))

(defun bufbar-expand-at-point (arg)
  "Expand current tree."
  (interactive "p")
  (let ((tree (bufbar-root-widget-at-point))
        (w (bufbar-widget-current-line))
        m buf pos)
    (if (or (not tree) (not (tree-widget-p tree)))
        (message "No tree widget is found at current position!")
      (and (setq m (widget-get w :imenu-marker))
           (setq buf (marker-buffer m)
                 pos (marker-position m)))
      (bufbar-expand tree arg)
      (when (and buf pos)
        (goto-char (widget-get tree :from))
        (bufbar-goto-entry)))))

(defun bufbar-expand (tree arg)
  "Expand the tree widget TREE to depth ARG.
Always return to the root position of TREE, thus the original
position has to be saved and restored manually if needed."
  (or (numberp arg) (error "ARG should be a number."))
  (goto-char (widget-get tree :from))
  (if (<= arg 0)
      (and (widget-get tree :open) (widget-apply-action tree))
    (or (widget-get tree :open) (widget-apply-action tree))
    (let (w stop wp depth)
      (forward-line)
      (while (and (not stop) (not (eobp)))
        (setq w (bufbar-widget-current-line))
        (cond
         ((or (bufbar-heading-widget-p w)
              (bufbar-buffer-node-p w))
          (setq stop t))
         ((widget-get w :alist)
          (setq wp    (widget-get w :parent)
                depth (widget-get w :depth))
          (and (< depth arg)
               (not (widget-get wp :open))
               (widget-apply-action wp))
          (and (not (< depth arg))
               (widget-get wp :open)
               (widget-apply-action wp))
          (forward-line))
         (t
          (forward-line))))))
  (goto-char (widget-get tree :from)))

(defun bufbar-expand-1 ()
  "Open current tree if it is closed."
  (let ((tree (bufbar-root-widget-at-point)))
    (if (or (not tree) (not (tree-widget-p tree)))
        nil
      (or (widget-get tree :open)
          (bufbar-expand tree 1)))))

(defun bufbar-up-level ()
  "Go up a level."
  (interactive)
  (let* ((w  (bufbar-widget-current-line t))
         (wp (widget-get w :parent)))
    (if wp
        (goto-char (widget-get wp :from))
      (message "%s" "No up level."))))

;; ===================================================================

;; Movement (No modification)

(defun bufbar-widget-current-line (&optional parent)
  "Return the widget (tree widget if PARENT is t) at current line."
  (let ((w (widget-at (1- (line-end-position)))))
    ;; return the tree widget if there is one
    (and parent
         (if (or (widget-get w :alist)
                 (bufbar-buffer-node-p w)
                 (memq (widget-type w)
                       '(tree-widget-close-icon
                         tree-widget-open-icon
                         tree-widget-empty-icon
                         tree-widget-leaf-icon)))
             (setq w (widget-get w :parent))))
    w))

(defun bufbar-root-widget-at-point ()
  "Return the outermost tree widget at *current line*.
Tree widget or ..."
  (let ((w (bufbar-widget-current-line t)))
    (when w
      (while (widget-get w :parent)
        (setq w (widget-get w :parent))))
    w))

(defun bufbar-next-tree (&optional direction group)
  "Go to next tree forward or backward when DIRECTION is positive
or negative. If GROUP is non-nil, go to next tree within current
group."
  (interactive "p")
  (let ((dir (if (and direction (< direction 0)) -1 1))
        stop w)
    (save-mark-and-excursion
      (forward-line dir)
      (while (and (not stop) (not (bobp)) (not (eobp)))
        (setq w (bufbar-widget-current-line))
        (cond
         ((bufbar-buffer-node-p w)
          (setq stop (point)))
         ((and group (bufbar-heading-widget-p w))
          (setq stop t))
         (t
          (forward-line dir)))))
    (and (number-or-marker-p stop) (goto-char stop))))

(defun bufbar-previous-tree (&optional group)
  "Go to next tree backward."
  (interactive)
  (bufbar-next-tree -1 (or group nil)))

;; ===================================================================

(defun bufbar--find-path (n alist path)
  "Return (path . distance) of position N in ALIST with the suffix PATH."
  (let (p   ; minimum path
        d   ; minimum distance
        pd  ; (cp . cd) value
        elt ; current element
        ;; cp  ; path of current elt
        cd) ; distance of current elt
    (while (and (car alist)
                (or (not d)
                    (not (zerop d))))
      (setq elt (car alist)
            alist (cdr alist))
      (if (and (cdr-safe elt) (atom (cdr-safe elt)))
          ;; if elt is a true cons (label . marker)
          (progn
            (setq cd (- n (cdr elt)))
            (and (>= cd 0)
                 (or (not d) (< cd d))
                 (setq p (cons (car elt) path)
                       d cd)))
        ;; if the head of sub-list has position
        (when (consp (car elt))
          (setq cd (- n (cdar elt)))
          (and (>= cd 0)
               (or (not d) (< cd d))
               (setq p (cons (caar elt) path)
                     d cd)))
        ;; if sub-list have more than one element
        (when (cdr elt)
          (setq pd (bufbar--find-path
                    n (cdr elt) (cons
                                 (if (consp (car elt)) (caar elt) (car elt))
                                 path))
                cd (cdr pd)) ; either cd >= 0 or cd is nil
          (and cd
               (or (not d) (< cd d))
               (setq p (car pd)
                     d cd))))
      )
    (cons p d)))

(defun bufbar-goto-buffer ()
  "Go to buffer BUF. Return the position if BUF is found, and nil
if not found."
  (let ((buf bufbar-current-buffer)
        stop1 w)
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (and (not stop1) (not (eobp)))
        (setq w (bufbar-widget-current-line t))
        (if (and (tree-widget-p w)
                 (eq buf (widget-get w :buffer)))
            (setq stop1 (point))
          (bufbar-next-tree 1))))
    (and stop1 (goto-char stop1))))

(defun bufbar-goto-entry ()
  "Go to the entry corresponding to position POS of buffer BUF.
Return the position if entry is found and nil if not found."
  (let ((buf bufbar-current-buffer)
        (pos bufbar-current-point)
        pd path d stop w wp marker)
    (setq pd (bufbar--find-path
              pos
              (bufbar--create-index buf)
              nil))
    (setq path (reverse (car pd))
          d (cdr pd))
    (setq w (bufbar-widget-current-line t))
    (save-mark-and-excursion
      (when (and path d
                 (eq buf (widget-get w :buffer))
                 (widget-get w :open))
        ;; (or (widget-get w :open) (widget-apply-action w))
        (forward-line)
        (while (and (not stop) (not (eobp)))
          (setq w (bufbar-widget-current-line))
          (cond
           ((bufbar-buffer-node-p w)
            (setq stop t))
           ((equal (car path) (widget-get w :tag))
            (setq marker (widget-get w :imenu-marker))
            (if (not (widget-get w :alist))
                (if (cdr path)
                    (forward-line)
                  (if (and marker
                           (not (= (- pos marker) d)))
                      (forward-line)
                    (setq stop (line-beginning-position))))
              (setq wp (widget-get w :parent))
              ;; (or (widget-get wp :open) (widget-apply-action wp))
              (if (not (widget-get wp :open))
                  ;; don't open tree and just stop searching
                  (setq stop (point))
                ;; if the tree is open
                (if (cdr path)
                    (and (pop path) (forward-line))
                  (if (and marker
                           (not (= (- pos marker) d)))
                      (forward-line)
                    (setq stop (line-beginning-position)))))))
           (t
            (forward-line))))
        (when (number-or-marker-p stop)
          (beginning-of-line)
          (skip-chars-forward "^[:word:]")
          (setq stop (point)))))
    (if (number-or-marker-p stop) (goto-char stop) nil)))

(defun bufbar-display-entry-in-separate-buffer (buf pos)
  "Display position POS of buffer BUF."
  (interactive "P")
  (unless (run-hook-with-args-until-success
           'bufbar-inhibit-functions buf)
    (let (win (get-buffer-window "*imenu tree entries*"))
      (if win
          (with-selected-window win
            (let ((inhibit-read-only t))
              (erase-buffer))
            (remove-overlays)
            (bufbar--insert buf)
            (widget-apply-action (widget-get (widget-at) :parent))
            (bufbar-goto-entry))))))

;; ===================================================================

;; Some redefinition of `tree-mode'

(defun tree-mode-opened-tree (tree)
  "Find all opened tree.
Return the tag list with the same depth."
  (if (widget-get tree :open)
      (cons (widget-get (tree-widget-node tree) :tag)
            (delq nil
                  (mapcar (lambda (child)
                            (and (tree-widget-p child)
                                 (tree-mode-opened-tree child)))
                          (widget-get tree :children))))))

(defun tree-mode-open-tree (tree path)
  "Open tree using tag list given by `tree-mode-opened-tree'."
  (when path
    (if (not (widget-get tree :open))
        (widget-apply-action tree))
    (setq path (cdr path))
    (and path
         (mapc (lambda (child)
                 (and (tree-widget-p child)
                      (let* ((tag (widget-get (tree-widget-node child) :tag))
                             (subpath (assoc tag path)))
                        (if subpath
                            (tree-mode-open-tree child subpath)))))
               (widget-get tree :children)))))

;;; buffer-sidebar.el ends here
