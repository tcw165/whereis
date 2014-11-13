;; Copyright (C) 2014, 2015
;;
;; Author: boyw165
;; Compatibility: GNU Emacs 24.3+
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

;; GNU library.
(require 'font-lock)
(require 'hl-line)
(require 'linum)
(require 'tabulated-list)

(defgroup whereis-ui nil
  "Whereis package's UI."
  :group 'whereis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signle Candidate Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar whereis-ui-candidate-mode-hook '((lambda ()
                                           (linum-mode 1)
                                           (setq-local hl-line-sticky-flag t)
                                           (hl-line-mode 1))))

(defvar whereis-ui-candidate-mode-map
  (let ((map (make-sparse-keymap))
        (usr-key (car (where-is-internal 'whereis-goto-symbol))))
    (suppress-keymap map t)
    (define-key map [?q] 'whereis-ui-back-to-source-window)
    (define-key map [escape] 'whereis-ui-back-to-source-window)
    (define-key map [?e] 'whereis-ui-open-candidate)
    (define-key map [return] 'whereis-ui-open-candidate)
    (and (arrayp usr-key)
         (define-key map usr-key 'whereis-ui-open-candidate))
    map))

(define-minor-mode whereis-ui-candidate-mode
  "Minor mode for preview buffer."
  :lighter " whereis:cand"
  :group 'whereis-ui
  (if whereis-ui-candidate-mode
      (progn
        (use-local-map whereis-ui-candidate-mode-map))
    (use-local-map nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface whereis-ui-hlline-face
  '((t (:background "cyan" :weight bold :slant italic)))
  "Face for highlight line in candidates preview window."
  :group 'whereis-face)

(defface whereis-ui-odd-candidate-face
  '((t (:background "azure")))
  "Face for odd candidate in candidates preview window."
  :group 'whereis-face)

(defface whereis-ui-even-candidate-face
  '((t (:background "cornsilk")))
  "Face for even candidate in candidates preview window."
  :group 'whereis-face)

(defcustom whereis-ui-visual-src-max 48
  "Maximum length of visualization of `:src' attribute of a candidate."
  :type 'integer
  :group 'whereis-ui)

(defvar whereis-ui-candidates-timer nil
  "An idle time to show preview of selected candidate in multiple candidates
mode.")

(defvar whereis-ui-candidates-mode-map
  (let ((map (make-sparse-keymap))
        (usr-key (car (where-is-internal 'whereis-goto-symbol))))
    (suppress-keymap map t)
    (define-key map [?e] 'whereis-ui-open-candidate)
    (define-key map [return] 'whereis-ui-open-candidate)
    (and (arrayp usr-key)
         (define-key map usr-key 'whereis-ui-open-candidate))
    (define-key map [?q] 'whereis-ui-back-to-source-window)
    (define-key map [escape] 'whereis-ui-back-to-source-window)
    (define-key map [up] 'whereis-ui-prev-candidate)
    (define-key map [down] 'whereis-ui-next-candidate)
    (dolist (key (list [left] [right]
                       [mouse-1] [mouse-2] [mouse-3]
                       [down-mouse-1] [down-mouse-2] [down-mouse-3]
                       [drag-mouse-1] [drag-mouse-2] [drag-mouse-3]
                       [double-mouse-1] [double-mouse-2] [double-mouse-3]
                       [triple-mouse-1] [triple-mouse-2] [triple-mouse-3]))
      (define-key map key 'whereis-undefined))
    map))

(defvar whereis-ui-candidates-mode-hook '((lambda ()
                                            (hl-line-mode 1)
                                            (hl-line-unhighlight))))

(defun whereis-ui-tabulated-list-print (id cols)
  "Copy function of `tabulated-list-print' with additional features, like row
fontification."
  (let ((beg   (point))
        (x     (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (if (> tabulated-list-padding 0)
        (insert (make-string x ?\s)))
    (dotimes (n ncols)
      (setq x (tabulated-list-print-col n (aref cols n) x)))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id)
    (put-text-property beg (point) 'tabulated-list-entry cols)
    ;; Also add background face.
    ;; (add-face-text-property beg (point)
    ;;                         (if (= 0 (% (1+ id) 2))
    ;;                             'whereis-ui-even-candidate-face
    ;;                           'whereis-ui-odd-candidate-face)
    ;;                         t)
    ))

(defun whereis-ui-preview-candidates ()
  (when whereis-ui-candidates-mode
    (let* ((candidate (nth (tabulated-list-get-id) whereis-ui-candidates))
           (src (plist-get candidate :src))
           (offset (plist-get candidate :offset))
           (linum (plist-get candidate :linum)))
      ;; Convert line number and offset to string if necessary.
      (when (numberp offset)
        (setq offset (number-to-string offset)))
      (when (numberp linum)
        (setq linum (number-to-string linum)))
      ;; Start with preview window 2.
      (whereis-ui-with-preview-window-2
       ;; Insert context.
       (erase-buffer)
       (if (file-exists-p src)
           ;; File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (progn
             (insert-file-contents-literally src)
             (whereis-ui-apply-major-mode src)
             ;; Line number.
             (linum-mode 1)
             ;; Prompt the absolute file path and line number.
             (message "%s:%s"
                      (propertize src
                                  'face 'whereis-ui-filepath-face)
                      (propertize (or offset linum)
                                  'face 'whereis-ui-number-face)))
         ;; Document ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (fundamental-mode)
         (insert src))
       ;; Goto position.
       (whereis-ui-recenter-candidate candidate)
       ;; Highlight keywords.
       (whereis-ui-fontify-candidate candidate)
       ;; Highlight line.
       (setq-local hl-line-sticky-flag t)
       (hl-line-mode 1)))))

(defun whereis-ui-prev-candidate ()
  (interactive)
  (when whereis-ui-candidates-mode
    (let ((old-id (tabulated-list-get-id)))
      (catch 'found
        (while (not (bobp))
          (beginning-of-line 0)
          (when (= (- old-id 2) (tabulated-list-get-id))
            (beginning-of-line 2)
            (throw 'found nil)))))))

(defun whereis-ui-next-candidate ()
  (interactive)
  (when whereis-ui-candidates-mode
    (let ((old-id (tabulated-list-get-id)))
      (ignore-errors
        (goto-char (catch 'found
                     (save-excursion
                       (while (not (eobp))
                         (beginning-of-line 2)
                         (and (/= old-id (tabulated-list-get-id))
                              (throw 'found (point)))))))))))

(define-minor-mode whereis-ui-candidates-mode
  "Minor mode for multiple candidates preview buffer."
  :lighter " whereis:cand(s)"
  :group 'whereis-ui
  (if whereis-ui-candidates-mode
      (progn
        ;; Disable cursor.
        (setq cursor-type nil)
        ;; Keymap.
        (use-local-map whereis-ui-candidates-mode-map)
        ;; Highlight line.
        (setq-local hl-line-face 'whereis-ui-hlline-face)
        (setq-local hl-line-sticky-flag nil)
        ;; Enable idle timer.
        (setq whereis-ui-candidates-timer (run-with-idle-timer
                                           whereis-idle-delay
                                           t 'whereis-ui-preview-candidates)))
    ;; Keymap.
    (use-local-map nil)
    ;; Disable idle timer.
    (when whereis-ui-candidates-timer
      (cancel-timer whereis-ui-candidates-timer)
      (setq whereis-ui-candidates-timer nil))
    ;; Kill `whereis-ui-preview-buffer-2'.
    (when (buffer-live-p whereis-ui-preview-buffer-2)
      (kill-buffer whereis-ui-preview-buffer-2)
      (setq whereis-ui-preview-buffer-2 nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface whereis-ui-number-face
  '((t (:foreground "maroon1")))
  "Face for number in candidates preview window."
  :group 'whereis-face)

(defface whereis-ui-filepath-face
  '((t (:foreground "blue" :underline t)))
  "Face for filepath in candidates preview window."
  :group 'whereis-face)

(defvar whereis-ui-preview-buffer-1 nil
  "Definition buffer 1, which is the bottom one. It's mainly useful for
`whereis-symbol-mode'.")

(defvar whereis-ui-preview-buffer-2 nil
  "Definition buffer 1, which is the upper one. It's mainly useful for
`whereis-goto-symbol'.")

(defvar whereis-ui-preview-window nil
  "Definition window.")

(defvar whereis-ui-candidates nil
  "Cache the return value from back-end with `:candidates' command.")

(defvar whereis-ui-candidates-index nil)

(defun whereis-ui-setup-preview-window ()
  "Sometimes the preview window will be altered by other features.
e.g. `bookmark-set'"
  (setq whereis-ui-preview-window nil)
  (and (buffer-live-p whereis-ui-preview-buffer-1)
       (setq whereis-ui-preview-window (get-buffer-window
                                        whereis-ui-preview-buffer-1))))

(defmacro whereis-ui-with-preview-window-1 (&rest body)
  "Bottom window and buffer showing the definition of symbol."
  (declare (indent 0) (debug t))
  `(progn
     (unless (buffer-live-p whereis-ui-preview-buffer-1)
       (setq whereis-ui-preview-buffer-1 (get-buffer-create "*Whereis-Symbol-Preview*")))
     (unless (window-live-p whereis-ui-preview-window)
       ;; Try to rescue preview window.
       (or (whereis-ui-setup-preview-window)
           ;; If it is not working, create a new one.
           (let* ((win (frame-root-window))
                  (height (/ (window-height win) -3)))
             (and win height
                  (setq whereis-ui-preview-window (split-window win height 'below))))))
     ;; Bind definition buffer to definition window.
     (set-window-buffer whereis-ui-preview-window whereis-ui-preview-buffer-1)
     (with-selected-window whereis-ui-preview-window
       (with-current-buffer whereis-ui-preview-buffer-1
         (setq buffer-read-only nil
               ;; Disable undo list.
               buffer-undo-list t)
         ,@body
         (setq buffer-read-only t)
         (set-buffer-modified-p nil)))))

(defmacro whereis-ui-with-preview-window-2 (&rest body)
  "Top window and buffer showing the definition of symbol."
  (declare (indent 0) (debug t))
  `(progn
     (unless (buffer-live-p whereis-ui-preview-buffer-2)
       (setq whereis-ui-preview-buffer-2 (get-buffer-create "*Whereis-Symbol-Preview-2*")))
     ;; Bind definition buffer to definition window.
     (set-window-buffer whereis-source-window whereis-ui-preview-buffer-2)
     (with-selected-window whereis-source-window
       (with-current-buffer whereis-ui-preview-buffer-2
         (setq buffer-read-only nil
               ;; Disable undo list.
               buffer-undo-list t)
         ,@body
         (setq buffer-read-only t)
         (set-buffer-modified-p nil)))))

(defun whereis-ui-toggle-symbol-preview-window (toggle)
  "Display or hide the `whereis-ui-preview-buffer-1' and `whereis-ui-preview-window'."
  (if (> toggle 0)
      (whereis-ui-with-preview-window-1)
    (when (windowp whereis-ui-preview-window)
      (delete-window whereis-ui-preview-window))
    (when (bufferp whereis-ui-preview-buffer-1)
      (kill-buffer whereis-ui-preview-buffer-1))
    (setq whereis-ui-preview-buffer-1 nil
          whereis-ui-preview-window nil)))

(defun whereis-ui-is-multiple-candidates ()
  (> (length whereis-ui-candidates) 1))

(defun whereis-ui-apply-major-mode (file)
  "Apply current buffer a major mode refer to FILE name."
  (catch 'applied
    (dolist (mode auto-mode-alist)
      (when (and (not (null (cdr mode)))
                 (string-match (car mode) file))
        (let* ((mode-func (cdr mode))
               (mode-hook (intern-soft (format "%s-hook"
                                               mode-func)))
               (hooks (symbol-value mode-hook)))
          ;; Disable ???-mode-hook temporarily so that redundant minor-modes can
          ;; be skipped.
          (set mode-hook nil)
          (funcall mode-func)
          ;; Restore ???-mode-hook.
          (set mode-hook hooks)
          (throw 'applied t)))
      nil)))

(defun whereis-ui-recenter-candidate (candidate)
  "Recenter the buffer refer to CANDIDATE."
  (let ((offset (plist-get candidate :offset))
        (linum (plist-get candidate :linum))
        (colnum (plist-get candidate :colnum)))
    ;; Check and convert line number.
    (when (stringp linum)
      (setq linum (string-to-int linum)))
    (cond
     ;; Offset ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((integerp offset)
      (goto-char offset))
     ;; Linum & Colnum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((integerp linum)
      (goto-char (point-min))
      (if colnum
          (progn
            (beginning-of-line linum)
            (forward-char (1- colnum)))
        (end-of-line linum)))
     (t (goto-char (point-min))))
    (recenter 3)))

(defun whereis-ui-fontify-keyword (keyword)
  "Use `font-lock' to add KEYWORD and fontify the buffer instantly as well."
  (when keyword
    (let ((linum (line-number-at-pos))
          (match-num (if (string-match "(\?\\([1-9]\\):.*)" keyword)
                         (string-to-int (match-string 1 keyword))
                       0)))
      (font-lock-add-keywords
       nil
       `(((lambda (limit)
            (catch 'found
              ;; Search THING only at LINUM line.
              (while (re-search-forward ,keyword limit t)
                (and (= (line-number-at-pos) ,linum)
                     (throw 'found t)))))
          ,match-num 'whereis-highlight-face prepend))
       'append)))
  ;; Fontify the buffer instantly.
  (save-excursion
    (font-lock-fontify-region (line-beginning-position) (line-end-position) nil)))

(defun whereis-ui-abbreviate-src (src)
  ;; Convert multiple lines into one if necessary.
  (setq src (car (split-string src "\n")))
  ;; Shrink length of the string if necessary.
  (when (> (length src) whereis-ui-visual-src-max)
    (setq src (concat "..."
                      (substring src (- (length src) whereis-ui-visual-src-max)))))
  src)

(defun whereis-ui-fontify-candidate (candidate)
  "Fontify keyword at current line, it should be called just after
`whereis-ui-recenter-candidate' function."
  (whereis-ui-fontify-keyword (plist-get candidate :keyword)))

(defun whereis-ui-create-snapshot (candidate)
  "Return a fontified snapshot of the CANDIDATE."
  (let ((src (plist-get candidate :src))
        (snapshot (plist-get candidate :snapshot))
        (keyword (plist-get candidate :keyword)))
    (when (and (stringp src) (stringp snapshot))
      (with-temp-buffer
        (insert snapshot)
        (when (file-exists-p src)
          (whereis-ui-apply-major-mode src))
        (whereis-ui-fontify-keyword keyword)
        (buffer-string)))))

(defun whereis-ui-back-to-source-window ()
  "Keymap function to back to source window."
  (interactive)
  (when (buffer-live-p whereis-ui-preview-buffer-2)
    (kill-buffer whereis-ui-preview-buffer-2)
    (setq whereis-ui-preview-buffer-2 nil))
  (when (window-live-p whereis-source-window)
    (select-window whereis-source-window)))

(defun whereis-ui-open-candidate ()
  "Keymap function to open file and goto the line for the situation of single
candidate."
  (interactive)
  (let* ((candidate (nth (if whereis-ui-candidates-mode
                             (tabulated-list-get-id)
                           0)
                         whereis-ui-candidates))
         (src (plist-get candidate :src)))
    (when (and (stringp src) (file-exists-p src)
               (window-live-p whereis-source-window))
      (select-window whereis-source-window)
      ;; Open file.
      (unless (string= src (buffer-file-name (window-buffer)))
        (find-file src))
      ;; Move point and recenter.
      (whereis-ui-recenter-candidate candidate))))

(defun whereis-ui-show-candidate ()
  "Show single candidate prompt."
  (let* ((candidate (nth 0 whereis-ui-candidates))
         (src (plist-get candidate :src))
         (mode (plist-get candidate :mode)))
    (when (and candidate (stringp src))
      (whereis-ui-with-preview-window-1
       (kill-all-local-variables)
       (remove-overlays)
       ;; Insert content and set major mode.
       (erase-buffer)
       (if (file-exists-p src)
           ;; File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (let ((buf (get-file-buffer src)))
             (if buf
                 ;; In case of the buffer is modified.
                 (insert (with-current-buffer buf
                           (buffer-string)))
               ;; Get content from file directly.
               (insert-file-contents-literally src))
             ;; Change major-mode refer to file name.
             (whereis-ui-apply-major-mode src))
         ;; Documentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (insert src))
       ;; Apply major mode if any.
       (and mode (funcall mode))
       ;; Move point and recenter.
       (whereis-ui-recenter-candidate candidate)
       ;; Highlight word.
       (whereis-ui-fontify-candidate candidate)
       ;; Minor mode.
       (whereis-ui-candidate-mode 1)))))

(defun whereis-ui-show-candidates (&optional select-index)
  "Show multiple candidates prompt."
  ;; TODO: Check if there're candidates need to be showed immediately.
  (whereis-ui-with-preview-window-1
   ;; Insert condidates.
   (kill-all-local-variables)
   (remove-overlays)
   (erase-buffer)
   ;; Content
   (let ((id 0))
     (tabulated-list-mode)
     (setq tabulated-list-format '[("Candidate" 0)]
           tabulated-list-entries nil)
     (dolist (candidate whereis-ui-candidates)
       (let ((src (plist-get candidate :src))
             (offset (plist-get candidate :offset))
             (linum (plist-get candidate :linum))
             (snapshot (whereis-ui-create-snapshot candidate)))
         ;; Convert number to string if necessary.
         (when (numberp offset)
           (setq offset (number-to-string offset)))
         (when (numberp linum)
           (setq offset (number-to-string linum)))
         ;; Create tabulated entry.
         (when src
           (push `(,id [,(concat
                          ;; File path.
                          (if (file-exists-p src)
                              (propertize (whereis-ui-abbreviate-src src)
                                          'face 'whereis-ui-filepath-face)
                            (propertize (whereis-ui-abbreviate-src src)
                                        'face 'font-lock-string-face))
                          ":"
                          ;; Line number or offset
                          (propertize (or offset linum "0")
                                      'face 'whereis-ui-number-face)
                          ": "
                          ;; Snapshot
                          (or snapshot ""))])
                 tabulated-list-entries)
           (setq id (1+ id)))))
     (setq tabulated-list-entries (nreverse tabulated-list-entries)
           tabulated-list-printer 'whereis-ui-tabulated-list-print)
     ;; Tabulate the buffer.
     (tabulated-list-print))
   ;; Recover the selection in the last session.
   (while (and (numberp select-index) (> select-index 0))
     (whereis-ui-next-candidate)
     (setq select-index (1- select-index)))
   ;; Minor mode.
   (whereis-ui-candidates-mode 1)))

(defun whereis-ui-helm-display-buffer (buffer)
  "Let `helm' always uses `whereis-ui-preview-window'."
  (if whereis-symbol-mode
      (progn
        ;; Find preview window if it is killed by someone. e.g. `bookmark-set'.
        (whereis-ui-with-preview-window-1)
        (select-window whereis-ui-preview-window)
        (set-window-buffer (selected-window) buffer))
    ;; Use helm default way.
    (helm-default-display-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-ui-previewer (command &optional candidates)
  "Show visualization of candidate(s) made by `whereis-symbol-mode'.

Commands:
---------
`:init'     - Initialize something (called once).
`:destroy'  - Destroy something (called once).
`:show'     - Show visualization.
`:go'       - Go to symbol's whence.
`:hide'     - Hide visualization.
`:update'   - Update visualization.

Example:
--------
  (defun some-frontend (command &rest args)
    (cond
     ((eq :init command) ...)
     ((eq :show command) ...))
     ((eq :go command) ...))
     ((eq :hide command) ...)
     ((eq :destroy command) ...))"
  (cond
   ((eq :init command)
    (whereis-ui-toggle-symbol-preview-window 1))
   ((eq :destroy command)
    (whereis-ui-toggle-symbol-preview-window -1))
   ((memq command '(:show :go :update))
    ;; Avoid rob `helm' buffer because frontend is rendered after a tiny delay.
    (unless (and (featurep 'helm) (helm-alive-p))
      ;; TODO: Lock everything in `whereis-ui-candidate-mode' and `whereis-ui-candidates-mode'.
      ;; (unless (eq command :go)
      ;;   (whereis-ui-toggle-symbol-preview-window 1))
      (when candidates
        (setq whereis-ui-candidates candidates
              whereis-index 0)
        (cond
         ;; Preview only ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq :show command)
          (if (whereis-ui-is-multiple-candidates)
              (whereis-ui-show-candidates)
            (whereis-ui-show-candidate)))
         ;; Goto ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq :go command)
          (if (whereis-ui-is-multiple-candidates)
              (progn
                (whereis-ui-show-candidates)
                (select-window whereis-ui-preview-window))
            (whereis-ui-open-candidate)))))))))

;; Integrate with `history' feature.
(when (featurep 'history)
  (add-to-list 'history-advised-before-functions 'whereis-ui-open-candidate t)
  (add-to-list 'history-advised-after-functions 'whereis-ui-open-candidate t))

;; Coordinate with `helm'.
(when (featurep 'helm)
  (custom-set-variables
   '(helm-display-function 'whereis-ui-helm-display-buffer)))

(provide 'whereis-ui)
;;; whereis-ui.el ends here
