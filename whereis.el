;;; whereis-symbol.el --- Smartly detect symbol at point and show its definition in another window.
;;
;; Copyright (C) 2014-2015
;;
;; Author: boyw165
;; Version: 20150722.1000
;; URL: https://github.com/boyw165/whereis
;; Package-Requires: ((emacs "24.4") (deferred "0.3.2"))
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
;; This is a FRAMEWORK which helps you to easily find symbol's definition:
;; * `whereis-symbol-mode':
;;   Detect symbol at point in a very short idle delay and show its definition
;;   in another window. All you need to do is to move your point and enjoy the
;;   convenience. (inspired by a commercial software, Source Insight)
;;
;; Support Lanuages:
;; ----------------
;; * Emacs Lisp
;; * Python (powered by YCMD)
;; * C/C++ (powered by YCMD)
;; * JavaScript (powered by Tern)
;;
;; TODO:
;; -----
;; * Press '?' to show help in preview window.
;; * Use `helm' for multiple candidates.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-01-27
;; * Support combined backends (both synchronous and asynchronous backends).
;;
;; 2014-12-11
;; * Improve performance by disable ???-mode-hook temporarily in the preview
;;   window.
;; * Support `dolist', `lambda', `defmacro' syntaxes of ELISP backend.
;;
;; 2014-10-15
;; * Support cases both of single or multiple candidates.
;; * Support preview of local variable like "let" for ELISP.
;; * Support single asynchronous backends by using `deferred' library.
;;
;; 2014-09-30
;; * Support cases both of single or multiple candidates.
;;
;; 2014-08-01
;; * Initial release which was inspired by `company-mode'.
;;   https://github.com/company-mode/company-mode
;;
;;; Code:

;; GNU library.
(eval-when-compile (require 'cl))

;; 3rd party library.
(require 'deferred)
(require 'helm)

;; Package's library.
(require 'whereis-ui)
(when load-file-name
  (let* ((dir (file-name-directory load-file-name))
         (bdir (concat dir "/backends")))
    (add-to-list 'load-path bdir)
    (mapc (lambda (file)
            (when (and (not (member file '("." "..")))
                       (string-match "\\.el$" file))
              (let ((library (car (split-string file "\\.el" t))))
                (when library
                  (require (intern library))))))
          (directory-files bdir))))

(defgroup whereis nil
  "An utility to prompt you the symbol's definition.")

(defgroup whereis-face nil
  "An utility to prompt you the symbol's definition."
  :group 'whereis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface whereis-highlight-face
  '((t (:background "gold" :foreground "black" :weight bold :height 1.3)))
  "Default face for highlighting keyword in definition window."
  :group 'whereis-face)

(defcustom whereis-backends `(whereis-elisp-backend
                              whereis-searchq-backend
                              whereis-shell-backend
                              whereis-js-tern-backend
                              whereis-ycmd-backend
                              ;; (whereis-ggtags-backend
                              ;;  whereis-etags-backend
                              ;;  whereis-files-backend)
                              ;; A dummy backend for orphan buffers.
                              whereis-dummy-backend)
  "The list of backends for the purpose of collecting candidates. The engine
will dispatch all the back-ends and pass specific commands in order. By passing
command and get return data from a backend, the engine gets information to show
the result in an isolated bottom window, minibuffer or popup a GUI dialog, etc.

Example:
--------
  (defun some-backend (command &rest args)
    (cond
     ((eq :init command) t)
     ((eq :symbol command)
      (and (member major-mode MAJOR_MODE_CANDIDATES)
           (whereis-thingatpt 'symbol))))
     ((eq :candidates command)
      (list STRING01 STRING02 STRING03 ...)))

Each backend is a function that takes numbers of arguments. The first argument is
the COMMAND requested from the enine. The remaining arguments depends on the 1st
argument, COMMAND.

Commands:
---------
1st time
`:init' --+-> `:symbol' ----> `:candidates' + `:tips' ---.
          ^                                              |
          |                  idle loop                   |
          '----------------------------------------------'

`:init': Called once for each buffer. The backend can check for external programs
and files and load any required libraries.

`:symbol': The back-end should return anything non-nil, nil or 'stop.
  * Return non-nil except `:stop' tells engine that it is for the current buffer.
    Then, the engine will stop iteration and ask backend for candidates refer
    to returned value; `:stop' tells engine to do nothing.
    Note: Return value will be saved in `whereis-symbols'.
  * Return nil tells engine to skip the backend and continue the iteration.

`:tips': The backend optionally return a STRING to show some information in the
  minibuffer.

`:candidates': The backend should return a CANDIDATE list or nil.
  The 2nd argument, SYMBOL, is the symbol (stored in `whereis-symbols');
  The 3rd argument, IS-SEARCH, is a boolean indicating search the SYMBOL in
  database;

  Backend could use the symbol and remaining argument to find candidates.
  * Return a CANDIDATE list tells engine where the definition are at. The CANDIDATE
    is an alist with pre-defined keys and respective values.
  * Return nil tells engine there's no definition.

  CANDIDATE's Keys:
  -----------------
  * `:src' The file path or documentation string.

  * `:linum' The line number.
  * `:colnum' (optional) The column number.
  or
  * `:offset' The offset of cursor in the buffer.
  Note. `:offset' > `:linum' or `:column'.

  * `:snapshot' (optional) The snapshot of the content.

  * `:keyword' (optional) A REGEXP to indicate the highlight word. Explicit
  numberd group construct is supported; Others like shy group construct will be
  ignored.

  * `:mode' (optional) Major mode for the context.

  Example - File Candidates
  -------------------------
  ((:src     ~/project/a/b/c
    :linum   100
    :keyword hello) ...)

  Example - Document Candidates
  -----------------------------
  ((:src     This is a documentary of the built-in ...
    :linum   1) ...)"
  :type '(repeat (choice
                  :tag "Backend"
                  (symbol :tag "Single Backend")
                  (repeat :tag "Combined Backends"
                          function)))
  :group 'whereis)

(defcustom whereis-ignored-buffers '("^\\*[tT]ags\\*$"
                                     "^\\*Help\\*$"
                                     "^\\*Messages\\*$"
                                     "^\\*[wW]hereis.*\\*$")
  "REGEXP of ignored buffer names."
  :type '(repeat regexp)
  :group 'whereis)

(defcustom whereis-deferred-timeout 1000
  "Timeout in ms for deferred candidates. Deferred candidates are produced in
asynchronous querying scenario. Some backends use `deferred:process' to get
candidates."
  :type 'integer
  :group 'whereis)

(defvar whereis-symbols nil
  "[internal usage]
Cache the returned value from backends by `:symbol' command.")
(make-variable-frame-local 'whereis-symbols)

(defvar whereis-backend nil
  "[internal usage]
Cache backend which is specifically for the current buffer.")
(make-variable-buffer-local 'whereis-backend)

(defun whereis-undefined (&rest ignore)
  "[internal usage]
Dummy keymap function."
  (interactive))

(defun whereis-thingatpt (thing)
  "[internal usage]
Adapter to `thing-at-point' for compatibility of Emacs 24.3 and 24.4. It will
skip line-break and whitespace."
  (let* ((bounds (if (region-active-p)
                    (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point thing)))
         (str (and bounds
                   (buffer-substring-no-properties (car bounds)
                                                   (cdr bounds)))))
    (and str (null (string-match "\\(\n\\|\r\\|\\s-\\)" str))
         str)))

(defun whereis-dummy-backend (command &rest args)
  "[internal usage] [sample]
A dummy backend for orphan buffers. It avoids looping in `whereis-backends'
so that give you better performance."
  (cond
   ((eq :symbol command) (random))
   ((eq :candidates command)
    ;; Test Code >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ;; Asynchronous backend.
    ;; (deferred:$
    ;;   (deferred:process-shell "echo 123456789")
    ;;   (deferred:nextc it
    ;;     (lambda (x)
    ;;       (message "%s" x)
    ;;       (list (list :src (expand-file-name "~/.emacs")
    ;;                   :linum (random 30))))))
    ;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    nil)))

(defun whereis-init-backend (backend)
  "[internal usage]
Ask BACKEND to initialize itself."
  (whereis-call-backend-1 backend :init))

(defun whereis-call-backend-1 (backend command &optional args)
  "[internal usage]
Tell type of BACKEND (function or functions list) and call it (them) with
COMMAND and ARGS."
  (cond
   ;; A function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((functionp backend)
    (list (apply backend command args)))
   ;; A list of functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((listp backend)
    (mapcar (lambda (backend)
              (apply backend command args))
            backend))))

(defun whereis-call-backend (command &rest args)
  "[internal usage]
Find workable BACKEND and call it/them."
  (if whereis-backend
      (whereis-call-backend-1 whereis-backend command args)
    (catch 'found
      (mapc (lambda (backend)
              (let ((ret (whereis-call-backend-1 backend command args)))
                (and (delq nil ret)
                     (setq whereis-backend backend)
                     (throw 'found ret))))
            whereis-backends)
      nil)))

(defun whereis-skip-buffer? ()
  "[internal usage]
Whether to skip current buffer. See `whereis-ignored-buffers'."
  (catch 'found
    (mapc (lambda (regexp)
            (and (string-match regexp (buffer-name))
                 (throw 'found t)))
          whereis-ignored-buffers)
    nil))

(defun whereis-skip-command? (&rest commands)
  "[internal usage]
Return t if `this-command' should be skipped."
  ;; If you want to skip additional commands, try example:
  ;; (whereis-skip-command? 'self-insert-command 'previous-line)
  (memq this-command `(self-insert-command
                       mwheel-scroll
                       save-buffer
                       eval-buffer
                       eval-last-sexp
                       ;; Additional commands.
                       ,@commands)))

(defun whereis-dispatch? ()
  "[internal usage]
Test whether to begin core process."
  (not (or (whereis-skip-buffer?)
           (whereis-skip-command?)
           (active-minibuffer-window)
           ;; Skip debug-mode.
           (and (featurep 'edebug) edebug-active))))

(defun whereis-dispatch (&optional go?)
  "[internal usage]
The core dispatcher."
  (when (whereis-dispatch?)
    (setq whereis-source-buffer (current-buffer)
          whereis-source-window (selected-window))
    (condition-case err
        (lexical-let ((index 0)
                      ;; Cache old symbols.
                      (old-symbols whereis-symbols)
                      ;; Frontend's action.
                      (frontend-act :hide)
                      ;; Deferred candidates.
                      deferred-candidates
                      ;; Total candidates
                      total-candidates)
          ;; Fetch new symbols.
          (setq whereis-symbols (whereis-call-backend :symbol))
          ;; Dispatch.
          (if (and (equal old-symbols whereis-symbols)
                   (null go?))
              ;; Update frontend only.
              (setq frontend-act :update)
            (while (< index (length whereis-symbols))
              (let ((symbol (nth index whereis-symbols)))
                (unless (memq symbol '(nil :stop))
                  ;; Get candidates.
                  (let ((candidates (apply (if (listp whereis-backend)
                                               ;; Combined backends
                                               (nth index whereis-backend)
                                             ;; Single backend.
                                             whereis-backend)
                                           :candidates
                                           (list symbol))))
                    (cond
                     ;; Normal candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ((listp candidates)
                      (setq total-candidates (append total-candidates
                                                     candidates)
                            frontend-act :show))
                     ;; `deferred' candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ((deferred-p candidates)
                      (push candidates deferred-candidates)
                      (setq frontend-act :show))))))
              (setq index (1+ index))))
          ;; Force to `:go' if GO? is t.
          (and go? (setq frontend-act :go))
          ;; Call frontend.
          (deferred:$
            ;; Wrap the deferred candidates with timeout.
            (deferred:timeout
              whereis-deferred-timeout nil
              (deferred:parallel
                deferred-candidates))
            (deferred:nextc it
              (lambda (all)
                ;; Extract deferred candidates.
                (mapc (lambda (cand-1)
                        (mapc (lambda (cand-2)
                                (when (plist-get cand-2 :src)
                                  (push cand-2 total-candidates)))
                              cand-1))
                      all)
                ;; (message "act:%s\ncand(s):%s" frontend-act total-candidates)
                ;; Check mode again because it's executed with a delay.
                (if (or (and (eq frontend-act :go)
                               (= 1 (length total-candidates)))
                          whereis-symbol-mode)
                    (whereis-call-frontend frontend-act total-candidates)
                  ;; Kill dispatcher.
                  (whereis-kill-dispatcher))))))
      (error (error "whereis error: %s when using %s" err whereis-backend)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Where Is Symbol Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom whereis-idle-delay 0.3
  "The idle delay in seconds before the `whereis-symbol-mode' engine starts."
  :type '(number :tag "Seconds"))

(defvar whereis-source-buffer nil
  "Cached source code buffer.")

(defvar whereis-source-window nil
  "Cached window where the source code buffer is at.")

(defvar whereis-dispatcher-timer nil
  "A idle timer for dispatching.")

(defun whereis-call-frontend (command &optional candidates)
  ;; Resolve duplicates.
  (when candidates
    (setq candidates (delete-dups (delq nil candidates))))
  (apply 'whereis-ui-previewer command (list candidates)))

(defun whereis-kill-dispatcher ()
  "Kill the idle timer for dispatching."
  (when (timerp whereis-dispatcher-timer)
    (cancel-timer whereis-dispatcher-timer)
    (setq whereis-dispatcher-timer nil)))

;;;###autoload
(define-minor-mode whereis-symbol-mode
  "This local minor mode gethers symbol returned from backends around the point
and show the reference visually through frontends."
  :lighter " Whereis"
  :global t
  :group 'whereis
  (if whereis-symbol-mode
      (progn
        ;; Addition minor mode.
        (when (featurep 'hl-anything)
          (add-to-list 'hl-highlight-special-faces 'whereis-highlight-face))
        (unless hl-highlight-mode
          (hl-highlight-mode 1))
        ;; Initialize front-ends & back-ends.
        (whereis-call-frontend :init)
        (mapc 'whereis-init-backend whereis-backends)
        ;; (whereis-setup-dispatcher)
        (setq whereis-dispatcher-timer
              (run-with-idle-timer whereis-idle-delay t 'whereis-dispatch)))
    ;; Cancel timer.
    (whereis-kill-dispatcher)
    ;; Destroy front-ends.
    (whereis-call-frontend :destroy)))

;; TODO: menu-bar and tool-bar keymap.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goto Symbol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom whereis-before-goto-hook nil
  "A hook point before executing `whereis-goto-symbol'."
  :type '(repeat function)
  :group 'whereis)

(defcustom whereis-after-goto-hook nil
  "A hook point after executing `whereis-goto-symbol'."
  :type '(repeat function)
  :group 'whereis)

;;;###autoload
(defun whereis-goto-symbol ()
  "Goto the definition of symbol."
  (interactive)
  (run-hooks 'whereis-before-goto-hook)
  (whereis-dispatch t)
  (run-hooks 'whereis-after-goto-hook))

;; Link with `history' feature.
(when (featurep 'history)
  (add-to-list 'history-advised-before-functions 'whereis-goto-symbol t))

;; Add menu item.
(when tool-bar-mode
  (define-key-after tool-bar-map [whereis-symbol-on]
    '(menu-item "Toggle whereis-symbol-mode" whereis-symbol-mode
                :image (find-image '((:type xpm :file "images/whereis-symbol-on.xpm")))
                :visible whereis-symbol-mode))
  (define-key-after tool-bar-map [whereis-symbol-off]
    '(menu-item "Toggle whereis-symbol-mode" whereis-symbol-mode
                :image (find-image '((:type xpm :file "images/whereis-symbol-off.xpm")))
                :visible (null whereis-symbol-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find Symbol/Reference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-find-symbol ()
  "Find the definition of the given symbol name."
  (interactive)
  (message "Sorry, function is yet ready."))

;;;###autoload
(defun whereis-find-symbol-refs ()
  "Find the reference of the given symbol name."
  (interactive)
  (message "Sorry, function is yet ready."))

(provide 'whereis)
;;; whereis.el ends here
