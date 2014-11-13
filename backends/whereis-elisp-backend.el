;; Copyright (C) 2014, 2015
;;
;; Author: boyw165 <boyw165@gmail.com>
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
(require 'help)
(require 'help-fns)
(require 'thingatpt)

(defgroup whereis-lisp-backend nil
  "Faces for default frontends."
  :group 'whereis)

(defconst whereis-elisp-function-regexp "^\\s-*(\\(?:def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|ine\\(?:-global\\)?-minor-mode\\|ine-compilation-mode\\|un-cvs-mode\\|foo\\|[^icfgv]\\(\\w\\|\\s_\\)+\\*?\\)\\|easy-mmode-define-[a-z-]+\\|easy-menu-define\\|menu-bar-make-toggle\\|cl-defun\\)\\(?:\\s-\\|\\|;.*\n\\)+\\(?:'\\|(quote \\)?\\\\?\\(?1:%s\\)\\(?:\\s-\\|$\\|(\\|)\\)"
  "Refer to `find-function-regexp'.")

(defconst whereis-elisp-variable-regexp "^\\s-*(\\(?:def[^fumag]\\(\\w\\|\\s_\\)+\\*?\\|\easy-mmode-def\\(?:map\\|syntax\\)\\|easy-menu-define\\)\\(?:\\s-\\|\n\\|;.*\n\\)+\\(?1:%s\\)\\(?:\\s-\\|$\\)"
  "Refer to `find-variable-regexp'.")

(defconst whereis-elisp-face-regexp "^\\s-*(defface\\(?:\\s-\\|\n\\|;.*\n\\)+\\(?1:%s\\)\\(?:\\s-\\|$\\)"
  "Refer to `find-face-regexp'.")

(defconst whereis-elisp-feature-regexp "^\\s-*(provide '\\(?1:%s\\)")

(defun whereis-elisp-thingatpt ()
  "Find symbol string around the point or text selection."
  (ignore-errors
    (let* ((bound (if mark-active
                      (cons (region-beginning) (region-end))
                    (unless (memq (get-text-property (point) 'face)
                                  '(font-lock-doc-face
                                    font-lock-string-face
                                    font-lock-comment-face))
                      (bounds-of-thing-at-point 'symbol))))
           (thing (buffer-substring-no-properties (car bound)
                                                  (cdr bound))))
      (and (string-match "[^0-9]" thing)
           thing))))

(defun whereis-elisp-normalize-path (file)
  (when (stringp file)
    ;; Convert extension from .elc to .el.
    (and (string-match "\\.el\\(c\\)\\'" file)
         (setq file (substring file 0 (match-beginning 1))))
    ;; Strip extension from .emacs.el to make sure symbol is searched in
    ;; .emacs too.
    (and (string-match "\\.emacs\\(.el\\)" file)
         (setq file (substring file 0 (match-beginning 1))))
    (and (file-exists-p file)
         (abbreviate-file-name file))))

(defun whereis-elisp-get-info (file symb-name regexp-temp)
  "Return information like line number, regexp matcher and more."
  (when file
    (let ((matcher (format regexp-temp (regexp-quote symb-name)))
          (case-fold-search nil))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (with-syntax-table lisp-mode-syntax-table
          (when (re-search-forward matcher nil t)
            ;; line number + keywords + snapshot.
            (list (line-number-at-pos)
                  matcher
                  ;; Strip prefix whitespaces.
                  (replace-regexp-in-string
                   "^\\s-*" ""
                   (buffer-substring (line-beginning-position)
                                     (line-end-position))))))))))

(defun whereis-elisp-find-feature (thing symb)
  "Return the absolute file name of the Emacs Lisp source of SYMB (the name of
the library). Refer to `find-function-source-path' or `load-path'."
  (ignore-errors
    (let* ((file (whereis-elisp-normalize-path
                  (locate-file thing
                               (or find-function-source-path load-path)
                               '(".el" ".elc" ".el.gz"))))
           (info (whereis-elisp-get-info file thing
                                         whereis-elisp-feature-regexp))
           (linum (nth 0 info))
           (keyword (nth 1 info))
           (snapshot (nth 2 info)))
      (and (stringp file) (file-exists-p file)
           (list :src file
                 :linum linum
                 :keyword keyword
                 :snapshot snapshot)))))

(defun whereis-elisp-find-function (thing symb)
  "Return the candidate pointing to the definition of SYMB. It was written
refer to `find-function-noselect', `find-function-search-for-symbol' and
`describe-function'."
  (when symb
    (ignore-errors
      (let* ((real-symb (progn
                          (while (and (symbol-function symb)
                                      (symbolp (symbol-function symb)))
                            ;; Try to dereference the symbol if it's a alias.
                            (setq symb (symbol-function
                                        (find-function-advised-original symb))
                                  thing (symbol-name symb)))
                          symb))
             (file (whereis-elisp-normalize-path (symbol-file real-symb 'defun))))
        (when (symbol-function real-symb)
          (if (and file (file-exists-p file))
              ;; Function with exist file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (let* ((info (whereis-elisp-get-info
                            file thing
                            whereis-elisp-function-regexp))
                     (linum (nth 0 info))
                     (keyword (nth 1 info))
                     (snapshot (nth 2 info)))
                (list :src file
                      :linum linum
                      :keyword keyword
                      :snapshot snapshot))
            ;; Function without readable file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (let* ((doc (describe-function real-symb))
                   (keyword (concat "(\\(?1:"
                                    (regexp-quote (symbol-name real-symb))
                                    "\\)\\s-+")))
              (when doc
                (quit-window t) (message "")
                (with-temp-buffer
                  (insert doc)
                  (goto-char (point-min))
                  (re-search-forward keyword nil t)
                  (list :src doc
                        :linum (line-number-at-pos)
                        :keyword keyword
                        :snapshot (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))))))))))

(defun whereis-elisp-find-variable (thing symb)
  "Return the candidate pointing to the definition of SYMB. It was written
refer to `find-variable-noselect', `find-function-search-for-symbol' and
`describe-variable'."
  (when symb
    (ignore-errors
      (let ((file (symbol-file symb 'defvar)))
        (if (and (stringp file) (file-exists-p file))
            ;; Variable with exist file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (let* ((file (whereis-elisp-normalize-path file))
                   (info (whereis-elisp-get-info file thing
                                                 whereis-elisp-variable-regexp))
                   (linum (nth 0 info))
                   (keyword (nth 1 info))
                   (snapshot (nth 2 info)))
              (list :src file
                    :linum linum
                    :keyword keyword
                    :snapshot snapshot))
          ;; Variable without readable file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (and (progn (symbol-value symb) t)
               (not (keywordp symb))
               (let* ((doc (describe-variable symb))
                      (keyword (regexp-quote (symbol-name symb))))
                 (when doc
                   (quit-window t) (message "")
                   (with-temp-buffer
                     (insert doc)
                     (goto-char (point-min))
                     (re-search-forward keyword nil t)
                     (list :src doc
                           :linum (line-number-at-pos)
                           :keyword keyword
                           :snapshot (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))))))))))

(defun whereis-elisp-find-face (thing symb)
  (ignore-errors
    (when (facep symb)
      (let* ((file (whereis-elisp-normalize-path (symbol-file symb 'defface)))
             (info (whereis-elisp-get-info file thing
                                           whereis-elisp-face-regexp))
             (linum (nth 0 info))
             (keyword (nth 1 info))
             (snapshot (nth 2 info)))
        (if (and (stringp file) (file-exists-p file))
            ;; Face with readable file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (list :src file
                  :linum linum
                  :keyword keyword
                  :snapshot snapshot)
          ;; Face without readable file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let* ((doc (when (facep symb)
                        (describe-face symb)
                        (buffer-string)))
                 (keyword (concat "Face:\\s-\\(?1:"
                                  (regexp-quote (symbol-name symb))
                                  "\\)")))
            (when doc
              (quit-window t) (message "")
              (list :src doc
                    :linum 1
                    :keyword keyword))))))))

(defun whereis-elisp-find-locals-common (thing &optional down-list-max)
  (let ((count 0))
    (ignore-errors
      (save-excursion
        (down-list 2)
        (catch 'found
          (while t
            ;; Compare SYMBOL and THING.
            (and (looking-at "(?\\(?1:\\(\\sw\\|\\s_\\)+\\)")
                 (equal thing (match-string 1))
                 (throw 'found
                        (list :src (or (buffer-file-name)
                                       (current-buffer))
                              :linum (line-number-at-pos)
                              :keyword (regexp-quote thing)
                              :snapshot (buffer-substring (point)
                                                          (line-end-position)))))
            ;; Check if count is greater than DOWN-LIST_MAX. t to break.
            (setq count (1+ count))
            (and (integerp down-list-max)
                 (>= count down-list-max)
                 (throw 'found nil))
            ;; Next
            (forward-sexp 2)
            (backward-sexp)
            nil))))))

(defun whereis-elisp-find-locals (thing)
  (let (candidates)
    (ignore-errors
      (save-excursion
        (while t
          (up-list -1)
          (push (cond
                 ((looking-at "(dolist[ \t\r\n]")
                  (whereis-elisp-find-locals-common thing 1))
                 ((or (looking-at "(\\(lexical-\\)?let[*]?[ \t\r\n]")
                      (looking-at "(defun[*]?[ \t\r\n]")
                      (looking-at "(defmacro[*]?[ \t\r\n]")
                      (looking-at "(lambda[ \t\r\n]"))
                  (whereis-elisp-find-locals-common thing)))
                candidates))))
    (setq candidates (delq nil candidates))
    (car candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar whereis-elisp-install-p nil
  "A boolean which indicates ELISP is installed.")

(defvar whereis-elisp-sudo-passwd nil)

;;;###autoload
(defun whereis-elisp-install-sources ()
  "Install *.el of Emacs LISP sources. Support MAC-OSX and Linux (with apt-get)."
  (interactive)
  (if whereis-elisp-install-p
      (message "Already installed.")
    (require 'deferred)
    (setq whereis-elisp-sudo-passwd (read-passwd "Install Emacs LISP sources, sudo: " t))
    (message "Install Emacs LISP sources ...")
    (cond
     ;; Mac OSX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq 'darwin system-type)
      (deferred:$
        (deferred:process-shell
          (format "echo %s | sudo -S find %s -name \"%s\" -exec %s"
                  whereis-elisp-sudo-passwd
                  "/Applications/Emacs.app/Contents/Resources/lisp"
                  "*.el.gz"
                  "gzip -fd {} +"))
        (deferred:nextc it
          (lambda (output)
            (message "Install Emacs LISP sources ...done")))))
     ;; Linux ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq 'gnu/linux system-type)
      (deferred:$
        (deferred:process-shell
          (format "echo %s | sudo -S apt-get -y install emacs%s-el"
                  whereis-elisp-sudo-passwd emacs-major-version))
        (deferred:nextc it
          (lambda (output)
            (deferred:process-shell
              (format "echo %s | sudo -S find %s -name \"%s\" -exec %s"
                      whereis-elisp-sudo-passwd
                      (format "/usr/local/share/emacs/%s.%s/lisp"
                              emacs-major-version
                              emacs-minor-version)
                      "*.el.gz"
                      "gzip -d {} +"))))
        (deferred:nextc it
          (lambda (output)
            (message "Install Emacs LISP sources ...done"))))))
    (setq whereis-elisp-install-p t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-elisp-backend (command &rest args)
  (cond
   ((eq :init command))
   ((eq :symbol command)
    (when (memq major-mode '(emacs-lisp-mode
                             lisp-interaction-mode))
      ;; Return the thing in string or `:stop'.
      (or (whereis-elisp-thingatpt) :stop)))
   ((eq :candidates command)
    (let* ((thing (nth 0 args))
           (is-search (nth 1 args))
           (symbol (intern-soft thing))
           ;; Force to select help window in order to get buffer string.
           (help-window-select t)
           ;; Force to collect autoload.
           (help-enable-auto-load t)
           candidates)
      (if is-search
          ;; TODO: don't know how to implement yet.
          (progn)
        ;; The last one gets the top priority.
        (unless (member thing '("t" "nil"))
          (dolist (cand (list (whereis-elisp-find-face thing symbol)
                              ;; Skip feature parsing if function is found.
                              (or (whereis-elisp-find-function thing symbol)
                                  (whereis-elisp-find-feature thing symbol))
                              ;; Skip variable parsing if local variable is found.
                              (or (whereis-elisp-find-locals thing)
                                  (whereis-elisp-find-variable thing symbol))))
            (and cand (push cand candidates)))))
      candidates))))

(provide 'whereis-elisp-backend)
;;; whereis-elisp-backend.el ends here
