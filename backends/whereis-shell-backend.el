;; Copyright (C) 2014, 2015
;;
;; Author: BoyW165
;; Compatibility: GNU Emacs 24.x
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
(require 'shell)
(require 'sh-script)
(require 'esh-mode)
(require 'thingatpt)

(defgroup whereis-shell nil
  "Whereis backend for Shell mode."
  :group 'whereis)

(defface whereis-man-section-face
  '((t (:foreground "tomato" :weight bold :height 1.3)))
  "Face for section context in man page. e.g. SYNOPSIS."
  :group 'whereis-shell)

(defface whereis-man-parameter-face
  '((t (:weight bold)))
  "Face for parameter in man page. e.g. --arg."
  :group 'whereis-shell)

(defface whereis-man-constant-face
  '((t (:foreground "dark cyan" :weight bold)))
  "Face for constant in man page. e.g. ``constant''."
  :group 'whereis-shell)

(defvar whereis-man-keywords
  '((("^[A-Z ]+$" . 'whereis-man-section-face)
     ("[.,( ]``\\(.+\\)''[ ),.]" (1 'whereis-man-constant-face))
     ("[[ \t]\\(-+[-a-zA-Z0-9]+\\)" (1 'whereis-man-parameter-face)))
    ;; don't use syntactic fontification.
    t
    ;; Case insensitive.
    nil)
  "See `font-lock-keywords'.")

(define-derived-mode whereis-man-mode fundamental-mode "Whereis-Man"
  "Whereis man page mode."
  :group 'whereis-shell
  (setq font-lock-defaults whereis-man-keywords))

(defun whereis-shell-thingatpt (&optional org-thing)
  (let ((thing (whereis-thingatpt 'symbol)))
    (when thing
      (cond
       ;; Pipe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((string-match "\\(.+\\)\|\\(.+\\)" thing)
        (let ((str1 (match-string 1 thing))
              (str2 (match-string 2 thing)))
          (if (looking-at ".*\|")
              (progn
                (backward-sexp)
                (whereis-shell-thingatpt (or org-thing
                                             str1)))
            str2)))
       ;; Parameter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((or (string-match "-[-a-zA-Z0-9]+" thing)
            (string-match "\\(\\.\\|\\./\\|\\.\\./\\|~/\\)" thing))
        (backward-sexp)
        (whereis-shell-thingatpt (or org-thing
                                     thing)))
       (t
        (if org-thing
            (cons thing org-thing)
          (cons thing thing)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-shell-backend (command &rest args)
  (cond
   ((eq command :symbol)
    (when (memq major-mode '(shell-mode eshell-mode sh-mode))
      (or (save-excursion
            (whereis-shell-thingatpt))
          :stop)))
   ((eq command :candidates)
    (let* ((thing (nth 0 args))
           (command (regexp-quote (car thing)))
           (keyword (regexp-quote (cdr thing))))
      (ignore-errors
        (with-temp-buffer
          (call-process-shell-command (format "man %s|col -x -b" command)
                                      nil (list (current-buffer) nil))
          ;; TODO: Support for local node modules.
          (unless (string= "" (buffer-string))
            (let* ((keyword-1 (format "\\s-+\\(?1:%s\\)" keyword))
                   ;; Command keyword.
                   (keyword-2 (format "^\\s-+\\(?1:%s\\)" command))
                   (keyword keyword-1)
                   (case-fold-search nil))
              (goto-char (point-min))
              ;; Search parameter. If not found. search command.
              (unless (re-search-forward keyword-1 nil t)
                (re-search-forward keyword-2 nil t)
                (setq keyword keyword-2))
              (list (list :src (buffer-string)
                          :linum (line-number-at-pos)
                          :keyword keyword
                          :snapshot (buffer-substring (match-beginning 1)
                                                      (line-end-position))
                          :mode 'whereis-man-mode))))))))))

(provide 'whereis-shell-backend)
;;; whereis-shell-backend.el ends here
