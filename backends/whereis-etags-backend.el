;;; ws-ggtags-backend.el --- whereis-symbol backend for ggtags.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
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
(require 'thingatpt)
(require 'etags)

(defgroup whereis-etags nil
  "Whereis backend for ctags."
  :group 'whereis)

(defcustom whereis-etags-search-max 20
  "Maximum amount of searching each time. Avoid stack overflow."
  :type 'integer
  :group 'whereis-etags)

(defcustom whereis-etags-testers '(tag-exact-file-name-match-p
                                   tag-file-name-match-p
                                   tag-exact-match-p
                                   tag-implicit-name-match-p
                                   tag-symbol-match-p
                                   tag-word-match-p
                                   tag-partial-file-name-match-p
                                   tag-any-match-p)
  "Qualification test functions list for tags."
  :type '(repeat symbol)
  :group 'whereis-etags)

(defconst whereis-etags-buffer "*tags*"
  "Tags's buffer name.")

(defvar whereis-etags-path nil
  "File path of ctags's database.")
(make-variable-buffer-local 'whereis-etags-path)

(defvar whereis-etags-modtime 0
  "Time stamp of tags file. It is for when to reload the tags file.")

;; (make-variable-buffer-local 'tags-file-name)

(defun whereis-etags-find-tags-path ()
  (progn
    (defun find-tags-r (path)
      (let ((tag (concat path "tags")))
        (cond
         ((file-exists-p tag) (throw 'found tag))
         ((string= tag "/tags") nil)
         (t (find-tags-r (file-name-directory
                        (directory-file-name path)))))))
    (when (buffer-file-name)
      (catch 'found
        (find-tags-r (file-name-directory (buffer-file-name)))))))

(defun whereis-etags-get-candidates (root-path symbol)
  (let ((count 0)
        ;; Case sensitive.
        (case-fold-search nil)
        candidates)
    ;; Start searching.
    (goto-char (point-min))
    (while (and (search-forward symbol nil t)
                (< count whereis-etags-search-max))
      (let ((testers whereis-etags-testers))
        ;; Get a qualified match.
        (when (catch 'qualified-found
                (while testers
                  (and (funcall (car testers) symbol)
                       (throw 'qualified-found t))
                  (setq testers (cdr testers))))
          ;; Extract information.
          (save-excursion
            (beginning-of-line)
            (let ((file (if (memq (car testers)
                                  '(tag-exact-file-name-match-p
                                    tag-file-name-match-p
                                    tag-partial-file-name-match-p))
                            (save-excursion
                              (forward-line 1)
                              (etags-file-of-tag t))
                          (etags-file-of-tag t)))
                  (info (etags-snarf-tag)))
              (push (list :src (abbreviate-file-name (concat root-path file))
                          :linum (cadr info)
                          :keyword symbol
                          :snapshot (car info))
                    candidates)))))
      (setq count (1+ count)))
    candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-etags-backend (command &rest args)
  (cond
   ((eq command :symbol)
    ;; Find ctags database.
    (unless whereis-etags-path
      (setq whereis-etags-path (whereis-etags-find-tags-path)))
    (or (unless (or (region-active-p)
                (string= (buffer-name) whereis-etags-buffer))
          (whereis-thingatpt 'symbol))
        :stop))
   ((eq command :candidates)
    (let ((symbol (nth 0 args)))
      (let ((tag-path whereis-etags-path)
            (root-path (file-name-directory whereis-etags-path)))
        (with-current-buffer (get-buffer-create whereis-etags-buffer)
          (setq buffer-read-only nil)
          ;; Update tags buffer if necessary.
          (let ((modtime (nth 5 (file-attributes tag-path))))
            (unless (or (equal modtime whereis-etags-modtime)
                        (string= buffer-file-name tag-path))
              (erase-buffer)
              (insert-file-contents-literally tag-path)
              (setq whereis-etags-modtime modtime
                    buffer-file-name tag-path)))
          (set-buffer-modified-p nil)
          ;; Find candidates.
          (prog1 (whereis-etags-get-candidates root-path symbol)
            (setq buffer-read-only t))))))))

(provide 'whereis-etags-backend)
