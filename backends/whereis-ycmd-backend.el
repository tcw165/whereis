;; Copyright (C) 2014
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
(require 'thingatpt)

;; 3rd Party.
(require 'ycmd)

(defgroup whereis-ycmd nil
  "Whereis backend for YCMD."
  :group 'whereis)

(defcustom whereis-ycmd-supported-modes '(python-mode)
  "Supported major modes of YCMD."
  :type '(repeat symbol)
  :group 'whereis-ycmd)

(defun whereis-ycmd-thingatpt ()
  "Find symbol string around the point or text selection."
  (let ((bound (if mark-active
                   (cons (region-beginning) (region-end))
                 (unless (memq (get-text-property (point) 'face)
                               '(font-lock-doc-face
                                 font-lock-string-face
                                 font-lock-comment-face))
                   (bounds-of-thing-at-point 'symbol)))))
    (when bound
      ;; TODO: detect function parameter.
      ;; Return THING
      (buffer-substring-no-properties (car bound) (cdr bound)))))

(defun whereis-ycmd-standard-content (&optional buffer)
  "Generate the 'standard' content for ycmd posts.

This extracts a bunch of information from BUFFER. If BUFFER is
nil, this uses the current buffer.
"
  (with-current-buffer (or buffer (current-buffer))
    (let* ((column-num (+ 1 (save-excursion
                              (goto-char (point)) (current-column))))
           (line-num (line-number-at-pos (point)))
           (full-path (buffer-file-name))
           (file-contents (buffer-substring-no-properties
                           (point-min) (point-max)))
           (file-types (or (ycmd--major-mode-to-file-types major-mode)
                           '("generic"))))
      `(("file_data" .
         ((,full-path . (("contents" . ,file-contents)
                         ("filetypes" . ,file-types)))))
        ("filepath" . ,full-path)
        ("line_num" . ,line-num)
        ("column_num" . ,column-num)))))

(defun whereis-ycmd-goto (thing)
  "See `ycmd--goto'."
  (lexical-let ((thing thing)
                (content (cons (list "command_arguments" "GoToDeclaration")
                               (ycmd--standard-content))))
    (deferred:nextc
      (ycmd--request
       "/run_completer_command"
       content
       :parser 'json-read)
      (lambda (location)
        (let* ((file (cdr (assoc 'filepath location)))
               (linum (cdr (assoc 'line_num location))))
          (when (and file (file-exists-p file))
            (list (list :src file
                        :linum linum
                        :keyword (regexp-quote thing)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-ycmd-backend (command &rest args)
  (cond
   ((eq command :symbol)
    (and ycmd-mode
         (or (whereis-ycmd-thingatpt)
             :stop)))
   ((eq command :candidates)
    (let ((thing (car args)))
      (whereis-ycmd-goto thing)))))

(provide 'whereis-ycmd-backend)
;;; whereis-ycmd-backend.el ends here
