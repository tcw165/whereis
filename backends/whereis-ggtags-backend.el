;; Copyright (C) 2014, 2015
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

(defun whereis-ggtags-p ()
  "Get path of the database of global."
  ;; (setenv "GTAGSDBPATH" "/Users/boyw165/.emacs.d/.workspace/All")
  ;; (getenv "GTAGSDBPATH")
  ;; (setenv "GTAGSROOT" "/Users/boyw165/.emacs.d/.workspace/All")
  ;; (getenv "GTAGSROOT")
  ;; (shell-command-to-string "export GTAGSDBPATH=/Users/boyw165/.emacs.d/.workspace/All")
  ;; (shell-command-to-string "echo $GTAGSDBPATH")
  ;; (shell-command-to-string "global -pr")
  (let ((path (with-temp-buffer
                ;; (call-process "global" nil t nil "global -pr")
                ;; (call-process "bash" nil t nil "-c" "global -pr")
                (call-process "bash" nil `(,(current-buffer) nil) nil "-c" "global -pr")
                (goto-char (point-min))
                (buffer-substring (point) (line-end-position)))))
    (if (equal path "") nil path)))

;;;###autoload
(defun whereis-ggtags-backend (command &rest args)
  (cond
   ((eq command :symbol)
    (when (and (memq major-mode '(c-mode c++-mode java-mode))
               (whereis-ggtags-p))
      (or (save-excursion
            (beginning-of-line)
            (and (looking-at "^\\s-*#\\s-*include\\s-*[<\"]\\(?1:.*\\)[\">]$")
                 ;; Return a file path struct.
                 (list :file (match-string-no-properties 1))))
          (let ((symbol (whereis-thingatpt 'symbol)))
            ;; Return a symbol struct.
            (and symbol (list :symbol symbol)))
          :stop)))
   ((eq command :candidates)
    (let* ((thing (nth 0 args))
           (is-search (nth 1 args))
           (thing-type (car thing))
           (thing-val (cadr thing)))
      (cond
       ;; Symbol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq :symbol thing-type)
        (mapcar (lambda (str)
                  (with-temp-buffer
                    (insert str)
                    (goto-char (point-min))
                    ;; Parse the result line by line.
                    (re-search-forward
                     "^\\(?1:.+\\):\\(?2:[0-9]+\\):\\s-*\\(?3:.*\\)$" nil t)
                    (list :src (abbreviate-file-name (match-string 1))
                          :linum (string-to-int (match-string 2))
                          :snapshot (match-string 3)
                          :keyword thing-val)))
                ;; Get data from GNU Global.
                (with-temp-buffer
                  (call-process "bash" nil `(,(current-buffer) nil) nil
                                "-c"
                                (format "global --result=grep --path-style=absolute %s"
                                        thing-val))
                  (split-string (buffer-string) "\n" t))))
       ;; File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq :file thing-type)
        ))))))

(provide 'whereis-ggtags-backend)
