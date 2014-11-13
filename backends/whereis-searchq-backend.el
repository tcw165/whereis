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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-12-09
;; * Support keyword highlight at specific line.
;;
;; 2014-10-01
;; * Initial release.
;;
;;; Code:

;; 3rd party library.

;;;###autoload
(defun whereis-searchq-backend (command &rest args)
  (cond
   ((eq command :symbol)
    (when (memq major-mode `(,(and (featurep 'searchq)
                                   'searchq-result-mode)
                             ,(and (featurep 'prj-grep-mode)
                                   'prj-grep-mode)
                             ,(and (featurep 'compile)
                                   'compilation-mode)
                             ,(and (featurep 'grep)
                                   'grep-mode)))
      (unless mark-active
        (save-excursion
          (beginning-of-line)
          (if (search-forward-regexp "^.+:[0-9]+:" (line-end-position) t)
              ;; Return (FILEPATH . NUM) struct.
              (let* ((text (buffer-substring-no-properties
                            (line-beginning-position) (- (point) 1)))
                     (offset (string-match ":[0-9]+$" text))
                     (file (substring text 0 offset))
                     (linum (string-to-int (substring text (1+ offset)))))
                (cons file linum))
            :stop)))))
   ((eq command :candidates)
    ;; 1st argument is (FILEPATH . NUM) struct.
    (let* ((file (caar args))
           (linum (cdar args))
           ;; TODO: compatible with searchq-result-mode and compilation-mode.
           (thing (save-excursion
                    (re-search-backward
                     (concat "^" (car searchq-delimiter) ".+$") nil t)
                    (buffer-substring-no-properties
                     (+ (length (car searchq-delimiter)) (point))
                     (line-end-position)))))
      (list (list :src file
                  :linum linum
                  :keyword (regexp-quote thing)))))))

(provide 'whereis-searchq-backend)
