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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-files-backend (command &rest args)
  (cond
   ((eq command :symbol)
    (whereis-thingatpt 'filename t))
   ((eq command :candidates)
    (let* ((file (nth 0 args))
           (full-file (concat (file-name-directory (buffer-file-name))
                              "/" file)))
      ;; TODO: Find files in project.
      (when (file-exists-p full-file)
        (list (list :src full-file
                    :linum 1)))))))

(provide 'whereis-files-backend)
;;; whereis-ycmd-backend.el ends here
