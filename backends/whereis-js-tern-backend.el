;; Copyright (C) 2015
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
;; This is a backend communicating with JavaScript Tern server
;; (http://ternjs.net/).
;;
;;; Prerequisite:
;;
;; * Node.js
;; * Tern installed from npm.
;;
;;; Code:

;; GNU library.
(require 'thingatpt)

(defgroup whereis-js-tern nil
  "Whereis backend for JavaScript by tern engine."
  :group 'whereis)

(defun whereis-js-tern-running? ()
  "[internal use]
Get whether server is online."
  (catch 'found
    (mapc (lambda (proc)
            (and (string-match "^Tern" (process-name proc))
                 (throw 'found t)))
          (process-list))
    nil))

(defun whereis-js-tern-run-query (data)
  "[internal use]
It uses `deferred' to create Tern request. Why don't it just use `tern-run-query'?
Because it need to be wrapped as a DEFERRED object for `whereis' framework."
  (let ((url-request-method "POST")
        (url-request-data data)
        (url-mime-charset-string nil)
        (url-show-status nil)
        (url (url-parse-make-urlobj "http" nil nil
                                    tern-server tern-known-port "/"
                                    nil nil nil)))
    (let ((d (deferred:$
               (deferred:url-retrieve url nil t t)
               (deferred:nextc it 'deferred:url-delete-header))))
      (deferred:set-next
        d (deferred:new 'deferred:url-delete-buffer))
      d)))

(defun whereis-js-tern-make-query (query pos)
  "[internal use]
The TERN request is a JSON object, with 3 fields:
query, files (optional) and timeout (optional).
e.g. {query: ___ ,
      files: ___ ,
      timeout: ___ }

* query: A query is an object with at least a `type' property. It might be
         omitted if the request is only used to push new code to the server.
* file: An array of file specifications. It might be omitted when the query
        operates on the code that server already has, without adding anything
        new.
* timeout: A number as the max amount of milliseconds to work.

See <http://ternjs.net> for more details.

It was written refer to `tern-run-query' function."
  (let ((doc `((query . ,query))) ;; construct a query object.
        (pos pos)
        (files (tern-modified-sibling-buffers))
        file-name)
    (cond
     ;; If there're no modified buffers.
     ((not tern-buffer-is-dirty)
      (setq file-name (tern-project-relative-file)))
     ;; If the buffer is modifed and the size is smaller than a threshold value.
     ((> (buffer-size) 8000)
      (push (tern-get-partial-file pos) files)
      (setq file-name "#0")
      (cl-decf pos (cdr (assq 'offset (car files)))))
     ;; By default, send the full context to server.
     (t
      (push `((type . "full") (text . ,(buffer-string)) (name . ,(tern-project-relative-file))) files)
      (setq file-name (tern-project-relative-file))))
    ;; Add files property in the request.
    (and files
         (push `(files . ,(apply #'vector files)) doc))
    ;; Update query property in the request.
    (push `(file . ,file-name) (cdr (assq 'query doc)))
    (push `(end . ,(1- pos)) (cdr (assq 'query doc)))
    (json-encode doc)))

;; (defun whereis-js-tern-test ()
;;   "A test function to produce POST request to tern server."
;;   (interactive)
;;   (deferred:nextc
;;     (whereis-js-tern-run-query (whereis-js-tern-make-query
;;                                 '((type . "definition")
;;                                   (variable . nil))
;;                                 (point)))
;;     (lambda (buf)
;;       (let* ((json-object-type 'plist)
;;              (data (with-current-buffer buf
;;                      (buffer-string))))))))

;; (defun whereis-js-tern-find-reference ()
;;   "Find reference of the symbol."
;;   (interactive)
;;   ;; (when (featurep 'searchq)
;;   ;;   (searchq:chain
;;   ;;    (searchq:process-shell
;;   ;;     )))
;;   (deferred:nextc
;;     (whereis-js-tern-run-query (whereis-js-tern-make-query
;;                                 `((type . "refs"))
;;                                 (point)))
;;     (lambda (buf)
;;       (let* ((json-object-type 'plist)
;;              (data (with-current-buffer buf
;;                      (buffer-string))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun whereis-js-tern-backend (command &rest args)
  "A backend communicating with JavaScript Tern server (http://ternjs.net/)."
  (cond
   ((eq command :symbol)
    (or (and (memq major-mode `(js-mode
                                ;; 3rd party package.
                                ,(and (featurep 'js2-mode)
                                      'js2-mode)))
             ;; 3rd party package.
             (featurep 'json)
             (featurep 'tern) tern-mode
             ;; Get symbol string.
             (whereis-thingatpt 'symbol))
        :stop))
   ((eq command :candidates)
    (lexical-let ((thing (nth 0 args)))
      (if (whereis-js-tern-running?)
          ;; Return a `deferred' object.
          (deferred:nextc
            ;; Send HTTP POST command and return a `deferred' object.
            (whereis-js-tern-run-query (whereis-js-tern-make-query
                                        `((type . "definition")
                                          (variable . nil))
                                        (point)))
            ;; Callback to handle the return data.
            (lambda (buf)
              ;; Clear dirty flag.
              (setf tern-buffer-is-dirty nil)
              ;; Return candidates.
              (let* ((json-object-type 'plist)
                     (raw-data (with-current-buffer buf
                                 (buffer-string)))
                     (data (json-read-from-string raw-data))
                     (file (plist-get data :file))
                     (start (plist-get data :start))
                     (end (plist-get data :end)))
                ;; (message "raw-data =%s" raw-data)
                (and
                 (stringp file) (numberp start) (numberp end)
                 (list (list :src (concat (tern-project-dir) file)
                             :offset (1+ end)
                             :keyword (regexp-quote thing)))))))
        ;; Launch Tern server.
        ;; TODO: This is a workaround to launch Tern server. It also expose the
        ;; whereis core code and is very inconsistent.
        (tern-run-query
         (lambda (data)
           (let ((file (cdr (assq 'file data)))
                 (start (plist-get data :start))
                 (end (cdr (assq 'end data))))
             (and (stringp file) (numberp start) (numberp end)
                  (whereis-call-frontend
                   :show
                   (list (list :src (concat (tern-project-dir) file)
                               :offset (1+ end)
                               :keyword (regexp-quote thing)))))))
         `((type . "definition")
           (variable . nil))
         (point)))))))

(provide 'whereis-js-tern-backend)
