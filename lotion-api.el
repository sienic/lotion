;;; lotion-api.el --- Lotion api -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022

;; URL: https://github.com/sienic/lotion
;; Version: 0.1.0
;; Keywords: org-mode, notion
;; Package-Requires: ((emacs "26.1") (org "9.4")

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Lotion api
;;
;;; Code:

(defun lotion-api-page-fetch (uuid &optional callback)
  (lotion-api--get (format "/v1/pages/%s" uuid) callback))

(defun lotion-api-blocks-fetch (uuid &optional callback)
  (lotion-api--get (format "/v1/blocks/%s/children" uuid) callback))

(defun lotion-api-block-patch (uuid payload &optional callback)
  (lotion-api--patch (format "/v1/blocks/%s" uuid) payload callback))

(defun lotion-api--get (path &optional callback)
  (lotion-api--request
   :path path
   :callback callback
   :payload '(("page_size . 100"))))

(defun lotion-api--patch (path payload &optional callback)
  (lotion-api--request
   :path path
   :method "PATCH"
   :callback callback
   :payload (json-encode payload)
   :headers '(("Content-Type" . "application/json"))))

(cl-defun lotion-api--request (&key path method callback payload headers)
  "Performs a request to notion"
  (let ((type (or method "GET")))
    (request (concat "https://" lotion-default-host path)
      :type type
      :parser 'json-read
      :data payload
      :headers (append headers `(("Notion-Version" . "2022-02-22")
                                 ("Authorization" . ,(concat "Bearer " (lotion-token)))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (if callback (funcall callback data nil))
                  (setq my/data data)))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (if callback (funcall callback nil error-thrown))
                            (message "Got error: %S" error-thrown))))))

(defun alist-get-in (alist symbols)
  "Navigate an ALIST via SYMBOLS.
Numbers in SYMBOLS are considered indeces of sequences."
  (if symbols
      (if-let ((index (and (numberp (car symbols))
                           (car symbols))))
          (alist-get-in (nth index (append alist nil)) (cdr symbols))
        (alist-get-in (alist-get (car symbols) alist) (cdr symbols)))
    alist))

(provide 'lotion-api)
;;; lotion-api.el ends here
