;;; lotion.el --- Use Notion in your Emacs           -*- lexical-binding: t; -*-

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

;;; Commentary:
;;
;; Lotion is an attempt to use Emacs and Notion simultaneously, by being able to
;; import and export content from Notion, while keeping data consistency.
;;

;;; Code:
(require 'org)
(require 'request)

;;; Options
(defgroup lotion nil
  "Use Notion with Org-mode."
  :group 'org
  :prefix "lotion-"
  :link '(url-link :tag "Github" "https://github.com/sienic/lotion"))

(defcustom lotion-directory (expand-file-name "~/lotion")
  "Default path to Lotion files."
  :type 'directory
  :group 'lotion)

(defconst lotion-default-host "api.notion.com")

(defconst lotion-token "I AM A TOKEN")
(defconst lotion-user "my@mail.to")

(defun lotion-token ()
  (auth-source-pick-first-password :host "api.notion.com" :user lotion-user))

(defun lotion-request--get (path &optional data)
  (lotion-request path))

(defun lotion-request (path &optional request-method data)
  "Performs a request to notion"
  (let ((type (or request-method "GET")))
    (request path
      :type type
      :parser 'json-read
      :data '(("page_size" . 100))
      :headers `(("Notion-Version" . "2022-02-22")
                 ("Authorization" . ,(concat "Bearer " (lotion-token))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq my/data data)
                  (message "I sent: %s" data)))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown))))))

;; (lotion-request "https://api.notion.com/v1/databases/2e574484a5654e928fcdfb413885c605/query" "POST" )
;; (lotion-request--get "https://api.notion.com/v1/pages/94876842588d441b8402f46f08e0e030" )

(provide 'lotion)
;;; lotion.el ends here
