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

;; Code related to notion api
(defconst lotion-default-host "api.notion.com")

(defconst lotion-token "I AM A TOKEN")
(defconst lotion-user "my@mail.to")
(defconst lotion-token nil "The token to use for accessing Notion.")

(defun lotion-token ()
  "Get token to query Notion API."
  (or lotion-token (auth-source-pick-first-password :host lotion-default-host :user lotion-user)))

(cl-defun lotion-request (&key path method callback payload headers)
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

(defun lotion-request--get (path &optional callback)
  (lotion-request
   :path path
   :callback callback
   :payload '(("page_size . 100"))))

(defun lotion-request--patch (path payload &optional callback)
  (lotion-request
   :path path
   :method "PATCH"
   :callback callback
   :payload (json-encode payload)
   :headers '(("Content-Type" . "application/json"))))

;; api resources
(defun lotion-page--fetch (uuid &optional callback)
  (lotion-request--get (format "/v1/pages/%s" uuid) callback))

(defun lotion-blocks--fetch (uuid &optional callback)
  (lotion-request--get (format "/v1/blocks/%s/children" uuid) callback))

(defun lotion-blocks--patch (uuid payload &optional callback)
  (lotion-request--patch (format "/v1/blocks/%s" uuid) payload callback))

(defun alist-get-in (alist symbols)
  "Navigate an ALIST via SYMBOLS.
Numbers in SYMBOLS are considered indeces of sequences."
  (if symbols
      (if-let ((index (and (numberp (car symbols))
                           (car symbols))))
          (alist-get-in (nth index (append alist nil)) (cdr symbols))
        (alist-get-in (alist-get (car symbols) alist) (cdr symbols)))
    alist))

;; example data
(setq page-data nil)
(setq blocks-data nil)
;; blocks-data ; => ((object . "list") (results . [((object . "block") (id . "ee245fe3-b41c-4d31-9033-70795fd09ba8") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "heading_1") (heading_1 (rich_text . [((type . "text") (text (content . "Header 1") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Header 1") (href))]) (color . "default"))) ((object . "block") (id . "18ceecee-ecd0-48e2-95d3-21dd9742ea9d") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "paragraph") (paragraph (rich_text . [((type . "text") (text (content . "Text") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Text") (href))]) (color . "default"))) ((object . "block") (id . "d695bcf2-2124-413d-9f15-b01144040e3a") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "heading_2") (heading_2 (rich_text . [((type . "text") (text (content . "Header 2") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Header 2") (href))]) (color . "default"))) ((object . "block") (id . "d7e66203-049b-4dd6-ad3d-76076ba35b47") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "paragraph") (paragraph (rich_text . [((type . "text") (text (content . "Another text") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Another text") (href))]) (color . "default")))]) (next_cursor) (has_more . :json-false) (type . "block") (block))
(lotion-page--fetch "201392c852284d7c8020d2d6421a9e58" (lambda (data err) (setq page-data data)))
;; (alist-get-in page-data '(properties Name title 0 plain_text)) ; => "Getting a normal card with simple text nodes"
(lotion-blocks--fetch "201392c852284d7c8020d2d6421a9e58" (lambda (data err) (setq blocks-data data)))
;; (alist-get-in my/data '(results 0 heading_1 rich_text 0 plain_text)) ; => "Header 1"
;; (alist-get-in my/data '(results 1 paragraph rich_text 0 plain_text)) ; => "Text"
;; (alist-get-in my/data '(results 2 heading_2 rich_text 0 plain_text)) ; => "Header 2"
;; (alist-get-in my/data '(results 3 paragraph rich_text 0 plain_text)) ; => "Another text"

;; make patch request
(setq payload-test '(("heading_1" ("rich_text" (("text" ("content" . "PUT Header 1")))))))
(lotion-blocks--patch "ee245fe3-b41c-4d31-9033-70795fd09ba8" payload-test (lambda (data err) (message "xxxxxxxxx %s" data)))

;; data models
(cl-defstruct block id type text)
(cl-defstruct page id title blocks)

;; mappers from lotion responses to data models
(defun parse-block (data)
  (let* ((id (alist-get-in data '(id)))
         (type (intern (alist-get-in data '(type))))
         (text (alist-get-in data `(,type rich_text 0 plain_text))))
    (make-block :id id :type type :text text)))

(defun parse-blocks (data)
  (let ((blocks (alist-get-in data '(results))))
    (mapcar #'parse-block blocks)))

;; (parse-blocks blocks-data)

(defun parse-page (page-data blocks-data)
  (make-page :id (alist-get-in page-data '(id))
             :title (alist-get-in page-data '(properties Name title 0 plain_text))
             :blocks (parse-blocks blocks-data)))

;; (parse-page page-data blocks-data)

;; view functions
(setq types-prefix '((heading_1 . "**") (heading_2 . "***") (paragraph . nil)))

(defun block-heading (type)
  (cdr (assoc type types-prefix)))

(defun block-to-org (block)
  (string-join
   (remq nil `(,(block-heading (block-type block))
               ,(block-text block)))
   " "))

(defun page-to-org (page)
  (string-join
   `(,(string-join `("*" ,(page-title page)) " ")
     ,@(mapcar #'block-to-org (page-blocks page))) "\n"))

;; rendering functions
(defun render-page (page)
  (lotion-write-into-buffer (page-to-org page)))

(defun lotion-write-into-buffer (header)
  (save-excursion
    (let ((lotion-buffer (get-buffer-create "*lotion*")))
      (with-current-buffer lotion-buffer
        (org-mode)
        (insert header)))))

;; function to dispatch retrieval and rendering
(defun lotion-page (uuid)
  (let ((page nil))
    (message "executing")
    (lotion-page--fetch
     uuid
     (lambda (page-data err)
       (lotion-blocks--fetch
        uuid
        (lambda (blocks-data err)
          (render-page (parse-page page-data blocks-data))))))))

;; fetch and store content in *lotion* buffer
(lotion-page "201392c852284d7c8020d2d6421a9e58")

(provide 'lotion)
;;; lotion.el ends here
