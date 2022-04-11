;;; lotion.el --- Use Notion in your Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; URL: https://github.com/sienic/lotion
;; Version: 0.1.0
;; Keywords: org-mode
;; Package-Requires: ((emacs "26.1") (request "0.3.3"))

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
(require 'org)              ;
(require 'request)          ;
(require 'lotion-api)       ; communication layer with Notion
(require 'lotion-parse)     ; transforms notion data into lotion data structures
(require 'lotion-oom)       ; creates org raw text nodes from pages and blocks
(require 'lotion-render)    ; renders text representing org elements

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

;;; playground
;; example data
(setq page-data nil)
(setq blocks-data nil)
;; blocks-data ; => ((object . "list") (results . [((object . "block") (id . "ee245fe3-b41c-4d31-9033-70795fd09ba8") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "heading_1") (heading_1 (rich_text . [((type . "text") (text (content . "Header 1") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Header 1") (href))]) (color . "default"))) ((object . "block") (id . "18ceecee-ecd0-48e2-95d3-21dd9742ea9d") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "paragraph") (paragraph (rich_text . [((type . "text") (text (content . "Text") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Text") (href))]) (color . "default"))) ((object . "block") (id . "d695bcf2-2124-413d-9f15-b01144040e3a") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "heading_2") (heading_2 (rich_text . [((type . "text") (text (content . "Header 2") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Header 2") (href))]) (color . "default"))) ((object . "block") (id . "d7e66203-049b-4dd6-ad3d-76076ba35b47") (created_time . "2022-04-07T17:50:00.000Z") (last_edited_time . "2022-04-07T17:50:00.000Z") (created_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (last_edited_by (object . "user") (id . "e8fb2099-0bfe-41e9-a688-9e9ca053464d")) (has_children . :json-false) (archived . :json-false) (type . "paragraph") (paragraph (rich_text . [((type . "text") (text (content . "Another text") (link)) (annotations (bold . :json-false) (italic . :json-false) (strikethrough . :json-false) (underline . :json-false) (code . :json-false) (color . "default")) (plain_text . "Another text") (href))]) (color . "default")))]) (next_cursor) (has_more . :json-false) (type . "block") (block))
(lotion-api-page-fetch "201392c852284d7c8020d2d6421a9e58" (lambda (data err) (setq page-data data)))
;; (alist-get-in page-data '(properties Name title 0 plain_text)) ; => "Getting a normal card with simple text nodes"
(lotion-api-blocks-fetch "201392c852284d7c8020d2d6421a9e58" (lambda (data err) (setq blocks-data data)))
;; (alist-get-in my/data '(results 0 heading_1 rich_text 0 plain_text)) ; => "Header 1"
;; (alist-get-in my/data '(results 1 paragraph rich_text 0 plain_text)) ; => "Text"
;; (alist-get-in my/data '(results 2 heading_2 rich_text 0 plain_text)) ; => "Header 2"
;; (alist-get-in my/data '(results 3 paragraph rich_text 0 plain_text)) ; => "Another text"

(defun lotion-page-get (page-uuid)
  "Fetch and render the page with PAGE-UUID and its children"
  (let ((page nil))
    (message "executing")
    (lotion-api-page-fetch
     page-uuid
     (lambda (page-data err)
       (lotion-api-blocks-fetch
        page-uuid
        (lambda (blocks-data err)
          (lotion-render-page
           (lotion-parse-page page-data blocks-data))))))))

;; fetch and store content in *lotion* buffer
(lotion-page-get "201392c852284d7c8020d2d6421a9e58")

;; updating notion from a model
(setq block-to-update
      (make-block :id "ee245fe3-b41c-4d31-9033-70795fd09ba8"
                  :type "heading_1"
                  :content-type "text"
                  :content "New text"))

;; TODO: Figure out in which layer this should live
(defun block-to-payload (block)
  `((,(block-type block) ("rich_text" ((,(block-content-type block) ("content" . ,(block-content block))))))))

(lotion-api-block-patch
 "ee245fe3-b41c-4d31-9033-70795fd09ba8"
 (block-to-payload block-to-update)
 (lambda (data err) (message "xxxxxxxxx %s" data)))

(provide 'lotion)
;;; lotion.el ends here
