;;; lotion-parse.el --- Lotion parse -*- lexical-binding: t; -*-
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
;;  Lotion parser
;;
;;; Code:

;; data models
;; block is a object-type block
(cl-defstruct block id type content-type content)
;; a page is a type of block (let's store it separate for now)
(cl-defstruct page id type title blocks)

(defun lotion-parse-page (page-data blocks-data)
  "Parses a PAGE-DATA and BLOCKS-DATA from Notion's API into Lotion models"
  (make-page :id (alist-get-in page-data '(id))
             :type (intern (alist-get-in page-data '(object)))
             :title (alist-get-in page-data '(properties Name title 0 plain_text))
             :blocks (lotion-parse--blocks blocks-data)))

(defun lotion-parse--blocks (data)
  (let ((blocks (alist-get-in data '(results))))
    (mapcar #'lotion-parse--block blocks)))

(defun lotion-parse--block (data)
  (let* ((id (alist-get-in data '(id)))
         (type (intern (alist-get-in data '(type))))
         (content-type (intern (alist-get-in data `(,type rich_text 0 type))))
         (content (alist-get-in data `(,type rich_text 0 ,content-type content))))
    (make-block :id id :type type :content-type content-type :content content)))

(provide 'lotion-parse)
;;; lotion-parse.el ends here
