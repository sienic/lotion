;;; lotion-oom.el --- Lotion org object model -*- lexical-binding: t; -*-

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
;; Lotion oom
;;
;;; Code:

(defconst lotion-oom-org-template-lookup
  '((page . "* %s") (heading_1 . "** %s") (heading_2 . "*** %s") (paragraph . "%s"))
  "Map of org templates per block type")

(defun lotion-oom--template-get (block-type)
  "Returns the template for the BLOCK-TYPE"
  (cdr (assq block-type lotion-oom-org-template-lookup)))

(defun lotion-oom--page--to-org (page)
  "Convert PAGE into an org line"
  (format (lotion-oom--template-get (page-type page))
          (page-title page)))

(defun lotion-oom--block-to-org (block)
  "Convert BLOCK into an org line"
  (format (lotion-oom--template-get (block-type block))
          (block-content block)))

(defun lotion-oom--element-to-org (elt)
  "Convert an ELT into an org line"
  (cond ((page-p elt) (lotion-oom--page--to-org elt))
        ((block-p elt) (lotion-oom--block-to-org elt))))

(defun lotion-oom-page-to-org (page)
  "Converts a page and its children blocks into org syntax"
  (string-join
   (mapcar #'lotion-oom--element-to-org (cons page (page-blocks page))) "\n"))

(provide 'lotion-oom)
