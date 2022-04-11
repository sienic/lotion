;;; lotion-render.el --- Lotion render -*- lexical-binding: t; -*-
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
;;  Lotion render
;;
;;; Code:

(defun lotion-render-page (page)
  "Render PAGE and its contents"
  (lotion-render--flush (lotion-oom-page-to-org page)))

(defun lotion-render--flush(content)
  "Flushes CONTENT into the lotion buffer"
  (save-excursion
    (with-current-buffer (get-buffer-create "*lotion*")
      (org-mode)
      (save-excursion
        (delete-region (point-min) (point-max))
        (insert content)))
    (switch-to-buffer-other-window "*lotion*")))

(provide 'lotion-render)
;;; lotion-render.el ends here
