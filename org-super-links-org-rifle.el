;;; org-super-links-org-rifle.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.4
;; Package-Requires: ((emacs "26.1") (helm-org-rifle "0.1"))
;; URL: https://github.com/toshism/org-super-links
;; Keywords: convenience, hypermedia

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

;; The most overly ambitiously named package to help you make links with backlinks.
;;
;; I should describe how it actually works here.

;;; Code:

(declare-function org-super-links--insert-link "org-super-links")
(declare-function helm-org-rifle "ext:helm-org-rifle")
(defvar helm-org-rifle-actions)

(defun org-super-links-org-rifle-insert-link-action (candidate)
  "Wrapper for `org-super-links--insert-link` for helm/rifle integration.
CANDIDATE is a helm candidate."
  (let ((buffer (car candidate))
	(pos (cdr candidate))
	(target (make-marker)))
    (set-marker target pos buffer)
    (org-super-links--insert-link target)))

(with-eval-after-load "helm-org-rifle"
  (add-to-list 'helm-org-rifle-actions '("Super Link" . org-super-links-org-rifle-insert-link-action) t))

(defun org-super-links-org-rifle-link-search-interface ()
  "Search interface for helm-rifle."
  (add-to-list 'helm-org-rifle-actions '("super-link-temp" . org-super-links-org-rifle-insert-link-action) nil)
  (helm-org-rifle)
  (pop helm-org-rifle-actions))

(provide 'org-super-links-org-rifle)

;;; org-super-links-org-rifle.el ends here
