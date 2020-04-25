;;; org-super-links-org-rifle.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.2
;; Package-Requires: (org helm-org-rifle)
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

(defvar helm-org-rifle-actions)
(declare-function sl--insert-link "org-super-links")
(declare-function helm-org-rifle "ext:helm-org-rifle")

(defvar helm-org-rifle-actions)

(defun sl-insert-link-rifle-action (candidate)
  "Wrapper for `sl--insert-link` for helm/rifle integration.
CANDIDATE is a helm candidate."
  (let ((buffer (car candidate))
	(pos (cdr candidate)))
    (sl--insert-link buffer pos)))

;;;###autoload
(add-to-list 'helm-org-rifle-actions '("Super Link" . sl-insert-link-rifle-action) t)

(defun sl-link-search-interface-rifle ()
  "Search interface for helm-rifle."
  (add-to-list 'helm-org-rifle-actions '("super-link-temp" . sl-insert-link-rifle-action) nil)
  (helm-org-rifle)
  (pop helm-org-rifle-actions))

;;;###autoload
(add-to-list 'helm-org-rifle-actions '("Super Link" . sl-insert-link-rifle-action) t)


(provide 'org-super-links-org-rifle)

;;; org-super-links-org-rifle.el ends here
