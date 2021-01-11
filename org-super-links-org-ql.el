;;; org-super-links-org-ql.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.4
;; Package-Requires: ((emacs "26.1") (helm-org-ql "0.5"))
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
(declare-function helm-org-ql "ext:helm-org-ql")
(declare-function org-agenda-files "ext:org-mode")
(defvar helm-org-ql-actions)

(defun org-super-links-org-ql-buffer-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
If BUFFER-OR-NAME is nil return current buffer's mode."
  (buffer-local-value 'major-mode
		      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun org-super-links-org-ql-get-search-buffers ()
  "Return the buffers to provide to `helm-org-ql`.
If the current buffer is an `org-mode` buffer add it to `org-agenda-files`.
Else just return `org-agenda-files`"
  ;; this needs to add the buffer you were on before opening a capture
  ;; template too (if it's an org mode file)
  (if (and (string= (org-super-links-org-ql-buffer-mode) "org-mode") (buffer-file-name))
      (cons (buffer-file-name) (org-agenda-files))
    (org-agenda-files)))


(defun org-super-links-org-ql-link-search-interface ()
  "Setup the `helm-org-ql' search interface."
  (add-to-list 'helm-org-ql-actions '("super-link-temp" . org-super-links-org-ql-insert-link-action) nil)
  (helm-org-ql (org-super-links-org-ql-get-search-buffers))
  (pop helm-org-ql-actions))

(with-eval-after-load "helm-org-ql"
  (add-to-list 'helm-org-ql-actions '("Super Link" . org-super-links-org-ql-insert-link-action) t))

(defun org-super-links-org-ql-insert-link-action (marker)
  "Wrapper for `org-super-links--insert-link` for `org-ql' integration.
MARKER is the point at first char in the selected heading."
  (org-super-links--insert-link marker))

(provide 'org-super-links-org-ql)

;;; org-super-links-org-ql.el ends here
