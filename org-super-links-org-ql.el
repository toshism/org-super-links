;;; org-super-links-org-ql.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.2
;; Package-Requires: (org helm-org-ql)
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

(defvar helm-org-ql-actions)
(declare-function sl--insert-link "org-super-links")
(declare-function helm-org-ql "ext:helm-org-ql")
(declare-function org-agenda-files "ext:org-mode")

(defun sl-buffer-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
If BUFFER-OR-NAME is nil return current buffer's mode."
  (buffer-local-value 'major-mode
		      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun sl-get-search-buffers ()
  "Return the buffers to provide to `helm-org-ql`.
If the current buffer is an `org-mode` buffer add it to `org-agenda-files`.
Else just return `org-agenda-files`"
  ;; this needs to add the buffer you were on before opening a capture
  ;; template too (if it's an org mode file)
  (if (and (string= (sl-buffer-mode) "org-mode") (buffer-file-name))
      (cons (buffer-file-name) (org-agenda-files))
    (org-agenda-files)))

(defun sl-link-search-interface-ql ()
  "Setup the helm-org-ql search interface."
  (add-to-list 'helm-org-ql-actions '("super-link-temp" . sl-insert-link-org-ql-action) nil)
  (helm-org-ql (sl-get-search-buffers))
  (pop helm-org-ql-actions))

;;;###autoload
(add-to-list 'helm-org-ql-actions '("Super Link" . sl-insert-link-org-ql-action) t)

(defun sl-insert-link-org-ql-action (marker)
  "Wrapper for `sl--insert-link` for org-ql integration.
MARKER is the point at first char in the selected heading."
  (let ((buffer (if marker (marker-buffer marker) nil))
	(pos (if marker (marker-position marker) nil)))
    (sl--insert-link buffer pos)))

(provide 'org-super-links-org-ql)

;;; org-super-links-org-ql.el ends here
