;;; org-super-links.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.1
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

(require 'org)
(require 'helm-org-rifle)

(defvar sl-backlink-into-drawer t
  "Controls how/where to insert the backlinks.
If non-nil a drawer will be created and backlinks inserted there.  The
default is BACKLINKS.  If this is set to a string a drawer will be
created using that string.  For example LINKS.  If nil backlinks will
just be inserted under the heading.")

(defvar sl-backlink-entry-format "[%s] <- [[%s][%s]]"
  "This is a string passed to format.
The substitution order being time, link, description.  If
`sl-backlink-prefix` is a string it will be inserted before this.  I
may refactor this to be a format function instead.")

(defvar sl-backlink-prefix nil
  "Prefix string to insert before the result of `sl-backlink-entry-format`.")

(defun sl-backlink-into-drawer ()
  "Name of the backlink drawer, as a string, or nil.
This is the value of `sl-backlink-into-drawer'.  However, if the
current entry has or inherits a BACKLINK_INTO_DRAWER property, it will
be used instead of the default value."
  (let ((p (org-entry-get nil "BACKLINK_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") "BACKLINKS")
	  ((stringp p) p)
	  (p "BACKLINKS")
	  ((stringp sl-backlink-into-drawer) sl-backlink-into-drawer)
	  (sl-backlink-into-drawer "BACKLINKS"))))

(defun sl-backlink-prefix ()
  "Return the name of the prefix for the link as a string or nil."
  (let ((p (org-entry-get nil "BACKLINK_PREFIX" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") "BACKLINK")
	  ((stringp p) p)
	  (p "BACKLINK")
	  ((stringp sl-backlink-prefix) sl-backlink-prefix)
	  (sl-backlink-prefix "BACKLINK"))))


(defun sl-insert-backlink (link desc)
  "Insert a backlink to LINK using DESC after the current headline."
  (let* ((note-format-base (concat sl-backlink-entry-format "\n"))
	 (time-format (substring (cdr org-time-stamp-formats) 1 -1))
	 (time-stamp (format-time-string time-format (current-time)))
	 (org-log-into-drawer (sl-backlink-into-drawer))
	 (prefix (sl-backlink-prefix))
	 (note-format (if (equal prefix nil) note-format-base (concat prefix ": " note-format-base)))
	 (beg (org-log-beginning t)))

    (goto-char beg)
    (insert (format note-format
		    time-stamp
		    link
		    desc))
    (org-indent-region beg (point))))

(defun sl-insert-link (buffer pos)
  "Insert link to BUFFER POS at current point, and create backlink to here.
Only create backlinks in files in `org-mode`, otherwise just act like a
normal link."
  (call-interactively 'org-store-link)
  (let ((last-link (pop org-stored-links)))
    (with-current-buffer buffer
      (save-excursion
	(goto-char pos)
	(when (string-equal major-mode "org-mode")
	  (sl-insert-backlink (car last-link) (cadr last-link)))
	(call-interactively 'org-store-link))))
  (org-insert-last-stored-link 1))

;;;###autoload
(defun sl-point-to-register ()
  "Store a point to the register for use in `sl-link-from-register`.
ARG and PRED are unused here but come form `org-capture`."
  (interactive)
  ;; we probably don't want to link to buffers not visiting a file?
  ;; definitely not if capture is called through org-protocol for example.
  (when (buffer-file-name (current-buffer))
    (point-to-register 'sl-link)))

;; not sure if this should be autoloaded or left to config?
;;;###autoload
(advice-add 'org-capture :before 'sl-point-to-register)

(defun sl-insert-link-action (candidate)
  "Wrapper for sl-insert-link for helm/rifle integration.
CANDIDATE is a helm candidate."
  (-let (((buffer . pos) candidate))
    (sl-insert-link buffer pos)))

;; not sure if this should be autoloaded or left to config?
;;;###autoload
(add-to-list 'helm-org-rifle-actions '("super-link" . sl-insert-link-action) t)

;;;###autoload
(defun sl-link-from-register ()
  "Insert a super link for the register."
  (interactive)
  (let* ((marker (get-register 'sl-link))
	 (buffer (if marker (marker-buffer marker) nil))
	 (pos (if marker (marker-position marker) nil)))
    (if (and buffer pos)
	(progn
	  (sl-insert-link buffer pos)
	  (set-register 'sl-link nil))
      (message "No link to insert!"))))

;;;###autoload
(defun sl-link ()
  "Insert a link and add a backlink to the target heading."
  (interactive)
  (add-to-list 'helm-org-rifle-actions '("super-link-temp" . sl-insert-link-action) nil)
  (helm-org-rifle)
  (pop helm-org-rifle-actions))

(provide 'org-super-links)
;;; org-super-links.el ends here
