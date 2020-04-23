;;; org-super-links.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.1
;; Package-Version: 20200411.31
;; Package-Requires: (org)
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

;; Setup search for finding link targets.  Prefer helm-org-ql if installed, if not helm-org-rifle.
;; If neither error.
(with-eval-after-load 'org
  (cond ((require 'helm-org-ql nil 'noerror)
	 (require 'helm-org-ql)
	 (require 'org-super-links-org-ql))

	((require 'helm-org-rifle nil 'noerror)
	 (require 'helm-org-rifle)
	 (require 'org-super-links-org-rifle))
	(t (error "`org-super-links` requires one of `helm-org-ql` or `helm-org-rifle`"))))

(declare-function sl-link-search-interface "ext:org-super-links-org")

(defvar sl-backlink-into-drawer t
  "Controls how/where to insert the backlinks.
If non-nil a drawer will be created and backlinks inserted there.  The
default is BACKLINKS.  If this is set to a string a drawer will be
created using that string.  For example LINKS.  If nil backlinks will
just be inserted under the heading.")

(defvar sl-backlink-prefix 'sl-backlink-prefix-timestamp
  "Prefix to insert before the backlink.
This can be a string, nil, or a function that takes no arguments and
returns a string.
Default is `sl-backlink-prefix-timestamp` which returns an inactive
timestamp formatted according to `org-time-stamp-formats` and a
separator ' <- '.")

(defvar sl-backlink-postfix nil
  "Postfix to insert after the backlink.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar sl-link-prefix nil
  "Prefix to insert before the link.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar sl-link-postfix nil
  "Postfix to insert after the link.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar sl-default-description-formatter org-make-link-description-function
  "What to use if no description is provided.
This can be a string, nil or a function that accepts two arguments
LINK and DESC and returns a string.
Default is org-make-link-desciption-function.")

(defun sl-backlink-prefix ()
  "Return an appropriate string based on `sl-backlink-prefix` variable."
  (cond ((equal sl-backlink-prefix nil) "")
	((stringp sl-backlink-prefix) sl-backlink-prefix)
	(t (funcall sl-backlink-prefix))))

(defun sl-backlink-postfix ()
  "Return an appropriate string based on `sl-backlink-postfix` variable."
  (cond ((equal sl-backlink-postfix nil) "\n")
	((stringp sl-backlink-postfix) sl-backlink-postfix)
	(t (funcall sl-backlink-postfix))))

(defun sl-link-prefix ()
  "Return an appropriate string based on `sl-link-prefix` variable."
  (cond ((equal sl-link-prefix nil) "")
	((stringp sl-link-prefix) sl-link-prefix)
	(t (funcall sl-link-prefix))))

(defun sl-link-postfix ()
  "Return an appropriate string based on `sl-link-postfix` variable."
  (cond ((equal sl-link-postfix nil) "\n")
	((stringp sl-link-postfix) sl-link-postfix)
	(t (funcall sl-link-postfix))))

(defun sl-backlink-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats` and
a separator ' <- '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
	 (time-stamp (format-time-string time-format (current-time))))
    (format "[%s] <- "
	    time-stamp)))

(defun sl-default-description-formatter (link desc)
  "Return a string to use as the default link desciption.
LINK is the link target.  DESC is the provided desc."
  (let ((p sl-default-description-formatter))
    (cond ((equal p nil) desc)
	  ((stringp p) p)
	  ((fboundp p) (funcall p link desc))
	  (t desc))))

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

(defun sl-insert-backlink (link desc)
  "Insert backlink to LINK with DESC.
Where the backlink is placed is determined by the `sl-backlink-into-drawer` variable."
  (let* ((org-log-into-drawer (sl-backlink-into-drawer))
	 (description (sl-default-description-formatter link desc))
	 (beg (org-log-beginning t)))
    (goto-char beg)
    (insert (sl-backlink-prefix))
    (insert (format "[[%s][%s]]"
		    link
		    description))
    (insert (sl-backlink-postfix))
    (org-indent-region beg (point))))

(defun sl--insert-link (buffer pos)
  "Insert link to BUFFER POS at current `point`, and create backlink to here.
Only create backlinks in files in `org-mode`, otherwise just act like a
normal link."
  (call-interactively 'org-store-link)
  (let ((back-link (pop org-stored-links)))
    (with-current-buffer buffer
      (save-excursion
	(goto-char pos)
	(when (string-equal major-mode "org-mode")
	  (sl-insert-backlink (car back-link) (cadr back-link)))
	(call-interactively 'org-store-link))))
  (let* ((forward-link (pop org-stored-links))
	(link (car forward-link))
	(description (sl-default-description-formatter link (cadr forward-link))))
    (insert (sl-link-prefix))
    (insert (format "[[%s][%s]]"
		    link
		    description))
    (insert (sl-link-postfix))))

;;;###autoload
(defun sl-store-link (&optional GOTO KEYS)
  "Store a point to the register for use in `sl-insert-link`.
This is primarily intended to be called before `org-capture`, but
could possibly even be used to replace `org-store-link` IF
`sl-insert-link` is used to replace `org-insert-link`.  This
has not been thoroughly tested outside of links to/form org files.
GOTO and KEYS are unused."
  (interactive)
  (ignore GOTO)
  (ignore KEYS)
  ;; we probably don't want to link to buffers not visiting a file?
  ;; definitely not if capture is called through org-protocol for example.
  ;; TODO actually this isn't true. org can handle this in some cases.
  ;; ie. custom link types like notmuch emails etc.
  (if (buffer-file-name (current-buffer))
      (progn
	(point-to-register 'sl-link)
	(message "Link copied"))
    (message "No method for storing a link to this buffer.")))

;; not sure if this should be autoloaded or left to config?
;;;###autoload
(advice-add 'org-capture :before 'sl-store-link)

;;;###autoload
(defun sl-insert-link ()
  "Insert a super link from the register."
  (interactive)
  (let* ((marker (get-register 'sl-link))
	 (buffer (if marker (marker-buffer marker) nil))
	 (pos (if marker (marker-position marker) nil)))
    (if (and buffer pos)
	(progn
	  (sl--insert-link buffer pos)
	  (set-register 'sl-link nil))
      (message "No link to insert!"))))

;;;###autoload
(defun sl-link ()
  "Insert a link and add a backlink to the target heading."
  (interactive)
  (sl-link-search-interface))

(provide 'org-super-links)

;;; org-super-links.el ends here
