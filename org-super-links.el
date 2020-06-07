;;; org-super-links.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.2
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

Default is the function `sl-backlink-prefix-timestamp' which returns
an inactive timestamp formatted according to the variable
`org-time-stamp-formats' and a separator ' <- '.")

(defvar sl-backlink-postfix nil
  "Postfix to insert after the backlink.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar sl-related-into-drawer nil
    "Controls how/where to insert links.
If non-nil a drawer will be created and links inserted there.  The
default is `sl-related-drawer-default-name'.  If this is set to a
string a drawer will be created using that string.  For example LINKS.
If nil links will just be inserted at point.")

(defvar sl-related-drawer-default-name "RELATED"
  "Default name to use for link drawer if `sl-related-into-drawer' is 't'.
See `sl-related-into-drawer' for more info.")

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

nil will return the default desciption or the link.
string will be used only as a default fall back if set.
function will be called for every link.

Default is the variable `org-make-link-desciption-function'.")

(defvar sl-search-function "helm-org-ql"
  "The interface to use for finding target links.
This can be a string with one of the values 'helm-org-ql',
'helm-org-rifle', or a custom function.  If you provide a custom
function it will be called with the `point` at the location the link
should be inserted.  The only other requirement is that it should call
the function `sl--insert-link' with a marker to the target link.  AKA
the place you want the backlink.

Using 'helm-org-ql' or 'helm-org-rifle' will also add a new action to
the respective action menu.

See the function `sl-link-search-interface-ql' or for an example.")

(defvar sl-pre-link-hook nil
  "Hook called before storing the link on the link side.
This is called with point at the location where it was called.")

(defvar sl-pre-backlink-hook nil
  "Hook called before storing the link on the backlink side.
This is called with point in the heading of the backlink.")

(declare-function sl-link-search-interface-ql "ext:org-super-links-org-ql")
(declare-function sl-link-search-interface-rifle "ext:org-super-links-org-rifle")

(defun sl-search-function ()
  "Call the search interface specified in `sl-search-function'."
  (cond ((string= sl-search-function "helm-org-ql")
	 (require 'org-super-links-org-ql)
	 (sl-link-search-interface-ql))
	((string= sl-search-function "helm-org-rifle")
	 (require 'org-super-links-org-rifle)
	 (sl-link-search-interface-rifle))
	(t (funcall sl-search-function))))

(defun sl-backlink-prefix ()
  "Return an appropriate string based on variable `sl-backlink-prefix'."
  (cond ((equal sl-backlink-prefix nil) "")
	((stringp sl-backlink-prefix) sl-backlink-prefix)
	(t (funcall sl-backlink-prefix))))

(defun sl-backlink-postfix ()
  "Return an appropriate string based on variable `sl-backlink-postfix'."
  (cond ((equal sl-backlink-postfix nil) "\n")
	((stringp sl-backlink-postfix) sl-backlink-postfix)
	(t (funcall sl-backlink-postfix))))

(defun sl-link-prefix ()
  "Return an appropriate string based on variable `sl-link-prefix'."
  (cond ((equal sl-link-prefix nil) "")
	((stringp sl-link-prefix) sl-link-prefix)
	(t (funcall sl-link-prefix))))

(defun sl-link-postfix ()
  "Return an appropriate string based on variable `sl-link-postfix'."
  (cond ((equal sl-link-postfix nil) "\n")
	((stringp sl-link-postfix) sl-link-postfix)
	(t (funcall sl-link-postfix))))

(defun sl-backlink-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' <- '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
	 (time-stamp (format-time-string time-format (current-time))))
    (format "[%s] <- "
	    time-stamp)))

(defun sl-default-description-formatter (link desc)
  "Return a string to use as the link desciption.
LINK is the link target.  DESC is the provided desc."
  (let ((p sl-default-description-formatter))
    (cond ((equal p nil) (or desc link))
	  ((stringp p) (or desc p))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPERIMENTAL related into drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sl-related-into-drawer ()
  "Name of the realted drawer, as a string, or nil.
This is the value of `sl-related-into-drawer'.  However, if the
current entry has or inherits a RELATED_INTO_DRAWER property, it will
be used instead of the default value."
  (let ((p (org-entry-get nil "RELATED_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") sl-related-drawer-default-name)
	  ((stringp p) p)
	  (p sl-related-drawer-default-name)
	  ((stringp sl-related-into-drawer) sl-related-into-drawer)
	  (sl-related-into-drawer sl-related-drawer-default-name))))

(defun sl-insert-relatedlink (link desc)
  "LINK DESC related experiment."
  (if (sl-related-into-drawer)
      (let* ((org-log-into-drawer (sl-related-into-drawer))
	     (beg (org-log-beginning t)))
	(goto-char beg)
	(insert (sl-link-prefix))
	(org-insert-link nil link desc)
	(insert (sl-link-postfix))
	(org-indent-region beg (point)))
    (insert (sl-link-prefix))
    (org-insert-link nil link desc)
    (insert (sl-link-postfix))))

(defun sl-link-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' -> '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
	 (time-stamp (format-time-string time-format (current-time))))
    (format "[%s] -> "
	    time-stamp)))

(defun sl-quick-insert-drawer-link ()
  (interactive)
  ;; how to handle prefix here?
  (let ((sl-related-into-drawer (or sl-related-into-drawer t))
	(sl-link-prefix 'sl-link-prefix-timestamp))
    (sl-link)))

(defun sl-quick-insert-inline-link ()
  (interactive)
  ;; how to handle prefix here?
  (let ((sl-related-into-drawer nil)
	(sl-link-prefix nil))
    (sl-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /EXPERIMENTAL related into drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sl-insert-backlink (link desc)
  "Insert backlink to LINK with DESC.
Where the backlink is placed is determined by the variable `sl-backlink-into-drawer'."
  (let* ((org-log-into-drawer (sl-backlink-into-drawer))
	 (description (sl-default-description-formatter link desc))
	 (beg (org-log-beginning t)))
    (goto-char beg)
    (insert (sl-backlink-prefix))
    (org-insert-link nil link description)
    (insert (sl-backlink-postfix))
    (org-indent-region beg (point))))

(defun sl--insert-link (target)
  "Insert link to marker TARGET at current `point`, and create backlink to here.
Only create backlinks in files in `org-mode' or a derived mode, otherwise just
act like a normal link."
  (run-hooks 'sl-pre-link-hook)
  (call-interactively 'org-store-link)
  (let ((back-link (pop org-stored-links)))
    (with-current-buffer (marker-buffer target)
      (save-excursion
	(goto-char (marker-position target))
	(run-hooks 'sl-pre-backlink-hook)
  (when (derived-mode-p 'org-mode)
	  (sl-insert-backlink (car back-link) (cadr back-link)))
	(call-interactively 'org-store-link))))
  (let* ((forward-link (pop org-stored-links))
	 (link (car forward-link))
	 (description (sl-default-description-formatter link (cadr forward-link))))
    (sl-insert-relatedlink link description)))

;;;###autoload
(defun sl-store-link (&optional GOTO KEYS)
  "Store a point to the register for use in function `sl-insert-link'.
This is primarily intended to be called before `org-capture', but
could possibly even be used to replace `org-store-link' IF
function `sl-insert-link' is used to replace `org-insert-link'.  This
has not been thoroughly tested outside of links to/form org files.
GOTO and KEYS are unused."
  (interactive)
  (ignore GOTO)
  (ignore KEYS)
  (save-excursion
    ;; this is a hack. if the point is at the first char of a heading
    ;; the marker is not updated as expected when text is inserted
    ;; above the heading. for exapmle a capture template inserted
    ;; above. that results in the link being to the heading above the
    ;; expected heading.
    (goto-char (line-end-position))
    (let ((c1 (make-marker)))
      (set-marker c1 (point) (current-buffer))
      (set-register ?^ c1)
      (message "Link copied"))))

;; not sure if this should be autoloaded or left to config?
;;;###autoload
(advice-add 'org-capture :before 'sl-store-link)

;;;###autoload
(defun sl-insert-link ()
  "Insert a super link from the register."
  (interactive)
  (let* ((target (get-register ?^)))
    (if target
	(progn
	  (sl--insert-link target)
	  (set-register ?^ nil))
      (message "No link to insert!"))))

;;;###autoload
(defun sl-link ()
  "Insert a link and add a backlink to the target heading."
  (interactive)
  (sl-search-function))

(provide 'org-super-links)

;;; org-super-links.el ends here
