;;; org-super-links.el --- Make super links          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
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
(require 'org-element)
(declare-function org-make-link-description-function "ext:org-mode")

(defvar org-super-links-backlink-into-drawer t
  "Controls how/where to insert the backlinks.
If non-nil a drawer will be created and backlinks inserted there.  The
default is BACKLINKS.  If this is set to a string a drawer will be
created using that string.  For example LINKS.  If nil backlinks will
just be inserted under the heading.")

(defvar org-super-links-backlink-prefix 'org-super-links-backlink-prefix-timestamp
  "Prefix to insert before the backlink.
This can be a string, nil, or a function that takes no arguments and
returns a string.

Default is the function `org-super-links-backlink-prefix-timestamp'
which returns an inactive timestamp formatted according to the variable
`org-time-stamp-formats' and a separator ' <- '.")

(defvar org-super-links-backlink-postfix nil
  "Postfix to insert after the backlink.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-super-links-related-into-drawer nil
    "Controls how/where to insert links.
If non-nil a drawer will be created and links inserted there.  The
default is `org-super-links-related-drawer-default-name'.  If this is set to a
string a drawer will be created using that string.  For example LINKS.
If nil links will just be inserted at point.")

(defvar org-super-links-related-drawer-default-name "RELATED"
  "Default name to use for link drawer.
If variable `org-super-links-related-into-drawer' is 't' use this
name for the drawer.  See variable `org-super-links-related-into-drawer' for more info.")

(defvar org-super-links-link-prefix nil
  "Prefix to insert before the link.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-super-links-link-postfix nil
  "Postfix to insert after the link.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-super-links-default-description-formatter org-make-link-description-function
  "What to use if no description is provided.
This can be a string, nil or a function that accepts two arguments
LINK and DESC and returns a string.

nil will return the default desciption or the link.
string will be used only as a default fall back if set.
function will be called for every link.

Default is the variable `org-make-link-desciption-function'.")

(defvar org-super-links-search-function
  (cond ((require 'helm-org-ql nil 'no-error) "helm-org-ql")
	((require 'helm-org-rifle nil 'no-error) "helm-org-rifle")
	(t 'org-super-links-get-location))
  "The interface to use for finding target links.
This can be a string with one of the values 'helm-org-ql',
'helm-org-rifle', or a function.  If you provide a custom
function it will be called with the `point` at the location the link
should be inserted.  The only other requirement is that it should call
the function `org-super-links--insert-link' with a marker to the target link.
AKA the place you want the backlink.

Using 'helm-org-ql' or 'helm-org-rifle' will also add a new
action to the respective action menu.

See the function `org-super-links-link-search-interface-ql' or for an example.

Default is set based on currently installed packages.  In order of priority:
- 'helm-org-ql'
- 'helm-org-rifle'
- `org-super-links-get-location'

`org-super-links-get-location' internally uses `org-refile-get-location'.")

(defvar org-super-links-pre-link-hook nil
  "Hook called before storing the link on the link side.
This is called with point at the location where it was called.")

(defvar org-super-links-pre-backlink-hook nil
  "Hook called before storing the link on the backlink side.
This is called with point in the heading of the backlink.")

(declare-function org-super-links-org-ql-link-search-interface "ext:org-super-links-org-ql")
(declare-function org-super-links-org-rifle-link-search-interface "ext:org-super-links-org-rifle")

(defun org-super-links-get-location ()
  "Default for function `org-super-links-search-function' that reuses the `org-refile' machinery."
  (let ((target (org-refile-get-location "Super Link")))
    (org-super-links--insert-link (set-marker (make-marker) (car (cdddr target))
				 (get-file-buffer (car (cdr target)))))))

(defun org-super-links-search-function ()
  "Call the search interface specified in variable `org-super-links-search-function'."
  (cond ((string= org-super-links-search-function "helm-org-ql")
	 (require 'org-super-links-org-ql)
	 (org-super-links-org-ql-link-search-interface))
	((string= org-super-links-search-function "helm-org-rifle")
	 (require 'org-super-links-org-rifle)
	 (org-super-links-org-rifle-link-search-interface))
	(t (funcall org-super-links-search-function))))

(defun org-super-links-backlink-prefix ()
  "Return an appropriate string based on variable `org-super-links-backlink-prefix'."
  (cond ((equal org-super-links-backlink-prefix nil) "")
	((stringp org-super-links-backlink-prefix) org-super-links-backlink-prefix)
	(t (funcall org-super-links-backlink-prefix))))

(defun org-super-links-backlink-postfix ()
  "Return an appropriate string based on variable `org-super-links-backlink-postfix'."
  (cond ((equal org-super-links-backlink-postfix nil) "\n")
	((stringp org-super-links-backlink-postfix) org-super-links-backlink-postfix)
	(t (funcall org-super-links-backlink-postfix))))

(defun org-super-links-link-prefix ()
  "Return an appropriate string based on variable `org-super-links-link-prefix'."
  (cond ((equal org-super-links-link-prefix nil) "")
	((stringp org-super-links-link-prefix) org-super-links-link-prefix)
	(t (funcall org-super-links-link-prefix))))

(defun org-super-links-link-postfix ()
  "Return an appropriate string based on variable `org-super-links-link-postfix'."
  (cond ((equal org-super-links-link-postfix nil) "")
	((stringp org-super-links-link-postfix) org-super-links-link-postfix)
	(t (funcall org-super-links-link-postfix))))

(defun org-super-links-backlink-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' <- '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
	 (time-stamp (format-time-string time-format (current-time))))
    (format "[%s] <- "
	    time-stamp)))

(defun org-super-links-default-description-formatter (link desc)
  "Return a string to use as the link desciption.
LINK is the link target.  DESC is the provided desc."
  (let ((p org-super-links-default-description-formatter))
    (cond ((equal p nil) (or desc link))
	  ((stringp p) (or desc p))
	  ((fboundp p) (funcall p link desc))
	  (t desc))))

(defun org-super-links-backlink-into-drawer ()
  "Name of the backlink drawer, as a string, or nil.
This is the value of variable
`org-super-links-backlink-into-drawer'.  However, if the current
entry has or inherits a BACKLINK_INTO_DRAWER property, it will be
used instead of the default value."
  (let ((p (org-entry-get nil "BACKLINK_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") "BACKLINKS")
	  ((stringp p) p)
	  (p "BACKLINKS")
	  ((stringp org-super-links-backlink-into-drawer) org-super-links-backlink-into-drawer)
	  (org-super-links-backlink-into-drawer "BACKLINKS"))))

;; delete related functions
(defun org-super-links--find-link (id)
  "Return link element for ID."
  (org-super-links--org-narrow-to-here)
  (let ((link
	 (org-element-map (org-element-parse-buffer) 'link
	   (lambda (link)
	     (when (string= (org-element-property :path link) id)
	       link)))))
    (widen)
    (if (> (length link) 1)
	(error "Multiple links found.  Canceling delete")
      (car link))))

(defun org-super-links--org-narrow-to-here ()
  "Narrow to current heading, excluding subheadings."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))


(defun org-super-links--in-drawer ()
  "Return nil if point is not in a drawer.
Return element at point is in a drawer."
  (let ((element (org-element-at-point)))
    (while (and element
		(not (memq (org-element-type element) '(drawer property-drawer))))
      (setq element (org-element-property :parent element)))
    element))


(defun org-super-links--delete-link (link)
  "Delete the LINK.
If point is in drawer, delete the entire line."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (if (org-super-links--in-drawer)
	(progn
	  (kill-whole-line 1)
	  (org-remove-empty-drawer-at (point)))
      (delete-region (org-element-property :begin link) (org-element-property :end link)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPERIMENTAL related into drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-super-links-related-into-drawer ()
  "Name of the related drawer, as a string, or nil.
This is the value of variable
`org-super-links-related-into-drawer'.  However, if the current
entry has or inherits a RELATED_INTO_DRAWER property, it will be
used instead of the default value."
  (let ((p (org-entry-get nil "RELATED_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") org-super-links-related-drawer-default-name)
	  ((stringp p) p)
	  (p org-super-links-related-drawer-default-name)
	  ((stringp org-super-links-related-into-drawer) org-super-links-related-into-drawer)
	  (org-super-links-related-into-drawer org-super-links-related-drawer-default-name))))

(defun org-super-links-insert-relatedlink (link desc)
  "LINK DESC related experiment."
  (if (org-super-links-related-into-drawer)
      (let* ((org-log-into-drawer (org-super-links-related-into-drawer))
	     (beg (org-log-beginning t)))
	(goto-char beg)
	(insert (org-super-links-link-prefix))
	(org-insert-link nil link desc)
	(insert (org-super-links-link-postfix) "\n")
	(org-indent-region beg (point)))
    (insert (org-super-links-link-prefix))
    (org-insert-link nil link desc)
    (insert (org-super-links-link-postfix))))

(defun org-super-links-link-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' -> '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
	 (time-stamp (format-time-string time-format (current-time))))
    (format "[%s] -> "
	    time-stamp)))

(defun org-super-links-quick-insert-drawer-link ()
  "Insert link into drawer regardless of variable `org-super-links-related-into-drawer' value."
  (interactive)
  ;; how to handle prefix here?
  (let ((org-super-links-related-into-drawer (or org-super-links-related-into-drawer t))
	(org-super-links-link-prefix 'org-super-links-link-prefix-timestamp))
    (org-super-links-link)))

(defun org-super-links-quick-insert-inline-link ()
  "Insert inline link regardless of variable `org-super-links-related-into-drawer' value."
  (interactive)
  ;; how to handle prefix here?
  (let ((org-super-links-related-into-drawer nil)
	(org-super-links-link-prefix nil))
    (org-super-links-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /EXPERIMENTAL related into drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-super-links-insert-backlink (link desc)
  "Insert backlink to LINK with DESC.
Where the backlink is placed is determined by the variable `org-super-links-backlink-into-drawer'."
  (let* ((org-log-into-drawer (org-super-links-backlink-into-drawer))
	 (description (org-super-links-default-description-formatter link desc))
	 (beg (org-log-beginning t)))
    (goto-char beg)
    (insert (org-super-links-backlink-prefix))
    (insert (org-link-make-string link description))
    (insert (org-super-links-backlink-postfix))
    (org-indent-region beg (point))))

(defun org-super-links-links-action (marker hooks)
  "Go to MARKER, run HOOKS and store a link."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (run-hooks hooks)
      (call-interactively #'org-store-link)
      (pop org-stored-links))))

(defun org-super-links-link-builder (link)
  "Format link description for LINK."
  (let* ((link-ref (car link))
	 (pre-desc (cadr link))
	 (description (org-super-links-default-description-formatter link-ref pre-desc)))
    (cons link-ref description)))

(defun org-super-links--insert-link (target &optional no-forward)
  "Insert link to marker TARGET at current `point`, and create backlink to here.
Only create backlinks in files in `org-mode' or a derived mode, otherwise just
act like a normal link.

If NO-FORWARD is non-nil skip creating the forward link.  Currently
only used when converting a link."
  (let* ((source (point-marker))
	 (source-link (org-super-links-links-action source 'org-super-links-pre-link-hook))
	 (target-link (org-super-links-links-action target 'org-super-links-pre-backlink-hook))
	 (source-formatted-link (org-super-links-link-builder source-link))
	 (target-formatted-link (org-super-links-link-builder target-link)))
    (with-current-buffer (marker-buffer target)
      (save-excursion
	(goto-char (marker-position target))
	(when (derived-mode-p 'org-mode)
	  (org-super-links-insert-backlink (car source-formatted-link) (cdr source-formatted-link)))))
    (unless no-forward
      (with-current-buffer (marker-buffer source)
	(save-excursion
	  (goto-char (marker-position source))
	  (org-super-links-insert-relatedlink (car target-formatted-link) (cdr target-formatted-link)))))))


;;;###autoload
(defun org-super-links-convert-link-to-super (arg)
  "Convert a normal `org-mode' link at `point' to super link, ARG prefix.
If variable `org-super-links-related-into-drawer' is non-nil move
the link into drawer.

When called interactively with a `C-u' prefix argument ignore
variable `org-super-links-related-into-drawer' configuration and
do not modify existing link."
  (interactive "P")
  (let ((from-m (point-marker))
	(target (save-window-excursion
		  (with-current-buffer (current-buffer)
		    (save-excursion
		      (org-open-at-point)
		      (point-marker))))))
    (org-super-links--insert-link target arg)
    (goto-char (marker-position from-m)))

  (when (and (not arg) (org-super-links-related-into-drawer))
    (let ((begin (org-element-property :begin (org-element-context)))
	  (end (org-element-property :end (org-element-context))))
      (delete-region begin end))))

;;;###autoload
(defun org-super-links-delete-link ()
  "Delete the link at point, and the corresponding reverse link.
If no reverse link exists, just delete link at point.
This works from either side, and deletes both sides of a link."
  (interactive)
  (save-window-excursion
    (with-current-buffer (current-buffer)
      (save-excursion
	(let ((id (org-id-get (point))))
	  (org-open-at-point)
	  (let ((link-element (org-super-links--find-link id)))
	    (if link-element
		(org-super-links--delete-link link-element)
	      (message "No backlink found. Deleting active only.")))))))
  (org-super-links--delete-link (org-element-context)))

;;;###autoload
(defun org-super-links-store-link (&optional GOTO KEYS)
  "Store a point to register for use in function `org-super-links-insert-link'.
This is primarily intended to be called before `org-capture', but
could possibly even be used to replace `org-store-link' IF
function `org-super-links-insert-link' is used to replace
`org-insert-link'.  This has not been thoroughly tested outside
of links to/form org files.  GOTO and KEYS are unused."
  (interactive "P")
  (ignore GOTO)
  (ignore KEYS)
  (save-excursion
    ;; this is a hack. if the point is at the first char of a heading
    ;; the marker is not updated as expected when text is inserted
    ;; above the heading. for example a capture template inserted
    ;; above. that results in the link being to the heading above the
    ;; expected heading.
    (goto-char (line-end-position))
    (let ((c1 (make-marker)))
      (set-marker c1 (point) (current-buffer))
      (set-register ?^ c1)
      (message "Link copied"))))

;; not sure if this should be autoloaded or left to config?
;;;###autoload
(advice-add 'org-capture :before #'org-super-links-store-link)

;;;###autoload
(defun org-super-links-insert-link ()
  "Insert a super link from the register."
  (interactive)
  (let* ((target (get-register ?^)))
    (if target
	(progn
	  (org-super-links--insert-link target)
	  (set-register ?^ nil))
      (message "No link to insert!"))))

;;;###autoload
(defun org-super-links-link ()
  "Insert a link and add a backlink to the target heading."
  (interactive)
  (org-super-links-search-function))

(provide 'org-super-links)

;;; org-super-links.el ends here
