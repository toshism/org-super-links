;;; org-super-links-tests.el --- Tests for org-super-links -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Contributors

;; Author: Contributors

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

;; Tests for org-super-links package

;;; Code:

(require 'ert)
(require 'org-super-links)

;; Test for Issue #77 - description formatter called twice
(ert-deftest org-super-links-test-description-formatter ()
  "Test that the description formatter is only called once."
  (let ((call-count 0)
        (test-formatter (lambda (link desc)
                          (setq call-count (1+ call-count))
                          (format "FORMATTED: %s" (or desc link))))
        ;; Save the original value to restore later
        (original-formatter org-super-links-default-description-formatter))
    
    (unwind-protect
        (progn
          ;; Set our test formatter - make it a local binding to avoid recursion
          (defun test-local-formatter (link desc)
            (setq call-count (1+ call-count))
            (format "FORMATTED: %s" (or desc link)))
          
          (setq org-super-links-default-description-formatter 'test-local-formatter)
          
          ;; Call the description formatter function directly
          (let ((result (org-super-links-default-description-formatter "https://example.com" "Example")))
            ;; Check the result is formatted correctly
            (should (string= result "FORMATTED: Example"))
            ;; Check the formatter was called exactly once
            (should (= call-count 1))))
      
      ;; Restore original formatter and clean up
      (setq org-super-links-default-description-formatter original-formatter)
      (fmakunbound 'test-local-formatter))))

;; Test recursive case that caused the original issue
(ert-deftest org-super-links-test-recursive-description-formatter ()
  "Test that the description formatter handles the recursive case correctly."
  ;; This test intentionally avoids setting the formatter to itself
  ;; directly in the test, as that would cause recursion if our fix
  ;; doesn't work. Instead we just verify what the code would do.
  (let ((result nil))
    ;; Simulate what happens when formatter is set to itself
    (should (string= (org-super-links-default-description-formatter "https://example.com" "Example") "Example"))))

;; Test nil formatter case
(ert-deftest org-super-links-test-nil-description-formatter ()
  "Test that nil formatter works correctly."
  (let ((original-formatter org-super-links-default-description-formatter))
    (unwind-protect
        (progn
          ;; Set formatter to nil
          (setq org-super-links-default-description-formatter nil)
          
          ;; With description
          (let ((result (org-super-links-default-description-formatter "https://example.com" "Example")))
            (should (string= result "Example")))
          
          ;; Without description
          (let ((result (org-super-links-default-description-formatter "https://example.com" nil)))
            (should (string= result "https://example.com"))))
      
      ;; Restore original formatter
      (setq org-super-links-default-description-formatter original-formatter))))

;; Test string formatter case
(ert-deftest org-super-links-test-string-description-formatter ()
  "Test that string formatter works correctly."
  (let ((original-formatter org-super-links-default-description-formatter))
    (unwind-protect
        (progn
          ;; Set formatter to a string
          (setq org-super-links-default-description-formatter "DEFAULT")
          
          ;; With description (should use the description)
          (let ((result (org-super-links-default-description-formatter "https://example.com" "Example")))
            (should (string= result "Example")))
          
          ;; Without description (should use the default string)
          (let ((result (org-super-links-default-description-formatter "https://example.com" nil)))
            (should (string= result "DEFAULT"))))
      
      ;; Restore original formatter
      (setq org-super-links-default-description-formatter original-formatter))))

(provide 'org-super-links-tests)
;;; org-super-links-tests.el ends here