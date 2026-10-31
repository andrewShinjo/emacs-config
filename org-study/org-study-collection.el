;;; org-study-collection.el --- Collect headings from org files -*- lexical-binding: t; -*-

(require 'org-study-model)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-id)
(require 'org-heading-at-point)

(defgroup org-study nil
  "Review queue for org-mode headings."
  :group 'org)

(defcustom org-study-review-tags '("study" "edit-later" "extract" "exercise")
  "Tags that identify headings for the review queue."
  :type '(repeat string))

(defun org-study--collect-file-level-items (file tags)
  "Return a heading for FILE when its FILETAGS intersect TAGS."
  (when-let* ((filetags-result (org-collect-keywords '("FILETAGS")))
              (all-tags (cl-mapcan (lambda (value) (split-string value ":" t))
                                   (cdar filetags-result))))
    (when (cl-some (lambda (tag) (member tag tags)) all-tags)
      (goto-char (point-min))
      (list
       (make-heading
        :file file
        :id file
        :text (or (cadar (org-collect-keywords '("TITLE")))
                  (file-name-nondirectory file))
        :tags all-tags
        :priority (let ((value (org-entry-get (point) REVIEW-PRIORITY-PROPERTY)))
                    (and value (string-to-number value)))
        :last-reviewed (org-entry-get (point) REVIEW-LAST-PROPERTY)
        :file-level t)))))

(cl-defun org-study-collect-headings (&key tags)
  "Collect reviewable headings, optionally restricted to TAGS."
  (let* ((tags (or tags org-study-review-tags))
         (files (directory-files-recursively org-directory "\\.org\\'")))
    (cl-mapcan
     (lambda (file)
       (let ((create-lockfiles nil))
         (with-current-buffer (find-file-noselect file)
           (let ((org-use-tag-inheritance nil)
                 (results nil))
             (org-map-entries
              (lambda ()
                (unless (andy/org-study/get-flashcard-types-on-heading-at-point)
                  (push
                   (make-heading
                    :file file
                    :id (org-id-get)
                    :text (org-get-heading 'no-todo 'no-tags)
                    :tags (org-get-tags nil t)
                    :priority (let ((value (org-entry-get
                                            nil REVIEW-PRIORITY-PROPERTY)))
                                (and value (string-to-number value)))
                    :last-reviewed (org-entry-get nil REVIEW-LAST-PROPERTY))
                   results)))
              (string-join tags "|") 'file)
             (nconc (nreverse results)
                    (org-study--collect-file-level-items file tags))))))
     files)))

(provide 'org-study-collection)
