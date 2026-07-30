;;; org-study-collection.el --- Collect headings from org files -*- lexical-binding: t -*-

(require 'org-study-model)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'org)

(defgroup org-study nil
  "Review queue for org-mode headings."
  :group 'org)

(defcustom org-study-review-tags '("study" "edit-later" "extract" "exercise")
  "Tags that identify headings for the review queue."
  :type '(repeat string))

(defun org-study--collect-file-level-items (file tags)
  (let ((filetags-result (org-collect-keywords '("FILETAGS"))))
    (when filetags-result
      (let ((all-tags (cl-mapcan (lambda (v) (split-string v ":" t))
                                 (cdar filetags-result))))
        (when (cl-some (lambda (ft) (member ft tags)) all-tags)
          (goto-char (point-min))
          (list
           (make-heading
            :file file
            :id file
            :text (or (cadar (org-collect-keywords '("TITLE")))
                      (file-name-nondirectory file))
            :tags all-tags
            :priority (let ((v (org-entry-get (point) REVIEW-PRIORITY-PROPERTY)))
                        (and v (string-to-number v)))
            :last-reviewed (org-entry-get (point) REVIEW-LAST-PROPERTY)
            :file-level t)))))))

(cl-defun org-study-collect-headings (&key tags)
  (let* ((tags (or tags org-study-review-tags))
         (files (seq-filter (lambda (f) (string= (file-name-extension f) "org"))
            (directory-files-recursively org-directory directory-files-no-dot-files-regexp))))
    (cl-mapcan
     (lambda (file)
       (let ((create-lockfiles nil))
         (with-current-buffer (find-file-noselect file)
           (let ((org-use-tag-inheritance nil)
                 (results '()))
             (org-map-entries
              (lambda ()
                (let* ((p (point))
                       (priority (let ((v (org-entry-get p REVIEW-PRIORITY-PROPERTY)))
                                   (and v (string-to-number v))))
                       (last-reviewed (org-entry-get p REVIEW-LAST-PROPERTY)))
                  (push
                   (make-heading
                    :file file
                    :id (org-id-get p)
                    :text (org-get-heading 'no-todo 'no-tags)
                    :tags (org-get-tags nil t)
                    :priority priority
                    :last-reviewed last-reviewed)
                   results)))
              (string-join tags "|") 'file)
             (nconc (nreverse results)
                    (org-study--collect-file-level-items file tags))))))
     files)))

(provide 'org-study-collection)
