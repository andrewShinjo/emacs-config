;;; org-study-update.el --- Update heading properties in org files -*- lexical-binding: t -*-

(require 'org-study-model)
(require 'org)
(require 'cl-lib)

(defun org-study--locate-heading (heading)
  (if (heading-file-level heading)
      (with-current-buffer (find-file-noselect (heading-file heading))
        (goto-char (point-min))
        (point-marker))
    (if-let ((id (heading-id heading))
             (marker (org-id-find id 'marker)))
        marker
      (let* ((file (heading-file heading))
             (text (heading-text heading)))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (catch 'found
              (org-map-entries
               (lambda ()
                 (when (string= (org-get-heading 'no-todo 'no-tags) text)
                   (org-id-get-create)
                   (throw 'found (point-marker))))
               nil 'file)
              nil)))))))

(cl-defun org-study-update-heading (heading &key review-due review-increment priority last-reviewed)
  (if-let ((marker (org-study--locate-heading heading)))
      (progn
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char (marker-position marker))
            (when review-due
              (org-entry-put (point) REVIEW-DUE-PROPERTY review-due))
            (when review-increment
              (org-entry-put (point) REVIEW-INCREMENT-PROPERTY review-increment))
            (when priority
              (org-entry-put (point) REVIEW-PRIORITY-PROPERTY (number-to-string priority))
              (setf (heading-priority heading) priority))
            (when last-reviewed
              (org-entry-put (point) REVIEW-LAST-PROPERTY last-reviewed)
              (setf (heading-last-reviewed heading) last-reviewed))
            (save-buffer)
            (let ((new-id (org-id-get (point))))
              (when new-id
                (setf (heading-id heading) new-id)))))
        heading)
    (message "org-study: heading not found: %s" (heading-text heading))))

(defun org-study-goto-heading (heading)
  (if-let ((marker (org-study--locate-heading heading)))
      (progn
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker)))
    (message "org-study: heading not found: %s" (heading-text heading))))

(provide 'org-study-update)
