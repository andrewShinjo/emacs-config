;;; org-study-update.el --- Update heading properties in Org files -*- lexical-binding: t; -*-

(require 'org-study-model)
(require 'org)
(require 'org-id)
(require 'cl-lib)

(defun org-study--locate-heading (heading)
  "Return a marker for HEADING, creating an ID if necessary."
  (if (org-study-heading-file-level heading)
      (with-current-buffer (find-file-noselect (org-study-heading-file heading))
        (goto-char (point-min))
        (point-marker))
    (or (when-let* ((id (org-study-heading-id heading)))
          (org-id-find id 'marker))
        (let ((file (org-study-heading-file heading))
              (text (org-study-heading-text heading)))
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

(cl-defun org-study-update-heading (heading &key priority last-reviewed)
  "Update HEADING's PRIORITY and/or LAST-REVIEWED date in its Org file."
  (if-let* ((marker (org-study--locate-heading heading)))
      (progn
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char (marker-position marker))
            (when priority
              (org-entry-put (point) REVIEW-PRIORITY-PROPERTY
                             (number-to-string priority))
              (setf (org-study-heading-priority heading) priority))
            (when last-reviewed
              (org-entry-put (point) REVIEW-LAST-PROPERTY last-reviewed)
              (setf (org-study-heading-last-reviewed heading) last-reviewed))
            (save-buffer)
            (when-let* ((new-id (org-id-get (point))))
              (setf (org-study-heading-id heading) new-id))))
        heading)
    (message "org-study: heading not found: %s"
             (org-study-heading-text heading))))

(defun org-study-goto-heading (heading)
  "Display HEADING's file and move point to it."
  (if-let* ((marker (org-study--locate-heading heading)))
      (progn
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker)))
    (message "org-study: heading not found: %s"
             (org-study-heading-text heading))))

(provide 'org-study-update)
