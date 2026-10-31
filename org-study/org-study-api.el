;;; org-study-api.el --- Review queue commands -*- lexical-binding: t -*-

(require 'subr-x)
(require 'org-study-queue)
(require 'org-study)

(defun org-study--review-tags-internal (tags &optional skip)
  "Review items matching TAGS (a list of strings).
Prompt for an interest rating on the previous item, then pick a
weighted random heading.  With SKIP non-nil, skip recording the
previous item."
  (when (and org-study--current-review-heading (not skip))
    (let ((heading org-study--current-review-heading))
      (pcase (read-char-choice
              (format "%s — (%s) (r)eviewed, (i)nterested, (n)ot interested: "
                      (file-name-nondirectory
                       (org-study-heading-file heading))
                      (truncate-string-to-width
                       (org-study-heading-text heading) 40 nil nil t))
              '(?r ?i ?n))
        (?i (org-study-record-review heading 'interested)
            (message "Recorded as interesting (priority +1)"))
        (?n (org-study-record-review heading 'not-interested)
            (message "Recorded as not interesting (priority -1)"))
        (?r (org-study-record-review heading 'neutral)
            (message "Recorded as reviewed")))))
  (if-let* ((next (org-study--weighted-random-pick
                   (org-study-collect-headings :tags tags))))
      (progn
        (org-study-goto-heading next)
        (setq org-study--current-review-heading next)
        (message "Reviewing: %s%s  [p:%d]"
                 (if (org-study-heading-file-level next) "[FILE] " "")
                 (org-study-heading-text next)
                 (or (org-study-heading-priority next)
                     org-study-priority-default)))
    (setq org-study--current-review-heading nil)
    (message "org-study: no headings%s found to review"
             (if tags
                 (format " with tags \"%s\"" (string-join tags ", "))
               ""))))

(defun andy/org-study/review-notes (&optional skip)
  "Review headings tagged with `org-study-review-tags'.
Pick a weighted random heading and prompt for an interest rating on the
previous one.
With prefix arg \\[universal-argument], skip recording the previous item."
  (interactive "P")
  (org-study--review-tags-internal nil skip))

(provide 'org-study-api)
