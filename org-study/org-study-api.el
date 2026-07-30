;;; org-study-api.el -*- lexical-binding: t -*-

(require 'org-study-queue)
(require 'org-study)

(defalias 'org-study/review-flashcards 'andy/org-study/review-flashcards)
(defalias 'org-study/review-tag 'andy/org-study/review-tag)

(defun org-study--review-tags-internal (tags &optional skip)
  "Review items matching TAGS (list of strings).
Prompt for interest rating on the previous item, then pick a weighted random heading.
With SKIP non-nil, skip recording the previous item."
  (when (and org-study--current-review-heading (not skip))
    (let ((heading org-study--current-review-heading)
          (text (heading-text org-study--current-review-heading)))
      (message "Previous: %s" text)
      (pcase (read-char-choice
              (format "%s — (%s) (r)eviewed, (i)nterested, (n)ot interested: "
                      (file-name-nondirectory (heading-file heading))
                      (truncate-string-to-width text 40 nil nil t))
              '(?r ?i ?n))
        (?i (org-study-record-review heading 'interested)
            (message "Recorded as interesting (priority +1)"))
        (?n (org-study-record-review heading 'not-interested)
            (message "Recorded as not interesting (priority -1)"))
        (?r (org-study-record-review heading 'neutral)
            (message "Recorded as reviewed")))))
  (let ((next (org-study--weighted-random-pick
               (org-study-collect-headings :tags tags))))
    (if next
        (progn
          (org-study-goto-heading next)
          (setq org-study--current-review-heading next)
          (message "Reviewing: %s%s  [p:%d]"
                   (if (heading-file-level next) "[FILE] " "")
                   (heading-text next)
                   (or (heading-priority next) org-study-priority-default)))
      (setq org-study--current-review-heading nil)
      (message "org-study: no headings%s found to review"
               (if tags
                   (format " with tags \"%s\"" (string-join tags ", "))
                 "")))))

(defun andy/org-study/review-tag (tag &optional skip)
  "Review items with a specific TAG using weighted random selection.
If a previous item is being tracked, prompt for interest rating first.
With prefix arg \\[universal-argument], skip recording the previous item.
When TAG is nil, use `org-study-review-tags' (the default set)."
  (interactive "sReview tag: \nP")
  (org-study--review-tags-internal
   (if (and tag (not (string= tag ""))) (list tag) nil)
   skip))

(defun andy/org-study/review-notes (&optional skip)
  (interactive "P")
  (andy/org-study/review-tag nil skip))

(defun andy/org-study/pick-project (&optional skip)
  (interactive "P")
  (andy/org-study/review-tag "project" skip))

(defun andy/org-study/review-extract-edit-later (&optional skip)
  (interactive "P")
  (org-study--review-tags-internal '("extract" "edit-later") skip))

(defun andy/org-study/review-exercise (&optional skip)
  (interactive "P")
  (org-study--review-tags-internal '("exercise") skip))

(defun andy/org-study/review-study (&optional skip)
  (interactive "P")
  (org-study--review-tags-internal '("study") skip))

(defun andy/org-study/review-cycle ()
  "Cycle through review types: flashcards, extract/edit-later, exercise, study.
Each call advances to the next review type.  Wraps around after study."
  (interactive)
  (setq org-study--review-cycle-position
        (if (null org-study--review-cycle-position)
            0
          (mod (1+ org-study--review-cycle-position) 4)))
  (pcase org-study--review-cycle-position
    (0 (andy/org-study/review-flashcards))
    (1 (andy/org-study/review-extract-edit-later))
    (2 (andy/org-study/review-exercise))
    (3 (andy/org-study/review-study))))

(defalias 'org-study/review-extract-edit-later 'andy/org-study/review-extract-edit-later)
(defalias 'org-study/review-exercise 'andy/org-study/review-exercise)
(defalias 'org-study/review-study 'andy/org-study/review-study)
(defalias 'org-study/review-cycle 'andy/org-study/review-cycle)

(provide 'org-study-api)
