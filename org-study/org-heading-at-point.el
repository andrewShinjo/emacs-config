;;; org-heading-at-point.el --- Heading text and card property helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-study-link)

;; Defined in flashcard.el, which requires this file.
(defvar org-study--flashcard-handlers)

;;; Heading text

(defun andy/org-heading-at-point/get-body-text ()
  "Return the body text of the heading at point.
Planning lines, property drawers, drawers and trailing blank lines are
excluded."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let ((beg (point))
          (end (save-excursion
                 (outline-next-heading)
                 (skip-chars-backward " \t\n")
                 (point))))
      (string-trim (buffer-substring-no-properties beg (max beg end))))))

(defun andy/org-heading-at-point/map-children (function)
  "Call FUNCTION at each direct child of the heading at point.
Return a list of the results in child order."
  (save-excursion
    (when (org-goto-first-child)
      (let (results)
        (push (save-excursion (funcall function)) results)
        (while (org-get-next-sibling)
          (push (save-excursion (funcall function)) results))
        (nreverse results)))))

(defun andy/org-study/get-question-context-at-point ()
  "Return the #+title and ancestor headings for the heading at point."
  (save-excursion
    (let ((context (when-let* ((title (andy/org-study/get-title-at-point)))
                     (list (format "#+title: %s" title))))
          parents)
      (org-back-to-heading t)
      (while (org-up-heading-safe)
        (let ((body (andy/org-study/expand-attachment-links
                     (andy/org-heading-at-point/get-body-text))))
          (push (concat (make-string (org-outline-level) ?*) " "
                        (org-get-heading 'no-todo 'no-tags)
                        (if (string-empty-p body) "" (concat "\n" body)))
                parents)))
      (string-join (append context parents) "\n"))))

(defun andy/org-study/get-title-at-point ()
  "Return the #+title of the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^[ \t]*#\\+title:[ \t]*\\(.*\\)$" nil t)
        (string-trim (match-string 1))))))

;;; Card types

(defun andy/org-study/get-flashcard-types-on-heading-at-point ()
  "Return the flashcard types that apply to the heading at point."
  (let ((text (org-get-heading 'no-todo 'no-tags))
        (tags (org-get-tags nil t)))
    (cl-loop for (type . props) in org-study--flashcard-handlers
             when (funcall (alist-get :is-flashcard props) text tags)
             collect type)))

;;; Card SM-2 state

(defun andy/org-study/card-due-p (due now)
  "Return non-nil when DUE is nil, empty, or not after NOW."
  (or (not due)
      (string= due "")
      (time-less-p (org-time-string-to-time due) now)))

(defun andy/org-study/read-sm2-state (due-property repetition-property
                                     ease-factor-property interval-property)
  "Read SM-2 state for the current heading.
Each argument names a property; missing properties fall back to the
SM-2 defaults."
  (list :due (org-entry-get nil due-property)
        :repetition (string-to-number
                     (or (org-entry-get nil repetition-property) "0"))
        :ease-factor (string-to-number
                      (or (org-entry-get nil ease-factor-property) "2.5"))
        :interval (string-to-number
                   (or (org-entry-get nil interval-property) "0"))))

(defun andy/org-study/write-sm2-state (card due-property repetition-property
                                       ease-factor-property interval-property)
  "Write CARD's SM-2 state to the current heading's properties."
  (when-let* ((due (plist-get card :due)))
    (org-entry-put (point) due-property due))
  (org-entry-put (point) repetition-property
                 (number-to-string (plist-get card :repetition)))
  (org-entry-put (point) ease-factor-property
                 (format "%.2f" (plist-get card :ease-factor)))
  (org-entry-put (point) interval-property
                 (number-to-string (plist-get card :interval))))

(provide 'org-heading-at-point)
