;;; flashcard-single.el --- Single-sided flashcards -*- lexical-binding: t; -*-

(require 'org-heading-at-point)

(defconst SINGLE-TAG "single")

(defconst SINGLE-DUE-PROPERTY "SINGLE_DUE")
(defconst SINGLE-INTERVAL-PROPERTY "SINGLE_INTERVAL")
(defconst SINGLE-EASE-FACTOR-PROPERTY "SINGLE_EASE_FACTOR")
(defconst SINGLE-REPETITION-PROPERTY "SINGLE_REPETITION")

(defun andy/org-study/flashcard-single/is-flashcard (_text tags)
  "Return non-nil when TAGS marks the heading as a single card."
  (member SINGLE-TAG tags))

(defun andy/org-study/flashcard-single/save (flashcard)
  "Save FLASHCARD's SM-2 state on the current heading."
  (andy/org-study/write-sm2-state flashcard
                                  SINGLE-DUE-PROPERTY
                                  SINGLE-REPETITION-PROPERTY
                                  SINGLE-EASE-FACTOR-PROPERTY
                                  SINGLE-INTERVAL-PROPERTY))

(defun andy/org-study/flashcard-single/parse (now)
  "Return a list containing the due SINGLE flashcard at point, or nil.
NOW is the current time used to decide whether the card is due."
  (let* ((heading-text (org-get-heading 'no-todo 'no-tags))
         (body-text (andy/org-study/expand-attachment-links
                     (andy/org-heading-at-point/get-body-text)))
         (context (andy/org-study/get-question-context-at-point))
         (level (org-outline-level))
         (question (andy/org-study/expand-attachment-links
                    (concat context "\n" (make-string level ?*) " " heading-text)))
         (state (andy/org-study/read-sm2-state
                 SINGLE-DUE-PROPERTY
                 SINGLE-REPETITION-PROPERTY
                 SINGLE-EASE-FACTOR-PROPERTY
                 SINGLE-INTERVAL-PROPERTY)))
    (when (andy/org-study/card-due-p (plist-get state :due) now)
      (list (append state
                    (list :ID (org-entry-get nil "ID")
                          :question question
                          :answer body-text
                          :type 'SINGLE))))))

(provide 'flashcard-single)
