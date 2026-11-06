;;; flashcard-bi.el --- Bidirectional flashcards -*- lexical-binding: t; -*-

(require 'org-heading-at-point)

(defconst BI-TAG "bi")
(defconst BI-DUE-FORWARD-PROPERTY "BI_FORWARD_DUE")
(defconst BI-INTERVAL-FORWARD-PROPERTY "BI_INTERVAL_FORWARD")
(defconst BI-EASE-FACTOR-FORWARD-PROPERTY "BI_EASE_FACTOR_FORWARD")
(defconst BI-REPETITION-FORWARD-PROPERTY "BI_REPETITION_FORWARD")
(defconst BI-DUE-REVERSE-PROPERTY "BI_REVERSE_DUE")
(defconst BI-INTERVAL-REVERSE-PROPERTY "BI_INTERVAL_REVERSE")
(defconst BI-EASE-FACTOR-REVERSE-PROPERTY "BI_EASE_FACTOR_REVERSE")
(defconst BI-REPETITION-REVERSE-PROPERTY "BI_REPETITION_REVERSE")

(defun andy/org-study/flashcard-bi/is-flashcard (_text tags)
  "Return non-nil when TAGS marks the heading as a bidirectional card."
  (member BI-TAG tags))

(defun andy/org-study/flashcard-bi/save (flashcard)
  "Save FLASHCARD's SM-2 state on the current heading."
  (let ((forward (eq (plist-get flashcard :bi-type) 'FORWARD)))
    (andy/org-study/write-sm2-state
     flashcard
     (if forward BI-DUE-FORWARD-PROPERTY BI-DUE-REVERSE-PROPERTY)
     (if forward BI-REPETITION-FORWARD-PROPERTY BI-REPETITION-REVERSE-PROPERTY)
     (if forward BI-EASE-FACTOR-FORWARD-PROPERTY BI-EASE-FACTOR-REVERSE-PROPERTY)
     (if forward BI-INTERVAL-FORWARD-PROPERTY BI-INTERVAL-REVERSE-PROPERTY))))

(defun andy/org-study/flashcard-bi/parse (now)
  "Return the due BI flashcards at point, or nil.
NOW is the current time used to decide whether the cards are due."
  (let* ((heading-text (org-get-heading 'no-todo 'no-tags))
         (body-text (andy/org-study/expand-attachment-links
                     (andy/org-heading-at-point/get-body-text)))
         (context (andy/org-study/get-question-context-at-point))
         (level (org-outline-level))
         (forward-question (andy/org-study/expand-attachment-links
                            (concat context "\n" (make-string level ?*) " "
                                    heading-text)))
         (reverse-question (andy/org-study/expand-attachment-links
                            (concat context "\n" body-text)))
         (id (org-entry-get nil "ID"))
         (forward-state (andy/org-study/read-sm2-state
                         BI-DUE-FORWARD-PROPERTY
                         BI-REPETITION-FORWARD-PROPERTY
                         BI-EASE-FACTOR-FORWARD-PROPERTY
                         BI-INTERVAL-FORWARD-PROPERTY))
         (reverse-state (andy/org-study/read-sm2-state
                         BI-DUE-REVERSE-PROPERTY
                         BI-REPETITION-REVERSE-PROPERTY
                         BI-EASE-FACTOR-REVERSE-PROPERTY
                         BI-INTERVAL-REVERSE-PROPERTY)))
    (delq nil
          (list
           (when (andy/org-study/card-due-p (plist-get forward-state :due) now)
             (append forward-state
                     (list :ID id
                           :question forward-question
                           :answer body-text
                           :type 'BI
                           :bi-type 'FORWARD)))
           (when (andy/org-study/card-due-p (plist-get reverse-state :due) now)
             (append reverse-state
                     (list :ID id
                           :question reverse-question
                           :answer (andy/org-study/expand-attachment-links
                                    heading-text)
                           :type 'BI
                           :bi-type 'REVERSE)))))))

(provide 'flashcard-bi)
