;;; flashcard-cloze.el --- Cloze-deletion flashcards -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-heading-at-point)

(defconst CLOZE-REGEX "\\*\\([^*]+\\)\\*")

(defconst CLOZE-DUE-PROPERTY-PREFIX "CLOZE_DUE_")
(defconst CLOZE-INTERVAL-PROPERTY-PREFIX "CLOZE_INTERVAL_")
(defconst CLOZE-EASE-FACTOR-PROPERTY-PREFIX "CLOZE_EASE_FACTOR_")
(defconst CLOZE-REPETITION-PROPERTY-PREFIX "CLOZE_REPETITION_")

(defun andy/org-study/flashcard-cloze/is-flashcard (text _tags)
  "Return non-nil when TEXT contains a cloze deletion."
  (string-match-p CLOZE-REGEX text))

(defun andy/org-study/flashcard-cloze/save (flashcard)
  "Save FLASHCARD's SM-2 state on the current heading."
  (let ((suffix (number-to-string (plist-get flashcard :cloze-idx))))
    (andy/org-study/write-sm2-state
     flashcard
     (concat CLOZE-DUE-PROPERTY-PREFIX suffix)
     (concat CLOZE-REPETITION-PROPERTY-PREFIX suffix)
     (concat CLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix)
     (concat CLOZE-INTERVAL-PROPERTY-PREFIX suffix))))

(defun andy/org-study/flashcard-cloze/parse (now)
  "Return the due CLOZE flashcards at point, or nil.
NOW is the current time used to decide whether the cards are due."
  (let* ((text (org-get-heading 'no-todo 'no-tags))
         (context (andy/org-study/get-question-context-at-point))
         (level (org-outline-level))
         (id (org-entry-get nil "ID"))
         (start 0)
         answers)
    (while (string-match CLOZE-REGEX text start)
      (push (match-string 1 text) answers)
      (setq start (match-end 0)))
    (cl-loop for answer in (nreverse answers)
             for i from 0
             for suffix = (number-to-string i)
             for state = (andy/org-study/read-sm2-state
                          (concat CLOZE-DUE-PROPERTY-PREFIX suffix)
                          (concat CLOZE-REPETITION-PROPERTY-PREFIX suffix)
                          (concat CLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix)
                          (concat CLOZE-INTERVAL-PROPERTY-PREFIX suffix))
             when (andy/org-study/card-due-p (plist-get state :due) now)
             collect (append state
                             (list :ID id
                                   :question (andy/org-study/expand-attachment-links
                                              (concat context
                                                      "\n"
                                                      (make-string level ?*) " "
                                                      (andy/org-study/make-cloze-question
                                                       text i)))
                                   :answer (andy/org-study/expand-attachment-links
                                            answer)
                                   :type 'CLOZE
                                   :cloze-idx i)))))

(defun andy/org-study/make-cloze-question (text target-idx)
  "Replace every cloze deletion in TEXT except TARGET-IDX with its answer.
The target deletion is replaced with \"[...]\"."
  (let ((idx 0))
    (replace-regexp-in-string
     CLOZE-REGEX
     (lambda (match)
       (prog1 (if (= idx target-idx) "[...]" (match-string 1 match))
         (setq idx (1+ idx))))
     text)))

(provide 'flashcard-cloze)
