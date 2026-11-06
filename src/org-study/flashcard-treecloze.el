;;; flashcard-treecloze.el --- Tree-cloze flashcards -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'org-heading-at-point)

(defconst TREECLOZE-DUE-PROPERTY-PREFIX "TREECLOZE_DUE_")
(defconst TREECLOZE-INTERVAL-PROPERTY-PREFIX "TREECLOZE_INTERVAL_")
(defconst TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX "TREECLOZE_EASE_FACTOR_")
(defconst TREECLOZE-REPETITION-PROPERTY-PREFIX "TREECLOZE_REPETITION_")
(defconst TREECLOZE-TAG "treecloze")

(defun andy/org-study/flashcard-treecloze/is-flashcard (_text tags)
  "Return non-nil when TAGS marks the heading as a tree-cloze card."
  (member TREECLOZE-TAG tags))

(defun andy/org-study/flashcard-treecloze/save (flashcard)
  "Save FLASHCARD's SM-2 state on the current heading."
  (let ((suffix (number-to-string (plist-get flashcard :cloze-idx))))
    (andy/org-study/write-sm2-state
     flashcard
     (concat TREECLOZE-DUE-PROPERTY-PREFIX suffix)
     (concat TREECLOZE-REPETITION-PROPERTY-PREFIX suffix)
     (concat TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix)
     (concat TREECLOZE-INTERVAL-PROPERTY-PREFIX suffix))))

(defun andy/org-study/flashcard-treecloze/parse (now)
  "Return the due TREECLOZE flashcards at point, or nil.
NOW is the current time used to decide whether the cards are due."
  (let* ((level (org-current-level))
         (context (andy/org-study/get-question-context-at-point))
         (parent (org-get-heading 'no-todo 'no-tags))
         (id (org-entry-get nil "ID"))
         (children (andy/org-study/flashcard-treecloze/children-data)))
    (cl-loop for child in children
             for i from 0
             for suffix = (number-to-string i)
             for state = (andy/org-study/read-sm2-state
                          (concat TREECLOZE-DUE-PROPERTY-PREFIX suffix)
                          (concat TREECLOZE-REPETITION-PROPERTY-PREFIX suffix)
                          (concat TREECLOZE-EASE-FACTOR-PROPERTY-PREFIX suffix)
                          (concat TREECLOZE-INTERVAL-PROPERTY-PREFIX suffix))
             when (andy/org-study/card-due-p (plist-get state :due) now)
             collect
             (let ((question-lines
                    (cl-loop for other in children
                             for j from 0
                             collect
                             (let ((is-target (= i j))
                                   (body (plist-get other :body)))
                               (format "%s %s%s"
                                       (make-string (1+ level) ?*)
                                       (if is-target
                                           "[...]"
                                         (plist-get other :title))
                                       (if (or is-target (string-empty-p body))
                                           ""
                                         (concat "\n" body)))))))
               (append state
                       (list
                        :ID id
                        :question (andy/org-study/expand-attachment-links
                                   (concat context
                                           "\n"
                                           (make-string level ?*) " " parent
                                           "\n"
                                           (mapconcat #'identity question-lines "\n")))
                        :answer (andy/org-study/expand-attachment-links
                                 (concat (plist-get child :title)
                                         "\n\n"
                                         (plist-get child :body)))
                        :type 'TREECLOZE
                        :cloze-idx i))))))

(defun andy/org-study/flashcard-treecloze/children-data ()
  "Return the title and body of each direct child heading."
  (andy/org-heading-at-point/map-children
   (lambda ()
     (list :title (org-get-heading 'no-todo 'no-tags)
           :body (andy/org-study/expand-attachment-links
                  (andy/org-heading-at-point/get-body-text))))))

(provide 'flashcard-treecloze)
