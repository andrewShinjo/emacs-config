;;; org-study-model.el -*- lexical-binding: t -*-

(require 'cl-lib)

(defconst REVIEW-DUE-PROPERTY "REVIEW_DUE")
(defconst REVIEW-INCREMENT-PROPERTY "REVIEW_INCREMENT")
(defconst REVIEW-PRIORITY-PROPERTY "REVIEW_PRIORITY")
(defconst REVIEW-LAST-PROPERTY "REVIEW_LAST")

(cl-defstruct heading
  file
  id
  text
  tags
  priority
  last-reviewed
  review-due
  review-increment
  file-level)

(provide 'org-study-model)
