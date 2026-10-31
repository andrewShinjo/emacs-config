;;; org-study-model.el --- Review heading model -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst REVIEW-PRIORITY-PROPERTY "REVIEW_PRIORITY")
(defconst REVIEW-LAST-PROPERTY "REVIEW_LAST")

(cl-defstruct (heading (:conc-name org-study-heading-))
  "A heading in the review queue."
  file
  id
  text
  tags
  priority
  last-reviewed
  file-level)

(provide 'org-study-model)
