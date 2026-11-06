;;; org-study-link.el --- Attachment link utilities for org-study -*- lexical-binding: t; -*-

(require 'org-attach)

(defun andy/org-study/expand-attachment-links (text)
  "Expand [[attachment:FILE]] links in TEXT to [[file:ABSOLUTE-PATH]].
Must be called from the original org buffer with point at the heading."
  (save-match-data
    (if-let* ((_ (string-match-p "\\[\\[attachment:" text))
              (attach-dir (org-attach-dir)))
        (replace-regexp-in-string
         "\\[\\[attachment:\\(.*?\\)\\]\\]"
         (lambda (match)
           (format "[[file:%s]]"
                   (expand-file-name (match-string 1 match) attach-dir)))
         text)
      text)))

(provide 'org-study-link)
