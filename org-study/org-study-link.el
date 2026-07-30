;;; org-study-link.el --- Attachment link utilities for org-study

(require 'org-attach)

(defun andy/org-study/expand-attachment-links (text)
  "Expand [[attachment:FILE]] links in TEXT to [[file:ABSOLUTE-PATH]].
Must be called from the original org buffer with point at the heading."
  (save-match-data
    (if (not (string-match-p "\\[\\[attachment:" text))
        text
      (let ((attach-dir (org-attach-dir)))
        (if (null attach-dir)
            text
          (replace-regexp-in-string
           "\\[\\[attachment:\\(.*?\\)\\]\\]"
           (lambda (match)
             (format "[[file:%s]]"
                     (expand-file-name (match-string 1 match) attach-dir)))
           text))))))

(provide 'org-study-link)
