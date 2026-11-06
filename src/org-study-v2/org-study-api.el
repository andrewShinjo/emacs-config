;;; org-study-api.el -*- lexical-binding: t -*-

(defun andy/org-study-api/v2/practice-flashcards()
  (interactive)
  (let* (
		 ;; Get all Org files in [org-directory].
		 (org-files (directory-files-recursively
					 ;; DIR=
					 org-directory
					 ;; REGEXP=
					 "\\.org\\'"))

		 ;; Get all Org headlines.
		 
  
		 nil)))

;;; internal

(defun org-file-to-flashcards (org-file)
  "Consumes an Org file, and produces a list of flashcards."
  ;; Load org file into a buffer
  (let ((buffer (find-file-no-select org-file)))
	
	(with-current-buffer buffer
	  (org-map-entries #'org-headline-to-flashcard "single")
	  (kill-buffer buffer))))

(defun org-headline-to-flashcard (org-headline)
  "Create a flashcard from the Org headline. This function assumes that the Org headline meets the definition of a flashcard."
  ;; Flashcard:
  ;; org-file
  ;; org-id
  ;; breadcrumb
  ;; question
  ;; answer
	  
  (let ((org-file (org-entry-get nil "FILE"))
		(org-id (org-entry-get nil "ID"))
		(breadcrumb (org-headline-get-breadcrumb))
		(question (org-get-heading t t t t))
		(answer (org-headline-get-body)))
	nil))

(defun org-headline-get-breadcrumb()
  "Gets the Org headline's breadcrumb. This function assumes that the cursor is on the headline."
  (save-excursion
	(let (breadcrumb-list)
	  (while (org-up-heading-safe)
		(let* ((title (org-get-heading t t t t))
			   (body (org-headline-get-body))
			   (breadcrumb-line (string-join '(title body) "\n"))
			   (push breadcrumb-line breadcrumb-list)))
		breadcrumb-list)))

(defun org-headline-get-body()
  "Gets the Org headline's body. This function assumes that the cursor is on the headline."
  (save-excursion
	(let* ((headline (org-element-at-point))
		   (body-begin (progn (org-end-of-metadata) (point)))
		   (body-end (org-element-property :contents-end headline)))
	  (when (and body-begin body-end (< body-begin body-end))
		(buffer-substring-no-properties body-begin body-end)))))
