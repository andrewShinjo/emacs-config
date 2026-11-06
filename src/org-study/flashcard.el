;;; flashcard.el --- Flashcard type registry -*- lexical-binding: t; -*-

(require 'org-id)
(require 'flashcard-bi)
(require 'flashcard-cloze)
(require 'flashcard-single)
(require 'flashcard-treecloze)

(defvar org-study--flashcard-handlers
  `((SINGLE . ((:is-flashcard . andy/org-study/flashcard-single/is-flashcard)
               (:save . andy/org-study/flashcard-single/save)
               (:parse . andy/org-study/flashcard-single/parse)))
    (BI . ((:is-flashcard . andy/org-study/flashcard-bi/is-flashcard)
           (:save . andy/org-study/flashcard-bi/save)
           (:parse . andy/org-study/flashcard-bi/parse)))
    (CLOZE . ((:is-flashcard . andy/org-study/flashcard-cloze/is-flashcard)
              (:save . andy/org-study/flashcard-cloze/save)
              (:parse . andy/org-study/flashcard-cloze/parse)))
    (TREECLOZE . ((:is-flashcard . andy/org-study/flashcard-treecloze/is-flashcard)
                  (:save . andy/org-study/flashcard-treecloze/save)
                  (:parse . andy/org-study/flashcard-treecloze/parse))))
  "Alist mapping flashcard TYPE to its handler functions.
Each entry maps an operation (:is-flashcard, :save, :parse) to a
function.")

(defun org-study--get-handler (type operation)
  "Get the handler function for TYPE and OPERATION."
  (alist-get operation (alist-get type org-study--flashcard-handlers)))

(defun andy/org-study/flashcard/save (flashcard)
  "Save FLASHCARD to its Org heading."
  (let* ((id (plist-get flashcard :ID))
         (flashcard-type (plist-get flashcard :type))
         (marker (org-id-find id 'marker))
         (save-handler (org-study--get-handler flashcard-type :save)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char (marker-position marker))
          (funcall save-handler flashcard)
          (save-buffer))))))

(provide 'flashcard)
