;;; org-study.el --- Simple SRS and Curation Queue for Org Mode -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'flashcard)
(require 'org-heading-at-point)
(require 'org-id)

;; --- Properties used for cached heading hashes ---
(defconst HASH-PROPERTY "HASH")
(defconst TREECLOZE-HASH-PROPERTY "TREE_CLOZE_HASH")

;; --- Flashcard session state ---

(defconst flashcard-buffer-name "FlashcardMode")

(defvar due-flashcards nil
  "Current list of due cards in a study session.")

(defvar-local flashcard-answer-shown nil
  "Non-nil when the current card's answer is already displayed.")

;;; --- UI & Mode ---

(define-derived-mode flashcard-mode org-mode "FlashcardMode"
  "Major mode for reviewing flashcards."
  (read-only-mode 1)
  (setq-local cursor-type nil))

(keymap-set flashcard-mode-map "SPC" 'andy/org-study/show-answer)
(keymap-set flashcard-mode-map "e" 'andy/org-study/rate-flashcard-easy)
(keymap-set flashcard-mode-map "h" 'andy/org-study/rate-flashcard-hard)
(keymap-set flashcard-mode-map "f" 'andy/org-study/rate-flashcard-forgot)
(keymap-set flashcard-mode-map "E" 'andy/org-study/mark-edit-later)
(keymap-set flashcard-mode-map "M" 'andy/org-study/mark-edit-later-with-message)

(defun andy/org-study--mark-edit-later (&optional with-message)
  "Tag the current card :edit-later: and skip it.
When WITH-MESSAGE is non-nil, prompt for and append a note to the
heading body."
  (let* ((card (car due-flashcards))
         (marker (and card (org-id-find (plist-get card :ID) 'marker))))
    (if (not marker)
        (message "Error: ID not found.")
      (let ((note (and with-message (read-string "Edit message: "))))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char (marker-position marker))
            (org-set-tags (append (org-get-tags nil t) '("edit-later")))
            (when note
              (org-end-of-meta-data t)
              (outline-next-heading)
              (skip-chars-backward " \t\n")
              (insert "\n" note)))
          (save-buffer)
          (message "Card marked :edit-later:%s."
                   (if note " with message" ""))))
      (pop due-flashcards)
      (andy/org-study/display-flashcard-question))))

(defun andy/org-study/mark-edit-later ()
  "Flag the current card with :edit-later: and skip it."
  (interactive)
  (andy/org-study--mark-edit-later))

(defun andy/org-study/mark-edit-later-with-message ()
  "Flag the current card with :edit-later: and append a note to it."
  (interactive)
  (andy/org-study--mark-edit-later t))

(defun andy/org-study/rate-flashcard (quality)
  "Rate the current flashcard with QUALITY and advance to the next one."
  (when-let* ((flashcard (car due-flashcards)))
    (andy/org-study/flashcard/save
     (andy/org-study/update-card-sm2 flashcard quality))
    (pop due-flashcards)
    (andy/org-study/display-flashcard-question)))

(defun andy/org-study/rate-flashcard-easy ()
  "Rate the current flashcard as easy."
  (interactive)
  (andy/org-study/rate-flashcard 5))

(defun andy/org-study/rate-flashcard-hard ()
  "Rate the current flashcard as hard."
  (interactive)
  (andy/org-study/rate-flashcard 3))

(defun andy/org-study/rate-flashcard-forgot ()
  "Rate the current flashcard as forgotten."
  (interactive)
  (andy/org-study/rate-flashcard 1))

;;; --- Core SRS Logic ---

(defun andy/org-study/shuffle-list (list)
  "Return a shuffled copy of LIST."
  (let ((vec (vconcat list))
        (len (length list)))
    (dotimes (i len)
      (let ((j (+ i (random (- len i))))
            (tmp (aref vec i)))
        (aset vec i (aref vec j))
        (aset vec j tmp)))
    (append vec nil)))

(defun andy/org-study/update-card-sm2 (flashcard quality)
  "Return FLASHCARD with its SM-2 state updated for QUALITY."
  (let* ((passed (>= quality 3))
         (old-repetition (plist-get flashcard :repetition))
         (repetition (if passed (1+ old-repetition) 0))
         (interval (cond ((not passed) 1)
                         ((= old-repetition 0) 1)
                         ((= old-repetition 1) 6)
                         (t (round (* (plist-get flashcard :interval)
                                      (plist-get flashcard :ease-factor))))))
         (delta (- 5 quality))
         (penalty (* delta (+ 0.08 (* delta 0.02))))
         (ease-factor (max 1.3 (+ (plist-get flashcard :ease-factor)
                                  0.1
                                  (- penalty))))
         (card (copy-sequence flashcard)))
    (setf (plist-get card :repetition) repetition
          (plist-get card :ease-factor) ease-factor
          (plist-get card :interval) interval
          (plist-get card :due) (format-time-string
                                 "%Y-%m-%d %H:%M"
                                 (time-add (current-time)
                                           (days-to-time interval))))
    card))

(defun andy/org-study/compute-next-due (flashcard quality)
  "Return the due date FLASHCARD would have after QUALITY."
  (plist-get (andy/org-study/update-card-sm2 flashcard quality) :due))

(defun andy/org-study--delete-properties-of-type (type)
  "Delete all card properties for TYPE at point.
Card properties are named TYPE_* by convention."
  (let ((regexp (concat "^" (symbol-name type) "_")))
    (dolist (entry (org-entry-properties nil 'standard))
      (when (string-match-p regexp (car entry))
        (org-entry-delete nil (car entry))))))

(defun andy/org-study/delete-properties (&optional types)
  "Reset SM-2 state when the card content at point has changed.
TYPES defaults to the flashcard types on the heading at point."
  (let ((types (or types
                   (andy/org-study/get-flashcard-types-on-heading-at-point))))
    (when types
      (let* ((new-hash (secure-hash 'sha256
                                    (org-get-heading 'no-todo 'no-tags)))
             (old-hash (org-entry-get nil HASH-PROPERTY))
             (new-tree-hash (secure-hash
                             'sha256
                             (andy/org-study/serialize-child-headings)))
             (old-tree-hash (org-entry-get nil TREECLOZE-HASH-PROPERTY)))
        (unless (equal new-hash old-hash)
          (dolist (type types)
            (unless (eq type 'TREECLOZE)
              (andy/org-study--delete-properties-of-type type)))
          (org-entry-put nil HASH-PROPERTY new-hash))
        (unless (equal new-tree-hash old-tree-hash)
          (andy/org-study--delete-properties-of-type 'TREECLOZE)
          (org-entry-put nil TREECLOZE-HASH-PROPERTY new-tree-hash))))))

(defun andy/org-study/create-properties (&optional types)
  "Only create an ID if the heading is actually a flashcard.
TYPES defaults to the flashcard types on the heading at point."
  (when (or types
            (andy/org-study/get-flashcard-types-on-heading-at-point))
    (org-id-get-create)))

(defun andy/org-study/serialize-child-headings ()
  "Serialize the headings of the direct children at point."
  (mapconcat #'identity
             (sort (andy/org-heading-at-point/map-children
                    (lambda () (org-get-heading 'no-todo 'no-tags)))
                   #'string<)
             " | "))

;;; --- Display ---

(defun andy/org-study/preview-latex ()
  "Preview LaTeX fragments in the current buffer when possible."
  (when (display-graphic-p)
    (cond ((fboundp 'org-latex-preview)
           (org-latex-preview '(16)))
          ((fboundp 'org-preview-latex-fragment)
           (org-preview-latex-fragment '(16))))))

(defun andy/org-study/display-inline-images ()
  "Display inline images in the current buffer.
Use the Org 9.8 preview API when it exists, and fall back to the
deprecated `org-display-inline-images' on older Org versions."
  (if (fboundp 'org-link-preview-region)
      (org-link-preview-region t t)
    (with-suppressed-warnings ((obsolete org-display-inline-images))
      (org-display-inline-images t))))

(defun andy/org-study/show-answer ()
  "Reveal the answer for the current flashcard, bypassing read-only mode."
  (interactive)
  (when-let* ((card (car due-flashcards)))
    (with-current-buffer (get-buffer-create flashcard-buffer-name)
      (unless flashcard-answer-shown
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert "\n\n" (make-string 30 ?-) "\n" "**Answer:**\n"
                    (or (plist-get card :answer) "No answer found."))
            (insert "\n\n")
            (insert (format "[e] Easy (%s)\n"
                            (andy/org-study/compute-next-due card 5)))
            (insert (format "[h] Hard (%s)\n"
                            (andy/org-study/compute-next-due card 3)))
            (insert (format "[f] Forgot (%s)\n"
                            (andy/org-study/compute-next-due card 1)))
            (insert "[E] Mark Edit-Later\n")
            (insert "[M] Mark Edit-Later with Message"))
          (andy/org-study/display-inline-images)
          (andy/org-study/preview-latex))
        (setq flashcard-answer-shown t)))))

(defun andy/org-study/display-flashcard-question ()
  "Display the next question, clearing the flashcard buffer first."
  (let ((flashcard (car due-flashcards))
        (buf (get-buffer-create flashcard-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (flashcard-mode)
        (setq flashcard-answer-shown nil)
        (if flashcard
            (progn
              (insert (format "Flashcards remaining: %d\n"
                              (length due-flashcards)))
              (insert (plist-get flashcard :question))
              (insert "\n\n" "Show answer: [SPC]")
              (andy/org-study/display-inline-images)
              (andy/org-study/preview-latex))
          (insert "Done with flashcards."))))
    (switch-to-buffer buf)))

;;; --- Session ---

(provide 'org-study)
