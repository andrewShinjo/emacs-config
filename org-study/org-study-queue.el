;;; org-study-queue.el --- Weighted random review picker -*- lexical-binding: t -*-

(require 'org-study-collection)
(require 'org-study-update)
(require 'cl-lib)

(defvar org-study--current-review-heading nil
  "Heading currently being reviewed, or nil.")

(defvar org-study--review-cycle-position nil
  "Current position in the review cycle (0-3).
0=flashcards, 1=extract/edit-later, 2=exercise, 3=study.
Set to nil when no cycle is active.")

(defcustom org-study-priority-default 5
  "Default priority for headings without REVIEW_PRIORITY."
  :type 'integer
  :group 'org-study)

(defcustom org-study-recency-base-days 30
  "Days to assume for headings never reviewed before.
Higher values give new items a stronger initial boost."
  :type 'integer
  :group 'org-study)

(defun org-study--compute-weight (heading)
  "Compute selection weight for HEADING.
Weight = priority * (1 + days-since-review / 7) * random-jitter"
  (let* ((priority (or (heading-priority heading) org-study-priority-default))
         (last-reviewed (heading-last-reviewed heading))
         (days-since (if last-reviewed
                         (/ (float (time-to-seconds
                                    (time-subtract (current-time)
                                                   (date-to-time last-reviewed))))
                            86400)
                       org-study-recency-base-days))
         (recency-mult (1+ (/ days-since 7.0)))
         (jitter (+ 0.5 (/ (float (random 1000)) 1000.0))))
    (* priority recency-mult jitter)))

(defun org-study--weighted-random-pick (items)
  "Pick one ITEM using weighted random selection.
Items with higher weight are more likely to be chosen."
  (when items
    (let* ((weights (mapcar #'org-study--compute-weight items))
           (total (cl-reduce #'+ weights))
           (r (* total (/ (float (random 1000)) 1000.0)))
           (cumulative 0.0))
      (cl-loop for item in items
               for weight in weights
               do (setq cumulative (+ cumulative weight))
               when (>= cumulative r)
               return item))))

(defun org-study-pick-next ()
  "Pick the next heading to review using weighted random selection."
  (let ((headings (org-study-collect-headings)))
    (org-study--weighted-random-pick headings)))

(defun org-study-record-review (heading interest)
  "Record review of HEADING with given INTEREST level.
INTEREST is one of: 'interested, 'neutral, 'not-interested.
Updates REVIEW_LAST and adjusts REVIEW_PRIORITY."
  (let* ((current-priority (or (heading-priority heading) org-study-priority-default))
         (new-priority (pcase interest
                         ('interested (min 10 (1+ current-priority)))
                         ('not-interested (max 1 (1- current-priority)))
                         ('neutral current-priority)))
         (now (format-time-string "%Y-%m-%d %H:%M")))
    (org-study-update-heading heading
      :priority new-priority
      :last-reviewed now)))

(provide 'org-study-queue)
