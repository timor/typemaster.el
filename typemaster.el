;;; typemaster.el --- Advanced Typing trainer -*- lexical-binding: t -*-

;; unlicensed

;; Author: timor <timor.dd@googlemail.com>
;; Version: 0.1
                                        ; ;; Package-Requires: ((flange "1.0"))
;; Keywords: games
;; URL: http://github.com/timor/typemaster2000

;;; Commentary:

;; Analyze texts and generate a markov model that can be used to generate typing
;; training data.

;; stats counts all the transitions from one state (pair of chars) to the next.
;;;###autoload
(defun analyze-text(k &optional index)
  "Analyze a given text, add the content to the content-buffer,
and extend the index.  Creates the buffer if necessary"
  (let ((content-buffer (get-buffer-create "*analyzed-text*"))
        (index-buffer (get-buffer-create "*text-index*"))
        (index (or index (make-hash-table :test 'equal))))
    (save-excursion
      (goto-char (point-min))
      (with-current-buffer content-buffer
        (erase-buffer))
      (with-current-buffer index-buffer
        (erase-buffer))
      (loop for next = (re-search-forward "[^ \t\n\r]+" nil t)
            for str = (match-string-no-properties 0)
            while next
            do (with-current-buffer content-buffer
                 (insert str " "))))
    (with-current-buffer content-buffer
      (goto-char (point-min))
      (loop for p from 1 to (- (point-max) (1+ k))
            for str = (buffer-substring p (+ p k 1))
            do (with-current-buffer index-buffer
                 (insert str "\n"))))
    (with-current-buffer index-buffer
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (loop for entry = (buffer-substring (point) (+ (point) k 1))
            for s-1 = (substring entry 0 k)
            for s = (elt entry k)
            while (progn (forward-line)
                         (not (looking-at "^$")))
            do (incf (alist-get s (gethash s-1 index) 0))))
    index))

(defun analyze-file (file k &optional index)
  (with-temp-buffer
    (insert-file-contents file)
    (analyze-text k index)))

(defun find-candidates (str index)
  (let* ((matches (gethash str index)))
    (loop for (s . num) in matches collect
          (cons num (concat str (list s))))))

(defun choose-randomly (candidates)
  (nth (random (length candidates)) candidates))

(defun choose-weighted (candidates)
  (loop with sum = (apply '+ (mapcar 'car candidates))
        for r = (random sum) then (- r p)
        for (p . next) in candidates
        until (<= r 0)
        finally (return next)))

(defun make-generator(index)
  "Generate new things based on the index in index-buffer"
  (let ((state (choose-weighted (find-candidates (choose-randomly (hash-table-keys index)) index))))
    (lambda()
      (let* ((s-1 (substring state 1))
             (next-state (choose-weighted (or (find-candidates s-1 index) (error "no candidate found for '%s'" s-1))))
             (output (substring next-state -1)))
        (setf state next-state)
        output))))

(defun choose-word(generator)
  (loop for c = (funcall generator)
        until (string= " " c)
        concat c))

(defun practice-typing (uses-spaces generator)
  (let ((word nil)
        (correct nil))
    (while (= 1 1) ;; Keep going until they press C-g.
      (let* ((old-time (current-time))
             (old-seconds (time-seconds old-time))
             (old-micro   (/ (caddr old-time) 1000000.0)))
        (setq word (choose-word generator))
        (if uses-spaces
            (setq correct (string= word (read-string (concat word "\n"))))
          (setq correct (string= word (read-no-blanks-input (concat word "\n")))))
        (unless correct (wrong-answer word))
        )
      )
    )
  )

(defun wrong-answer (wrong-entry)
  ;; (push wrong-entry wrong-word-list)
  (message "WRONG!!")
  (sleep-for 1)
  )
