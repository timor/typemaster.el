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
      (loop for p from 1 to (- (point-max) 5)
            for str = (buffer-substring p (+ p 5))
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

(defun find-prefix (str)
  "Return the line number that contains the first occurrence of str."
  (loop ;; with next-min
        ;; with next-max
        for min = 1 then next-min
        for max = (count-lines (point-min) (point-max)) then next-max
        for line = (/ (+ max min) 2)
        for cmp = (progn (goto-line line) (compare-strings (buffer-substring (point) (+ (point) (length str)))
                                                           nil nil
                                                           str nil nil))
        for gt = (or (eq cmp t) (> cmp 0))
        while (not (< max min))
        for next-min = (with-current-buffer index-buffer (if gt min (1+ line)))
        for next-max = (if gt (1- line) max)
        finally (return min)))

(defun find-candidates (str l index-buffer)
  (with-current-buffer index-buffer
    (let* ((first (find-prefix str))
           (slen (length str))
           (clen l))
      (loop for line = first then (1+ line)
            for candidate = (progn
                              (goto-line line)
                              (buffer-substring (point) (+ (point) clen)))
            while (eq t (compare-strings candidate 0 slen
                                         str 0 slen))
            collect candidate))))

(defun find-candidates (str index)
  (let* ((matches (gethash str index)))
    (loop for (s . num) in matches collect
          (cons num (concat str (list s))))))

(defun choose-randomly (candidates)
  (nth (random (length candidates)) candidates))

(defun make-generator(k index)
  "Generate new things based on the index in index-buffer"
  (let ((state (choose-randomly ())))
    (lambda()
      (let* ((s-1 (substring state 1))
             (next-state (choose-randomly (find-candidates s-1 (1+ k) index-buffer)))
             (output (elt next-state k)))
        (setf state next-state)
        output))))
