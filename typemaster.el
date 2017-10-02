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
(defun analyze-text(k &optional filter index)
  "Analyze a given text, add the content to the content-buffer,
and extend the index. An optional character filter in the form of a set of chars can be
supplied that skips over these characters"
  (let ((content-buffer (get-buffer-create "*analyzed-text*"))
        (index-buffer (get-buffer-create "*text-index*"))
        (index (or index (make-hash-table :test 'equal)))
        (re (or filter "[^ \t\n\r]+")))
    (save-excursion
      (goto-char (point-min))
      (with-current-buffer content-buffer
        (erase-buffer))
      (loop for next = (re-search-forward re nil t)
            for str = (if next (match-string-no-properties 0))
            while next
            do (with-current-buffer content-buffer
                 (insert str " "))))
    (with-current-buffer content-buffer
      (goto-char (point-min))
      (loop for p from 1 to (- (point-max) (1+ k))
            for str = (buffer-substring p (+ p k 1))
            for s-1 = (substring str 0 k)
            for s = (elt str k)
            do (incf (alist-get s (gethash s-1 index) 0))))
    index))

;; supply some simple standard filters
(defvar filter-common-text "[-_a-zA-Z0-9.,:()!?;]+")

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

(defun typemaster-save-index-to-file (index filename)
  (with-temp-buffer
    (prin1 (loop for k being the hash-keys of index using (hash-values v)
                 collect (cons k v))
           (current-buffer))
    (write-file filename t)))

(defun typemaster-load-index-from-file (filename)
  (let ((index (make-hash-table :test 'equal))
        (alist (with-temp-buffer
                 (insert-file-contents filename)
                 (goto-char 0)
                 (read (current-buffer)))))
    (loop for (k . v) in alist do
          (setf (gethash k index) v)
          finally (return index))))

(defun typemaster-make-buffer (index)
  "Create a type-master buffer"
  (with-current-buffer (get-buffer-create "*typemaster2000*")
    (erase-buffer)
    (insert "Next: ")
    (setq-local next-marker (point-marker))
    ;; (insert "\nInp :")
    ;; (setq-local input-marker (point-marker))
    ;; (setq-local fill-timer (run-at-time time time 'typemaster-fill generator (current-buffer)))
    (setq-local num-chars 0)
    ;; (setq-local speed 0.5)
    (setq-local typemaster-index index)
    (setq-local typemaster-generator (make-generator index))
    (setq-local typemaster-prompt-string (loop for i from 0 below 30 concat (funcall typemaster-generator)))
    (typemaster-fill)
    (typemaster-type)
    ))

(defun typemaster-fill ()
  (typemaster-update-prompt)
  (goto-char next-marker)
  (delete-region (point) (line-end-position))
  (insert typemaster-prompt-string))

(defun typemaster-update-prompt ()
  (setq typemaster-prompt-string (concat (subseq typemaster-prompt-string 1)
                                         (let ((next (funcall typemaster-generator)))
                                           (if (string= next " ")
                                               (propertize next 'face 'highlight)
                                             next)))))

(defun update-speed()
  (let* ((speed-mult (cond ((> num-chars 15) 1.1)
                           ((> num-chars 5) 1)
                           (t 0.9)))
         (new-speed (* speed speed-mult)))
    (message "setting speed to %s" new-speed)
    (setf (timer--repeat-delay fill-timer) new-speed
          speed new-speed)))

(defun typemaster-type()
  (while t
    (let ((char (read-char typemaster-prompt-string))
          (test (char-after next-marker)))
      (when (= char test)
        (typemaster-fill)))))
