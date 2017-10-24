;;; typemaster.el --- Advanced Typing trainer -*- lexical-binding: t -*-

;; unlicensed

;; Author: timor <timor.dd@googlemail.com>
;; Version: 0.2
                                        ; ;; Package-Requires: ((flange "1.0"))
;; Keywords: games
;; URL: http://github.com/timor/typemaster2000

;;; Commentary:

;; Analyze texts and generate a markov model that can be used to generate typing
;; training data.


(defvar-local typemaster-statistics '()
  "Holds the current statistics for a buffer.  Records include time, hit delay ans mismatch count.")

(defvar-local typemaster-manual-input ""
  "These characters will be output before the original generator is resumed.")

(defvar-local typemaster-missed-digrams '()
  "Used to store missed digrams.  If a certain threshold is violated, these are inserted into the character stream as a training pattern.")
(defvar typemaster-digram-repeat-threshold 2
  "After how much missed digrams a set of training digrams is inserted.")

(defvar typemaster-prob-adjustments ()
  "Alist which influences the choice of next characters.")

;; supply some simple standard filters
(defvar typemaster-util-filter-common-text "[-_a-zA-Z0-9.,:()!?;]+")
(defvar typemaster-util-filter-text-with-quotes "[-_a-zA-Z0-9.,:()!?;'\"]+")
(defconst typemaster-resource-path (or load-file-name buffer-file-name))

(defun typemaster-analyze-text(k &optional filter index)
  "Analyze a given text, add the content to the content-buffer,
and extend the index. An optional character filter in the form of a set of chars can be
supplied that skips over these characters.  The paramter k determines the length of the markovian chain, and must be at least 1."
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
            with pr = (make-progress-reporter "Tokenizing text..." (point-min) (point-max))
            while next
            count next into token-count
            sum (length str) into char-count
            do (with-current-buffer content-buffer
                 (insert str " "))
            (progress-reporter-update pr (point))
            finally do (message "copied %s chars in %s tokens" char-count token-count)
            (progress-reporter-done pr)))
    (with-current-buffer content-buffer
      (goto-char (point-min))
      (loop for p from 1 to (- (point-max) (1+ k))
            for str = (buffer-substring p (+ p k 1))
            for s-1 = (substring str 0 k)
            for s = (elt str k)
            with pr = (make-progress-reporter "Analyzing text..." (point-min) (point-max))
            do (incf (alist-get s (gethash s-1 index) 0))
            (progress-reporter-update pr p)
            finally do (progress-reporter-done pr)
            ))
    index))

;; (defun typemaster-calculate-adjustments (record)
;;   (let ((char (nth 1 record))
;;         (mismatches (nth 3 record)))
;;     (if (< mismatches 0)
;;         (incf (alist-get char typemaster-prob-adjustments 0)))))

;; utility for stripping python comments, to be used manually
(defun typemaster-util-strip-python-comments (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char 0)
    (while (re-search-forward "\"\"\"[^\"]*\"\"\"" nil t)
      (replace-match ""))
    (write-region nil nil file)))

(defun typemaster-analyze-file (file k &optional filter index)
  (with-temp-buffer
    (insert-file-contents file)
    (typemaster-analyze-text k filter index)))

(defun typemaster-analyze-files (files k &optional filter index)
  (loop for f in files
        for i from 1
        with l = (length files)
        with ind = (or index nil)
        do
        (message "anylzing [%s/%s]:%s" i l f)
        (setf ind (typemaster-analyze-file f k filter ind))
        finally (return ind)))

(defun typemaster-find-candidates (str index)
  (let* ((matches (gethash str index)))
    (loop for (s . num) in matches collect
          (cons num (concat str (list s))))))

(defun typemaster-choose-randomly (candidates)
  (nth (random (length candidates)) candidates))

(defun typemaster-choose-weighted (candidates)
  (loop with sum = (apply '+ (mapcar 'car candidates))
        for r = (random sum) then (- r p)
        for (p . next) in candidates
        until (<= r 0)
        finally (return next)))

(defun typemaster-adjust-candidates (candidates adjustments)
  (loop for (prob . str) in candidates
        with adjusted
        when (some (lambda(adj) (seq-contains str (car adj))) adjustments)
        collect (cons prob str) into adjusted-candidates and do (setf adjusted t)
        finally (return (if adjusted
                            (progn
                              ;; (message "Adjusted: %s" adjusted-candidates)
                              adjusted-candidates)
                          candidates))))

(defun typemaster-make-generator(index)
  "Generate new things based on the index in index-buffer. Take into account adjustments when choosing the next entry."
  (let ((state (typemaster-choose-weighted (typemaster-find-candidates (typemaster-choose-randomly (hash-table-keys index)) index))))
    (lambda()
      (let* ((s-1 (substring state 1))
             (next-state (typemaster-choose-weighted (typemaster-adjust-candidates (or (typemaster-find-candidates s-1 index) (error "no candidate found for '%s'" s-1))
                                                                                   typemaster-prob-adjustments)))
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

(defun typemaster-make-buffer (index &optional arg)
  "Create a type-master buffer"
  (with-current-buffer (get-buffer-create "*typemaster2000*")
    (erase-buffer)
    ;; (insert "Next: ")
    (setq-local next-marker (point-marker))
    ;; (insert "\nInp :")
    ;; (setq-local input-marker (point-marker))
    ;; (setq-local fill-timer (run-at-time time time 'typemaster-fill generator (current-buffer)))
    (setq-local num-chars 0)
    ;; (setq-local speed 0.5)
    (setq-local typemaster-index index)
    (setq-local typemaster-generator (typemaster-make-generator index))
    (setq-local typemaster-prompt-string (loop for i from 0 below 30 concat (funcall typemaster-generator)))
    (unless arg (switch-to-buffer (current-buffer)))
    (setq header-line-format "Press C-q to quit")
    (typemaster-fill)
    (typemaster-type)
    (unless arg (bury-buffer))
    ))

(defun typemaster-fill ()
  (typemaster-update-prompt)
  (goto-char next-marker)
  (delete-region (point) (line-end-position))
  (insert (propertize typemaster-prompt-string 'face '(:height 2.0))))

(defun typemaster-update-prompt ()
  (setq typemaster-prompt-string (concat (subseq typemaster-prompt-string 1)
                                         (let ((next
                                                (if (> (length typemaster-manual-input) 0)
                                                    (prog1
                                                        (seq-take typemaster-manual-input 1)
                                                      (setf typemaster-manual-input
                                                            (seq-drop typemaster-manual-input 1)))
                                                  (funcall typemaster-generator))))
                                           (if (string= next " ")
                                               (propertize next 'face 'highlight)
                                             next)))))

(defun typemaster-update-speed()
  (let* ((speed-mult (cond ((> num-chars 15) 1.1)
                           ((> num-chars 5) 1)
                           (t 0.9)))
         (new-speed (* speed speed-mult)))
    (message "setting speed to %s" new-speed)
    (setf (timer--repeat-delay fill-timer) new-speed
          speed new-speed)))

(defun typemaster-type()
  (loop
   with query-time = (current-time)
   with mismatches = 0
   with last-read
   for first = t then nil
   for char = (read-char-exclusive typemaster-prompt-string)
   for quit = (= char ?\C-q)
   for test = (char-after next-marker)
   while (not quit)
   when (= char test) do
   (setq last-read test)
   (let ((delta (float-time (time-since query-time))))
     (when (and (not first) (< delta 3.0))
       (push (list query-time char delta mismatches) typemaster-statistics)))
   (typemaster-fill)
   (setq query-time (current-time))
   (setq mismatches 0)
   (when (alist-get test typemaster-prob-adjustments)
     ;; (message "decreasing mismatches for '%s'" (string test))
     (decf (alist-get test typemaster-prob-adjustments 0 t)))
   ;; (message "Penalties: %s" (mapcar (lambda(x) (cons (string (car x)) (cdr x))) typemaster-prob-adjustments))
   else do (incf mismatches)
   ;; (message "increasing adjust for '%s'" (string test))
   (incf (alist-get test typemaster-prob-adjustments 0) 2)
   (when last-read
     (let ((digram (string last-read test)))
       (unless (seq-contains digram 32)
         (push digram typemaster-missed-digrams)))
     ;; (message "missed digrams: %s" typemaster-missed-digrams)
     (let ((maybe-practice-digram (some (lambda (x)
                                          (and (>= (count x typemaster-missed-digrams :test 'equal) typemaster-digram-repeat-threshold)
                                               x)) typemaster-missed-digrams)))
       (when maybe-practice-digram
         (setf typemaster-manual-input (concat typemaster-manual-input
                                               (loop for i from 1 to 3 concat maybe-practice-digram concat maybe-practice-digram concat " ")))
         (setf typemaster-missed-digrams (remove maybe-practice-digram typemaster-missed-digrams))
         )))))

(defun typemaster-show-stats (&optional stats)
  "Give a summary of current typing stats. If stats is not given,
  try to use the current buffer-local value of typemaster-statistics."
  (let (delays)
    (loop
     for (time char delta mismatch) in (or stats typemaster-statistics)
     do (push delta (alist-get char delays ())))
    (let ((averages (loop for (char . dlist) in delays collect
                          (cons char (/ (apply '+ dlist) (length dlist))))))
      (princ "Average delays by character:\n")
      (loop for (char . avg) in (cl-sort averages '> :key 'cdr) do
            (princ (format "'%s': %s hits, avg. delay %.2f sec.\n" (string char) (length (alist-get char delays)) avg))))))

(defun typemaster-find-index-file (fname)
  (let* ((path (file-name-directory typemaster-resource-path)))
    (expand-file-name (concat "./" fname) path)))

;;;###autoload
(defun typemaster-practice-english (&optional arg)
  (interactive "P")
  (typemaster-make-buffer (typemaster-load-index-from-file (typemaster-find-index-file "encyc3.gz")) arg))

;;;###autoload
(defun typemaster-practice-python (&optional arg)
  (interactive "P")
  (typemaster-make-buffer (typemaster-load-index-from-file (typemaster-find-index-file "pyind.gz")) arg))

(provide 'typemaster)

;;; typemaster.el ends here
