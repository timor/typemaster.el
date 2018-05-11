;;; typemaster.el --- Advanced Typing trainer -*- lexical-binding: t -*-

;; unlicensed

;; Author: timor <timor.dd@googlemail.com>
;; Version: 0.4
;; Keywords: games
;; URL: http://github.com/timor/typemaster.el

;;; Commentary:

;;; Code:
(require 'cl)
(require 'subr-x)

(defgroup typemaster nil "Group for typemaster options."
  :group 'games)

(defcustom typemaster-digram-repeat-threshold 2
  "After how much missed digrams a set of training digrams is inserted."
  :type 'integer
  :group 'typemaster)

(defcustom typemaster-color-p t
  "If non-nil, color the prompt string in colors corresponding to different fingers."
  :type 'boolean
  :group 'typemaster)

(defcustom typemaster-show-penalties-p nil
  "If non-nil, show a line which displays the current character penalties."
  :type 'boolean
  :group 'typemaster)

(defcustom typemaster-training-font-height 2.0
  "Scaling factor of training input text font"
  :type 'number
  :group 'typemaster)

(defvar-local typemaster-statistics '()
  "Holds the current statistics.  Records include time, hit delay ans mismatch count.")

(defvar-local typemaster-manual-input ""
  "These characters will be output before the original generator is resumed.")

(defvar-local typemaster-missed-digrams '()
  "Used to store missed digrams.  If a certain threshold is violated, these are inserted into the character stream as a training pattern.")

(defvar-local typemaster-prob-adjustments ()
  "Alist which influences the choice of next characters.")

(defvar-local num-chars 30)
(defvar-local speed nil)
(defvar-local fill-timer nil)
(defvar-local next-marker nil)
(defvar-local typemaster-prompt-string nil)
(defvar-local typemaster-generator nil)
(defvar-local penalty-marker-start nil)
(defvar-local penalty-marker-end nil)
(defvar-local target-pace-marker nil)
(defvar-local histogram-marker-start nil)
(defvar-local histogram-marker-end nil)

(defconst typemaster-resource-path (or load-file-name buffer-file-name))

(defconst typemaster-finger-colors
  '(
    (lh-1 . "orchid2")
    (lh-2 . "SteelBlue1")
    (lh-3 . "PaleGreen1")
    (lh-4 . "yellow1")
    (rh-4 . "goldenrod3")
    (rh-3 . "chartreuse3")
    (rh-2 . "LightBlue3")
    (rh-1 . "plum2")
    ))

(defconst typemaster-fingers-en
  '(("`~1!2@qQaAzZ" . lh-1)
    ("3#wWsSxX" . lh-2)
    ("4$eEdDcC" . lh-3)
    ("5%6^rRtTfFgGvVbB" . lh-4)
    ("7&8*yYuUhHjJnNmM" . rh-4)
    ("9(iIkK,<" . rh-3)
    ("0)oOlL.>" . rh-2)
    ("-_=+pP[{}];:'\"\\|/?" . rh-1)))

(defun typemaster-char-color (char)
  "Look up the color for char.  Only supports english keyboard for now.  Return nil if nothing was found."
  (alist-get (cdr (find char typemaster-fingers-en :key 'car :test (lambda (char elt)
                                                                     (seq-contains elt char))))
             typemaster-finger-colors))

(defun typemaster-propertize (charstring)
  (let* ((color-type (if (eq (frame-parameter nil 'background-mode) 'light)
                         :background
                       :foreground))
         (extra-props
          (if (string= charstring " ")
              '(highlight)
            (when typemaster-color-p (list color-type (typemaster-char-color (elt charstring 0)))))))
    (propertize charstring 'face (append `(:weight bold :height ,typemaster-training-font-height) extra-props))))

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
        (typemaster-propertize output)
        ))))

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
    (insert "\n")
    (setq-local target-pace-marker (point-marker))
    (when typemaster-color-p (insert "\n\n\n\nHomerow: ")
          (loop for i in '("A" "S" "D" "F" "J" "K" "L" ";")
                for x from 0
                do (insert (typemaster-propertize i) " ")
                when (= x 3) do (insert " ")))
    (when typemaster-show-penalties-p
      (insert "\n\n\n\n\n\n\nPenalties: ")
      (setq-local penalty-marker-start (point-marker))
      (insert " ")
      (setq-local penalty-marker-end (point-marker)))
    (insert "\n")
    (setq histogram-marker-start (point-marker))
    (insert " ")
    (setq histogram-marker-end (point-marker))
    (setq-local typemaster-generator (typemaster-make-generator index))
    (setq-local typemaster-prompt-string (loop for i from 0 below num-chars concat (funcall typemaster-generator)))
    (unless arg (switch-to-buffer (current-buffer)))
    (setq header-line-format "Press C-q to quit")
    (typemaster-fill)
    (typemaster-type)
    (unless arg (kill-buffer))
    ))

(defun typemaster-fill ()
  (typemaster-update-prompt)
  (goto-char next-marker)
  (delete-region (point) (line-end-position))
  (insert typemaster-prompt-string)
  )

(defun typemaster-update-penalties ()
  (goto-char penalty-marker-start)
  (delete-region (point) (1- penalty-marker-end))
  (loop for (char . penalty) in (cl-sort (copy-seq typemaster-prob-adjustments) '> :key 'cdr)
        do (insert (typemaster-propertize (string char)) (format ": %s, " penalty))))

(defun typemaster-update-prompt ()
  (setq typemaster-prompt-string (concat (subseq typemaster-prompt-string 1)
                                         (let ((next
                                                (if (> (length typemaster-manual-input) 0)
                                                    (prog1
                                                        (typemaster-propertize (seq-take typemaster-manual-input 1))
                                                      (setf typemaster-manual-input
                                                            (seq-drop typemaster-manual-input 1)))
                                                  (funcall typemaster-generator))))
                                           next))))

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
   for char = (read-char-exclusive (substring-no-properties typemaster-prompt-string))
   for quit = (= char ?\C-q)
   for test = (char-after next-marker)
   while (not quit)
   do
   (when typemaster-show-penalties-p
     (typemaster-update-penalties))
   if (= char test) do
   (setq last-read test)
   (let ((delta (float-time (time-since query-time))))
     (when (not first)
       (add-to-list 'typemaster-statistics (list query-time char delta mismatches) t)))
   (typemaster-fill)
   (typemaster-do-statistics 30)
   (setq query-time (current-time))
   (setq mismatches 0)
   (when (alist-get test typemaster-prob-adjustments)
     ;; (message "decreasing mismatches for '%s'" (string test))
     (decf (alist-get test typemaster-prob-adjustments 0 t)))
   ;; (message "Penalties: %s" (mapcar (lambda(x) (cons (string (car x)) (cdr x))) typemaster-prob-adjustments))
   else do (incf mismatches)
   ;; (message "increasing adjust for '%s'" (string test))
   (incf (alist-get test typemaster-prob-adjustments 0) 3)
   (when last-read
     (let ((digram (string last-read test)))
       (unless (seq-contains digram 32)
         (push digram typemaster-missed-digrams)))
     ;; (message "missed digrams: %s" typemaster-missed-digrams)
     (let* ((applicable-digrams (delete-dups (remove-if-not (lambda(x) (>= (count x typemaster-missed-digrams :test 'equal) typemaster-digram-repeat-threshold))
                                                            typemaster-missed-digrams)))
            (maybe-practice-digrams (when (>= (length applicable-digrams) 2)
                                      (subseq applicable-digrams 0 2))))
       (when maybe-practice-digrams
         (destructuring-bind (a b) maybe-practice-digrams
           (setf typemaster-manual-input (concat typemaster-manual-input
                                                 (loop for i from 1 to 3
                                                       concat (concat a a " " b b " "))))
           (setf typemaster-missed-digrams (remove b (remove a typemaster-missed-digrams))))
         )))))

(defun typemaster-do-statistics (&optional bins)
  "Update statistics-related display.  BINS is for the debug
display for the histogram and can be a number, or one of the
methods :square-root or :rice."
  (when typemaster-statistics
    (let* ((deltas (mapcar 'third (last typemaster-statistics 30)))
           (n (length deltas))
           (min ;; (apply 'min deltas)
            0)
           (max (apply 'max deltas))
           (mean (/ (apply '+ deltas) n))
           (median (if (evenp n)
                       (/ (+ (nth (/ n 2) deltas) (nth (1- (/ n 2)) deltas))
                          2)
                     (nth (/ (1- n) 2) deltas)))
           (sdev (sqrt (/ (loop for i from 0 below n sum (expt (- (nth i deltas) mean) 2))
                          (1- n))))
           (k (ceiling (cond ((eq bins :square-root)
                              (sqrt n))
                             ((eq bins :rice)
                              (* 2 (expt n (/ 1.0 3))))
                             ((natnump bins)
                              bins)
                             (t (error "bins must be a valid method keyword or a positive integer")))))
           (h (/ (- max min) k))
           (bins (make-list k 0))
           (centers (loop for i from 1 to k collect (- (* i h) (/ h 2.0)))))
      (let* ((pace-sdev-good 0.05)
             (pace-sdev-bad 0.5)
             (pace-marker-position (- num-chars (ceiling (* num-chars (1- (exp (if (isnan sdev)
                                                                                   0
                                                                                 (* 1.5 sdev)))))))))
        (goto-char target-pace-marker)
        (delete-region (point) (line-end-position))
        (let ((str (loop for i from 0 below num-chars concat
                         (if (= i pace-marker-position)
                             "|"
                           "-"))))
          (insert (propertize str 'face `(:height ,typemaster-training-font-height)))))
      (loop for d in deltas do
            (loop for i from (1- k) downto 0
                  for test downfrom (- max h) by h
                  when (>= d test)
                  do (incf (nth i bins))
                  and return nil))
      (let* ((maxbin (apply 'max bins))
             (height 8)
             (cols (mapcar (lambda (x) (ceiling (* 8 (/ (float x) maxbin)))) bins)))
        (goto-char histogram-marker-start)
        (delete-region (point) (1- histogram-marker-end))
        (loop for v downfrom (1- height) to 0 do
              (insert "\n")
              (loop for c in cols do
                    (if (> c v)
                        (insert ?#)
                      (insert " ")))))
      (insert "\n")
      (insert (format "min: %3f max: %3f mean: %3f median: %3f sdev: %3f" min max mean median sdev)))))

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

;;;###autoload
(defun typemaster-practice-german (&optional arg)
  (interactive "P")
  (typemaster-make-buffer (typemaster-load-index-from-file (typemaster-find-index-file "wp-featured-de.gz")) arg))

;;;###autoload
(defun typemaster-practice-nix (&optional arg)
  (interactive "P")
  (typemaster-make-buffer (typemaster-load-index-from-file (typemaster-find-index-file "nix4.gz")) arg))

(provide 'typemaster)

;;; typemaster.el ends here
