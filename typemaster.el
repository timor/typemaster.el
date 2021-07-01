;;; typemaster.el --- Advanced Typing trainer -*- lexical-binding: t -*-

;; unlicensed

;; Author: timor <timor.dd@googlemail.com>
;; Version: 0.8.1
;; Keywords: games
;; URL: http://github.com/timor/typemaster.el

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'seq)

(defgroup typemaster nil "Group for typemaster options."
  :group 'games)

(defcustom typemaster-digram-repeat-threshold 2
  "After how much missed digrams a set of training digrams is inserted."
  :type 'integer
  :group 'typemaster)

(defcustom typemaster-valid-max-delta 3
  "After how many seconds delay a typed character is excluded from statistics and multiplier calculation."
  :type 'number
  :group 'typemaster)

(defcustom typemaster-color-p t
  "If non-nil, color the prompt string in colors corresponding to different fingers."
  :type 'boolean
  :group 'typemaster)

(defcustom typemaster-show-penalties-p nil
  "If non-nil, show a line which displays the current character penalties."
  :type 'boolean
  :group 'typemaster)

(defcustom typemaster-show-histogram-p nil
  "If non-nil, show a histogram for recent key press delays."
  :type 'boolean
  :group 'typemaster)

(defcustom typemaster-training-font-height 2.0
  "Scaling factor of training input text font"
  :type 'number
  :group 'typemaster)

(defcustom typemaster-show-last-char 'mismatch
  "Set to non-nil to display the last pressed character below the prompt line."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "After Mismatch" mismatch))
  :group 'typemaster)

(defvar-local typemaster-statistics '()
  "Holds the current statistics.  Records include time, hit delay ans mismatch count.")

(defvar-local typemaster-manual-input ""
  "These characters will be output before the original generator is resumed.")

(defvar-local typemaster-missed-digrams '()
  "Used to store missed digrams.  If a certain threshold is violated, these are inserted into the character stream as a training pattern.")

(defvar-local typemaster-prob-adjustments ()
  "Alist which influences the choice of next characters.")

(defvar-local typemaster-ignored-chars '()
  "Set of chars which are replaced by spaces during training.")

(defvar-local typemaster-score 0)
(defvar-local typemaster-highscore 0)
(defvar-local typemaster-multiplier 1)

(defvar-local typemaster-num-chars 30)
(defconst typemaster-min-flow 0.5)
(defvar-local typemaster-best-flow typemaster-min-flow)
(defvar-local typemaster-flow typemaster-min-flow)
(defvar-local typemaster-accel nil)
(defconst typemaster-max-accel 0.3)
(defconst typemaster-slow-accel -0.25 "If current accel value is below that, penalize char.")
(defvar-local typemaster-fill-timer nil)
(defvar-local typemaster-next-marker nil)
(defvar-local typemaster-last-char-marker nil)
(defvar-local typemaster-prompt-string nil)
(defvar-local typemaster-generator nil)
(defvar-local typemaster-penalty-marker-start nil)
(defvar-local typemaster-penalty-marker-end nil)
(defvar-local typemaster-info-region-start nil)
(defvar-local typemaster-info-region-end nil)
(defvar-local typemaster-target-pace-marker nil)
(defvar-local typemaster-histogram-marker-start nil)
(defvar-local typemaster-histogram-marker-end nil)

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

(defconst typemaster-fingers-de
  '(("^°1!2\"qQ@aAyY<>|" . lh-1)
    ("3§wWsSxX" . lh-2)
    ("4$eE€dDcC" . lh-3)
    ("5%6&rRtTfFgGvVbB" . lh-4)
    ("7/{8([zZuUhHjJnNmM" . rh-4)
    ("9)]iIkK,;" . rh-3)
    ("0=}oOlL.:" . rh-2)
    ("ß?\´`üÜ+*~öÖäÄ#'-_pP\\" . rh-1)))


(defconst typemaster-keyboard-de "
[^][1][2][3][4][5][6] [7][8][9][0][ß][´]
    [Q][W][E][R][T] [Z][U][I][O][P][Ü][+]
[⇩  ][A][S][D][F̱][G] [H][J̱̱̱̱][K][L][Ö][Ä][#]
[⇧][<][Y][X][C][V][B] [N][M][,][.][-][⇧    ]
         [                   ]"
  )

(defconst typemaster-keyboard-en-us-105 "
[`][1][2][3][4][5][6] [7][8][9][0][-][=]
    [Q][W][E][R][T] [Y][U][I][O][P][[][]]
[⇩  ][A][S][D][F̱][G] [H][J̱̱̱̱][K][L][;]['][\\]
[⇧][\\][Y][X][C][V][B] [N][M][,][.][/][⇧    ]
         [                   ]"
  )

(defconst typemaster-keyboard-en-us-104 "
[`][1][2][3][4][5][6] [7][8][9][0][-][=]
    [Q][W][E][R][T] [Y][U][I][O][P][[][]][\\]
[⇩  ][A][S][D][F̱][G] [H][J̱̱̱̱][K][L][;][']
[⇧   ][Y][X][C][V][B] [N][M][,][.][/][⇧    ]
         [                   ]"
  )

(defvar typemaster-keyboard-ascii-art nil)

;; TODO: can be deducted from finger layout!!!
(defvar typemaster-fingers typemaster-fingers-en)

(defvar typemaster-homerow-en '("A" "S" "D" "F" "J" "K" "L" ";"))

(defvar typemaster-homerow-de '("A" "S" "D" "F" "J" "K" "L" "Ö"))

(defvar typemaster-homerow typemaster-homerow-en)

(defmacro typemaster-stat (field stat  &optional default)
  "Shorthand for accessing statistics fields"
  `(alist-get ,field ,stat ,default))

(defun typemaster-char-color (char)
  "Look up the color for char.  Return nil if nothing was found."
  (alist-get (cdr (cl-find char typemaster-fingers :key 'car :test (lambda (char elt)
                                                                     (seq-contains-p elt char))))
             typemaster-finger-colors))

(defun typemaster-propertize (charstring &optional exceptions)
  (let* ((color-type (if (eq (frame-parameter nil 'background-mode) 'light)
                         :background
                       :foreground))
         (extra-props
          (if (string= charstring " ")
              '(highlight)
            (when (and typemaster-color-p (not (member charstring exceptions))) (list color-type (typemaster-char-color (elt charstring 0)))))))
    (propertize charstring 'face (append `(:weight bold :height ,typemaster-training-font-height) extra-props))))

(defun typemaster-find-candidates (str index)
  (let* ((matches (gethash str index)))
    (cl-loop for (s . num) in matches collect
          (cons num (concat str (list s))))))

(defun typemaster-choose-randomly (candidates)
  (nth (random (length candidates)) candidates))

(defun typemaster-choose-weighted (candidates)
  (cl-loop with sum = (apply '+ (mapcar 'car candidates))
        for r = (random sum) then (- r p)
        for (p . next) in candidates
        until (<= r 0)
        finally (return next)))

(defun typemaster-adjust-candidates (candidates adjustments)
  (cl-loop for (prob . str) in candidates
        with adjusted
        when (cl-some (lambda(adj) (seq-contains-p str (car adj))) adjustments)
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
    (cl-loop for (k . v) in alist do
          (setf (gethash k index) v)
          finally (return index))))

(defun typemaster-generate-char ()
  "Return a string with 1 character, according to current ignore-state."
  (cl-loop for next = (if (> (length typemaster-manual-input) 0)
                       (prog1
                           (typemaster-propertize (seq-take typemaster-manual-input 1))
                         (setf typemaster-manual-input
                               (seq-drop typemaster-manual-input 1)))
                     (funcall typemaster-generator))
        until (not (member (elt next 0) typemaster-ignored-chars))
        finally (return next)))

(defun typemaster-init-prompt-string ()
  (setq-local typemaster-prompt-string (cl-loop for i from 0 below typemaster-num-chars concat (typemaster-generate-char))))

(defun typemaster-init-vars ()
  (setf typemaster-accel 0.0)
  (typemaster-update-score nil))

(defun typemaster-make-buffer (index &optional arg)
  "Create a type-master buffer"
  (with-current-buffer (get-buffer-create "*typemaster2000*")
    (erase-buffer)
    ;; (insert "Next: ")
    (setq-local typemaster-next-marker (point-marker))
    ;; (insert "\nInp :")
    ;; (setq-local input-marker (point-marker))
    ;; (setq-local typemaster-fill-timer (run-at-time time time 'typemaster-refill generator (current-buffer)))
    (insert "\n")
    (setq-local typemaster-last-char-marker (point-marker))
    (insert "\n")
    (setq-local typemaster-target-pace-marker (point-marker))
    (when typemaster-color-p (insert "\n\n\n\nHomerow: ")
          (cl-loop for i in typemaster-homerow
                for x from 0
                do (insert (typemaster-propertize i) " ")
                when (= x 3) do (insert " ")))
    (when typemaster-keyboard-ascii-art (insert "\n\n" (cl-loop for c across typemaster-keyboard-ascii-art concat (typemaster-propertize (string c) '("[" "]" " " "\n")))))
    (insert "\n\n")
    (setq-local typemaster-info-region-start (point-marker))
    (insert " ")
    (setq-local typemaster-info-region-end (point-marker))
    (when typemaster-show-penalties-p
      (insert "\n\n\n\n\n\n\nPenalties: ")
      (setq-local typemaster-penalty-marker-start (point-marker))
      (insert " ")
      (setq-local typemaster-penalty-marker-end (point-marker)))
    (insert "\n")
    (setq typemaster-histogram-marker-start (point-marker))
    (insert " ")
    (setq typemaster-histogram-marker-end (point-marker))
    (setq-local typemaster-generator (typemaster-make-generator index))
    (setq-local typemaster-ignored-chars '())
    (typemaster-init-prompt-string)
    (unless arg (switch-to-buffer (current-buffer)))
    (setq header-line-format "Press C-q to quit")
    (typemaster-init-vars)
    (typemaster-redraw nil nil)
    (typemaster-type)
    (unless arg (bury-buffer))
    (setq-local pre-command-hook nil)
    (setq-local post-command-hook nil)
    ))

(defun typemaster-refill ()
  "Fill the prompt until it has `num-char' characters."
  (let ((missing (- typemaster-num-chars (length typemaster-prompt-string))))
    (setq typemaster-prompt-string (concat typemaster-prompt-string
                                           (cl-loop repeat missing concat (typemaster-generate-char)))))
  (goto-char typemaster-next-marker)
  (delete-region (point) (line-end-position))
  (insert typemaster-prompt-string))

(defun typemaster-update-penalties ()
  (goto-char typemaster-penalty-marker-start)
  (delete-region (point) (1- typemaster-penalty-marker-end))
  (cl-loop for (char . penalty) in (cl-sort (copy-sequence typemaster-prob-adjustments) '> :key 'cdr)
        do (insert (typemaster-propertize (string char)) (format ": %s, " penalty))))

(defun typemaster--remove-char (char string)
  "Returns string with occurrences of CHAR removed, keeping text properties intact."
  (cl-loop for str = string then (cl-subseq str 1)
           while (> (length str) 0)
           for next = (elt str 0)
           unless (= next char) concat (cl-subseq str 0 1)))

(defun typemaster-ignore-char (char)
  (cl-pushnew char typemaster-ignored-chars)
  (setf (alist-get char typemaster-prob-adjustments) 0)
  (setf typemaster-prompt-string (typemaster--remove-char char typemaster-prompt-string))
  (typemaster-refill))

(defun typemaster--score-color (score)
  "Return a color that depends on the current highscore."
  (let* ((upper (max 0.1 typemaster-highscore))
        (above-p (> score upper))
        (diff (/ (abs (- upper score))
                 upper))
        (b (- 1.0 (max 0.0 (min 1.0 diff))))
        (r (if above-p b 1.0))
        (g (if above-p 1.0 b)))
    (color-rgb-to-hex r g b)))

(defun typemaster--propertize-score (str score)
  "Return STR with props."
  (propertize str 'face
              (append `(:weight bold :height ,typemaster-training-font-height :foreground ,(typemaster--score-color score)))))

(defun typemaster--propertize-bold (str)
  (propertize str 'face
              (append `(:weight bold :height ,typemaster-training-font-height))))

(defun typemaster--best-multiplier ()
  (/ typemaster-best-flow typemaster-min-flow))

(defun typemaster-show-score ()
  (let ((score (ceiling typemaster-score))
        (highscore (ceiling typemaster-highscore)))
    (goto-char typemaster-info-region-start)
    (delete-region (point) (1- typemaster-info-region-end))
    ;; (when typemaster-accel
    ;;   (insert "Accel: " (format "%.5f" typemaster-accel) "\n"))
    ;; (insert "Current Flow: " (format "%.5f" typemaster-flow) "\n")
    (insert "Current Score: "
            (typemaster--propertize-score
             (format "%d (x%.1f)" score typemaster-multiplier)
             score)
            (if (typemaster--outlier-p)
                (typemaster--propertize-bold " Too fast!")
              "")
            "\n")
    (insert "Highscore: " (typemaster--propertize-score
                           (format "%d (x%.1f)" highscore (typemaster--best-multiplier))
                           highscore))))

(defun typemaster--extrapolate-accel (flow dt)
  (- (log (* flow dt) 10)))

(defun typemaster--outlier-p ()
  (if typemaster-accel
      (> typemaster-accel typemaster-max-accel)
    t))

;; NOTE: also updates flow!
(defun typemaster-update-multiplier (delta)
  "Speed-dependent multiplier.  Integrates over deltas (with a decay factor) to
gain smooth speed-dependent value.  Filter out outliers, so multplier can only
  be increased by steady typing."
  (when delta
    (let* ((alpha-speed 0.1)
           (alpha-comp (- 1 alpha-speed))
           ;; (reference 1.0)
           ;; (reference typemaster-min-flow)
           (x (/ delta))
           ;; (delta (min typemaster-valid-max-delta delta))
           (y typemaster-flow)
           (next (+ (* alpha-speed x) (* alpha-comp y))))
      (unless (typemaster--outlier-p)
        (setf typemaster-flow next)
        (setf typemaster-multiplier (/ typemaster-flow typemaster-min-flow))))))

(defun typemaster-restart-flow ()
  (setf typemaster-flow
        (max (* typemaster-best-flow 0.25)
             typemaster-min-flow
             (/ typemaster-flow 2))))

(defun typemaster-update-score (matchp)
  (if matchp
      (cl-incf typemaster-score typemaster-multiplier)
    (if (> typemaster-score typemaster-highscore)
        (setf typemaster-highscore typemaster-score))
    (setf typemaster-score 0)
    (setf typemaster-best-flow (max typemaster-best-flow typemaster-flow))
    (typemaster-restart-flow)))

(defun typemaster-redraw (char match-p)
  (when typemaster-show-last-char
    (goto-char typemaster-last-char-marker)
    (delete-region (point) (line-end-position))
    (insert (typemaster-propertize (if (and (characterp char)
                                            (or (eq typemaster-show-last-char t)
                                                (not match-p)))
                                       (string char)
                                     " "))))
  (typemaster-show-score)
  (when typemaster-show-penalties-p
    (typemaster-update-penalties))
  (typemaster-refill)
  (when (get-buffer-window "*typemaster-statistics*")
    (typemaster-update-statistics-buffer nil typemaster-prob-adjustments)))

(defun typemaster--add-penalty (char value)
  (cl-incf (alist-get char typemaster-prob-adjustments 0) value))

(defun typemaster--valid-char-p (char valid-chars)
  (and (not (= 32 char))
       (cl-member char valid-chars)))

(defun typemaster--valid-digram (first second valid-chars)
  (when (and (typemaster--valid-char-p first valid-chars)
             (typemaster--valid-char-p second valid-chars))
    (string first second)))

(defun typemaster-type()
  (cl-loop
   with valid-chars = (cl-loop for (s . f) in typemaster-fingers append (append s nil))
   with query-time = (current-time)
   with mismatches = 0
   with last-read                       ; last correctly read char
   for first = t then nil
   for prompt-time = (current-time)
   for char = (read-char-exclusive (substring-no-properties typemaster-prompt-string))
   for hit-time = (current-time)
   for delta = nil then (float-time (time-subtract hit-time prompt-time))
   for valid-delta = nil then (float-time (time-since query-time)) ; delta since last correct keypress
   for quit = (= char ?\C-q)
   for show-stats = (= char ?\C-s)
   for ignore-next = (= char ?\C-i)
   with stats-window
   finally (if stats-window (delete-window stats-window))
   for test = (char-after typemaster-next-marker)
   for spacep = (= test 32)
   for last-match-p = nil then match-p
   for match-p = (= char test)
   for last2-valid-p = (and match-p last-match-p)
   for accel = nil then (typemaster--extrapolate-accel typemaster-flow delta)
   do (setf typemaster-accel accel)
   while (not quit)
   if show-stats do (if stats-window (setq stats-window (progn (delete-window stats-window)))
                      (setq stats-window (display-buffer (typemaster-get-statistics-buffer) '(display-buffer-pop-up-window ((inhibit-same-window . t))))))
   else if ignore-next do
   (typemaster-ignore-char test)
   else if match-p do
   ;; on-match
   (setq last-read test)
   (when (not first)
     (push `((:query-time . ,query-time) (:char . ,char) (:delta . ,valid-delta) (:mismatches . ,mismatches)) typemaster-statistics))
   (setq typemaster-prompt-string (cl-subseq typemaster-prompt-string 1))
   ;; (when typemaster-show-histogram-p
   ;;   (typemaster-update-statistics 30))
   (setq mismatches 0)
   (when (alist-get test typemaster-prob-adjustments)
     ;; (message "decreasing mismatches for '%s'" (string test))
     (when (< (alist-get test typemaster-prob-adjustments) 0)
       (error "Negative adjustment"))
     (cl-decf (alist-get test typemaster-prob-adjustments 0 t)))
   ;; (message "Penalties: %s" (mapcar (lambda(x) (cons (string (car x)) (cdr x))) typemaster-prob-adjustments))
   ;; If the char was correct, but the time too slow, add penalty
   (when (and (not spacep) last2-valid-p (< typemaster-accel typemaster-slow-accel))
     (let ((penalty (1+ (round (/ typemaster-accel typemaster-slow-accel)))))
       (typemaster--add-penalty test (min 10 penalty))))
   (typemaster-update-multiplier delta)
   (setf query-time (current-time))
   else do
   ;; on-mismatch
   (cl-incf mismatches)
   ;; (message "increasing adjust for '%s'" (string test))
   (when (< mismatches 4) (typemaster--add-penalty test 3)
         (when (typemaster--valid-char-p last-read valid-chars)
           (typemaster--add-penalty last-read 1)))
   (when last-read
     (let ((prompted-digram (typemaster--valid-digram last-read test valid-chars))
           (typed-digram (typemaster--valid-digram char test valid-chars)))
       (when prompted-digram
         (push prompted-digram typemaster-missed-digrams))
       (when typed-digram
         (push typed-digram typemaster-missed-digrams)
         ))
     ;; (message "missed digrams: %s" typemaster-missed-digrams)
     (let* ((applicable-digrams (delete-dups (cl-remove-if-not (lambda(x) (>= (cl-count x typemaster-missed-digrams :test 'equal) typemaster-digram-repeat-threshold))
                                                            typemaster-missed-digrams)))
            (maybe-practice-digrams (when (>= (length applicable-digrams) 2)
                                      (cl-subseq applicable-digrams 0 2))))
       (when maybe-practice-digrams
         (cl-destructuring-bind (a b) maybe-practice-digrams
           (setf typemaster-manual-input (concat typemaster-manual-input
                                                 (cl-loop for i from 1 to 3
                                                       concat (concat a a " " b b " "))))
           (setf typemaster-missed-digrams (cl-remove b (cl-remove a typemaster-missed-digrams :test 'string-equal) :test 'string-equal)))
         ))) end
    and do
    ;; unconditionally after every input
    (typemaster-update-score match-p)
    (typemaster-redraw char match-p)
   ))

(defun typemaster-util-draw-gauge (value width)
  (let* ((position (- width (ceiling (* width value))))
         (str (cl-loop for i from 0 below width concat
                   (if (= i position)
                       "|"
                     "-"))))
    (insert (propertize str 'face `(:height ,typemaster-training-font-height)))))

(defun typemaster-util-draw-inline-histogram (values num-bins &optional max)
  "Return string representing a small histogram of a list of
  values and width. Requires unicode."
  (apply 'string
         (if (not values)
             (make-list num-bins #x2581)
           (let* ((min 0)
                  (max (or max (apply 'max values)))
                  (bins (make-list num-bins 0))
                  (h (/ (float (- max min)) num-bins)))
             (cl-loop for v in values do
                   (cl-loop for i from (1- num-bins) downto 0
                         for test downfrom (- max h) by h
                         when (>= v test)
                         do (cl-incf (nth i bins))
                         and return nil))
             (let* ((maxbin (apply 'max bins))
                    (height 8)
                    (cols (mapcar (lambda (x) (ceiling (* (1- height) (/ (float x) maxbin)))) bins)))
               (cl-loop for c in cols collect (+ c #x2581)))))))

;; unused (currently deprecated)
;; (defun typemaster-update-statistics (&optional bins)
;;   "Update statistics-related display.  BINS is for the debug
;; display for the histogram and can be a number, or one of the
;; methods :square-root or :rice."
;;   (when typemaster-statistics
;;     (let* ((deltas (mapcar (lambda (stat) (typemaster-stat :delta stat)) (last typemaster-statistics 30)))
;;            (n (length deltas))
;;            (min ;; (apply 'min deltas)
;;             0)
;;            (max (apply 'max deltas))
;;            (mean (/ (apply '+ deltas) n))
;;            (median (if (cl-evenp n)
;;                        (/ (+ (nth (/ n 2) deltas) (nth (1- (/ n 2)) deltas))
;;                           2)
;;                      (nth (/ (1- n) 2) deltas)))
;;            (sdev (sqrt (/ (cl-loop for i from 0 below n sum (expt (- (nth i deltas) mean) 2))
;;                           (1- n))))
;;            (k (ceiling (cond ((eq bins :square-root)
;;                               (sqrt n))
;;                              ((eq bins :rice)
;;                               (* 2 (expt n (/ 1.0 3))))
;;                              ((natnump bins)
;;                               bins)
;;                              (t (error "bins must be a valid method keyword or a positive integer")))))
;;            (h (/ (- max min) k))
;;            (bins (make-list k 0))
;;            (centers (cl-loop for i from 1 to k collect (- (* i h) (/ h 2.0)))))
;;       (let* ((pace-sdev-good 0.05)
;;              (pace-sdev-bad 0.5)
;;              (pace-sdev (1- (exp (if (isnan sdev)
;;                                                 0
;;                                               (* 1.5 sdev))))))
;;         (goto-char typemaster-target-pace-marker)
;;         (delete-region (point) (line-end-position))
;;         (typemaster-util-draw-gauge pace-sdev typemaster-num-chars))
;;       (cl-loop for d in deltas do
;;             (cl-loop for i from (1- k) downto 0
;;                   for test downfrom (- max h) by h
;;                   when (>= d test)
;;                   do (cl-incf (nth i bins))
;;                   and return nil))
;;       (let* ((maxbin (apply 'max bins))
;;              (height 8)
;;              (cols (mapcar (lambda (x) (ceiling (* 8 (/ (float x) maxbin)))) bins)))
;;         (goto-char typemaster-histogram-marker-start)
;;         (delete-region (point) (1- typemaster-histogram-marker-end))
;;         (cl-loop for v downfrom (1- height) to 0 do
;;               (insert "\n")
;;               (cl-loop for c in cols do
;;                     (if (> c v)
;;                         (insert ?#)
;;                       (insert " ")))))
;;       (insert "\n")
;;       (insert (format "min: %3f max: %3f mean: %3f median: %3f sdev: %3f" min max mean median sdev)))))

(defun typemaster-get-statistics-buffer ()
  (get-buffer-create "*typemaster-statistics*"))

(defun typemaster-update-statistics-buffer (&optional stats penalties)
  "Give a summary of current typing stats. If stats is not given,
  try to use the current buffer-local value of typemaster-statistics."
  (let (delays presented total-mismatches max-delay)
    (cl-loop
     for s in (or stats typemaster-statistics)
     ;; for time = (typemaster-stat :query-time s)
     for char = (typemaster-stat :char s)
     for delta = (typemaster-stat :delta s)
     for mismatches = (typemaster-stat :mismatches s)
     when (< delta typemaster-valid-max-delta) do       ; disregard inputs with delays larger than 5 seconds
     (when (= 0 mismatches) (push delta (alist-get char delays ())))
     and
     maximize delta into dmax
     end
     finally do (setf max-delay dmax)
     do
     (cl-incf (alist-get char presented 0))
     (cl-incf (alist-get char total-mismatches 0) mismatches))
    (let ((per-char-stats (cl-loop for (char . num) in presented collect
                                (list char (- 1 (/ (float (alist-get char total-mismatches)) num)))))
          (format-string "%4s | %10s | %10s | %10s | %10s | %20s\n"))
      (with-current-buffer (typemaster-get-statistics-buffer)
        (erase-buffer)
        (insert "Character statistics:\n\n")
        (insert (format format-string  "char" "# prompted" "mismatches" "penalty" "hit ratio" "delay hist"))
        (cl-loop for (char hit-ratio) in (cl-sort per-char-stats '< :key 'cadr)
              ;; TODO: pack prompted and mismatched into the same data set as the other stuff above...
              for prompted = (alist-get char presented)
              for mismatches = (alist-get char total-mismatches)
              for char-delays = (alist-get char delays)
              do
              (insert (format format-string
                              (string char)
                              prompted
                              mismatches
                              (or (alist-get char penalties) "")
                              (format "%.2f" hit-ratio)
                              ;; set the maximum value of the histogram to 1/3rd
                              ;; of the max delay. This is completely empiric
                              ;; though, it might make more sense to choose that
                              ;; based on standard deviation or stuff
                              (typemaster-util-draw-inline-histogram char-delays 20 (/ (float (or max-delay typemaster-valid-max-delta)) 3)))))))))

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
(defun typemaster-practice-german-de (&optional arg)
  (interactive "P")
  (let ((typemaster-fingers typemaster-fingers-de)
        (typemaster-homerow typemaster-homerow-de)
        (typemaster-keyboard-ascii-art typemaster-keyboard-de))
    (typemaster-make-buffer (typemaster-load-index-from-file (typemaster-find-index-file "wp-featured-de.gz")) arg)))

;;;###autoload
(defun typemaster-practice-nix (&optional arg)
  (interactive "P")
  (typemaster-make-buffer (typemaster-load-index-from-file (typemaster-find-index-file "nix4.gz")) arg))

(provide 'typemaster)

;;; typemaster.el ends here
