;; -*- lexical-binding:t -*-
(require 'json)


;; Analyze texts and generate a markov model that can be used to generate typing
;; training data.

;; supply some simple standard filters
(defconst typemaster-util-char-filter-common-text "[-_a-zA-Z0-9.,:()!?;]+")
(defconst typemaster-util-char-filter-text-with-quotes "[-_a-zA-Z0-9.,:()!?;'\"/]+")
(defconst typemaster-util-char-filter-german "[-+=_a-zA-ZäöüÄÖÜß0-9.,:;()!?'\"/]+")

(require 'request)

(defun typemaster-extract-fetch-json (url)
  (let (result)
    ;; (message "fetching url: %s" url)
    (request url
             :parser 'json-read
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (setf result data)))
             :sync t
             )
    result))

(defun typemaster-extract-wikipage (lang page)
  "Download a single wiki page, return page in plaintext."
  (let ((query (etypecase page
                 (number (format "pageids=%s" page))
                 (string (concat "titles=" page)))))
    (alist-get 'extract (first (alist-get 'pages (alist-get 'query (typemaster-extract-fetch-json
                                                                   (format "http://%s.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exlimit=max&explaintext&exsectionformat=plain&%s&redirects="
                                                                           lang query))))))))

(defun typemaster-extract-fetch-category (lang category &optional limit)
  "Request the list of featured articles for LANG"
  (let ((limit (or limit 1000)))
    (loop for resp-continue = nil then (alist-get 'cmcontinue (alist-get 'continue response))
          for continue = (if resp-continue (concat "&cmcontinue=" resp-continue) "")
          for cmlimit = (min 500 limit) then i
          for response = (typemaster-extract-fetch-json (format
                                                         "http://%s.wikipedia.org/w/api.php?format=json&action=query&list=categorymembers&cmtitle=%s&cmlimit=%s&cmtype=page%s"
                                                         lang category cmlimit continue))
          for page-list = (alist-get 'categorymembers (alist-get 'query response))
          for length = (length page-list)
          for first = t then nil
          for i = (- limit length) then (- i length)
          append (mapcar (lambda (x) (alist-get 'pageid x)) page-list)
          while (> i 0)
          until (and (not first) (null resp-continue))
          )))

(defun typemaster-extract-for-category-articles (lang category func &optional limit)
  "Fetch all article texts from the specified category, call func with the extract text on each."
  (let ((alist (typemaster-extract-fetch-category lang category limit)))
    (loop
     with pr = (make-progress-reporter "Fetching Articles" 0 (length alist))
     for id in alist
     for i from 0
     for article = (typemaster-extract-wikipage lang id)
     do (progress-reporter-update pr i)
     do (funcall func id article)
     finally do (progress-reporter-done pr))))

(defun typemaster-extract-analyze-text(k &optional filter index)
  "Analyze a given text, add the content to the content-buffer,
and extend the index. An optional character filter in the form of a set of chars can be
supplied that skips over these characters.  The paramter k determines the length of the markovian chain, and must be at least 1."
  (let ((content-buffer (get-buffer-create "*analyzed-text*"))
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

(defun typemaster-extract-save-index-to-file (index filename)
  (with-temp-buffer
    (prin1 (loop for k being the hash-keys of index using (hash-values v)
                 collect (cons k v))
           (current-buffer))
    (write-file filename t)))

(defun typemaster-extract-analyze-file (file k &optional char-filter line-filter index)
  "Analyze one file.  LINE-FILTER is a regexp which is applied via `flush-lines'
before passing the contents to the analyzer, which will in turn use the
  char-string CHAR-FILTER to select only the chars present in that string."
  (with-temp-buffer
    (insert-file-contents file)
    (when line-filter
      (flush-lines line-filter))
    (typemaster-extract-analyze-text k char-filter index)))

(defun typemaster-extract-analyze-files (files k &optional char-filter line-filter index)
  (loop for f in files
        for i from 1
        with l = (length files)
        with ind = (or index nil)
        do
        (message "anylzing [%s/%s]:%s" i l f)
        (setf ind (typemaster-extract-analyze-file f k char-filter ind))
        finally (return ind)))

(defun typemaster-extract-save-wikipedia-category (lang category dir &optional limit)
  "Save all plaintext articles of the chosen category to files in directory dir."
  (let ((dir (file-name-as-directory dir)))
    (typemaster-extract-for-category-articles lang category
                                              (lambda(id text)
                                                (with-temp-buffer
                                                  (insert text)
                                                  (when (re-search-backward "References$")
                                                    (replace-match ""))
                                                  (when (re-search-backward "see also$")
                                                    (replace-match ""))
                                                  (write-file (concat dir (number-to-string id) ".gz"))))
                                              limit)))

