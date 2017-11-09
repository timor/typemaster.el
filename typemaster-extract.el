;; -*- lexical-binding:t -*-
(require 'json)
(require 'request)

(defun typemaster-extract-fetch-json (url)
  (let (result)
    (message "fetching url: %s" url)
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
                                                                   (format "http://%s.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exlimit=max&explaintext&%s&redirects="
                                                                           lang query))))))))

(defun typemaster-extract-fetch-category (lang category &optional limit)
  "Request the list of featured articles for LANG"
  (loop for resp-continue = nil then (alist-get 'cmcontinue (alist-get 'continue response))
        for continue = (if resp-continue (concat "&cmcontinue=" resp-continue) "")
        for response = (typemaster-extract-fetch-json (format
                                   "http://%s.wikipedia.org/w/api.php?format=json&action=query&list=categorymembers&cmtitle=%s&cmlimit=500&cmtype=page%s"
                                   lang category continue))
        for page-list = (alist-get 'categorymembers (alist-get 'query response))
        for length = (length page-list)
        for first = t then nil
        for i = (- (or limit 1000) length) then (- i length)
        while (> i 0)
        append (mapcar (lambda (x) (alist-get 'pageid x)) page-list)
        until (and (not first) (null resp-continue))
        ))

(defun typemaster-extract-fetch-category-articles (lang category &optional limit)
  "Fetch all article texts from the specified category"
  (loop for id in (typemaster-extract-fetch-category lang category limit)
        collect (typemaster-extract-wikipage lang id)))
