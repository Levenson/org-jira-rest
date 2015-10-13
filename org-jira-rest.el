;;; Code:

(require 'cl)
(require 'json)
(require 'org)
(require 'url-queue)

(require 'concurrent)

(defvar *debug* t)

(defvar *org-jira-rest-auth-info* nil)

(setq url-proxy-services
      '(("http" . "127.0.0.1:8081")))

(defvar *lock* (cc:semaphore-create 1))

(defun load-auth-info ()
  (let ((jira-pwd-file (expand-file-name "~/.jira-auth-info.el.gpg")))
    (if (file-regular-p jira-pwd-file)
	(load jira-pwd-file))))

(defun org-jira-rest--login ()
  (if (load-auth-info)
      (let ((enc (base64-encode-string
		  (concat jira-username ":" jira-password))))
	(setq *org-jira-rest-auth-info* (concat "Basic " enc)))
    (message "You must provide your login information.")))

(defun parse-rest-kv (key-value-list)
  (cl-loop for pos from 0 below (length rest)
	   when (evenp pos) collect
	   (list (nth pos rest)
		 (nth (+ 1 pos) rest))))

(cl-defun org-jira-rest--api-search (jql &optional &key
					 (start-at 0 start-at-p)
					 (max-results 50 max-results-p)
					 (validate-query t validate-query-p)
					 (fields)
					 (expand))
  "Searches for issues using JQL.

Sorting the 'jql' parameter is a full JQL expression, and
includes an ORDER BY clause.

The 'fields' param (which can be specified multiple times) gives
a list of fields to include in the response. This can be used to
retrieve a subset of fields. A particular field can be excluded
by prefixing it with a minus.

By default, only navigable (*navigable) 'fields' are returned in
this search resource. Note: the default is different in the
get-issue resource -- the default there all 'fields' (*all).

*all - include all fields
*navigable - include just navigable fields
summary,comment - include just the summary and comments
-description - include navigable fields except the
 description (the default is *navigable for search)
*all,-comment - include everything except comments

GET vs POST: If the JQL query is too large to be encoded as a
query param you should instead POST to this resource.

Expanding Issues in the Search Result: It is possible to expand
the issues returned by directly specifying the expansion on the
'expand' parameter passed in to this resources.

For instance, to expand the _changelog_ for all the issues on the
search result, it is neccesary to specify _changelog_ as one of
the values to 'expand'."
  (let ((query (list (cons "jql" jql))))
    (when start-at-p
      (push (cons "startAt" start-at) query))
    (when max-results-p
      (push (cons "maxResults" max-results) query))
    (when validate-query-p
      (push (cons "validateQuery" (if validate-query "true" "false")) query))
    (when fields
      (push (cons "fields" fields) query))
    (when expand
      (push (cons "expand" expand) query))
    (let ((url-request-method "POST")
	  (url-request-data (json-encode query))
	  (url-request-extra-headers `(("Content-Type" . "application/json")
				       ("Authorization" . ,*org-jira-rest-auth-info*))))
      (with-current-buffer (get-buffer-create "*ORG-JIRA-REST*")
	(erase-buffer)
	(insert "#+STARTUP: fold")
	(newline 2)
	(org-mode)
	(url-retrieve (concat jira-rest-endpoint "search") 'parse-api-search)
	(switch-to-buffer (current-buffer))))))


(cl-defun parse-api-search (status &optional cbargs)
  (with-current-buffer (current-buffer)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))
    ;; Don't try to read the buffer if the method was DELETE,
    ;; since we won't get a 'response' back.
    (let* ((data (buffer-substring (search-forward-regexp "^$")
				   (point-max)))
	   (issues (json-read-from-string data)))
      (mapcar (lambda (issue)
		(org-jira-rest--api-issue (apath issue :key) :expand '(comments)))
	      (cdr (assoc 'issues issues))))))


(cl-defun org-jira-rest--api-issue (id-or-key &optional &key
					      (start-at 0 start-at-p)
					      (max-results 50 max-results-p)
					      (validate-query t validate-query-p)
					      (fields)
					      (expand)
					      (method "GET"))
  (let ((query '()))
    (when start-at-p
      (push (cons "startAt" start-at) query))
    (when max-results-p
      (push (cons "maxResults" max-results) query))
    (when validate-query-p
      (push (cons "validateQuery" (if validate-query "true" "false")) query))
    (when fields
      (push (cons "fields" fields) query))
    (when expand
      (push (cons "expand" expand) query))
    (let ((url-request-method method)
	  (url-request-data (json-encode query))
	  (url-request-extra-headers `(("Authorization" . ,*org-jira-rest-auth-info*))))
      (url-retrieve (concat jira-rest-endpoint "issue" "/" id-or-key) 'parse-api-issue))))


(cl-defun org-jira-rest--parse-time (string)
  (let ((final))
    (dolist (func '(parse-time-string parse-iso8601-time-string) final)
      (ignore-errors
	(let* ((date-time (funcall func string))
	       (decoded (if (= (length date-time) 2)
			    (decode-time date-time)
			  date-time)))
	  (when decoded (setq final decoded)))))
    (if (and (integerp (nth 1 final))
	     (integerp (nth 2 final)))
	(format "<%04d-%02d-%02d %02d:%02d>"
		(nth 5 final) (nth 4 final) (nth 3 final)
		(nth 2 final) (nth 1 final))
      (format "<%04d-%02d-%02d>" (nth 5 final) (nth 4 final) (nth 3 final)))))


(cl-defun apath (alist &rest path)
  "Walk through alists by theirs keys and return its cdr"
  (reduce (lambda (result item) (cdr (assoc item result)))
	  path :initial-value alist :key (lambda (key) (intern (extract-method key)))))

(cl-defun extract-method (keywoard-name)
  "Convert keywoard-like argument to jira's json key"
  (let ((pattern-list (split-string (symbol-name keywoard-name) "[:-]" 1)))
    (mapconcat 'identity (cons
			  (car pattern-list)
			  (mapcar 'capitalize (cdr pattern-list))) "")))

(cl-defun parse-api-issue (status &optional cbargs)
  (with-current-buffer (current-buffer)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))
    ;; Don't try to read the buffer if the method was DELETE,
    ;; since we won't get a 'response' back.
    (let* ((data (buffer-substring (search-forward-regexp "^$")
				   (point-max)))
	   (issue (json-read-from-string data)))
      (with-current-buffer (get-buffer-create (format "*%s*" (apath issue :key)))
	(erase-buffer)
	(org-mode)
	(let ((fields (apath issue :fields)))
	  (insert (format "* %s - %s"
			  (apath issue :key)
			  (apath fields :summary)))
	  (newline)
	  (save-excursion
	    (org-insert-property-drawer)
	    (when (apath fields :duedate)
	      (org-set-property "duedate" (org-jira-rest--parse-time (apath fields :duedate))))
	    (org-set-property "created" (org-jira-rest--parse-time (apath fields :created)))
	    (org-set-property "assignee" (apath fields :assignee :email-address))
	    (org-set-property "reporter" (apath fields :reporter :email-address))
	    (org-set-property "status" (apath fields :status :name)))
	  (when (apath fields :description)
	    (newline)
	    (insert (format "%s" (apath fields :description)))
	    (newline))
	  (let ((comments (apath fields 'comment 'comments)))
	    (loop for comment being the elements of comments do
		  (progn
		    (insert (format "** Comment: %s - %s"
				    (apath comment :author :display-name)
				    (org-jira-rest--parse-time (apath comment :created))))
		    (newline)
		    (insert (format "%s\n\n" (apath comment :body)))))))
	(indent-region 0 (point-max))
	(cc:semaphore-with *lock*
	  (append-to-buffer (get-buffer-create "*ORG-JIRA-REST*") (point-min) (point-max))
	  (with-current-buffer (get-buffer-create "*ORG-JIRA-REST*")
	    (org-set-startup-visibility)
	    (redisplay)))))))



(provide 'org-jira-rest)
;;; jira-rest.el ends here
