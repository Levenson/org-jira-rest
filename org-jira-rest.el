;;; Code:

(require 'cl)
(require 'org)
(require 'json)

(defvar *debug* t)

(defvar *org-jira-rest-auth-info* nil)

(setq url-proxy-services
      '(("http" . "127.0.0.1:8081")))


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
      (url-retrieve (concat jira-rest-endpoint "search") 'parse-api-search))))


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
		(org-jira-rest--api-issue (apath issue 'key) :expand '(comments)))
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
  (reduce (lambda (result item)
	    (if result
		(cdr (assoc item result))
	      (cdr (assoc item alist))))
	  path :initial-value '()))

(cl-defun parse-api-issue (status &optional cbargs)
  (with-current-buffer (current-buffer)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))
    ;; Don't try to read the buffer if the method was DELETE,
    ;; since we won't get a 'response' back.
    (let* ((data (buffer-substring (search-forward-regexp "^$")
				   (point-max)))
	   (issue (json-read-from-string data)))
      (ignore-errors
	(kill-buffer (format "*ORG-JIRA-REST--%s*" (apath issue 'key))))
      (with-current-buffer (get-buffer-create (format "*ORG-JIRA-REST--%s*" (apath issue 'key)))
	(org-mode)
	(let ((fields (apath issue 'fields)))
	  (insert (format "* %s - %s\n"
			  (apath issue 'key)
			  (apath fields 'summary)))
	  (save-excursion
	    (org-insert-property-drawer)
	    (when (apath fields 'duedate)
	      (org-set-property "duedate" (org-jira-rest--parse-time (apath fields 'duedate))))
	    (org-set-property "created" (org-jira-rest--parse-time (apath fields 'created)))
	    (org-set-property "assignee" (apath fields 'assignee 'emailAddress))
	    (org-set-property "reporter" (apath fields 'reporter 'emailAddress))
	    (org-set-property "status" (apath fields 'status 'name)))
	  (insert (format "\n%s\n\n" (apath fields 'description)))
	  (let ((comments (apath fields 'comment 'comments)))
	    (loop for comment being the elements of comments do
		  (progn
		    (insert (format "** Comment: %s - %s \n"
				    (apath comment 'author 'displayName)
				    (org-jira-rest--parse-time (apath comment 'created))))
		    (insert (format "%s\n\n" (apath comment 'body)))))))
	(fill-region 0 (point-max))
	(indent-region 0 (point-max))))))

;; (cl-defun merge ()
;;   (ignore-errors
;;     (kill-buffer "*ORG-JIRA-REST*"))
;;   (with-current-buffer (get-buffer-create "*ORG-JIRA-REST*")
;;     ))


(provide 'org-jira-rest)
;;; jira-rest.el ends here
