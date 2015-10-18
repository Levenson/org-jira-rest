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


(defun apath (alist &rest path)
  "Walk through alists 'alist' by the path 'path' return its
cdr."
  (reduce (lambda (result item) (cdr (assoc item result))) path
	  :initial-value alist
	  :key (lambda (key) (intern (jira-keyword key)))))

(defun jira-keyword (keyword)
  "Convert keyword-like argument to Jira's json key."
  (let ((pattern-list (split-string (symbol-name keyword) "[:-]" 1)))
    (mapconcat 'identity (cons
			  (car pattern-list)
			  (mapcar 'capitalize (cdr pattern-list))) "")))

(defun jira-key-value-pair (keyword value)
  (cons (jira-keyword keyword)
	(case value
	  ((t) "true")
	  ((nil) (unless (member keyword '(fields expand)) "false"))
	  (t value))))

(defun jira-rest--function-arguments ()
  "Look through backtrace to extract function arguments"
  (let* ((frame-index 1))
    (while (not (car (backtrace-frame frame-index)))
      (incf frame-index))
    (let ((locals (backtrace--locals (incf frame-index))))
      (loop for key-value in locals
	    ;; We need to ignore -p variables
	    when (and (not (string-suffix-p "-p" (symbol-name (car key-value))))
		      (cdr key-value))
	    collect (jira-key-value-pair (car key-value) (cdr key-value))))))

(cl-defun org-jira-rest--api-search (jql &optional &key
					 (start-at 0)
					 (max-results 50)
					 (validate-query t)
					 (fields)
					 (expand))
  "Searches for issues using JQL."
  (let ((query (jira-rest--function-arguments)))
    (push (cons "jql" jql) query)
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


(cl-defun org-jira-rest--api-issue (id-or-key &optional &key
					      (start-at 0)
					      (max-results 50)
					      (validate-query t)
					      (fields)
					      (expand))
  (let* ((query (jira-rest--function-arguments))
	 (url-request-method "GET")
	 (url-request-data (json-encode query))
	 (url-request-extra-headers `(("Authorization" . ,*org-jira-rest-auth-info*))))
    (url-retrieve (concat jira-rest-endpoint "issue" "/" id-or-key) 'parse-api-issue)))


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
	      (apath issues :issues)))))


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


(cl-defun parse-api-issue (status &optional cbargs)
  (with-current-buffer (current-buffer)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))
    ;; Don't try to read the buffer if the method was DELETE,
    ;; since we won't get a 'response' back.
    (let* ((data (buffer-substring (search-forward-regexp "^$")
				   (point-max)))
	   (content-encoding
	    (or (reduce (lambda (result p)
			  (when (string-prefix-p "charset=" p)
			    (intern (downcase (cadr (split-string p "="))))))
			(split-string
			 (buffer-local-value 'url-http-content-type (current-buffer)) ";"))
		'utf-8))
	   (issue (json-read-from-string data)))
      (with-current-buffer (get-buffer-create (format "*%s*" (apath issue :key)))
	(erase-buffer)
	(org-mode)
	(let ((fields (apath issue :fields)))
	  (insert (decode-coding-string (format "* %s - %s"
						(apath issue :key)
						(apath fields :summary))
					content-encoding))
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
	    (insert (decode-coding-string (apath fields :description) content-encoding))
	    (newline))
	  (let ((comments (apath fields 'comment 'comments)))
	    (loop for comment being the elements of comments do
		  (progn
		    (insert (decode-coding-string
			     (format "** Comment: %s - %s"
				     (apath comment :author :display-name)
				     (org-jira-rest--parse-time (apath comment :created)))
			     content-encoding))
		    (newline)
		    (insert (decode-coding-string (apath comment :body)
						  content-encoding))
		    (newline)
		    (newline)))))
	(indent-region 0 (point-max))
	(cc:semaphore-with *lock*
	  (append-to-buffer (get-buffer-create "*ORG-JIRA-REST*") (point-min) (point-max))
	  (with-current-buffer (get-buffer-create "*ORG-JIRA-REST*")
	    (org-set-startup-visibility)
	    (redisplay)))))))



(provide 'org-jira-rest)
;;; jira-rest.el ends here
