(in-package :status-net)

(define-condition status-net-error (error)
  ((message :initarg :message
            :reader status-net-error/message))
  (:report (lambda (condition stream)
             (format stream "Status net error: ~a" (status-net-error/message condition))))
  (:documentation "Generic status-net error"))

(define-condition status-net-response-error (status-net-error)
  ((code :type integer
         :initarg :code
         :reader status-net-response-error/code
         :documentation "The HTTP status code from the server")
   (text :type string
         :initarg :text
         :reader status-net-response-error/text
         :documentation "The HTTP reason string"))
  (:report (lambda (condition stream)
             (format stream "Error from server: HTTP error ~a: ~a"
                     (status-net-response-error/code condition)
                     (status-net-response-error/text condition))))
  (:documentation "Error that is raised when the server responded with a HTTP error"))

(defclass credentials ()
  ((url      :type string
             :initform "https://quitter.se/"
             :initarg :url
             :reader credentials/url)
   (user     :type string
             :initform (error "~s required" :user)
             :initarg :user
             :reader credentials/user)
   (password :type string
             :initform (error "~s required" :password)
             :initarg :password
             :reader credentials/password)))

(defvar *credentials* nil
  "Default credentials")

#+nil(defclass user ()
  ((timeline-url      :type (or null string)
                      :initform nil
                      :initarg :timeline-url
                      :reader user/timeline-url)
   (subscriptions-url :type (or null string)
                      :initform nil
                      :initarg :subscriptions-url
                      :reader user/subscriptions-url)
   (favourites-url    :type (or null string)
                      :initform nil
                      :initarg :favourites-url
                      :reader user/favourites-url)
   (memberships-url   :type (or null string)
                      :initform nil
                      :initarg :memberships-url
                      :reader user/memberships-url)))
(defclass timestamp-node ()
  ((published    :type (or null string)
                 :reader post/published
                 :xpath "atom:published/text()")
   (updated      :type (or null string)
                 :xpath "atom:updated/text()"))
  (:metaclass atom-entity-class))

#+nil(defun update-slot-from-xpath (obj slot doc xpath)
  (with-status-net-namespaces
    (let ((timeline (xpath:evaluate xpath doc)))
      (unless (xpath:node-set-empty-p timeline)
        (setf (slot-value obj slot) (dom:node-value (xpath:first-node timeline)))))))

#+nil(defmethod initialize-instance :after ((user user) &key doc)
  (update-slot-from-xpath user 'timeline-url doc
                          "/app:service/app:workspace/app:collection[activity:verb='http://activitystrea.ms/schema/1.0/post']/@href")
  (update-slot-from-xpath user 'subscriptions-url doc
                          "/app:service/app:workspace/app:collection[activity:verb='http://activitystrea.ms/schema/1.0/follow']/@href")
  (update-slot-from-xpath user 'favourites-url doc
                          "/app:service/app:workspace/app:collection[activity:verb='http://activitystrea.ms/schema/1.0/favorite']/@href")
  (update-slot-from-xpath user 'memberships-url doc
                          "/app:service/app:workspace/app:collection[activity:verb='http://activitystrea.ms/schema/1.0/join']/@href"))

(defun fill-in-xpath-content (class-name doc)
  (with-status-net-namespaces
    (let* ((class (find-class class-name))
           (obj (make-instance class)))
      (loop
        for slot in (closer-mop:class-slots class)
        when (typep slot 'atom-entity-class-effective-slot-definition)
          do (let ((xpath (atom-slot/xpath slot)))
               (when xpath
                 (let ((result (xpath:evaluate xpath doc)))
                   (unless (xpath:node-set-empty-p result)
                     (setf (closer-mop:slot-value-using-class class obj slot) (dom:node-value (xpath:first-node result))))))))
      obj)))

(defclass feed (atom-entity)
  ()
  (:metaclass atom-entity-class))

(defclass post (timestamp-node)
  ((id           :type (or null string)
                 :xpath "atom:id/text()")
   (title        :type (or null string)
                 :reader post/title
                 :xpath "atom:title/text()")
   (conversation :type (or null string)
                 :xpath "ostatus:conversation/text()")
   (content-html :type (or null string)
                 :reader post/content-html
                 :xpath "atom:content[@type='html']/text()"))
  (:metaclass atom-entity-class))

(defclass note (post)
  ()
  (:metaclass atom-entity-class))

(defclass comment (post)
  ((in-reply-to-ref :xpath "thr:in-reply-to/@ref")
   (in-reply-to-url :xpath "thr:in-reply-to/@href"))
  (:metaclass atom-entity-class))

(defclass author (atom-entity)
  ((uri                 :xpath "atom:uri/text()"
                        :reader author/uri)
   (name                :xpath "atom:name/text()"
                        :reader author/name)
   (preferred-user-name :xpath "poco:preferredUsername/text()"
                        :reader author/preferred-user-name)
   (display-name        :xpath "poco:displayName/text()"
                        :reader author/display-name)
   (profile-info        :xpath "statusnet:profile_info/@local_id"
                        :reader author/profile-info)
   (summary             :xpath "atom:summary/text()"
                        :reader author/summary)
   (subscribers-url     :xpath "atom:followers/@url"
                        :reader author/subscribers-url))
  (:metaclass atom-entity-class))

(defmethod print-object ((obj post) stream)
  (print-unreadable-safely (id title) obj stream
    (format stream "ID ~s TITLE ~s" id title)))

(defun parse-feed-entry (node)
  (let ((object-type (value-by-xpath "activity:object-type/text()" node :default-value nil)))
    (if object-type
        (let ((name (string-case:string-case (object-type)
                      ("http://activitystrea.ms/schema/1.0/comment" 'comment)
                      ("http://activitystrea.ms/schema/1.0/note" 'note)
                      (t nil))))
          (when name
            (fill-in-xpath-content name node)))
        ;; ELSE: No object type, we should probably ignore it
        (progn
          (warn "No object type: ~s" node)
          nil))))

(defun parse-feed (doc)
  (with-status-net-namespaces
    (let ((result nil))
      (xpath:do-node-set (node (xpath:evaluate "atom:feed/atom:entry" doc))
        (let ((entry (parse-feed-entry node)))
          (when entry
            (push entry result))))
      (reverse result))))

(defvar *debug-requests* nil)

(defun display-stream-if-debug (stream)
  (when *debug-requests*
    (format *debug-io* "~&====== ERROR OUTPUT ======~%")
    (let ((input (flexi-streams:make-flexi-stream stream
                                                  :external-format :UTF8
                                                  :element-type 'character)))
      (loop
        for s = (read-line input nil nil)
        while s
        do (format *debug-io* "~a~%" s)))
    (format *debug-io* "~&====== END OF ERROR OUTPUT ======~%")))

(defun send-request (url &optional cred)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request url
                           :want-stream t
                           :force-binary t
                           :basic-authorization (if cred
                                                    (list (credentials/user cred)
                                                          (credentials/password cred))
                                                    nil))
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (display-stream-if-debug stream)
             (error 'status-net-response-error :code code :text reason-string))
           (let ((doc (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
             doc))
      (when need-close
        (close stream)))))

#+nil(defun load-user (user &key (cred *credentials*) url)
  (let* ((url-prefix (or url (credentials/url cred)))
         (doc (send-request (format nil "~aapi/statusnet/app/service/~a.xml" url-prefix user) cred)))
    (make-instance 'user :doc doc)))

(defun load-feed (url)
  (let ((doc (load-from-unknown-url url)))
    (with-status-net-namespaces
      (list
       ;; First element is the author (if existing)
       (let ((author-nodes (xpath:evaluate "/atom:feed/atom:author" doc)))
         (unless (xpath:node-set-empty-p author-nodes)
           (fill-in-xpath-content 'author (xpath:first-node author-nodes))))
       ;; Second element is the list of feed entries
       (parse-feed doc)))))

#+nil(defun timeline (user &key (cred *credentials*))
  (let ((doc (send-request (user/timeline-url user) cred)))
    (with-status-net-namespaces
      (xpath:map-node-set->list #'parse-post (xpath:evaluate "/atom:feed/atom:entry" doc)))))

#+nil(defun subscriptions (user &key (cred *credentials*))
  (send-request (user/subscriptions-url user) cred))

(defun find-atom-url-from-html (url)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request url :want-stream t)
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error 'status-net-response-error :code code :text reason-string))
           (let ((doc (closure-html:parse stream (cxml-dom:make-dom-builder))))
             (with-html-namespaces
               (let ((atom-url (value-by-xpath "//h:link[@type='application/atom+xml'][@rel='alternate']/@href" doc)))
                 atom-url))))
      (when need-close
        (close stream)))))

(defun parse-content-type (headers)
  (let ((v (assoc :content-type headers)))
    (if v
        (let* ((s (cdr v))
               (pos (position #\; s)))
          (if s
              (subseq s 0 pos)
              s))
        "binary/octet-stream")))

(defun load-from-unknown-url (url)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request url :want-stream t)
    (declare (ignore content url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error 'status-net-response-error :code code :text reason-string))
           (string-case:string-case ((parse-content-type return-headers))
             ("text/html" (let ((doc (closure-html:parse stream (cxml-dom:make-dom-builder))))
                            (with-html-namespaces
                              (let ((atom-url (value-by-xpath "//h:link[@type='application/atom+xml'][@rel='alternate']/@href" doc)))
                                (send-request atom-url)))))
             ("application/atom+xml" (cxml:parse-stream stream (cxml-dom:make-dom-builder)))
             (t (error 'status-net-error :message (format nil "Unknown content type for url: ~s" url)))))
      (when need-close
        (close stream)))))

(defun load-webfinger (url)
  (let* ((uri (puri:uri url))
         (scheme (puri:uri-scheme uri)))
    (unless (or (eq scheme :http)
                (eq scheme :https))
      (error "Unknown scheme: ~s" scheme))
    (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
        (drakma:http-request (format nil "~a://~a/.well-known/webfinger"
                                     (ecase scheme
                                       (:http "http")
                                       (:https "https"))
                                     (puri:uri-host uri))
                             :parameters `(("resource" . ,url)))
      (declare (ignore return-headers url-reply stream need-close))
      (unless (= code 200)
        (error "Error loading webfinger content. Code: ~a, reason: ~a" code reason-string))
      (let* ((json (yason:parse (babel:octets-to-string content :encoding :utf-8)))
             (subject (gethash "subject" json))
             (prefix "acct:"))
        (if (alexandria:starts-with-subseq prefix subject)
            (subseq subject (length prefix))
            ;; ELSE: The subject is not an acct link, search the aliases
            (loop
              for a in (gethash "aliases" json)
              when (alexandria:starts-with-subseq prefix a)
                return (subseq a (length prefix))))))))
