(in-package :status-net-mastodon)

(defun json-request (url &key (method :get) parameters)
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request url
                           :method method
                           :parameters parameters
                           :want-stream t
                           :external-format-in :utf-8)
    (declare (ignore content return-headers url-reply))
    (unwind-protect
         (progn
           (unless (= code 200)
             (error "HTTP error when requesting application id: ~a. Reason: ~a" code reason-string))
           (let ((result (yason:parse stream)))
             result))
      (when need-close
        (close stream)))))

(defun request-new-application-id ()
  (multiple-value-bind (content code return-headers url-reply stream need-close reason-string)
      (drakma:http-request "https://mastodon.brussels/api/v1/apps"
                           :method :post
                           :parameters '(("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                         ("client_name" . "status-net-lisp")
                                         ("scopes" . "read write follow")))
    (declare (ignore return-headers url-reply stream need-close))
    (unless (= code 200)
      (error "HTTP error when requesting application id: ~a. Reason: ~a" code reason-string))
    (let ((result (yason:parse (babel:octets-to-string content :encoding :utf-8))))
      (list (gethash "client_id" result)
            (gethash "client_secret" result)))))

(defclass credentials ()
  ((url   :type string
          :initarg :url)
   (token :type string
          :initarg :token)))

(defun login (url client-id client-secret username password)
  (let ((result (json-request (format nil "~aoauth/token" url)
                              :method :post
                              :parameters `(("client_id" . ,client-id)
                                            ("client_secret" . ,client-secret)
                                            ("grant_type" . "password")
                                            ("username" . ,username)
                                            ("password" . ,password)))))
    (make-instance 'credentials
                   :url url
                   :token (gethash "access_token" result))))
