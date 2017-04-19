(in-package :status-net-clim)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *link-colour* (clim:make-rgb-color 0.0 0.0 1.0))

(defclass user-info-view (clim:view)
  ())

(defclass activity-list-view (clim:view)
  ())

(defclass user-ref ()
  ())

(defgeneric user-ref/url (user-ref)
  (:documentation "Returns the URL for the given user"))

(defclass text-link ()
  ((content :initarg :content
            :reader text-link/content)
   (href    :initarg :href
            :reader text-link/href)
   (title   :initarg :title
            :reader text-link/title)))

(defclass mention-link (text-link user-ref)
  ())

(defmethod user-ref/url ((ref mention-link))
  (text-link/href ref))

(clim:define-presentation-method clim:present (obj (type text-link) stream (view t) &key)
  (clim:with-drawing-options (stream :ink *link-colour*)
    (present-node-list (text-link/content obj) stream)))

(defun resolve-class-from-style (style)
  (let ((styles (split-sequence:split-sequence #\Space style)))
    (if (member "mention" styles :test #'equal)
        'mention-link
        'text-link)))

(defun parse-link (node)
  (let ((href (dom:get-attribute node "href")))
    (make-instance (resolve-class-from-style (dom:get-attribute node "class"))
                   :content (dom:child-nodes node)
                   :href href
                   :title (nil-if-empty (dom:get-attribute node "title")))))

(defun present-element (node stream)
  (labels ((present-element-bold ()
             (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
               (present-node-list (dom:child-nodes node) stream)))
           (present-element-italics ()
             (clim:with-text-style (stream (clim:make-text-style nil :italic nil))
               (present-node-list (dom:child-nodes node) stream))))
    (let ((name (dom:node-name node)))
      (string-case:string-case (name)
        ("b" (present-element-bold))
        ("i" (present-element-italics))
        ("a" (present-to-stream (parse-link node) stream))
        (t
         (present-node-list (dom:child-nodes node) stream))))))

(defun present-node (node stream)
  (cond ((dom:text-node-p node)
         (princ (dom:node-value node) stream))
        ((dom:element-p node)
         (present-element node stream))))

(defun present-node-list (nodes stream)
  (loop
    for node across nodes
    do (present-node node stream)))

(defun present-html-string (s stream)
  (let ((doc (closure-html:parse s (cxml-dom:make-dom-builder))))
    (status-net:with-html-namespaces
      (let ((body (xpath:first-node (xpath:evaluate "/h:html/h:body" doc))))
        (present-node-list (dom:child-nodes body) stream)))))

(clim:define-presentation-method clim:present (obj (type status-net:post) stream (view t) &key)
  (clim:with-text-style (stream (clim:make-text-style nil :bold nil))
    (format stream "~a" (status-net:post/title obj)))
  (format stream " ~a~%" (status-net:post/published obj))
  (present-html-string (status-net:post/content-html obj) stream))

(defun display-activity-list (frame stream)
  (let ((messages (activity-list-frame/messages frame)))
    (loop
      for msg in messages
      do (present-to-stream msg stream)
      do (format stream "~%"))))

(defun display-user-info (frame stream)
  (let ((user (activity-list-frame/current-user frame)))
    (when user
      (clim:with-text-style (stream (clim:make-text-style nil nil 18))
        (format stream "~a" (status-net:author/display-name user)))
      (format stream "~%~a~%~a"
              (status-net:author/name user)
              (status-net:author/uri user)))))

(clim:define-application-frame activity-list-frame ()
  ((messages :type list
             :initform nil
             :accessor activity-list-frame/messages)
   (current-user :initform nil
                 :accessor activity-list-frame/current-user))
  (:panes (activity-list :application
                         :default-view (make-instance 'activity-list-view)
                         :display-function 'display-activity-list)
          (user-info :application
                     :default-view (make-instance 'user-info-view)
                     :display-function 'display-user-info)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default (clim:horizontally ()
                       (3/10 user-info)
                       (7/10 activity-list))
                     bottom-adjuster
                     interaction-pane)))

(defun show-feed-url (url)
  (destructuring-bind (user feed)
      (status-net:load-feed url)
    (setf (activity-list-frame/current-user clim:*application-frame*) user)
    (setf (activity-list-frame/messages clim:*application-frame*) feed)))

(define-activity-list-frame-command (show-feed-from-url :name "Load feed from URL")
    ((url 'string))
  (handler-case
      (show-feed-url url)
    (error (condition)
      (let ((interactor (clim:find-pane-named clim:*application-frame* 'interaction-pane)))
        (format interactor "Error loading feed: ~a" condition)))))

(define-activity-list-frame-command (show-user :name "Load user feed")
    ((user 'user-ref))
  (show-feed-url (user-ref/url user)))

(clim:define-presentation-to-command-translator select-user
    (user-ref show-user activity-list-frame)
    (obj)
  (list obj))

(defun status-net-clim ()
  (let ((frame (clim:make-application-frame 'activity-list-frame
                                            :width 700 :height 500
                                            :left 10 :top 10)))
    (clim:run-frame-top-level frame)))
