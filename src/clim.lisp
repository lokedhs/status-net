(in-package :status-net-clim)

(defclass activity-list-view (clim:view)
  ())

(clim:define-application-frame activity-list-frame ()
  ((messages :type list
             :initform nil
             :accessor activity-list-frame/messages))
  (:panes (activity-list :application
                         :default-view (make-instance 'activity-list-view)
                         :display-function 'display-activity-list)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default activity-list
                     bottom-adjuster
                     interaction-pane)))

(defun display-activity-list (frame stream)
  (let ((messages (activity-list-frame/messages frame)))
    (loop
      for msg in messages
      do (format stream "~a~%" msg))))

(define-activity-list-frame-command (load-feed :name "Load feed")
    ((url 'string))
  (let ((feed (status-net::parse-feed (status-net::send-request (status-net::find-atom-url-from-html url)))))
    (setf (activity-list-frame/messages clim:*application-frame*) feed)))

(defun status-net-clim ()
  (let ((frame (clim:make-application-frame 'activity-list-frame
                                            :width 700 :height 500
                                            :left 10 :top 10)))
    (clim:run-frame-top-level frame)))
