(in-package :status-net-clim)

(defclass activity-list-view (clim:view)
  ())

(defun display-activity-list (frame stream)
  (declare (ignore frame))
  (format stream "Activity list should be here"))

(clim:define-application-frame activity-list-frame ()
  ()
  (:panes (activity-list :application
                         :default-view (make-instance 'activity-list-view)
                         :display-function 'display-activity-list)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interaction-pane :interactor))
  (:layouts (default activity-list
                     bottom-adjuster
                     interaction-pane)))

(defun status-net-clim ()
  (let ((frame (clim:make-application-frame 'activity-list-frame
                                            :width 700 :height 500
                                            :left 10 :top 10)))
    (clim:run-frame-top-level frame)))
