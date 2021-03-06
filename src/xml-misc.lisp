(in-package :status-net)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *status-net-namespaces* '(("app" "http://www.w3.org/2007/app")
                                          ("atom" "http://www.w3.org/2005/Atom")
                                          ("activity" "http://activitystrea.ms/spec/1.0/")
                                          ("thr" "http://purl.org/syndication/thread/1.0")
                                          ("georss" "http://www.georss.org/georss")
                                          ("media" "http://purl.org/syndication/atommedia")
                                          ("poco" "http://portablecontacts.net/spec/1.0")
                                          ("ostatus" "http://ostatus.org/schema/1.0")
                                          ("statusnet" "http://status.net/schema/api/1/")
                                          ("thr" "http://purl.org/syndication/thread/1.0")))
  (defparameter *html-namespace* "http://www.w3.org/1999/xhtml")
  (defparameter *html-namespaces* (list (list "h" *html-namespace*))))

(defmacro with-status-net-namespaces (&body body)
  `(xpath:with-namespaces ,*status-net-namespaces*
     ,@body))

(defmacro with-html-namespaces (&body body)
  `(xpath:with-namespaces ,*html-namespaces*
     ,@body))

(defmacro print-unreadable-safely ((&rest slots) object stream &body body)
  "A version of PRINT-UNREADABLE-OBJECT and WITH-SLOTS that is safe to use with unbound slots"
  (let ((object-copy (gensym "OBJECT"))
        (stream-copy (gensym "STREAM")))
    `(let ((,object-copy ,object)
           (,stream-copy ,stream))
       (symbol-macrolet ,(mapcar (lambda (slot-name)
                                   `(,slot-name (if (and (slot-exists-p ,object-copy ',slot-name)
                                                         (slot-boundp ,object-copy ',slot-name))
                                                    (slot-value ,object-copy ',slot-name)
                                                    :not-bound)))
                                 slots)
         (print-unreadable-object (,object-copy ,stream-copy :type t :identity nil)
           ,@body)))))

(defun value-by-xpath (expression node &key (default-value nil default-value-assigned-p))
  (let ((result (xpath:evaluate expression node)))
    (if (xpath:node-set-empty-p result)
        (if default-value-assigned-p
            default-value
            (error "No value found for expression: ~s" expression))
        (dom:node-value (xpath:first-node result)))))

(defun element-by-xpath (expression node &key (default-value nil default-value-assigned-p))
  (let ((result (xpath:evaluate expression node)))
    (if (xpath:node-set-empty-p result)
        (if default-value-assigned-p
            default-value
            (error "No value found for expression: ~s" expression))
        (xpath:first-node result))))

(defun debug-print-dom (doc &optional (stream *standard-output*))
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc)
  nil)
