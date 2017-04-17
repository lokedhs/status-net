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
                                          ("statusnet" "http://status.net/schema/api/1/"))))

(defmacro with-status-net-namespaces (&body body)
  `(xpath:with-namespaces ,*status-net-namespaces*
     ,@body))
