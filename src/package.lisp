(defpackage :status-net
  (:use :cl)
  (:documentation "GNU Social client")
  (:export #:load-user
           #:timeline
           #:subscriptions
           #:*credentials*
           #:credentials))

(defpackage :status-net-clim
  (:use :cl)
  (:documentation "CLIM client for status-net")
  (:export #:status-net-clim))
