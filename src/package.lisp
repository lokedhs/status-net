(defpackage :status-net
  (:use :cl)
  (:documentation "GNU Social client")
  (:export #:*credentials*
           #:credentials
           #:post/content-html
           #:post/title
           #:post
           #:note
           #:comment
           #:post/published
           #:with-html-namespaces
           #:with-status-net-namespaces
           #:author
           #:author/uri
           #:author/name
           #:author/preferred-user-name
           #:author/display-name
           #:author/profile-info
           #:author/summary
           #:author/subscribers-url
           #:load-feed))

(defpackage :status-net-clim
  (:use :cl)
  (:documentation "CLIM client for status-net")
  (:export #:status-net-clim))
