(in-package :status-net-mastodon)

;;; To request a new application id:
#+nil(drakma:http-request "https://mastodon.brussels/api/v1/apps"
                     :method :post
                     :parameters '(("redirect_uris" . "urn:ietf:wg:oauth:2.0:oob")
                                   ("client_name" . "status-net-lisp")
                                   ("scopes" . "read write follow")))
