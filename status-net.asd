(asdf:defsystem #:status-net
  :description "Experimental GNU Social client"
  :license "Apache"
  :serial t
  :depends-on (:drakma
               :cxml
               :xpath
               :mcclim)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "xml-misc")
                                     (:file "status-net")
                                     (:file "clim")))))
