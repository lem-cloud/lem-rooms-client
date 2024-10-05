(defsystem "lem-rooms-client"
  :depends-on ("lem")
  :components ((:file "utils")
               (:file "config")
               (:file "http")
               (:file "sign-in")
               (:file "main")))
