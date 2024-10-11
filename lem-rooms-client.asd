(defsystem "lem-rooms-client"
  :depends-on ("lem"
               "jsonrpc/transport/websocket"
               "frugal-uuid"
               "crdt")
  :components ((:file "utils")
               (:file "config")
               (:file "http")
               (:file "sign-in")
               (:file "main")))
