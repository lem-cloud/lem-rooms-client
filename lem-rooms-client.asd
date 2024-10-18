(defsystem "lem-rooms-client"
  :depends-on ("lem"
               "jsonrpc"
               "jsonrpc/transport/websocket"
               "frugal-uuid"
               "crdt")
  :components ((:file "utils")
               (:file "config")
               (:file "http")
               (:file "sign-in")
               (:file "cursor")
               (:file "main")))
