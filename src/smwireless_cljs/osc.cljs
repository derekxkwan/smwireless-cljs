(ns smwireless-cljs.osc
  (:require
   [smwireless-cljs.routes :refer [send-ws provide-client-list]]
    [cljs.nodejs :as node])
  )


(def in-port 33333)
(def out-port 11111)

(def node-osc (node/require "node-osc"))

(def osc-client (node-osc.Client. "127.0.0.1" out-port))

(defn format-ws-args [method-name args]
  (into [method-name] args)
  )


(defn send-client-list []
  (.send osc-client "/clients" (apply array (provide-client-list))))

(defn display-msg [target msgs]
    (send-ws target "display" msgs)
    )
  

(defn server-router [msg rinfo]
  (let [address (first msg)
        args (vec (rest msg))]
    (cond
      (= address "/clients") (send-client-list)
      (= address "/display") (display-msg "all" args)
      (= address "/display_targeted") (display-msg (first args) (vec (rest args)))
      :else nil
      )))


(def osc-server
  (let [cur-server (node-osc.Server. in-port "0.0.0.0")]
    (.on cur-server "message" server-router)
    cur-server))


  
