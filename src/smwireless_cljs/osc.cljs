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
    (send-ws target "display" msgs))

(defn start-end [start-flag]
  (send-ws "all" "start_end" [start-flag]))

(defn noise-patterns [target delay-flag]
  (send-ws target "noise_patterns" [delay-flag]))

(defn telegraph [play-flag]
  (send-ws "all" "telegraph" [play-flag]))

(defn server-router [msg rinfo]
  (let [address (first msg)
        args (vec (rest msg))]
    (cond
      (= address "/clients") (send-client-list)
      (= address "/display") (display-msg "all" args)
      (= address "/display_targeted") (display-msg (first args) (vec (rest args)))
      (= address "/start_end") (start-end (first args))
      (= address "/noise_patterns") (noise-patterns (first args) (second args))
      (= address "/telegraph") (telegraph play-flag)
      :else nil
      )))


(def osc-server
  (let [cur-server (node-osc.Server. in-port "0.0.0.0")]
    (.on cur-server "message" server-router)
    cur-server))


  
