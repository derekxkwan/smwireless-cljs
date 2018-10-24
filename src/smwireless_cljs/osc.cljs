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

(defn sec-display-msg [target sec-num]
  (let [sec-str (cond
                  (= sec-num 1) "section one"
                  (= sec-num 2) "section two"
                  (= sec-num 3) "section three"
                  :else "socketMusic: wireless")]


    (send-ws target "display" [sec-str])
    )
  )

(defn start-end [start-flag]
  (send-ws "all" "start_end" [start-flag]))

(defn noise-patterns [target delay-flag]
  (send-ws target "noise_patterns" [delay-flag]))

(defn telegraph [play-flag]
  (send-ws "all" "telegraph" [play-flag]))

(defn modem-play [target play-flag]
  (send-ws target "modem" [play-flag]))

(defn modem-drone-play [target play-flag]
  (send-ws target "modem_drone" [play-flag]))

(defn modem-clicks-play [target play-flag]
  (send-ws target "modem_clicks" [play-flag]))

(defn radio-player-play [target bufnum delay?]
  (send-ws target "radio_player" [bufnum delay?])
  )

(defn main-gain-set [target gain]
  (send-ws target "main_gain" [gain])
  )

(defn main-mute [target mute-flag]
  (send-ws target "main_mute" [mute-flag])
  )

(defn server-router [msg rinfo]
  (let [address (first msg)
        args (vec (rest msg))]
    (cond
      (= address "/clients") (send-client-list)
      (= address "/display") (display-msg "all" args)
      (= address "/sec_display") (sec-display-msg "all" (first args))
      (= address "/display_targeted") (display-msg (first args) (vec (rest args)))
      (= address "/start_end") (start-end (first args))
      (= address "/noise_patterns") (noise-patterns (first args) (second args))
      (= address "/telegraph") (telegraph (first args))
      (= address "/modem") (modem-play (first args) (second args))
      (= address "/modem_drone") (modem-drone-play (first args) (second args))
      (= address "/modem_clicks") (modem-clicks-play (first args) (second args))
      (= address "/radio_player") (radio-player-play (first args) (second args)
                                                     (get args 2))
      (= address "/main_gain") (main-gain-set (first args) (second args))
      (= address "/main_mute") (main-mute (first args) (second args))

      :else nil
      )))


(def osc-server
  (let [cur-server (node-osc.Server. in-port "0.0.0.0")]
    (.on cur-server "message" server-router)
    cur-server))


  
