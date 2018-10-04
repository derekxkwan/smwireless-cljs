(ns smwireless-cljs.routes
  (:require
    [bidi.bidi :as bidi]
    [hiccups.runtime]
    [macchiato.middleware.anti-forgery :as af]
    [macchiato.util.response :as r]
    [cljs.nodejs :as node])
  (:require-macros
    [hiccups.core :refer [html]]))

(def client-map (atom {}))
(def client-count (atom 0))
(def sio nil)

(defn provide-client-list []
  (map name (keys @client-map)))
  

(defn home [req res raise]
  (->
    [:html
     [:link {:rel "stylesheet" :href "css/style.css"}]
     [:body
      [:div {:id "app"}]
      [:script {:src "js/Tone.min.js"}]
      [:script {:src "js/NoSleep.min.js"}]
      [:script {:src "js/client.js"}]
       ]]
    (html)
    (r/ok)
    (r/content-type "text/html")
    (res)))

(defn not-found [req res raise]
  (-> (html
        [:html
         [:body
          [:h2 (:uri req) " was not found"]]])
      (r/not-found)
      (r/content-type "text/html")
      (res)))

(def routes
  ["/" {:get home}])


;;(match-route* [route path options]
(defn router [req res raise]
  ((:handler (bidi/match-route* routes (:uri req) req) not-found)
    req res raise))

(defn socket-connect-callbacks [socket]
    (.on socket "connection"
         (fn [client]
           (swap! client-count inc)
           (swap! client-map assoc (keyword (.-id client)) client)
           (.on client "disconnect"
                (fn []
                  (swap! client-map dissoc (keyword (.-id client)))
                  (swap! client-count dec)
                  ))
           ))
  )

(defn broadcast-one [address arg1]
  (.emit sio address arg1))

(defn broadcast-two [address arg1 arg2]
  (.emit sio address arg1 arg2))

(defn broadcast-three [address arg1 arg2 arg3]
  (.emit sio address arg1 arg2 arg3))

(defn target-one [target address arg1]
  (.emit (.to sio target) address arg1))

(defn target-two [target address arg1 arg2]
  (.emit (.to sio target) address arg1 arg2))

(defn target-three [target address arg1 arg2 arg3]
  (.emit (.tio sio target) address arg1 arg2 arg3))

(defn send-ws [target address args]
  (let [numargs (count args)
        newargs (into [address] args)]
    (if (= target "all")
      (cond
        (= numargs 1) (apply broadcast-one newargs)
        (= numargs 2) (apply broadcast-two newargs)
        (= numargs 3) (apply broadcast-three newargs)
        :else nil)
      (let [newargs2 (into [target] newargs)]
        (cond
          (= numargs 1) (apply target-one newargs2)
          (= numargs 2) (apply target-two newargs2)
          (= numargs 3) (apply target-three newargs2)
          :else nil))
      )
  ))

(defn start-ws [server]
  (let [io ((node/require "socket.io") server)]
    (socket-connect-callbacks io)

   (set! sio io)
    ))



