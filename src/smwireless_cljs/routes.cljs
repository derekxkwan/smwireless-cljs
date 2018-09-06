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
(def socket-io nil)

(defn provide-client-list []
  (map name (keys @client-map)))
  

(defn home [req res raise]
  (->
    [:html
     [:body
      [:div {:id "app"}]
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

(defn send-ws [target args]
  (if (= target "all")
    (apply (-> socket-io .-sockets .emit) args)
    (let [target-socket (get @client-map target)]
      (apply (-> socket-io (.to target-socket) .emit) args)
      )))
  

(defn start-ws [server]
  (let [io ((node/require "socket.io") server)]
    (socket-connect-callbacks io)
    (set! socket-io io)
    ))



