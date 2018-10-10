(ns smwireless-cljs.core
  (:require

    [smwireless-cljs.middleware :refer [wrap-defaults]]
    [smwireless-cljs.routes :refer [router start-ws]]
    [smwireless-cljs.osc :as osc]
    [macchiato.server :as http]
    [macchiato.session.memory :as mem]
    [mount.core :as mount :refer [defstate]]
    [taoensso.timbre :refer-macros [log trace debug info warn error fatal]]))

(defn app []
  (mount/start)
  (let [host "0.0.0.0"
        port 8080]
    (->
      (http/start
        {:handler    (wrap-defaults router)
         :host       host
         :port       port
         :on-success #(info "smwireless started on" host ":" port)})
      (start-ws))))

(defn start-workers [os cluster]
  (dotimes [_ (-> os .cpus .-length)]
    (.fork cluster))
  (.on cluster "exit"
       (fn [worker code signal]
         (info "worker terminated" (-> worker .-process .-pid)))))

(defn main [& args]
  (let [os      (js/require "os")
        cluster (js/require "cluster")]
    (if (.-isMaster cluster)
      (start-workers os cluster)
      (app))))
