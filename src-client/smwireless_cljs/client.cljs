(ns smwireless-cljs.client
  (:require
    [cljsjs.socket-io]
    [smwireless-cljs.audio :as au]
    [mount.core :refer [defstate]]
    [reagent.core :as r]))

(defstate socket :start (js/io))
(defonce nosleep (js/NoSleep.))
(defonce wakelock-enabled false)

(def win js/window)
(def status-text (r/atom "press to load"))
(def progress-text (r/atom "socketMusic: wireless (make sure phone is unmuted and volume is up"))
(def bg-colors
  ["#ffffff" "#331177" "#aa0000" "#117733"])

(defn unload-callback [evt]
  (when (false? wakelock-enabled)
         (.disble nosleep)
         (set! wakelock-enabled false))
  (au/cleanup)
  )


(defn set-background-color [section]
  (let [cur-color (get bg-colors section)]
    (set! (-> js/document .-body .-style .-background) cur-color))
  )

(defn render-progress []
  [:span {:style {:text-align "center"} :id "progress-text"}
   @progress-text
   ])
   

(defn render-button []
  [:button
   {:id "load-button"
    :on-click
    #(let [already-loaded? (not (= @status-text "press to load"))
           ctx-success? (au/init-audio win)]
       (when (false? wakelock-enabled)
         (.enable nosleep)
         (set! wakelock-enabled true)
         )
       (when (and (not (nil? ctx-success?)) (not already-loaded?))
         (reset! status-text "loaded")))
    }
    @status-text ])
  

(defn home-page []
  [:div
   [:div
    (render-progress)
    ]
   [:div
    (render-button)
    ]
   ]
  )

(defn mount-components []
  (r/render-component [#'home-page] (.getElementById js/document "app")))

(defn set-socket-callbacks []
  (.on @socket "display" #(reset! progress-text %))
  ;;(.on @socket "messages" #(reset! messages (js->clj % :keywordize-keys true)))
  (.on @socket "noise_patterns"
       (fn [delay-flag]

         (if (= delay-flag 1)
           (do (au/play-noise-pattern (rand 3))
               (println "noise_patterns delayed"))
           (do (au/play-noise-pattern 0)
               (println "noise_patterns not_delayed"))
           ))
       )
  (.on @socket "start_end"
       (fn [start-flag]
         (if (= start-flag 1)
           (do (.log js/console "start")
               (au/mute-main-out false)
               (reset! progress-text "section 1"))
           (do (au/cleanup)
               (reset! progress-text "end"))
           ))
       )
         
 )

(defn init []
  (set! (.-onbeforeunloead win) unload-callback)
  (set-socket-callbacks)
   (mount-components))

(init)
