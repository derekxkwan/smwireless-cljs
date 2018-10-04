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
(def progress-text (r/atom "socketMusic: wireless"))
(def info-text (r/atom "make sure phone is unmuted and volume is up"))
(def bg-colors {:default "#000000" :filt-noise "#dbcc00"})
(def event-texts {:default "" :filt-noise "filt-noise"})
(def bg-flash-dur 0.25)
(def info-flash-dur 1)
(def bg-callback nil)
(def info-callback nil)

(defn unload-callback [evt]
  (when (false? wakelock-enabled)
         (.disble nosleep)
         (set! wakelock-enabled false))
  (au/cleanup)
  )


(defn set-background-color [evt]
  (let [cur-color (get bg-colors evt)]
    (set! (-> js/document .-body .-style .-background) cur-color))
  )

(defn event-background-color [evt]
  (when (not (nil? bg-callback)) (.clearTimeout win bg-callback))
  (set-background-color evt)
  (set! bg-callback (js/setTimeout #(set-background-color :default)(* 1000 bg-flash-dur)))
  )

(defn event-info-text [evt]
  (when (not (nil? info-callback)) (.clearTimeout win info-callback))
  (reset! info-text (get event-texts evt))
  (set! info-callback (js/setTimeout #(reset! info-text (get event-texts :default)) (* 1000 info-flash-dur)))
  )

(defn event-vis [evt]
  (event-background-color evt)
  (event-info-text evt)
  )

(defn render-progress []
  [:span {:style {:text-align "center"} :id "progress-text"}
   @progress-text
   ])

(defn render-info []
  [:span {:style {:text-align "center"} :id "info-text"}
   @info-text
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
   [:div {:class "textbox"}
    (render-progress)
    ]
   [:div
    (render-button)
    ]
   [:div {:class "textbox"}
    (render-info)
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
           (let [delay-time (rand 3)]
             (au/play-noise-pattern delay-time)
             (js/setTimeout #(event-vis :filt-noise) (* 1000 delay-time))
             (println "noise_patterns delayed"))
           (do (au/play-noise-pattern 0)
               (event-vis :filt-noise)
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
