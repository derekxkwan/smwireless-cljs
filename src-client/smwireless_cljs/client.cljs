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
(def cur-timeouts (atom []))
(def status-text (r/atom "press to load"))
(def progress-text (r/atom "socketMusic: wireless"))
(def info-text (r/atom "make sure phone is unmuted and volume is up"))
(def bg-colors {:default "#000000" :filt-noise "#dbcc00" :telegraph "#5dafba"
                :modem-drone "#ff0055" :modem-clicks "#8f002f" :radio-player "#31c926"})
(def event-texts {:default "" :filt-noise "filt-noise" :telegraph "telegraph"
                  :modem-drone "modem-drone" :modem-clicks "modem-clicks" :radio-player "radio"})
(def bg-flash-dur 0.25)
(def info-flash-dur 1)
(def bg-callback nil)
(def info-callback nil)
(def gain-val (r/atom 1))


(defn clear-timeouts []
  (doall (map #(js/clearTimeout %) @cur-timeouts))
  (reset! cur-timeouts [])
  )

(defn sched-timeout [timeout-func delay-time]
  (clear-timeouts)
  (let [new-timeout (js/setTimeout timeout-func (* 1000 delay-time))]
    (swap! cur-timeouts conj new-timeout)
    ))

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

(defn flash-event-vis [evt]
  (event-background-color evt)
  (event-info-text evt)
  )

(defn set-event-vis [evt]
  (set-background-color evt)
  (reset! info-text (get event-texts evt))
  )

;; end piece/ cleanup

(defn end-piece []
  (set-event-vis :default)
  (when (false? wakelock-enabled)
         (.disble nosleep)
         (set! wakelock-enabled false))
  (au/cleanup)
  )

(defn unload-callback [evt]
  (end-piece)
  )


;; rendering stuff
(defn render-gain-slider []
  [:input {:type "range" :min 0 :max 1 :default-value @gain-val
           :class "gain-slider"
           :on-change #(let [tval (-> % .-target .-value)]
                         (au/set-master-gain-volume tval))
           :step 0.001}]
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
         (au/set-master-gain-volume @gain-val)
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
   [:div {:class "gui"}
    "(gain)" [:br]
    (render-gain-slider)
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
             (sched-timeout #(flash-event-vis :filt-noise) delay-time)
             (println "noise_patterns delayed"))
           (do (au/play-noise-pattern 0)
               (flash-event-vis :filt-noise)
               (println "noise_patterns not_delayed"))
           ))
       )

  (.on @socket "telegraph"
       (fn [play-flag]
         (if (= play-flag 1)
           (do (au/telegraph-play-rand-seq (+ 350 (rand-int 50)) (+ 0.06 (rand 0.09)))
               (set-event-vis :telegraph))
           (do (au/telegraph-stop)
               (set-event-vis :default))
         )
       )
       )

  (.on @socket "modem_drone"
       (fn [play-flag]
         (if (= play-flag 1)
           (do (set-event-vis :modem-drone)
               (au/modem-whine-play true))
           (do (set-event-vis :default)
               (au/modem-whine-play false))
           )
         )
       )

    (.on @socket "modem_clicks"
       (fn [play-flag]
         (if (= play-flag 1)
           (do (set-event-vis :modem-clicks)
               (au/modem-clicks-play true))
           (do (set-event-vis :default)
               (au/modem-clicks-play false))
           )
         )
       )
  
  (.on @socket "start_end"
       (fn [start-flag]
         (if (= start-flag 1)
           (do (.log js/console "start")
               (au/mute-main-out false)
               (au/set-master-gain-volume @gain-val)
               (au/start-transport true 0.1))
           (do (au/mute-main-out true)
               (au/cleanup)
               (reset! progress-text "end"))
           ))
       )

  (.on @socket "radio_player"
       (fn [bufnum delay-flag]
         (let [seqlen (+ 5 (rand-int 7))
               delay-sec (if (= delay-flag 1)
                           (+ 0.25 (rand 1.75))
                           0)
               ]
           (au/player-play-rand-seq delay-sec seqlen bufnum)
           (if (> delay-sec 0)
             (sched-timeout #(flash-event-vis :radio-player) delay-sec)
             (flash-event-vis :radio-player))
           )
         )
       )
  
  (.on @socket "main_gain"
       (fn [gain]
         (au/set-main-volume gain)))

  (.on @socket "main_mute"
       (fn [mute-flag]
         (au/mute-main-out (= 1 mute-flag))
         )
       )
 )

(defn init []
  (set! (.-onbeforeunloead win) unload-callback)
  (set-socket-callbacks)
   (mount-components))

(init)
