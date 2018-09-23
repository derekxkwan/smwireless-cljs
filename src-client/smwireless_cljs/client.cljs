(ns smwireless-cljs.client
  (:require
    [cljsjs.socket-io]
    [smwireless-cljs.audio :as au]
    [mount.core :refer [defstate]]
    [reagent.core :as r]))

(defstate socket :start (js/io))

(def win js/window)
(def status-text (r/atom "press to load"))
(def progress-text (r/atom "socketMusic: wireless"))

(defn render-progress []
  [:span {:align "center" :id "progress-text"}
   @progress-text
   ])
   

(defn render-button []
  [:button
   {:on-click
    #(let [already-loaded? (not (= @status-text "press to load"))
           ctx-success? (au/init-audio win)]
       (when (and (not (nil? ctx-success?)) (not (already-loaded?)))
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

(defn init []
  (.on @socket "display" #(swap! progress-text conj (js->clj % :keywordize-keys true)))
  ;;(.on @socket "messages" #(reset! messages (js->clj % :keywordize-keys true)))
  (.on @socket "noisePatterns"
       (fn [delay?]
         (if (true? delay?)
           (js/setTimeOut
            #(au/play-noise-pattern)
            (rand 1000))
           (au/play-noise-pattern)
           ))
       )
  (mount-components))

(init)
