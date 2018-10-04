(ns smwireless-cljs.audio
  (:require
    [cljsjs.socket-io]
    ;;[mount.core :refer [defstate]]
    ))

(def Tone js/Tone)
(def ctx nil)
(def main-out nil)
(def max-db 0)
(def loops [])
(def noise-node {:src nil :filter nil :env nil :gain nil})
(def telegraph-node {:src nil :env nil :gain nil :part nil})
(def modem-parts {:clicks nil})
(def transport? false)

(defn start-transport [start? delay-time]
  (.cancel (.-Transport Tone))
  (let [delay-str (str "+" delay-time)]
     (cond
      (and (true? start?) (false? transport?)) (do (.start (.-Transport Tone) delay-str)
                                                   (set! transport? true))
      (and (not (true? start?)) (true? transport?)) (do (.stop (.-Transport Tone) delay-str)
                                                        (set! transport? false))
      :else nil
      )
    )
  )



(defn set-main-volume [volume]
  (set! (.-volume main-out) volume))

(defn mute-main-out [mute?]
  (if (true? mute?)
    (do (set! (.-mute main-out) true)
        (.log js/console "muted"))
    (do (set! (.-mute main-out) false)
        (.log js/console "unmuted"))
    )
  )

(defn clip-val [val lo hi]
  (if (< val lo) lo
      (if (> val hi) hi
          val))
  )

(defn rand-seq [len lo hi]
  (let [rng (Math.abs (- hi lo))]
   (vec (repeatedly len #(+ lo (rand rng))))
   ))

(defn grain-round [ipt grain]
  (* (Math.round (/ ipt grain)) grain)
  )

(defn grain-rand [maxval grain]
  (grain-round (rand maxval) grain)
  )

;; with grain
(defn grain-rand-seq [len lo hi grain]
  (let [rng (Math.abs (- hi lo))]
    (vec (repeatedly len #(+ lo (grain-rand rng grain))))
    ))


;; generate time sequence from durational sequence
(defn dur-to-time-seq [dur-seq]
  ((comp (partial into [0]) (partial reductions +) butlast) dur-seq)
  )
  

;; telegraph stuff

(defn telegraph-init [freq vol]
  (let [osc-param (js-obj "type" "sine" "frequency" freq "detune" 0)
        cur-src (new Tone.Oscillator. osc-param)
        cur-env (new Tone.AmplitudeEnvelope. 0.01 0.0 1.0 0.01)
        cur-gain (new Tone.Gain. vol)]
    (.start cur-src)
    (.sync cur-src)
    (.chain cur-src cur-env cur-gain main-out)
    (set! telegraph-node
          {:src cur-src
           :env cur-env
           :gain cur-gain})
    (.log js/console "telegraph init")
    )
  )



(defn telegraph-gen-rand-seq [len base-dur]
  ;;chance-seq - use to determine pattern
  ;; 0 - rest, 1 - short, 2-long 
  ;; rest has duration of short
  ;; long is twice as long as short
  (let [chance-seq (rand-seq len 0 100)
        patt-seq (mapv #(cond (< % 15) 0 (< % 70) 1 :else 2) chance-seq)
        dur-seq (mapv #(if (<= % 1) base-dur (* 2 base-dur)) patt-seq)
        vel-seq (mapv #(if (< % 1) 0 1) patt-seq)
        time-seq (dur-to-time-seq dur-seq)
        evts (apply array (mapv #(js-obj "time" %1 "dur" %2 "vel" %3) time-seq dur-seq vel-seq))
        total-len (reduce + dur-seq)]
    {:total-len total-len :evts evts}
        
    )
  )

(defn telegraph-play-rand-seq [len base-dur]
  (let [rand-seq-info (telegraph-gen-rand-seq len base-dur)
        evts (get rand-seq-info :evts)
        total-len (get rand-seq-info :total-len)
        cur-callback (fn [time val]
                         (.triggerAttackRelease noise-env (.-dur val) time (.-vel val))
                       )
        part-params (js-obj "callback" cur-callback "loop" true "loopEnd" total-len "events" evts)
        cur-part (new Tone.Part. part-params)
        ]
    (when (nil? (get telegraph-node :part))
      (.start cur-part)
      (set! telegraph-node (assoc telegraph-node :part cur-part))
      )
  )
 )

(defn telegraph-stop []
  (let [tel-part (get telegraph-node :part)]
    (when (not (nil? tel-part))
      (.stop tel-part)
      (set! telegraph-node (assoc telegraph-node :part nil))
      )
    )
  )

;; noise stuff


(defn noise-init []
  (let [noise-param (js-obj "playbackRate" 1 "volume" -6 "type" "brown")
        filt-param (js-obj "frequency" 200 "type" "bandpass" "gain" 0 "Q" 5)
        cur-src (new Tone.Noise. noise-param)
        cur-filt (new Tone.Filter. filt-param)
        cur-env (new Tone.AmplitudeEnvelope. 0.01 0.0 1.0 0.001)
        cur-gain (new Tone.Gain. 1)]
    (.start cur-src)
    (.sync cur-src)
    (.chain cur-src cur-filt cur-env cur-gain main-out)
    (set! noise-node
          {:src cur-src
           :filter cur-filt
           :env cur-env
           :gain cur-gain}
          )
    (.log js/console "noise-init")
    )
  )


;; modem clicks stuff

(defn modem-clicks-play [freq subdiv]
  (let [cur-filt (get noise-node :filter)]
    (set! (-> cur-filt .-Q .-value) 10)
    (set! (-> cur-filt .-frequency .-value) freq)
    )
  (let [cur-env (get noise-node :env)
        loop-dur (/ 4.0 subdiv)
        cur-callback (fn [time]
                       (.triggerAttackRelease cur-env modem-click-dur time 1))
        loop-params (js-obj "callback" cur-callback "interval" loop-dur)
        cur-loop (new Tone.Loop. loop-params)]
    (when (not (nil? (get modem-parts :clicks)))
      (.start cur-loop)
      (set! modem-parts (assoc modem-parts :clicks cur-loop))
      )
    )
        
  )

(defn modem-clicks-stop []
  (let [clicks-part (get modem-parts :clicks)]
    (when (not (nil? clicks-part))
      (.stop clicks-part)
      (set! modem-parts (assoc modem-parts :clicks nil))

      )
    )
  )

;; filtered noise stuff

(defn filt-noise-gen-rand-seq [len freq-bds q-bds time-bds]
  (let [freq-lo (get freq-bds :lo)
        freq-hi (get freq-bds :hi)
        q-lo (get q-bds :lo)
        q-hi (get q-bds :hi)
        time-lo (get time-bds :lo)
        time-hi (get time-bds :hi)
        freq-seq (grain-rand-seq len freq-lo freq-hi 0.1)
        q-seq (grain-rand-seq len q-lo q-hi 0.1)
        dur-seq  (grain-rand-seq len time-lo time-hi 0.1)
        time-seq (dur-to-time-seq dur-seq)
        ]
    ;;(.log js/console (apply array time-seq) (apply array dur-seq))
    (apply array
           (mapv #(js-obj "time" %1 "freq" %2 "q" %3 "dur" %4) time-seq freq-seq q-seq dur-seq))
    ))

(defn filt-noise-play-rand-seq [delay-sec len freq-bds q-bds time-bds]
  (let [delay-str (str "+" delay-sec)
        gen-seq (filt-noise-gen-rand-seq
                 len freq-bds q-bds time-bds)
        noise-filt (get noise-node :filter) 
        noise-env (get noise-node :env)
        cur-callback (fn [time val]
                       (let [cur-vel 1]
                         (.setValueAtTime (.-Q noise-filt) (.-q val) time)
                         (.setValueAtTime (.-frequency noise-filt) (.-freq val) time)
                         (.triggerAttackRelease noise-env (.-dur val) time cur-vel)
                         )
                       )
       cur-part (Tone.Part. cur-callback gen-seq)]
    (if (> delay-sec 0)
      (.start cur-part delay-str)
      (.start cur-part))

    ))

(defn play-noise-pattern [delay-sec]
  (.cancel (.-Transport Tone))
  (filt-noise-play-rand-seq delay-sec (+ 2 (rand-int 5))
   {:lo 500 :hi 2000} {:lo 2 :hi 10} {:lo 0.1 :hi 0.5})
  )
  

(defn master-init []
  (let [cur-master (.-Master Tone)]
    (set! main-out cur-master)
    (mute-main-out true)
    (set-main-volume max-db)
  ))


(defn get-current-time []
  (.-currentTime ctx))


(defn stop-loops []
  (telegraph-stop)
  (modem-clicks-stop)
  )

(defn cleanup []
    ;; clear all callbacks, set main gain to 0
  (mute-main-out true)
  (start-transport false 0.1)
  ) 


(defn init-audio [win]
  (when (nil? ctx)
    (.log js/console "loading context")
    (set! ctx (if (.-AudioContext win) (win.AudioContext.) (win.webkitAudioContext.)))
    (if (nil? ctx) (js/alert "context not available") (.setContext Tone ctx))
    (set! ctx true)
    (master-init)
    (start-transport true 0.1)
    (noise-init)
    (telegraph-init (+ 100 (rand 1000)) 1)
    ;;(doall (map osc-create (keys oscs)))
    ;;(osc-connect :carrier true nil nil)
  ;;(println oscs)
    )

  (not (nil? ctx))
  )
  
