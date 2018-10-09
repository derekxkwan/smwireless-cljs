(ns smwireless-cljs.audio
  (:require
    [cljsjs.socket-io]
    ;;[mount.core :refer [defstate]]
    ))

(def Tone js/Tone)
(def ctx nil)
(def main-out nil)
(def master-gain nil)
(def max-db 0)
(def loops [])
(def noise-node {:src nil :filter nil :env nil :gain nil})
(def telegraph-node {:src nil :env nil :gain nil :part nil})
(def modem-whine-node {:src nil :filters [] :scalers [] :env nil :gain nil :is-playing false})
(def player-nodes {:srcs [] :gain nil})
(def sound-paths ["res/es-rad.wav", "res/mul-rad-noise.wav", "res/sing-rad1.wav"])
(def modem-parts {:clicks nil})
(def modem-click-param {:dur 0.025 :freq (+ 500 (rand 500)) :subdiv (+ 2 (rand-int 7))})
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


(defn set-adsr [cur-env attack decay sustain release]
  (set! (.-attack cur-env) attack)
  (set! (.-decay cur-env) decay)
  (set! (.-sustain cur-env) sustain)
  (set! (.-release cur-env) release))

(defn set-main-volume [volume]
  (when ((comp not nil?) main-out)
    (.rampTo (.-volume main-out) volume 0.01)
    )
    
  )

(defn set-master-gain-volume [volume]
  (when ((comp not nil?) master-gain)
    (.linearRampToValueAtTime (.-gain master-gain) volume 2)
    ))

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

;; sound player stuff
(defn players-init []
  (let [cur-srcs (mapv #(new Tone.Player. % ) sound-paths)
        cur-gain (new Tone.Gain. 1)]
    (doall (map #(.connect % cur-gain) cur-srcs))
    (doall (map #(.sync %) cur-srcs))
    (.connect cur-gain master-gain)
    (set! player-nodes {:srcs cur-srcs :gain cur-gain})
    )
  )

(defn player-gen-rand-seq [len player-num time-bds]
  ;; loc-seq specifies RELATIVE postions
  ;; ind-seq has 1 for the first val and 2 for the last
  (let [cur-player (get-in player-nodes [:srcs player-num])
        real-len (inc len) ;;since last entry will be to stop playback
        loc-lo 0
        loc-hi 0.75
        rev-chance 0.3
        time-lo (get time-bds :lo)
        time-hi (get time-bds :hi)
        dur-seq  (grain-rand-seq real-len time-lo time-hi 0.06)
        time-seq (dur-to-time-seq dur-seq)
        loc-seq (rand-seq real-len loc-lo loc-hi)
        rev-seq (mapv #(< % rev-chance) (repeatedly real-len #(rand 1.0)))
        ind-seq (conj (into [1] (repeat (max (- real-len 2) 0) 0)) 2)]
    (apply array
           (mapv #(js-obj "time" %1 "dur" %2 "loc" %3 "rev" %4 "ind" %5) time-seq dur-seq loc-seq rev-seq ind-seq))
        
    )
        
  )

(defn player-play-rand-seq [delay-sec len player-num]
  (let [delay-str (str "+" delay-sec)
        time-bds {:lo 0.06 :hi 0.72}
        evts (player-gen-rand-seq len player-num time-bds)
        cur-player (get-in player-nodes [:srcs player-num])
        cur-bufdur (-> cur-player .-buffer .-duration)
        cur-callback (fn [time val]
                       (let [reverse? (.-rev val)
                             cur-dur (.-dur val)
                             cur-loc (if (true? reverse?)
                                       (* (.-loc val) (- 1.0 cur-bufdur)) ;;time specify proportion from ending
                                       (* (.-loc val) cur-bufdur))
                             cur-ind (.-ind val)]
                         (set! (.-reverse cur-player) reverse?)
                         (cond
                           (= 1 cur-ind) (.start cur-player cur-loc) ;;first evt, start playback
                           (= 2 cur-ind) (.stop cur-player) ;;last evt, stop playback
                           :else (.seek cur-player cur-loc) ;; middle evt, just seek
                           )
                         ))
        cur-part (new Tone.Part. cur-callback evts)]
     (if (> delay-sec 0)
      (.start cur-part delay-str)
      (.start cur-part))
                             
        
     )
  )

;; telegraph stuff

(defn telegraph-init [freq vol]
  (let [osc-param (js-obj "type" "sine" "frequency" freq "detune" 0)
        cur-src (new Tone.Oscillator. osc-param)
        cur-env (new Tone.AmplitudeEnvelope. 0.001 0.0 1 0.001)
        cur-gain (new Tone.Gain. vol)]
    (.start cur-src)
    (.sync cur-src)
    (.chain cur-src cur-env cur-gain master-gain)
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
  ;; temp-seq are actual durations (want to cut short hear articulation)
  (let [chance-seq (rand-seq len 0 100)
        patt-seq (mapv #(cond (< % 10) 0 (< % 65) 1 :else 2) chance-seq)
        temp-seq (mapv #(if (<= % 1) base-dur (* 2 base-dur)) patt-seq)
        dur-seq (mapv #(if (= % base-dur) (* % 0.35) (* % 0.75)) temp-seq)
        vel-seq (mapv #(if (< % 1) 0 1) patt-seq)
        time-seq (dur-to-time-seq temp-seq)
        evts (apply array (mapv #(js-obj "time" %1 "dur" %2 "vel" %3) time-seq dur-seq vel-seq))
        total-len (reduce + temp-seq)]
    (.log js/console (apply array temp-seq) base-dur)
    {:total-len total-len :evts evts}
        
    )
  )

(defn telegraph-play-rand-seq [len base-dur]
  (let [rand-seq-info (telegraph-gen-rand-seq len base-dur)
        evts (get rand-seq-info :evts)
        total-len (get rand-seq-info :total-len)
        tele-env (get telegraph-node :env)
        cur-callback (fn [time val]
                         (.triggerAttackRelease tele-env (.-dur val) time (.-vel val))
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
  (let [noise-param (js-obj "playbackRate" 1 "volume" 0 "type" "brown")
        filt-param (js-obj "frequency" 200 "type" "bandpass" "gain" 0 "Q" 5)
        cur-src (new Tone.Noise. noise-param)
        cur-filt (new Tone.Filter. filt-param)
        cur-env (new Tone.AmplitudeEnvelope. 0.01 0.0 1.0 0.001)
        cur-gain (new Tone.Gain. 1.5)]
    (.start cur-src)
    (.sync cur-src)
    (.chain cur-src cur-filt cur-env cur-gain master-gain)
    (set! noise-node
          {:src cur-src
           :filter cur-filt
           :env cur-env
           :gain cur-gain}
          )
    (.log js/console "noise-init")
    )
  )

;; modem whine stuff

(defn modem-whine-init [freq]
  (let [freqs (mapv #(* % freq) [1 1.976 3.05])
        vols [1 0.75 0.5]
        noise-param (js-obj "playbackRate" 1 "volume" -6 "type" "brown")
        cur-src (new Tone.Noise. noise-param)
        filt-params (mapv #(js-obj "frequency" % "type" "bandpass" "Q" 30) freqs)
        filts (mapv #(new Tone.Filter. %) filt-params)
        scalers (mapv #(new Tone.Gain. %)  vols)
        cur-env (new Tone.AmplitudeEnvelope. 0.1 0.0 1.0 0.01)
        cur-gain (new Tone.Gain. 1)]
    (.start cur-src)
    (.sync cur-src)
    (doall (map #(do (.connect cur-src %1)
                  (.connect %1 %2)
                  (.connect %2 cur-env))
                filts scalers))
    (.chain cur-env cur-gain master-gain)
    (set! modem-whine-node
          {:src cur-src
           :filters filts
           :scalers scalers
           :env cur-env
           :gain cur-gain
           :is-playing false})
    )
  )

(defn modem-whine-play [play?]
  (let [is-playing (get modem-whine-node :is-playing)
        cur-env (get modem-whine-node :env)]
    (cond
      (and (false? is-playing) (true? play?)) (do (.triggerAttack cur-env)
                                                  (set! modem-whine-node (assoc modem-whine-node :is-playing true))
                                                  (.log js/console "modem-whine on"))
      (and (true? is-playing) (false? play?)) (do (.triggerRelease cur-env)
                                                  (set! modem-whine-node (assoc modem-whine-node :is-playing false))
                                                  (.log js/console "modem-whine off"))
      :else nil
      ))
  )
        

;; modem clicks stuff

(defn modem-clicks-start []
  (.log js/console "starting clicks")
  (let [cur-filt (get noise-node :filter)
        cur-gain (get noise-node :gain)
        freq (get modem-click-param :freq)]
    (set! (-> cur-filt .-Q .-value) 10)
    (set! (-> cur-filt .-frequency .-value) freq)
    (set! (-> cur-gain .-gain) 3)
    )
  (let [cur-env (get noise-node :env)
        subdiv (get modem-click-param :subdiv)
        dur (get modem-click-param :dur)
        loop-dur (/ 4.0 subdiv)
        cur-callback (fn [time]
                       (.triggerAttackRelease cur-env dur time 1))
        loop-params (js-obj "callback" cur-callback "interval" loop-dur)
        cur-loop (new Tone.Loop. loop-params)]
    (set-adsr cur-env 0.01 0.03 0.25 0.2)
    (when (nil? (get modem-parts :clicks))
      (.start cur-loop)
      (set! modem-parts (assoc modem-parts :clicks cur-loop))
      )
    )
        
  )

(defn modem-clicks-stop []
  (.log js/console "stopping clicks")
  (let [clicks-part (get modem-parts :clicks)]
    (when (not (nil? clicks-part))
      (.stop clicks-part)
      (set! modem-parts (assoc modem-parts :clicks nil))

      )
    )
  )

  
(defn modem-clicks-play [play?]
  (let [is-playing ((comp not nil?) (get modem-parts :clicks))
        cur-env (get modem-whine-node :env)]
    (cond
      (and (false? is-playing) (true? play?)) (modem-clicks-start)
      (and (true? is-playing) (false? play?)) (modem-clicks-stop)
      :else nil
      ))
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
        noise-gain (get noise-node :gain)
        cur-callback (fn [time val]
                       (let [cur-vel 1]
                         (.setValueAtTime (.-Q noise-filt) (.-q val) time)
                         (.setValueAtTime (.-frequency noise-filt) (.-freq val) time)
                         (.triggerAttackRelease noise-env (.-dur val) time cur-vel)
                         )
                       )
       cur-part (Tone.Part. cur-callback gen-seq)]
    (set! (-> noise-gain .-gain) 2)
    (set-adsr noise-env 0.01 0.0 1.0 0.001)
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

(defn master-gain-init []
  (let [cur-gain (new Tone.Gain. 1)]
    (.toMaster cur-gain)
    (set! master-gain cur-gain)
    ))

(defn get-current-time []
  (.-currentTime ctx))


(defn stop-loops []
  (telegraph-stop)
  (modem-clicks-play false)
  (modem-whine-play false)
  )

(defn cleanup []
    ;; clear all callbacks, set main gain to 0
  (mute-main-out true)
  (start-transport false 0.1)
  ) 

(defn synths-init []
  (noise-init)
  (telegraph-init (+ 1000 (rand 5000)) 0.5)
  (modem-whine-init (+ 2050 (rand 2000)))
  (players-init)
  )

(defn init-audio [win]
  (when (nil? ctx)
    (.log js/console "loading context")
    (set! ctx (if (.-AudioContext win) (win.AudioContext.) (win.webkitAudioContext.)))
    (if (nil? ctx) (js/alert "context not available") (.setContext Tone ctx))
    (set! ctx true)
    (master-init)
    (master-gain-init)
    (start-transport true 0.1)
    (synths-init)
    ;;(doall (map osc-create (keys oscs)))
    ;;(osc-connect :carrier true nil nil)
  ;;(println oscs)
    )

  (not (nil? ctx))
  )
  
