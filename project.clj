(defproject smwireless-cljs "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :dependencies [[bidi "2.0.14"]
                 [com.taoensso/timbre "4.7.4"]
                 [hiccups "0.3.0"]
                 [macchiato/core "0.1.2"]
                 [macchiato/env "0.0.3"]
                 [macchiato/sql "0.0.1"]
                 [mount "0.1.10"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [reagent "0.6.0"]
                 [cljsjs/socket-io "1.6.0-0"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :plugins [[lein-doo "0.1.7"]
            [lein-npm "0.6.2"]
            [lein-figwheel "0.5.8"]
            [lein-cljsbuild "1.1.4"]
            [org.clojure/clojurescript "1.9.293"]]
  :npm {:dependencies [[source-map-support "0.4.6"]
                       [node-osc "2.1.0"]
                       [socketio "1.0.0"]]}
  :source-paths ["src" "target/classes"]
  :clean-targets ["target"]
  :target-path "target"
  :profiles
  {:client
   {:cljsbuild
                  {:builds [{:id "dev"
                            :source-paths ["src-client"]

                             :figwheel true                            

                             :compiler     {:main          smwireless-cljs.client
                                            :output-to     "public/js/client.js"
                                            :output-dir    "public/js"
                                            :asset-path    "/js"
                                            :optimizations :none
                                            :pretty-print  true
                                            :source-map    true}}
                    {:id "min"
                     :source-paths ["src-client"]
                     :figwheel false
                     :compiler {:output-to "target/release/js/client.js"
                                :main smwireless-cljs.client
                                :optimizations :advanced
                                :pretty-print false}}]
                   

                   }
    :figwheel
     {:http-server-root "public"
                              :nrepl-port       7001
                              :server-port      3450
                              :reload-clj-files {:clj false :cljc true}
                              :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

    :dependencies [[com.cemerick/piggieback "0.2.1"]]

    :repl-options {:init-ns user}}
   :dev
   {:cljsbuild
                  {:builds {:dev
                            {:source-paths ["env/dev" "src"]
                             :figwheel     true
                             :compiler     {:main                 smwireless-cljs.app
                                            :output-to            "target/out/smwireless-cljs.js"
                                            :output-dir           "target/out"
                                            :target               :nodejs
                                            :optimizations        :none
                                            :pretty-print         true
                                            :source-map           true
                                            :source-map-timestamp false}}}}
    :figwheel
                  {:http-server-root "public"
                   :nrepl-port       7000
                   :reload-clj-files {:clj false :cljc true}
                   :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

    :dependencies [[com.cemerick/piggieback "0.2.1"]]
    :source-paths ["env/dev"]
    :repl-options {:init-ns user}}
   :test
   {:cljsbuild
         {:builds
          {:test
           {:source-paths ["env/test" "src" "test"]
            :compiler     {:main          smwireless-cljs.app
                           :output-to     "target/test/smwireless-cljs.js"
                           :target        :nodejs
                           :optimizations :none
                           :source-map    true
                           :pretty-print  true}}}}
    :doo {:build "test"}}
   :release
   {:cljsbuild
    {:builds
     {:release
      {:source-paths ["env/prod" "src"]
       :compiler     {:main          smwireless-cljs.app
                      :output-to     "target/release/smwireless-cljs.js"
                      :target        :nodejs
                      :optimizations :simple
                      :pretty-print  false}}}}}}
  :aliases
  {"build-client"
             ["with-profile" "client" "figwheel" "dev"]
   "build-client-min"
             ["with-profile" "client" "cljsbuild" "once" "min"]
   "build"   ["do"
              ["clean"]
              ["npm" "install"]
              ["figwheel" "dev"]
              ]
   "package" ["do"
              ["clean"]
              ["npm" "install"]
              ["npm" "init" "-y"]
              ["with-profile" "release" "cljsbuild" "once"]]
   "test"    ["do"
              ["npm" "install"]
              ["with-profile" "test" "doo" "node"]]})
