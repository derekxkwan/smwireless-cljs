 (ns smwireless-cljs.app
  (:require
    [smwireless-cljs.core :as core]
    [cljs.nodejs]
    [mount.core :as mount]))

(mount/in-cljc-mode)

(cljs.nodejs/enable-util-print!)

(set! *main-cli-fn* core/main)
