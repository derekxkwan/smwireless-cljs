(ns smwireless-cljs.app
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [smwireless-cljs.core-test]))

(doo-tests 'smwireless-cljs.core-test)


