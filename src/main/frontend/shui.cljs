(ns frontend.shui
  (:require 
    [frontend.date :refer [int->local-time-2]]
    [frontend.state :as state]
    [logseq.shui.context :refer [make-context]]))

(defn make-shui-context [block-config inline]
  (make-context {:block-config block-config 
                 :app-config (state/get-config) 
                 :inline inline 
                 :int->local-time-2 int->local-time-2}))
