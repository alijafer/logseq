(ns frontend.handler.common.config-edn
  "Common fns related to config.edn - global and repo"
  (:require [malli.error :as me]
            [malli.core :as m]
            [goog.string :as gstring]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [lambdaisland.glogi :as log]
            [frontend.handler.notification :as notification]))

(defn- humanize-more
  "Make error maps from me/humanize more readable for users. Doesn't try to handle
nested keys or positional errors e.g. tuples"
  [errors]
  (map
   (fn [[k v]]
     (if (map? v)
       [k (str "Has errors in the following keys - " (string/join ", " (keys v)))]
       ;; Only show first error since we don't have a use case yet for multiple yet
       [k (->> v flatten (remove nil?) first)]))
   errors))

(defn- validate-config-map
  [m schema path]
  (if-let [errors (->> m (m/explain schema) me/humanize)]
    (do
      (notification/show! (gstring/format "The file '%s' has the following errors:\n%s"
                                          path
                                          (->> errors
                                               humanize-more
                                               (map (fn [[k v]]
                                                      (str k " - " v)))
                                               (string/join "\n")))
                          :error)
      false)
    true))

(defn validate-config-edn
  "Validates a global config.edn file for correctness and pops up an error
  notification if invalid. Returns a boolean indicating if file is invalid.
  Error messages are written with consideration that this validation is called
  regardless of whether a file is written outside or inside Logseq."
  [path file-body schema]
  (let [parsed-body (try
                      (edn/read-string file-body)
                      (catch :default _ ::failed-to-read))]
    (cond
      (nil? parsed-body)
      true

      (= ::failed-to-read parsed-body)
      (do
        (notification/show! (gstring/format "Failed to read file '%s'. Make sure your config is wrapped
in {}. Also make sure that the characters '( { [' have their corresponding closing character ') } ]'."
                                            path)
                            :error)
        false)
      ;; Custom error message is better than malli's "invalid type" error
      (not (map? parsed-body))
      (do
        (notification/show! (gstring/format "The file '%s' is not valid. Make sure the config is wrapped in {}."
                                            path)
                            :error)
        false)
      :else
      (validate-config-map parsed-body schema path))))

(defn detect-deprecations
  "Detects config keys that will or have been deprecated"
  [path content]
  (let [body (try (edn/read-string content)
               (catch :default _ ::failed-to-detect))
        warnings {:editor/command-trigger
                  "will no longer be supported soon. Please use '/' and report bugs on it."}]
    (cond
      (= body ::failed-to-detect)
      (log/info :msg "Skip deprecation check since config is not valid edn")

      (not (map? body))
      (log/info :msg "Skip deprecation check since config is not a map")

      :else
      (when-let [deprecations (seq (keep #(when (body (key %)) %) warnings))]
        (notification/show! (gstring/format "The file '%s' has the following deprecations:\n%s"
                                            path
                                            (->> deprecations
                                                 (map (fn [[k v]]
                                                        (str "- " k " " v)))
                                                 (string/join "\n")))
                            :warning
                            false)))))
