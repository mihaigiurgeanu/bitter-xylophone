(ns bitter-xylophone.core
  (:require [secretary.core :as secretary :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [bitter-xylophone.filtering :refer [show-all show-device show-category set-active-nav! update-count-summaries!]]
            [bitter-xylophone.actions :refer [set-up-actions! get-processes]]
            [bitter-xylophone.running-apps :refer []]
            [shodan.console :as console :include-macros true])
  (:import goog.History
           goog.history.Html5History))

(console/info "Starting bitter-xylophone application")


(secretary/set-config! :prefix "#")

(console/debug "setting up routes")

(defroute "/" [] (show-all))
(defroute "/categories/:catid" [catid]
  (show-category catid))
(defroute "/devices/:devid" [devid]
  (show-device devid))

(console/debug "Setting up history events listeners")

;; Quick and dirty history configuration.
(let [h (if (.isSupported Html5History) (Html5History.) (History.))]
  (events/listen h EventType/NAVIGATE #(let [path (.-token %)
                                             path- (if (empty? path) "/" path)]
                                         (console/debug (str "routing path: " path-))
                                         (secretary/dispatch! path-)
                                         (set-active-nav! path-)))
  (doto h (.setEnabled true)))

(set-up-actions!)
(update-count-summaries!)
(get-processes)
