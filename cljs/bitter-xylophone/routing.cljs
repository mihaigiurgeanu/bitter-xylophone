(ns bitter-xylophone.routing
  (:require [secretary.core :as secretary :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [shodan.console :as console :include-macros true])
  (:import goog.History
           goog.history.Html5History))

(console/info "Starting bitter-xylophone application")

(defn show-all
  "displays all devices and categories"
  []
  (console/debug "show-all called"))

(defn show-category
  "displays all drivers in a certai category"
  [catid]
  (console/debug (str "show-category called: " catid)))

(defn show-device
  "displays all drivers for a specific device"
  [devid]
  (console/debug (str "show-device called: " devid)))

(secretary/set-config! :prefix "#")

(console/debug "setting up routes")

(defroute "/" [] show-all)
(defroute "/categories/:catid" [catid]
  (show-category catid))
(defroute "/devices/:devid" [devid]
  (show-device devid))

(console/debug "Setting up history events listeners")

;; Quick and dirty history configuration.
(let [h (if (.isSupported Html5History) (Html5History.) (History.))]
  (events/listen h EventType/NAVIGATE #(let [path (.-token %)]
                                         (console/debug (str "routing path: " path))
                                         (secretary/dispatch! path)))
  (doto h (.setEnabled true)))
