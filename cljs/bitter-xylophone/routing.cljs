(ns bitter-xylophone.routing
  (:require [secretary.core :as secretary :refer-macros [defroute]]))


(defn show-all
  "displays all devices and categories"
  []
  (js/console.log "show-all called"))

(defn show-category
  "displays all drivers in a certai category"
  [catid]
  (js/console.log "show-category called" catid))

(defn show-device
  "displays all drivers for a specific device"
  [devid]
  (js/console.log "show-device called" devid))

(defroute "/" [] show-all)
(defroute "/categories/:catid" [catid]
  (show-category catid))
(defroute "/devices/:devid" [devid]
  (show-device devid))
