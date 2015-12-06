(ns bitter-xylophone.filtering
  (:require [domina :refer [add-class! remove-class! toggle-class! by-class]]
            [domina.xpath :refer [xpath]]
            [shodan.console :as console :include-macros true]))

(defn- sel-dev
  "Selects the nodes corresponding to a device"
  [devid]
  (xpath (str "//div[@data-device='" devid "']")))

(defn- sel-not-dev
  "Selects the nodes that are not for a device"
  [devid]
  (xpath (str "//div[@data-device!='" devid "']")))

(defn- sel-cat
  "Selects the nodes corresponding to a category"
  [catid]
  (xpath (str "//div[@data-category='" catid "']")))

(defn- sel-not-cat
  "Selects the nodes that don't have a category"
  [catid]
  (xpath (str "//div[@data-category!='" catid "']")))

(defn- sel-all
  "Select all the nodes that list drivers"
  []
  (xpath "//div[@data-category or @data-device]"))

(defn set-active-nav!
  "Marks the active selection in side navbar"
  [link]
  (let [link- (str "#" link)
        root (by-class "side-nav")
        active-query (str "li[a/@href='" link- "']")
        all-query "li"
        active (xpath root active-query)
        all (xpath root all-query)]
    (console/debug (str "removing active class from (" all-query ")"))
    (remove-class! all "active")
    (console/debug (str "adding active class to (" active-query ")"))
    (add-class! active "active")))

(defn show-all
  "displays all devices and categories"
  []
  (console/debug "show-all called")
  (remove-class! (sel-all) "hidden"))

(defn show-category
  "displays all drivers in a certai category"
  [catid]
  (console/debug (str "show-category called: " catid))
  (add-class! (sel-not-cat catid) "hidden")
  (remove-class! (sel-cat catid) "hidden"))

(defn show-device
  "displays all drivers for a specific device"
  [devid]
  (console/debug (str "show-device called: " devid))
  (add-class! (sel-not-dev devid) "hidden")
  (remove-class! (sel-dev devid) "hidden"))
