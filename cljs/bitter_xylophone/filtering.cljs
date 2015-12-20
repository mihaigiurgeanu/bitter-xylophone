(ns bitter-xylophone.filtering
  (:require [domina :refer [add-class! remove-class! toggle-class! by-class clone text destroy-children! append! prepend! set-text! attr has-class? nodes]]
            [domina.xpath :refer [xpath]]
            [domina.css :refer [sel]]
            [shodan.console :as console :include-macros true]))

(def ^private data-rows (sel "div.data"))
(defn- filter-data-nodes [content]
  (doall (filter #(has-class? % "data") (nodes content))))

(defn- sel-dev
  "Selects the nodes corresponding to a device"
  [devid]
  (filter-data-nodes (xpath (str "//div[@data-device='" devid "']"))))

(defn- sel-not-dev
  "Selects the nodes that are not for a device"
  [devid]
  (filter-data-nodes (xpath (str "//div[@data-device!='" devid "' or (@data-category and not(@data-device))]"))))

(defn- sel-cat
  "Selects the nodes corresponding to a category"
  [catid]
  (filter-data-nodes (xpath (str "//div[@data-category='" catid "']"))))

(defn- sel-not-cat
  "Selects the nodes that don't have a category"
  [catid]
  (filter-data-nodes (xpath (str "//div[@data-category!='" catid "' or (@data-device and not(@data-category))]"))))

(defn- sel-all
  "Select all the nodes that list drivers"
  []
  data-rows)

(defn set-active-nav!
  "Marks the active selection in side navbar"
  [link]
  (let [link- (str "#" link)
        root (by-class "side-nav")
        active-query (str "li[a/@href='" link- "']")
        all-query "li"
        active (xpath root active-query)
        all (xpath root all-query)
        page-header (sel "h1.page-header")
        active-icon (clone (sel active ".icon"))
        active-text (text active)
        active-title (text (attr (sel active "a") "title"))]
    (console/debug (str "removing active class from (" all-query ")"))
    (remove-class! all "active")
    (console/debug (str "adding active class to (" active-query ")"))
    (add-class! active "active")
    (console/debug "Removing page header content")
    (destroy-children! page-header)
    (console/debug (str "Setting page title to '" active-text "'"))
    (set-text! page-header (str " " active-text " "))
    (console/debug "Adding current icon to the page header")
    (prepend! page-header active-icon)
    (console/debug (str "Setting page header sub-title to '" active-title "'"))
    (append! page-header (str "<small> " active-title "</small>"))))

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

(defn- count-cat
  "Count entries in a category"
  [catid]
  (let [cat-nodes (sel-cat catid)
        file-nodes (xpath cat-nodes "*//a[@data-file]")]
    (count (nodes file-nodes))))

(defn- category-of [node] (attr node "data-category"))
(defn- count-of [node] (let [catid (category-of node)
                             the-count (count-cat catid)]
                         (console/debug (str "Count for " catid " is " the-count))
                         the-count))
(defn- display-count! [node] (set-text! node (count-of node)))

(defn update-count-summaries!
  "Updates the fields showing drivers count per category"
  []
  (let [summary-nodes (sel ".count-cat")]
    (console/debug (str "Updating count summaries for " (count (nodes summary-nodes)) " nodes"))
    (doseq [node (nodes summary-nodes)]
      (display-count! node))))
