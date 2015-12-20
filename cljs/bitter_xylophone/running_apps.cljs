;; bitter-xylophone.running-apps
;; Displays the list of running apps
(ns bitter-xylophone.running-apps
  (:require [bitter-xylophone.actions :refer [app-state post-terminate]]
            [domina :refer [append! destroy-children! nodes single-node]]
            [domina.css :refer [sel]]
            [domina.events :refer [listen! prevent-default]]))

(def ^:dynamic app-root (sel "#bitter-xylophone-app"))

(defn- make-descriptions
  "Add description to the processes list, yielding a 
  list of objects with :uuid and :desription keys."
  [processes exectuions]
  (let [make-description (fn [uuid]
                          {:uuid uuid
                           :description (exectuions uuid)})]
   (map make-description processes)))

(defn- make-row
  "Makes a row in the running apps table"
  [{:keys [uuid description]}]
  (let [tr (single-node "<tr></tr>")]
    (append! tr (str "<td>" description "</td>"))
    (append! tr (str "<td><a href='#' data-uuid='" uuid "'><i class='fa fa-stop-circle-o'></i></a></td>"))
    (listen! (sel tr "a") :click (fn [e]
                                   (prevent-default e)
                                   (post-terminate uuid)))
    tr))

(defn- show-running-apps
  "Displays the list of running apps"
  [{:keys [processes executions]}]
  (let [cols "<colgroup><col style='width: 100%'><col style='text-align: right'></col></colgroup>"
        div (single-node "<div class='col-lg-12'></div>")
        table (single-node "<table class='table'></table>")
        thead (single-node "<thead><tr><th>Running Processes</th><th>&nbsp;</th></tr>")
        tbody (single-node "<tbody></tbody>")
        rows (map make-row (make-descriptions processes executions))
        no-running-apps-row (single-node "<tr><td colspan='2' class='info'>No running processes</td></tr>")]
    (destroy-children! app-root)
    (append! app-root div)
    (append! div table)
    (-> table
        (append! cols)
        (append! thead)
        (append! tbody))
    (if (empty? rows)
      (append! tbody no-running-apps-row)
      (append! tbody rows))))


(defn- on-state-update
  "Called on each change to the app-state to refresh the running apps list"
  [key state-ref old-state new-state]
  (when (not-empty (nodes app-root)) (show-running-apps new-state)))

(add-watch app-state ::running-aps-watch on-state-update)

