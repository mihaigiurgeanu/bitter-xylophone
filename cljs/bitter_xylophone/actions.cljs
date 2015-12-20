;; bitter-xylophone.actions
;; setting up the links that perform desired actions on
;; lins to files
(ns bitter-xylophone.actions
  (:require [domina.xpath :refer [xpath]]
            [domina.events :refer [listen! prevent-default current-target]]
            [domina :refer [attr text]]
            [shodan.console :as console :include-macros true]
            [ajax.core :refer [GET POST]]
            [cemerick.url :refer (map->query)]
            [siren.core :refer (siren!)]
            [cljs.reader :refer [read-string]]))

(def ^:dynamic app-state (atom {:known 0,
                                :retry false,
                                :processes [],
                                :executions {}}))

(defn- exec-success
  "Handler for success return of POST execute command"
  [description data]
  (console/log (str "POST execute request received response from server: " data))
  (siren! {:content (str "<i class='fa fa-spinner'></i> Executing " description), :delay 7000})
  (swap! app-state (fn [state]
                     (assoc state :executions (assoc (state :executions) uuid description)))))

(defn- exec-failed
  "Handler for failure of POST execute command"
  [{:keys [status status-text failure]}]
  (console/error (str "POST execute request failed (" status " - " status-text "): " failure))
  (siren! {:content (str "<i class='fa fa-exclamation-circle'></i> " failure ": " status " - " status-text),
           :delay 7000}))

(defn- post-execute
  "Sending execute file command to the server"
  [description file-name args]
  (let [post-url "/processes"]
    (console/debug (str "Posting execute command to '" post-url "'"))
    (POST post-url {:handler (fn [data] (exec-success description data))
                    :error-handler exec-failed
                    :format :json
                    :params {:command file-name, :args args}})))

(defn- file-description
  "Selects the description of the current data-file click target from the html structure"
  [content]
  (let [path-query "ancestor-or-self::div[(@data-category or @data-device) and 1]//h2"]
    (-> content (xpath path-query) text)))

(defn- extract-args
  "Reads the args list from the data-args attribute. 'args-str' should be a clojure vector literal, i.e. enclosed in square brackets []"
  [args-str]
  (if (empty? args-str)
    []
    (read-string args-str)))

(defn set-up-actions!
  "Setting up links to files to post execute command to the server"
  []
  (console/debug "Setting up action on links")
  (let [action-links (xpath "//a[@data-file]")]
    (listen! action-links :click (fn [e]
                                   (prevent-default e)
                                   (let [target (current-target e)
                                         file-name (attr target "data-file")
                                         description (file-description target)
                                         args (extract-args (attr target "data-args"))]
                                     (console/log (str "Executing <" file-name ">"))
                                     (post-execute description file-name args))))))

(defn- update-state
  "Update the current app data state in the long pooling app"
  [data]
  (console/debug (str "New state received from server: known = " (:crt data)
                      ", processes = " (pr-str (:uuids data))))
  (swap! app-state assoc
         :known (:crt data)
         :processes (:uuids data)
         :retry false))

(defn- update-state-error
  "Handler for error received in long polling data request"
  [{:keys [status status-text]}]
  (console/error (str "Error in long polling request (") status "/" status-text ")")
  (swap! app-state assoc :processes [] :retry true))


(defn get-processes
  "Long polling request to get the list of current processes on the server"
  []
  (console/debug "Polling the new state from the server")
  (GET "/processes" {:handler update-state
                     :error-handler update-state-error
                     :finally #(if (:retry @app-state)
                                 (js/setTimeout get-processes 5000)
                                 (get-processes))
                     :response-format :json
                     :keywords? true
                     :params {:known (:known @app-state)}}))


(defn post-terminate
  "Sends terminate request to the server"
  [uuid]
  (POST (str "/processes/" uuid)))
