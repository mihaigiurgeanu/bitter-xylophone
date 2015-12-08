;; bitter-xylophone.actions
;; setting up the links that perform desired actions on
;; lins to files
(ns bitter-xylophone.actions
  (:require [domina.xpath :refer [xpath]]
            [domina.events :refer [listen! prevent-default current-target]]
            [domina :refer [attr]]
            [shodan.console :as console :include-macros true]
            [ajax.core :refer [POST]]
            [cemerick.url :refer (map->query url-encode)]))

(defn- exec-success
  "Handler for success return of POST execute command"
  [response]
  (console/log (str "POST execute request received response from server: " response)))

(defn- exec-failed
  "Handler for failure of POST execute command"
  [{:keys [status status-text failure]}]
  (console/error (str "POST execute request failed (" status " - " status-text "): " failure)))

(defn- post-execute
  "Sending execute file command to the server"
  [file-name]
  (let [post-url (str "/files/" (url-encode file-name))]
    (console/debug (str "Posting execute command to '" post-url "'"))
    (POST post-url {:handler exec-success
                    :error-handler exec-failed
                    :headers {"Content-type" "application/x-www-form-urlencoded"}
                    :body (map->query {:command "execute"})})))

(defn set-up-actions!
  "Setting up links to files to post execute command to the server"
  []
  (console/debug "Setting up action on links")
  (let [action-links (xpath "//a[@data-file]")]
    (listen! action-links :click (fn [e]
                                   (prevent-default e)
                                   (let [file-name (-> (current-target e) (attr "data-file"))]
                                     (console/log (str "Executing <" file-name ">"))
                                     (post-execute file-name))))))

