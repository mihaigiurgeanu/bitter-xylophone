;; bitter-xylophone.actions
;; setting up the links that perform desired actions on
;; lins to files
(ns bitter-xylophone.actions
  (:require [domina.xpath :refer [xpath]]
            [domina.events :refer [listen! prevent-default current-target]]
            [domina :refer [attr]]
            [shodan.console :as console :include-macros true]
            [ajax.core :refer [POST]]))

(defn set-up-actions
  "Setting up links to files to post execute command to the server"
  []
  (let [action-links (xpath "//a[@data-file]")]
    (listen! action-links :click (fn [e]
                                   (prevent-default e)
                                   (let [file-name (-> (current-target e) (attr "data-file"))]
                                     (console/log (str "Executing <" file-name ">")))))))

