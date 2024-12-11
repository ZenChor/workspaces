(ns nubank.workspaces.data
  (:require [com.fulcrologic.fulcro.application :as app]
            [com.fulcrologic.fulcro.react.version18 :refer [with-react18]]
            [nubank.workspaces.ui.events :as events]))

(defonce app* (-> (app/fulcro-app
                    {:shared {}
                     :client-did-mount
                     (fn [app]
                       (js/setTimeout #(events/trigger-event js/window {::events/event "resize"}) 600))})
                  with-react18))

(defonce workspace-definitions* (atom {}))
(defonce card-definitions* (atom {}))
(defonce card-definitions-snap* (atom {}))

(defonce active-cards* (atom {}))

(defn card-definition [card-id]
  (get @card-definitions* card-id))

(defn active-card [card-id]
  (get @active-cards* card-id))
