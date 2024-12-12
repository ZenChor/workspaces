(ns nubank.workspaces.card-types.fulcro3
  (:require
    [cljs.spec.alpha :as s]
    [com.fulcrologic.fulcro-css.css-injection :as cssi]
    [com.fulcrologic.fulcro.algorithms.merge :as f.merge]
    [com.fulcrologic.fulcro.algorithms.normalize :refer [tree->db]]
    [com.fulcrologic.fulcro.application :as fapp]
    [com.fulcrologic.fulcro.components :as fc]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.inspect.inspect-client :as fi.client]
    [com.fulcrologic.fulcro.react.version18 :refer [with-react18]]
    [goog.functions :as gfun]
    [goog.object :as gobj]
    [nubank.workspaces.card-types.util :as ct.util]
    [nubank.workspaces.data :as data]
    [nubank.workspaces.model :as wsm]
    [nubank.workspaces.ui :as ui]
    [nubank.workspaces.ui.core :as uc]
    ["react-dom/client" :as ReactDOMClient]))


(s/def ::root any?)
(s/def ::wrap-root? boolean?)
(s/def ::app map?)
(s/def ::persistence-key any?)
(s/def ::initial-state (s/or :fn? fn? :factory-param any?))
(s/def ::root-state map?)
(s/def ::computed map?)
(s/def ::root-node-props map?)

(defonce css-components* (atom #{}))
(defonce persistent-apps* (atom {}))

(defn debug-node [node prefix]
  (let [node-info {:node node
                   :id (.-id node)
                   :class-name (.-className node)}]
    (println prefix node-info)
    node-info))

(defn gen-css-component []
  (let [generated-name (gensym)
        component-key  (keyword "nubank.workspaces.card-types.fulcro3" (name generated-name))]
    (fc/configure-component! (fn *dyn-root* [])
      component-key
      {:query (fn [_] (into
                        []
                        (keep-indexed (fn [i v] {(keyword (str "item" i))
                                                 (or (fc/get-query v) (with-meta [] {:component v}))}))
                        @css-components*))})))

(defn safe-initial-state [comp params]
  (if (fc/has-initial-app-state? comp)
    (fc/get-initial-state comp params)
    params))

(defn make-root [Root]
  (let [generated-name (gensym)
        component-key  (keyword "nubank.workspaces.card-types.fulcro3" (name generated-name))]
    (fc/configure-component! (fn *dyn-root* [])
      component-key
      {:initial-state (fn [_ params]
                        {:ui/root (or (safe-initial-state Root params) {})})
       :query         (fn [_] [:fulcro.inspect.core/app-id {:ui/root (fc/get-query Root)}])
       :render        (fn [this]
                        (let [{:ui/keys [root]} (fc/props this)
                              Root     (-> Root fc/class->registry-key fc/registry-key->class)
                              factory  (fc/factory Root)
                              computed (fc/shared this ::computed)]
                          (when (seq root)
                            (factory (cond-> root computed (fc/computed computed))))))})))

(defn fulcro-initial-state [{::keys [initial-state wrap-root? root root-state]
                             :or    {wrap-root? true initial-state {}}}]
  (let [state-tree (if (fn? initial-state)
                     (initial-state (safe-initial-state root nil))
                     (safe-initial-state root initial-state))
        wrapped    (merge
                     (if wrap-root?
                       {:ui/root state-tree}
                       state-tree)
                     root-state)
        Root       (if wrap-root? (make-root root) root)
        db         (tree->db Root wrapped true (f.merge/pre-merge-transform {}))]
    db))

(defn upsert-app [{::keys [app persistence-key computed] :fulcro.inspect.core/keys [app-id] :as config}]
  (println "Upserting app:"
             {:persistence-key persistence-key
              :app-id app-id})

  (if-let [instance (and persistence-key (get @persistent-apps* persistence-key))]
    (do
      (println "Found existing app instance for key" persistence-key)
      instance)
    (let [app (cond-> app
                (not (contains? app :initial-state))
                (assoc :initial-db (fulcro-initial-state config))

                computed
                (update :shared assoc ::computed computed)

                app-id
                (assoc-in [:initial-db :fulcro.inspect.core/app-id] app-id))
          instance (with-react18 (fapp/fulcro-app app))]
      (println "Created new app instance"
                 {:persistence-key persistence-key
                  :app-id app-id})
      (when persistence-key
        (swap! persistent-apps* assoc persistence-key instance))
      instance)))

(defn dispose-app [{::keys [persistence-key] :as app}]
  (println "Disposing app:"
             {:persistence-key persistence-key
              :app-uuid (fi.client/app-uuid app)})

  (when persistence-key
    (swap! persistent-apps* dissoc persistence-key))
  (when-let [app-uuid (fi.client/app-uuid app)]
    (fi.client/dispose-app app-uuid)))

;; region CSS management
(def debounced-refresh-css!
  (gfun/debounce #(cssi/upsert-css "fulcro-portal-css" {:component (gen-css-component)}) 100))

(defn add-component-css! [comp]
  (swap! css-components* conj comp)
  (debounced-refresh-css!))

(defn mount-at [app {::keys [root wrap-root? persistence-key] :or {wrap-root? true}} node]
  (println "Mount-at called with:"
             {:persistence-key persistence-key
              :wrap-root? wrap-root?
              :node (debug-node node "")})

  (add-component-css! root)
  (let [instance (if wrap-root? (make-root root) root)
        new-app  (fapp/mount! app instance node {:initialize-state? false})]
    (when persistence-key
      (println "Persisting app for key" persistence-key)
      (swap! persistent-apps* assoc persistence-key new-app))
    new-app))



(fc/defsc FulcroPortal
  [this {::keys [root-node-props]}]
  {:componentDidMount
   (fn [this]
     (let [props (fc/props this)
           app   (upsert-app props)
           node  (dom/node this)]
       (println "FulcroPortal mounted:"
                  {:props props
                   :node (debug-node node "")})
       (gobj/set this "app" app)
       (mount-at app props node)))

   :componentDidUpdate
   (fn [this _ _]
     (println "FulcroPortal updated")
     (when-let [app (gobj/get this "app")]
       (fapp/force-root-render! app)))

   :componentWillUnmount
   (fn [this]
     (let [app  (gobj/get this "app")
           node (dom/node this)]
       (println "FulcroPortal will unmount:"
                  {:node (debug-node node "")})
       (dispose-app app)
       (reset! app nil)))

   :shouldComponentUpdate
   (fn [this _ _] false)}

  (dom/div root-node-props))


(def fulcro-portal* (fc/factory FulcroPortal))

(defn fulcro-portal
  "Create a new portal for a Fulcro app. Available options:

  ::root - the root component to be mounted
  ::app - app configuration, same options as fulcro/new-fulcro-client
  ::wrap-root? - (default true) wraps component with ident in an actual root
  ::initial-state - value or function for initial state
  ::root-state - map merged into app root state
  ::computed - computed props for root
  ::root-node-props - props for root DOM node"
  [component options]
  (fulcro-portal* (assoc options ::root component)))

;; region card implementation
(defn inspector-set-app [card-id]
  (when-let [{::keys [app]} (data/active-card card-id)]
    (when-let [app-uuid (fi.client/app-uuid app)]
      (fi.client/set-active-app app-uuid))))

(defn fulcro-card-init
  [{::wsm/keys [card-id]
    :as        card}
   config]
  (println "Initializing Fulcro card:"
             {:card-id card-id
              :config config})

  (let [app (upsert-app (assoc config :fulcro.inspect.core/app-id card-id))]
    (ct.util/positioned-card card
      {::wsm/dispose
       (fn [node]
         (println "Disposing card:"
                    {:card-id card-id
                     :node (debug-node node "")})
         (dispose-app app))

       ::wsm/refresh
       (fn [_]
         (println "Refreshing card:" {:card-id card-id})
         (debounced-refresh-css!)
         (fapp/force-root-render! app))

       ::wsm/render
       (fn [node]
         (println "Rendering card:"
                    {:card-id card-id
                     :node (debug-node node "")})
         (swap! data/active-cards* assoc-in [card-id ::app] app)
         (mount-at app config node))

       ::wsm/render-toolbar
       (fn []
         (dom/div
           (uc/button {:onClick #(inspector-set-app card-id)}
             "Inspector")
           (uc/button {:onClick #(ui/restart-card card-id)}
             "Restart")))

       ::app
       app})))

(defn fulcro-card [config]
  {::wsm/init
   #(fulcro-card-init % config)})

(s/fdef fulcro-card
  :args (s/cat :config (s/keys
                         :req [::root]
                         :opt [::wrap-root?
                              ::app
                              ::initial-state]))
  :ret ::wsm/card-instance)
