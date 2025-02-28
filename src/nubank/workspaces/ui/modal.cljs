(ns nubank.workspaces.ui.modal
  (:require [goog.dom :as gdom]
            [goog.object :as gobj]
            [goog.style :as style]
            [com.fulcrologic.fulcro-css.localized-dom :as dom]
            [com.fulcrologic.fulcro.components :as fp]
            [nubank.workspaces.ui.events :as events]
             ["react-dom" :as ReactDOM]))

(defn $ [s] (.querySelector js/document s))

(defn create-portal-node [props]
  (let [node (doto (gdom/createElement "div")
               (style/setStyle (clj->js (:style props))))]
    (cond
      (:append-to props) (gdom/append ($ (:append-to props)) node)
      (:insert-after props) (gdom/insertSiblingAfter node ($ (:insert-after props))))
    node))

(defn portal-render-children [children]
  (apply dom/div nil children))

(fp/defsc Portal [this _]
  {:initLocalState (fn [_] {:mounted? false})

   :componentDidMount
   (fn [this]
     (let [props (fp/props this)
           node (create-portal-node props)]
       (gobj/set this "node" node)
       ; Force a re-render after the node is created
       (fp/set-state! this {:mounted? true})))

   :componentWillUnmount
   (fn [this]
     (when-let [node (gobj/get this "node")]
       (gdom/removeNode node)))

   :shouldComponentUpdate
   (fn [this _ _]
     true)}

  (if (fp/get-state this :mounted?)
    (when-let [node (gobj/get this "node")]
      (ReactDOM/createPortal
       (portal-render-children (fp/children this))
       node))
    (dom/noscript)))

(def portal (fp/factory Portal))

(fp/defsc WidgetContent [this props]
  {:css [[:.container {:max-height "70vh"
                       :overflow   "auto"}]]}
  (dom/div :.container props
    (fp/children this)))

(def widget-content (fp/factory WidgetContent))

(fp/defsc Modal [this {::keys [on-close]
                       :or    {on-close identity}}]
  {:css         [[:.background {:position        "fixed"
                                :left            0
                                :top             0
                                :background      "rgba(0, 0, 0, 0.5)"
                                :width           "100vw"
                                :height          "100vh"
                                :display         "flex"
                                :align-items     "center"
                                :justify-content "center"
                                :z-index         "100"
                                :overflow-y      "scroll"}]
                 [:.container {:display        "flex"
                               :flex-direction "column"
                               :max-width      "90vw"
                               :max-height     "80vh"}]
                 [:.close {:align-self     "flex-end"
                           :color          "white"
                           :cursor         "pointer"
                           :font-size      "10px"
                           :text-transform "uppercase"}]]
   :css-include [WidgetContent]}
  (portal {:append-to "body"}
    (events/dom-listener {::events/keystroke "escape"
                          ::events/action    on-close})
    (dom/div :.background {:onClick (fn [e]
                                      (if (= (.-currentTarget e) (.-target e))
                                        (on-close e)))}
      (dom/div :.container
        (dom/div
          (fp/children this))))))

(def modal (fp/factory Modal))
