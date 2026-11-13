(ns rhythainer.core
  (:require [reagent.dom.client :as rdc]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [rhythainer.events]
            [rhythainer.subs]
            [rhythainer.views :as views]
            [rhythainer.guard :as guard]
            [rhythainer.audio :as audio]
            [rhythainer.storage :as storage]))

(defonce root (atom nil))

(defn ^:dev/after-load mount []
  (rf/clear-subscription-cache!)
  (when-not @root
    (reset! root (rdc/create-root (js/document.getElementById "app"))))
  (rdc/render @root [guard/guard [views/app]]))

(def captured
  "Keys the app takes over, even on a focused button: Space taps rather than clicking it again."
  #{" " "Enter"})

(defn- on-key [e]
  (let [tag (some-> (.-target e) .-tagName str/lower-case)
        k (.-key e)]
    (when-not (or (#{"input" "select" "textarea"} tag) (.-altKey e) (.-metaKey e) (.-ctrlKey e))
      (when (captured k) (.preventDefault e))
      (when-not (.-repeat e)
        (rf/dispatch [:key k (audio/tap-time (.-timeStamp e))])))))

(defn init []
  (rf/dispatch-sync [:init])
  (rf/dispatch-sync [:resize (.-innerWidth js/window)])
  (storage/load-all! [:settings :attempts :sessions] #(rf/dispatch [:storage/loaded %]))
  (audio/install-unlock!)
  (.addEventListener js/document "keydown" on-key)
  (.addEventListener js/window "resize" #(rf/dispatch [:resize (.-innerWidth js/window)]))
  (when (and (not ^boolean goog.DEBUG) (.-serviceWorker js/navigator))
    (.register (.-serviceWorker js/navigator) "sw.js"))
  (mount))
