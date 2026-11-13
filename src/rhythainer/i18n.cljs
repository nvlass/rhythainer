(ns rhythainer.i18n
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tongue.core :as tongue]
            [rhythainer.i18n.en :as en]
            [rhythainer.i18n.el :as el]))

;; Every entry is a string, a hiccup vector, or a fn of (loc & args); `loc` is {:lang}.

;; Hiccup values pass through as they are; without this tongue would call vectors as fns.
(extend-type PersistentVector
  tongue/IInterpolate
  (interpolate-named [v _ _ _] v)
  (interpolate-positional [v _ _ _] v))

(def translate (tongue/build-translate {:en en/dict :el el/dict :tongue/fallback :en}))

(defn locale [lang] {:lang lang})

(defn browser-lang []
  (let [l (some-> js/globalThis .-navigator .-language)]
    (if (and l (str/starts-with? l "el")) :el :en)))

(defn t [loc k & args] (apply translate (:lang loc) k loc args))

(defn tr [k & args] (apply t @(rf/subscribe [:loc]) k args))
