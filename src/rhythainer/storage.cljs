(ns rhythainer.storage
  (:require ["idb-keyval" :as idb]
            [cljs.reader :as reader]))

;; Its own database: on music-trainer.app the apps share an origin, so idb-keyval's default store
;; would mix their keys.

(def store (delay (idb/createStore "rhythainer" "keyval")))

(defn- parse [s] (when (string? s) (try (reader/read-string s) (catch :default _ nil))))

(defn save! [k v]
  (-> (idb/set (name k) (pr-str v) @store)
      (.catch #(js/console.warn "save failed" (name k) %))))

(defn load-all! [ks on-done]
  (-> (js/Promise.all (clj->js (for [k ks] (idb/get (name k) @store))))
      (.then #(on-done (zipmap ks (map parse %))))
      (.catch (fn [e]
                (js/console.warn "load failed" e)
                (on-done {})))))

(defn clear! [] (idb/clear @store))
