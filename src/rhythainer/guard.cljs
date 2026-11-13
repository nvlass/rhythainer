(ns rhythainer.guard
  (:require [reagent.core :as r]
            [rhythainer.storage :as storage]))

;; If rendering throws (usually saved data the code no longer understands), show a way out
;; instead of a blank page. Plain text in both languages: i18n may be what broke.

(defn- wipe! [] (-> (storage/clear!) (.finally #(.reload js/location))))

(defn- crash [e]
  [:div.app
   [:div.card.crash
    [:h3 "Something went wrong · Κάτι πήγε στραβά"]
    [:p "The app couldn't load what it saved on this device. Resetting clears this app's settings and progress here, nothing else."]
    [:p "Η εφαρμογή δεν μπόρεσε να φορτώσει όσα είχε αποθηκεύσει σε αυτή τη συσκευή. Η επαναφορά σβήνει τις ρυθμίσεις και την πρόοδο αυτής της εφαρμογής εδώ, τίποτε άλλο."]
    [:pre.crash-msg (str e)]
    [:div.actions
     [:button {:on-click #(.reload js/location)} "Reload · Επαναφόρτωση"]
     [:button.primary {:on-click wipe!} "Reset saved data · Επαναφορά δεδομένων"]]]])

(defn guard [_]
  (r/create-class
   {:display-name "guard"
    :get-derived-state-from-error (fn [e] #js {:error e})
    :render (fn [this]
              (if-let [e (some-> (.-state this) .-error)]
                [crash e]
                (second (r/argv this))))}))
