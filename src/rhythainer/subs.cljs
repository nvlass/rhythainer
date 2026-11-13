(ns rhythainer.subs
  (:require [re-frame.core :as rf]
            [rhythainer.events :as ev]
            [rhythainer.i18n :as i18n]))

(doseq [k [:view :settings :attempts :sessions :session :ex :run :result :calib :hl :count :width]]
  (rf/reg-sub k (fn [db _] (k db))))

(rf/reg-sub :loc :<- [:settings] (fn [s _] (i18n/locale (:lang s))))

(rf/reg-sub :hits (fn [db _] (ev/hits db)))
(rf/reg-sub :expected (fn [db _] (ev/expected db)))
(rf/reg-sub :beat-s (fn [db _] (ev/beat-s db)))
(rf/reg-sub :memory-step (fn [db _] (ev/memory-step db)))
