(ns rhythainer.events
  (:require [re-frame.core :as rf]
            [rhythainer.rhythm :as rh]
            [rhythainer.grade :as grade]
            [rhythainer.audio :as audio]
            [rhythainer.storage :as storage]
            [rhythainer.i18n :as i18n]))

(def default-settings
  {:lang (i18n/browser-lang)
   :mode :listen
   :level 1
   :focus false
   :beats 4
   :bars 2
   :tempo 84
   :timing :strict
   :lead-in true
   :tolerance :kids
   :syllables :takadimi
   :syl-read false
   :session-len 5
   :tap-sound true
   :latency 0})

(def session-settings #{:mode :level :focus :beats :bars :session-len})

(def initial-db
  {:view :practice
   :settings default-settings
   :attempts []
   :sessions []
   :run {:token 0 :phase :ready}
   :width 800})

(rf/reg-cofx :now (fn [cofx _] (assoc cofx :now (audio/now))))

(rf/reg-fx :audio/schedule audio/schedule!)
(rf/reg-fx :audio/stop (fn [_] (audio/stop-all!)))
(rf/reg-fx :knock (fn [_] (audio/knock!)))
(rf/reg-fx :later (fn [items] (doseq [[ms ev] items] (js/setTimeout #(rf/dispatch ev) (max 0 ms)))))
(rf/reg-fx :save (fn [kvs] (doseq [[k v] kvs] (storage/save! k v))))
(rf/reg-fx :html-lang (fn [lang] (set! (.. js/document -documentElement -lang) (name lang))))

;; ---------------------------------------------------------------- derived

(defn beat-s [db] (/ 60 (get-in db [:settings :tempo])))
(defn mode [db] (get-in db [:session :mode]))
(defn dive? [db] (= :dive (mode db)))
(defn lead-in? [db] (or (dive? db) (get-in db [:settings :lead-in])))
(defn relaxed? [db] (and (not (dive? db)) (= :relaxed (get-in db [:settings :timing]))))

(defn hits [db] (rh/hits (get-in db [:ex :bars]) (get-in db [:ex :beats])))

(defn expected
  "Onsets in seconds from the first downbeat."
  [db]
  (let [k (/ (beat-s db) rh/per-beat)]
    (mapv #(* k (:at %)) (hits db))))

(defn total-s [db] (* (beat-s db) (get-in db [:ex :beats]) (count (get-in db [:ex :bars]))))

(defn- ms [secs] (* 1000 secs))

(defn- idle-ms
  "How long without a tap ends the turn: longer than any silence the rhythm has."
  [db]
  (let [e (expected db)
        gap (if (> (count e) 1) (apply max (map - (rest e) e)) 0)]
    (ms (max 2.5 (+ gap (* 2 (beat-s db)))))))

;; ---------------------------------------------------------------- sessions

(defn- load-ex [db]
  (let [{:keys [k items]} (:session db)
        s (:settings db)]
    (assoc db
           :ex {:bars (if items (items k) (rh/exercise rand s)) :beats (:beats s)}
           :run {:token (inc (get-in db [:run :token])) :phase :ready}
           :hl nil :count nil :result nil)))

(defn- new-session [db]
  (let [s (:settings db)
        items (when (= :dive (:mode s)) (rh/tour (:beats s) (:level s)))]
    (-> db
        (assoc :session {:mode (:mode s) :level (:level s) :k 0 :items items :scores {}
                         :n (if items (count items) (:session-len s))})
        load-ex)))

(rf/reg-event-db :init (fn [_ _] (new-session initial-db)))

(rf/reg-event-fx :storage/loaded
  (fn [{:keys [db]} [_ {:keys [settings attempts sessions]}]]
    (let [s (merge default-settings (select-keys settings (keys default-settings)))]
      {:db (new-session (assoc db :settings s :attempts (vec attempts) :sessions (vec sessions)))
       :html-lang (:lang s)})))

(rf/reg-event-db :resize (fn [db [_ w]] (assoc db :width w)))

(rf/reg-event-fx :set-view
  (fn [{:keys [db]} [_ v]]
    (let [running? (#{:listening :recording} (get-in db [:run :phase]))]
      {:db (cond-> (assoc db :view v)
             running? (-> (update-in [:run :token] inc) (assoc-in [:run :phase] :ready) (assoc :hl nil :count nil)))
       :audio/stop nil})))

(rf/reg-event-fx :settings/set
  (fn [{:keys [db]} [_ k v]]
    (let [db (cond-> (assoc-in db [:settings k] v)
               (= k :tolerance) (assoc-in [:settings :tempo] (get-in grade/tolerances [v :tempo]))
               (session-settings k) new-session)]
      (cond-> {:db db :save {:settings (:settings db)}}
        (session-settings k) (assoc :audio/stop nil)
        (= k :lang) (assoc :html-lang v)))))

(rf/reg-event-fx :session/new (fn [{:keys [db]} _] {:db (new-session db) :audio/stop nil}))

;; ---------------------------------------------------------------- playing

(defn- count-fx
  "A bar of clicks from `t`, with the count shown as it goes."
  [tok t beats b now]
  {:clicks (for [k (range beats)] [(+ t (* k b)) (zero? k)])
   :later (concat (for [k (range beats)] [(ms (- (+ t (* k b)) now)) [:count tok (inc k)]])
                  [[(ms (- (+ t (* beats b)) now)) [:count tok nil]]])})

(defn- pattern-fx
  "The exercise on the woodblock from downbeat `t0`, highlighting each note as it sounds."
  [db tok t0 now]
  (let [k (/ (beat-s db) rh/per-beat)
        bar (rh/bar-ticks (get-in db [:ex :beats]))
        hs (hits db)]
    {:blocks (for [h hs] [(+ t0 (* k (:at h))) (zero? (mod (:at h) bar))])
     :later (concat (for [h hs] [(ms (- (+ t0 (* k (:at h))) now)) [:hl tok [(:bar h) (:i h)]]])
                    [[(ms (- (+ t0 (total-s db)) now)) [:hl tok nil]]])}))

(rf/reg-event-db :hl (fn [db [_ tok x]] (if (= tok (get-in db [:run :token])) (assoc db :hl x) db)))
(rf/reg-event-db :count (fn [db [_ tok x]] (if (= tok (get-in db [:run :token])) (assoc db :count x) db)))

(defn- deadline
  "When a turn with a count-in ends at the latest; in relaxed time the student may be slower."
  [db t0]
  (if (relaxed? db)
    (+ t0 (* 2 (total-s db)) (beat-s db))
    (+ t0 (peek (expected db)) (max 0.6 (beat-s db)))))

(rf/reg-event-fx :start
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} _]
    (when (= :ready (get-in db [:run :phase]))
      (let [tok (get-in db [:run :token])
            b (beat-s db)
            t (+ now 0.15)
            ci (if (lead-in? db) (get-in db [:ex :beats]) 0)
            t0 (+ t (* ci b))
            c (count-fx tok t ci b now)
            p (pattern-fx db tok t0 now)
            sched {:clicks (:clicks c) :blocks (:blocks p)}]
        (case (mode db)
          :read {:dispatch [:turn tok]}
          :dive {:db (assoc db :run {:token tok :phase :recording :t0 t0 :taps []})
                 :audio/schedule sched
                 :later (concat (:later c) (:later p) [[(ms (- (deadline db t0) now)) [:finish tok]]])}
          {:db (assoc-in db [:run :phase] :listening)
           :audio/schedule sched
           :later (concat (:later c) (:later p) [[(ms (- (+ t0 (total-s db)) now)) [:turn tok]]])})))))

(rf/reg-event-fx :turn
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} [_ tok]]
    (when (= tok (get-in db [:run :token]))
      (let [b (beat-s db)
            beats (get-in db [:ex :beats])
            t (+ now 0.1)
            t0 (when (lead-in? db) (+ t (* beats b)))
            db (assoc db :run {:token tok :phase :recording :t0 t0 :taps []})]
        (if t0
          (let [c (count-fx tok t beats b now)]
            {:db db
             :audio/schedule {:clicks (:clicks c)}
             :later (conj (vec (:later c)) [(ms (- (deadline db t0) now)) [:finish tok]])})
          {:db db})))))

(rf/reg-event-fx :tap
  (fn [{:keys [db]} [_ t]]
    (let [{:keys [phase token t0 taps]} (:run db)]
      (cond
        (= :running (get-in db [:calib :phase]))
        {:db (update-in db [:calib :taps] conj t)}

        (and (= phase :recording) (or (nil? t0) (> t (- t0 (* 0.5 (beat-s db))))))
        (let [taps (conj taps t)
              ;; With a fixed downbeat the deadline ends the turn; otherwise the last expected tap does.
              by-count? (and (not (dive? db)) (or (nil? t0) (relaxed? db)) (>= (count taps) (count (expected db))))]
          (cond-> {:db (assoc-in db [:run :taps] taps)}
            (not (dive? db)) (assoc :later (cond-> [[(idle-ms db) [:idle token (count taps)]]]
                                             by-count? (conj [400 [:finish token]])))
            (and (get-in db [:settings :tap-sound]) (not (dive? db))) (assoc :knock nil)))))))

(rf/reg-event-fx :idle
  (fn [{:keys [db]} [_ tok n]]
    (let [{:keys [token phase taps]} (:run db)]
      (when (and (= tok token) (= :recording phase) (= n (count taps)))
        {:dispatch [:finish tok]}))))

(defn- cell-tally
  "{cell [hits notes]} for one attempt."
  [hs notes]
  (reduce (fn [m [h {:keys [mark]}]]
            (update m (:cell h) (fn [[hit n]] [(+ (or hit 0) (if (= mark :missed) 0 1)) (inc (or n 0))])))
          {} (map vector hs notes)))

(rf/reg-event-fx :finish
  (fn [{:keys [db]} [_ tok]]
    (let [{:keys [token phase t0 taps]} (:run db)]
      (when (and (= tok token) (= phase :recording))
        (let [s (:settings db)
              lat (/ (:latency s) 1000)
              g (grade/grade {:expected (expected db)
                              :taps (map #(- % lat (or t0 0)) taps)
                              :anchored? (some? t0)
                              :relaxed? (relaxed? db)
                              :beat-s (beat-s db)
                              :tolerance (:tolerance s)})
              {:keys [mode level k]} (:session db)
              attempt (merge (select-keys g [:score :stars :n :perfect :good :missed :mean :spread])
                             {:mode mode :level level :beats (get-in db [:ex :beats])
                              :bars (count (get-in db [:ex :bars])) :tempo (:tempo s)
                              :timing (if (relaxed? db) :relaxed :strict) :anchored (some? t0)
                              :tolerance (:tolerance s) :extra (count (:extra g))
                              :cells (cell-tally (hits db) (:notes g)) :t (js/Date.now)})
              attempts (vec (take-last 3000 (conj (:attempts db) attempt)))]
          {:db (-> db
                   (assoc :result (assoc g :msg (rand-int 4) :anchored? (some? t0) :relaxed? (relaxed? db))
                          :attempts attempts :hl nil :count nil)
                   (assoc-in [:run :phase] :done)
                   (assoc-in [:session :scores k] (:score g)))
           :save {:attempts attempts}})))))

(rf/reg-event-fx :stop
  (fn [{:keys [db]} _]
    {:db (-> db (update-in [:run :token] inc) (assoc-in [:run :phase] :ready) (assoc :hl nil :count nil))
     :audio/stop nil}))

(rf/reg-event-fx :again
  (fn [{:keys [db]} _]
    {:db (-> db (update-in [:run :token] inc) (assoc-in [:run :phase] :ready) (assoc :result nil :hl nil))
     :dispatch [:start]}))

(rf/reg-event-fx :hear
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} _]
    (let [p (pattern-fx db (get-in db [:run :token]) (+ now 0.1) now)]
      {:audio/schedule {:blocks (:blocks p)} :later (:later p)})))

(rf/reg-event-fx :hear-yours
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} _]
    (let [taps (get-in db [:result :taps])
          from (min 0 (or (first taps) 0))]
      {:audio/schedule {:blocks (for [t taps] [(+ now 0.1 (- t from)) false])}})))

(defn- finish-session [db]
  (let [{:keys [mode level scores]} (:session db)
        avg (/ (reduce + (vals scores)) (max 1 (count scores)))
        prev (keep #(when (and (= mode (:mode %)) (= level (:level %))) (:score %)) (:sessions db))
        sess {:mode mode :level level :score avg :n (count scores) :t (js/Date.now)}
        sessions (vec (take-last 500 (conj (:sessions db) sess)))]
    {:db (-> db
             (assoc :sessions sessions)
             (assoc-in [:session :summary] {:score avg :stars (grade/stars avg) :msg (rand-int 4)
                                            :record? (boolean (and (seq prev) (> avg (apply max prev))))}))
     :save {:sessions sessions}}))

(rf/reg-event-fx :next
  (fn [{:keys [db]} _]
    (let [{:keys [k n]} (:session db)]
      (cond-> (if (< (inc k) n)
                {:db (load-ex (assoc-in db [:session :k] (inc k)))}
                (finish-session db))
        true (assoc :audio/stop nil)))))

(rf/reg-event-fx :stats/reset
  (fn [{:keys [db]} _]
    {:db (assoc db :attempts [] :sessions []) :save {:attempts [] :sessions []}}))

;; ---------------------------------------------------------------- calibration

(defn- median [xs]
  (let [v (vec (sort xs)) n (count v)]
    (when (pos? n)
      (if (odd? n) (v (quot n 2)) (/ (+ (v (dec (quot n 2))) (v (quot n 2))) 2)))))

(def calib-clicks 12)
(def calib-beat 0.6)

(rf/reg-event-fx :calib/start
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} _]
    (let [tok (inc (get-in db [:calib :token] 0))
          t (+ now 0.4)
          clicks (for [k (range calib-clicks)] [(+ t (* k calib-beat)) (zero? (mod k 4))])]
      {:db (assoc db :calib {:token tok :phase :running :clicks (mapv first clicks) :taps []})
       :audio/schedule {:clicks clicks}
       :later [[(ms (- (+ t (* calib-clicks calib-beat) 0.3) now)) [:calib/finish tok]]]})))

(rf/reg-event-fx :calib/finish
  (fn [{:keys [db]} [_ tok]]
    (let [{:keys [token phase clicks taps]} (:calib db)]
      (when (and (= tok token) (= phase :running))
        ;; The first bar is for finding the beat; after that, each tap against its nearest click.
        (let [diffs (for [t taps
                          :let [c (apply min-key #(Math/abs (- t %)) clicks)
                                d (- t c)]
                          :when (>= c (clicks 4))
                          :when (< (Math/abs d) 0.25)]
                      d)]
          (if (>= (count diffs) 5)
            (let [lat (Math/round (* 1000 (median diffs)))
                  db (-> db
                         (assoc-in [:settings :latency] lat)
                         (assoc :calib {:token tok :phase :ok :result lat}))]
              {:db db :save {:settings (:settings db)}})
            {:db (assoc db :calib {:token tok :phase :failed})}))))))

;; ---------------------------------------------------------------- keys

(def tap-keys #{" " "j" "f" "k" "d"})

(rf/reg-event-fx :key
  (fn [{:keys [db]} [_ k t]]
    (let [phase (get-in db [:run :phase])
          tapping? (or (= :running (get-in db [:calib :phase]))
                       (and (= :practice (:view db)) (= :recording phase)))]
      (cond
        (and tapping? (tap-keys k)) {:dispatch [:tap t]}
        (not= :practice (:view db)) nil
        (not (#{"Enter" " "} k)) nil
        (get-in db [:session :summary]) {:dispatch [:session/new]}
        (= :ready phase) {:dispatch [:start]}
        (= :done phase) {:dispatch [:next]}))))
