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
   :memory-kind :grow
   :stream-len 16
   :stream-click true
   :tap-sound true
   :latency 0})

(def session-settings #{:mode :level :focus :beats :bars :session-len :memory-kind :stream-len :stream-click})

(def memory-tries 2)
(def memory-cap {:grow 16 :span 8})

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
(rf/reg-fx :knock #(audio/knock! %))
(rf/reg-fx :later (fn [items] (doseq [[ms ev] items] (js/setTimeout #(rf/dispatch ev) (max 0 ms)))))
(rf/reg-fx :save (fn [kvs] (doseq [[k v] kvs] (storage/save! k v))))
(rf/reg-fx :html-lang (fn [lang] (set! (.. js/document -documentElement -lang) (name lang))))

;; ---------------------------------------------------------------- derived

(defn beat-s [db] (/ 60 (get-in db [:settings :tempo])))
(defn mode [db] (get-in db [:session :mode]))
(defn dive? [db] (= :dive (mode db)))
(defn stream? [db] (= :stream (mode db)))
(defn along?
  "Modes where the student taps while time runs on, with the downbeat fixed by the count-in."
  [db]
  (or (dive? db) (stream? db)))
(defn lead-in? [db] (or (along? db) (get-in db [:settings :lead-in])))
(defn relaxed? [db] (and (not (along? db)) (= :relaxed (get-in db [:settings :timing]))))

(defn hits [db] (rh/hits (get-in db [:ex :bars]) (get-in db [:ex :beats])))

(defn expected
  "Onsets in seconds from the first downbeat."
  [db]
  (let [k (/ (beat-s db) rh/per-beat)]
    (mapv #(* k (:at %)) (hits db))))

(defn bar-s [db] (* (beat-s db) (get-in db [:ex :beats])))

(defn bar-onsets
  "Onsets of bar `k` in seconds from the first downbeat."
  [db k]
  (let [sec (/ (beat-s db) rh/per-beat)]
    (mapv #(+ (* k (bar-s db)) (* sec (:at %))) (rh/hits [(get-in db [:ex :bars k])] (get-in db [:ex :beats])))))

(defn stream-len [db] (let [l (get-in db [:settings :stream-len])] (when (number? l) l)))

(defn total-s [db] (* (beat-s db) (get-in db [:ex :beats]) (count (get-in db [:ex :bars]))))

(defn- ms [secs] (* 1000 secs))

(defn- idle-ms
  "How long without a tap ends the turn: longer than any silence the rhythm has."
  [db]
  (let [e (expected db)
        gap (if (> (count e) 1) (apply max (map - (rest e) e)) 0)]
    (ms (max 2.5 (+ gap (* 2 (beat-s db)))))))

;; ---------------------------------------------------------------- sessions

(defn- another-bar [db first?]
  (let [{:keys [beats level focus]} (:settings db)]
    (rh/gen-bar rand beats level focus first?)))

(defn- load-ex
  "A stream starts with the bar to play and the one to read ahead; `:stream/bar` adds the rest as it goes."
  [db]
  (let [{:keys [mode k items kind len grown]} (:session db)
        s (:settings db)]
    (assoc db
           :ex {:bars (cond items (items k)
                            (= mode :memory) (if (= kind :grow) grown (rh/exercise rand (assoc s :bars len)))
                            (= mode :stream) [(another-bar db true) (another-bar db false)]
                            :else (rh/exercise rand s))
                :beats (:beats s)}
           :run {:token (inc (get-in db [:run :token])) :phase :ready}
           :stream {:k 0 :results [] :streak 0 :best 0 :silent 0}
           :hl nil :count nil :beat nil :result nil)))

(defn- new-session
  "Memory mode has no fixed length: `:len` grows while the student keeps up, `:tries` are left at
   that length, `:grown` is the rhythm the one-bar-at-a-time variant keeps extending."
  [db]
  (let [s (:settings db)
        items (when (= :dive (:mode s)) (rh/tour (:beats s) (:level s)))
        memory? (= :memory (:mode s))]
    (-> db
        (assoc :session (cond-> {:mode (:mode s) :level (:level s) :k 0 :items items :scores {}
                                 :n (cond items (count items) (#{:memory :stream} (:mode s)) nil :else (:session-len s))}
                          memory? (assoc :kind (:memory-kind s) :len 1 :tries memory-tries :best 0
                                         :grown (when (= :grow (:memory-kind s)) [(another-bar db true)]))))
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

(defn- pulse-fx
  "A bar of beats from `t`: shown on screen, and heard unless the metronome is off."
  [db tok t now]
  (let [b (beat-s db)
        ts (for [j (range (get-in db [:ex :beats]))] [j (+ t (* j b))])]
    {:clicks (when (get-in db [:settings :stream-click]) (for [[j u] ts] [u (zero? j)]))
     :later (for [[j u] ts] [(ms (- u now)) [:beat tok j]])}))

(rf/reg-event-db :beat (fn [db [_ tok j]] (if (= tok (get-in db [:run :token])) (assoc db :beat j) db)))
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
          :stream (let [p (pulse-fx db tok t0 now)]
                    {:db (assoc db :run {:token tok :phase :recording :t0 t0 :taps []})
                     :audio/schedule {:clicks (concat (:clicks c) (:clicks p))}
                     :later (concat (:later c) (:later p) [[(ms (- t0 now)) [:stream/bar tok 0]]])})
          :dive {:db (assoc db :run {:token tok :phase :recording :t0 t0 :taps []})
                 :audio/schedule sched
                 :later (concat (:later c) (:later p) [[(ms (- (deadline db t0) now)) [:finish tok]]])}
          ;; The turn's count-in is scheduled with the rhythm, on the bar right after it, so the pulse
          ;; carries on unbroken; starting it from a timer put it ~100 ms off the beat just heard.
          (let [end (+ t0 (total-s db))
                c2 (count-fx tok end ci b now)
                turn-t0 (when (pos? ci) (+ end (* ci b)))]
            {:db (assoc-in db [:run :phase] :listening)
             :audio/schedule (update sched :clicks concat (:clicks c2))
             :later (concat (:later c) (:later p) (:later c2) [[(ms (- end now)) [:turn tok turn-t0]]])}))))))

(rf/reg-event-fx :turn
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} [_ tok t0]]
    (when (= tok (get-in db [:run :token]))
      (let [b (beat-s db)
            beats (get-in db [:ex :beats])
            fresh? (and (nil? t0) (lead-in? db))
            t (+ now 0.1)
            t0 (if fresh? (+ t (* beats b)) t0)
            db (assoc db :run {:token tok :phase :recording :t0 t0 :taps []})]
        (cond-> {:db db}
          fresh? (merge (let [c (count-fx tok t beats b now)]
                          {:audio/schedule {:clicks (:clicks c)} :later (:later c)}))
          t0 (update :later (fnil conj []) [(ms (- (deadline db t0) now)) [:finish tok]]))))))

(rf/reg-event-fx :tap
  (fn [{:keys [db]} [_ t]]
    (let [{:keys [phase token t0 taps]} (:run db)]
      (cond
        (= :running (get-in db [:calib :phase]))
        {:db (update-in db [:calib :taps] conj t)}

        ;; A second tap within 60 ms is a bounce, not a note (a 16th at 160 bpm is 94 ms).
        (and (= phase :recording)
             (or (nil? t0) (> t (- t0 (* 0.5 (beat-s db)))))
             (or (empty? taps) (> (- t (peek taps)) 0.06)))
        (let [taps (conj taps t)
              ;; With a fixed downbeat the deadline ends the turn; otherwise the last expected tap does.
              by-count? (and (not (along? db)) (or (nil? t0) (relaxed? db)) (>= (count taps) (count (expected db))))]
          (cond-> {:db (assoc-in db [:run :taps] taps)}
            (not (along? db)) (assoc :later (cond-> [[(idle-ms db) [:idle token (count taps)]]]
                                             by-count? (conj [400 [:finish token]])))
            (get-in db [:settings :tap-sound]) (assoc :knock (if (dive? db) :tick :wood))))))))

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
                             {:mode mode :kind (get-in db [:session :kind]) :level level :beats (get-in db [:ex :beats])
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
    (if (and (stream? db) (= :recording (get-in db [:run :phase])))
      {:dispatch [:stream/end (get-in db [:run :token])]}
      {:db (-> db (update-in [:run :token] inc) (assoc-in [:run :phase] :ready) (assoc :hl nil :count nil :beat nil))
       :audio/stop nil})))

(rf/reg-event-fx :hidden
  (fn [{:keys [db]} _]
    (when (#{:listening :recording} (get-in db [:run :phase])) {:dispatch [:stop]})))

(rf/reg-event-fx :again
  (fn [{:keys [db]} _]
    {:db (if (stream? db)
           (load-ex db)
           (-> db (update-in [:run :token] inc) (assoc-in [:run :phase] :ready) (assoc :result nil :hl nil)))
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

(defn- finish-memory [db]
  (let [{:keys [kind level best]} (:session db)
        prev (keep #(when (and (= :memory (:mode %)) (= kind (:kind %)) (= level (:level %))) (:best %))
                   (:sessions db))
        sess {:mode :memory :kind kind :level level :best best :t (js/Date.now)}
        sessions (vec (take-last 500 (conj (:sessions db) sess)))]
    {:db (-> db
             (assoc :sessions sessions)
             (assoc-in [:session :summary] {:best best :stars (grade/memory-stars best) :msg (rand-int 4)
                                            :record? (boolean (and (seq prev) (> best (apply max prev))))}))
     :save {:sessions sessions}}))

(defn memory-step
  "What Next does in memory mode: :longer after a pass, :retry with a try left, :end otherwise."
  [{:keys [session result]}]
  (let [{:keys [kind len tries]} session]
    (cond (and (grade/passed? result) (< len (memory-cap kind))) :longer
          (grade/passed? result) :end
          (> tries 1) :retry
          :else :end)))

(defn- memory-next [db]
  (let [{:keys [kind len best]} (:session db)
        passed? (grade/passed? (:result db))
        db (cond-> (update-in db [:session :k] inc)
             passed? (assoc-in [:session :best] (max best len)))]
    (case (memory-step db)
      :longer (load-ex (update db :session #(cond-> (assoc % :len (inc len) :tries memory-tries)
                                              (= kind :grow) (update :grown conj (another-bar db false)))))
      :retry (load-ex (update-in db [:session :tries] dec))
      :end db)))

(rf/reg-event-fx :next
  (fn [{:keys [db]} _]
    (let [{:keys [mode k n]} (:session db)]
      (cond-> (cond (= mode :stream) {:db (load-ex db)}
                    (= mode :memory) (let [step (memory-step db) db (memory-next db)]
                                       (if (= step :end) (finish-memory db) {:db db}))
                    (< (inc k) n) {:db (load-ex (assoc-in db [:session :k] (inc k)))}
                    :else (finish-session db))
        true (assoc :audio/stop nil)))))

(rf/reg-event-fx :stream/bar
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} [_ tok k]]
    (let [{:keys [token phase t0]} (:run db)]
      (when (and (= tok token) (= phase :recording))
        (let [len (stream-len db)
              last? (and len (= k (dec len)))
              db (cond-> (assoc-in db [:stream :k] k)
                   (or (nil? len) (< (+ k 2) len)) (update-in [:ex :bars] conj (another-bar db false)))
              start (+ t0 (* (inc k) (bar-s db)))
              ;; A bar is graded once its last note's taps can't still come: halfway to the next bar's first.
              hi (if last?
                   (+ t0 (* (inc k) (bar-s db)) (* 0.5 (beat-s db)))
                   (+ t0 (/ (+ (peek (bar-onsets db k)) (first (bar-onsets db (inc k)))) 2)))
              grade-later [(ms (- (+ hi 0.15) now)) [:stream/grade tok k]]]
          (if last?
            {:db db :later [grade-later]}
            (let [p (pulse-fx db tok start now)]
              {:db db
               :audio/schedule {:clicks (:clicks p)}
               :later (concat (:later p) [grade-later [(ms (- start now)) [:stream/bar tok (inc k)]]])})))))))

(def stream-silence
  "Bars without a tap that end a stream: the student has stopped."
  2)

(rf/reg-event-fx :stream/grade
  (fn [{:keys [db]} [_ tok k]]
    (let [{:keys [token phase t0 taps]} (:run db)
          {:keys [results streak best silent]} (:stream db)]
      (when (and (= tok token) (= phase :recording) (= k (count results)))
        (let [s (:settings db)
              lat (/ (:latency s) 1000)
              on (bar-onsets db k)
              mine (grade/bar-taps (mapv #(- % lat t0) taps)
                                   (when (pos? k) (peek (bar-onsets db (dec k))))
                                   on
                                   (when (get-in db [:ex :bars (inc k)]) (first (bar-onsets db (inc k)))))
              g (grade/grade {:expected on :taps mine :anchored? true :relaxed? false
                              :beat-s (beat-s db) :tolerance (:tolerance s)})
              streak (if (grade/clean? g) (inc streak) 0)
              silent (if (empty? mine) (inc silent) 0)
              len (stream-len db)]
          (cond-> {:db (update db :stream assoc :results (conj results g) :streak streak
                               :best (max best streak) :silent silent)}
            (or (>= silent stream-silence) (and len (= k (dec len))))
            (assoc :dispatch [:stream/end tok])))))))

(rf/reg-event-fx :stream/end
  (fn [{:keys [db]} [_ tok]]
    (let [{:keys [token phase]} (:run db)
          {:keys [silent best]} (:stream db)
          lost? (>= silent stream-silence)
          ;; Bars after the student stopped tapping aren't theirs to be graded on.
          results (cond-> (get-in db [:stream :results]) lost? (->> (drop-last silent) vec))
          db (-> db (update-in [:run :token] inc) (assoc :hl nil :count nil :beat nil))]
      (when (and (= tok token) (= phase :recording))
        (if (empty? results)
          {:db (-> db (assoc-in [:run :phase] :ready) (assoc-in [:stream :lost?] lost?)) :audio/stop nil}
          (let [s (:settings db)
                g (grade/combine results (beat-s db))
                level (get-in db [:session :level])
                bars (count results)
                prev (keep #(when (and (= :stream (:mode %)) (= level (:level %))) (:streak %)) (:sessions db))
                attempt (merge (select-keys g [:score :stars :n :perfect :good :missed :mean :spread])
                               {:mode :stream :level level :beats (get-in db [:ex :beats]) :bars bars :tempo (:tempo s)
                                :timing :strict :anchored true :tolerance (:tolerance s) :extra (count (:extra g))
                                :streak best :cells (cell-tally (hits db) (:notes g)) :t (js/Date.now)})
                attempts (vec (take-last 3000 (conj (:attempts db) attempt)))
                sessions (vec (take-last 500 (conj (:sessions db) {:mode :stream :level level :score (:score g)
                                                                    :streak best :bars bars :t (js/Date.now)})))]
            {:db (-> db
                     (assoc :result (assoc g :msg (rand-int 4) :anchored? true :lost? lost? :bars bars :streak best
                                           :clean (count (filter grade/clean? results))
                                           :bar-stars (mapv :stars results)
                                           :record? (boolean (and (seq prev) (> best (apply max prev)))))
                            :attempts attempts :sessions sessions)
                     (assoc-in [:run :phase] :done))
             :audio/stop nil
             :save {:attempts attempts :sessions sessions}}))))))

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
(def calib-lead
  "Clicks counted down before the ones that are measured."
  4)

(rf/reg-event-fx :calib/start
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} _]
    (let [tok (inc (get-in db [:calib :token] 0))
          t (+ now 0.4)
          clicks (for [k (range calib-clicks)] [(+ t (* k calib-beat)) (zero? (mod k 4))])]
      {:db (assoc db :calib {:token tok :phase :running :clicks (mapv first clicks) :taps [] :count nil})
       :audio/schedule {:clicks clicks}
       :later (concat (for [k (range (inc calib-lead))]
                        [(ms (- (+ t (* k calib-beat)) now)) [:calib/count tok (when (< k calib-lead) (- calib-lead k))]])
                      [[(ms (- (+ t (* calib-clicks calib-beat) 0.3) now)) [:calib/finish tok]]])})))

(rf/reg-event-db :calib/count
  (fn [db [_ tok n]] (if (= tok (get-in db [:calib :token])) (assoc-in db [:calib :count] n) db)))

(rf/reg-event-fx :calib/finish
  (fn [{:keys [db]} [_ tok]]
    (let [{:keys [token phase clicks taps]} (:calib db)]
      (when (and (= tok token) (= phase :running))
        ;; The countdown is for finding the beat; after it, each tap against its nearest click.
        (let [diffs (for [t taps
                          :let [c (apply min-key #(Math/abs (- t %)) clicks)
                                d (- t c)]
                          :when (>= c (clicks calib-lead))
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
        (and (stream? db) (= :recording phase) (#{"Enter" "Escape"} k)) {:dispatch [:stop]}
        (and tapping? (tap-keys k)) {:dispatch [:tap t]}
        (not= :practice (:view db)) nil
        (not (#{"Enter" " "} k)) nil
        (get-in db [:session :summary]) {:dispatch [:session/new]}
        (= :ready phase) {:dispatch [:start]}
        (= :done phase) {:dispatch [:next]}))))
