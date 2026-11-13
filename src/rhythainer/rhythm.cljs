(ns rhythainer.rhythm)

;; Durations are ticks, 48 to the quarter, so triplets and (later) 32nds stay whole numbers.
;; An event is {:v value :dot :trip :rest :cell id}. A bar is a vector of events.

(def per-beat 48)

(def values {:whole 192 :half 96 :quarter 48 :eighth 24 :16th 12})

(defn ticks [{:keys [v dot trip]}]
  (let [t (values v)]
    (cond dot (quot (* 3 t) 2)
          trip (quot (* 2 t) 3)
          :else t)))

(defn bar-ticks [beats] (* beats per-beat))

(defn total [evs] (reduce + (map ticks evs)))

(defn onsets [bar] (vec (butlast (reductions + 0 (map ticks bar)))))

;; ---------------------------------------------------------------- cells

(defn- ev [v & flags] (merge {:v v} (zipmap flags (repeat true))))

(def cells
  "Rhythm cells in teaching order: the level that introduces them and where in the bar they may start
   (:strong = beat 1 or 3 in 4/4, any beat otherwise; :downbeat = beat 1)."
  [{:id :q :level 1 :notes [(ev :quarter)]}
   {:id :q-rest :level 1 :notes [(ev :quarter :rest)]}
   {:id :h :level 2 :notes [(ev :half)] :starts :strong}
   {:id :h-rest :level 2 :notes [(ev :half :rest)] :starts :strong}
   {:id :h-dot :level 2 :notes [(ev :half :dot)] :starts :downbeat}
   {:id :w :level 2 :notes [(ev :whole)] :starts :downbeat}
   {:id :ee :level 3 :notes [(ev :eighth) (ev :eighth)]}
   {:id :e-rest-e :level 4 :notes [(ev :eighth :rest) (ev :eighth)]}
   {:id :e-e-rest :level 4 :notes [(ev :eighth) (ev :eighth :rest)]}
   {:id :q-dot-e :level 5 :notes [(ev :quarter :dot) (ev :eighth)] :starts :strong}
   {:id :e-q-e :level 6 :notes [(ev :eighth) (ev :quarter) (ev :eighth)] :starts :strong}
   {:id :ssss :level 7 :notes [(ev :16th) (ev :16th) (ev :16th) (ev :16th)]}
   {:id :e-ss :level 7 :notes [(ev :eighth) (ev :16th) (ev :16th)]}
   {:id :ss-e :level 7 :notes [(ev :16th) (ev :16th) (ev :eighth)]}
   {:id :e-dot-s :level 8 :notes [(ev :eighth :dot) (ev :16th)]}
   {:id :s-e-s :level 8 :notes [(ev :16th) (ev :eighth) (ev :16th)]}
   {:id :trip :level 9 :notes [(ev :eighth :trip) (ev :eighth :trip) (ev :eighth :trip)]}
   {:id :q-trip :level 9 :notes [(ev :quarter :trip) (ev :quarter :trip) (ev :quarter :trip)] :starts :strong}])

(def max-level 9)

(def cell-by-id (into {} (map (juxt :id identity) cells)))

(defn level-cells [level] (filter #(= level (:level %)) cells))

(defn- rest-cell? [{:keys [notes]}] (:rest (first notes)))

(defn- fits? [{:keys [notes starts]} beats at end]
  (and (<= (+ at (total notes)) end)
       (case starts
         :strong (zero? (mod at (if (= beats 4) (* 2 per-beat) per-beat)))
         :downbeat (zero? at)
         true)))

(defn- tag [c] (mapv #(assoc % :cell (:id c)) (:notes c)))

;; ---------------------------------------------------------------- generation

(defn- pick [rnd pairs]
  (let [sum (reduce + (map second pairs))
        r (* (rnd) sum)]
    (loop [[[x w] & more] pairs acc 0]
      (if (or (empty? more) (< r (+ acc w))) x (recur more (+ acc w))))))

(defn- weight [level focus {cl :level :as c}]
  (* (if (rest-cell? c) 0.5 1)
     (cond (= cl level) (if focus 8 4)
           focus 1
           :else 2)))

(defn- fill
  "Cells from `at` up to `end` ticks; `first?` keeps a rest off the very first beat."
  [rnd beats level focus at end first?]
  (loop [at at out []]
    (if (>= at end)
      out
      (let [ok (for [c cells
                     :when (<= (:level c) level)
                     :when (fits? c beats at end)
                     :when (not (and first? (zero? at) (rest-cell? c)))]
                 [c (weight level focus c)])
            c (pick rnd ok)]
        (recur (+ at (total (:notes c))) (into out (tag c)))))))

(defn- ok-bar?
  "At least one note, and at most half the bar silent."
  [bar beats]
  (and (some (complement :rest) bar)
       (<= (* 2 (total (filter :rest bar))) (bar-ticks beats))))

(defn gen-bar
  "One bar; `first?` keeps a rest off its first beat."
  [rnd beats level focus first?]
  (loop [k 0]
    (let [bar (fill rnd beats level focus 0 (bar-ticks beats) first?)]
      (if (or (ok-bar? bar beats) (> k 20)) bar (recur (inc k))))))

(defn- final-bar
  "Once there are half notes, the last bar ends on a long note."
  [rnd beats level focus first?]
  (let [bar (bar-ticks beats)]
    (if (or (< level 2) (= beats 2))
      (gen-bar rnd beats level focus first?)
      (let [long-end (fn [len] (into (fill rnd beats level focus 0 (- bar len) first?)
                                     (tag (cell-by-id (if (= len 96) :h :h-dot)))))]
        (pick rnd (cond-> [[(long-end 96) 3]]
                    (= beats 3) (conj [(tag (cell-by-id :h-dot)) 1])
                    (= beats 4) (conj [(tag (cell-by-id :w)) 1])))))))

(defn exercise
  "`n` bars of `beats` quarters drawn from the cells up to `level`; `focus` favours the level's own."
  [rnd {:keys [beats bars level focus]}]
  (let [n (max 1 bars)]
    (if (= n 1)
      [(gen-bar rnd beats level focus true)]
      (-> (into [(gen-bar rnd beats level focus true)]
                (repeatedly (- n 2) #(gen-bar rnd beats level focus false)))
          (conj (final-bar rnd beats level focus false))))))

(defn tour
  "Quick dive: for every cell up to `level`, a bar of it repeated, played twice. Quarters fill where it
   doesn't fit, and a cell that is only a rest always follows a note."
  [beats level]
  (vec (for [c cells
             :when (<= (:level c) level)
             :let [silent? (every? :rest (:notes c))
                   end (bar-ticks beats)
                   q (assoc (ev :quarter) :cell :q)
                   bar (loop [at 0 out []]
                         (cond (>= at end) out
                               (and (fits? c beats at end) (not (and silent? (or (empty? out) (:rest (peek out))))))
                               (recur (+ at (total (:notes c))) (into out (tag c)))
                               :else (recur (+ at per-beat) (conj out q))))]
             :when (some #(= (:id c) (:cell %)) bar)]
         [bar bar])))

(defn hits
  "The sounding notes of `bars`: {:at ticks from the start :bar :i :cell}."
  [bars beats]
  (vec (for [[b bar] (map-indexed vector bars)
             [i o] (map-indexed vector (onsets bar))
             :let [e (bar i)]
             :when (not (:rest e))]
         {:at (+ (* b (bar-ticks beats)) o) :bar b :i i :cell (:cell e)})))

;; ---------------------------------------------------------------- notation groups

(defn- beamable? [e] (and (#{:eighth :16th} (:v e)) (not (:rest e))))

(defn beam-groups
  "Index runs of eighths and shorter that share a beat; runs of one get flags instead."
  [bar]
  (let [on (onsets bar)]
    (->> (range (count bar))
         (partition-by (fn [i] (if (beamable? (bar i)) [:beam (quot (on i) per-beat)] [:single i])))
         (filter #(beamable? (bar (first %))))
         (map vec))))

(defn triplet-groups
  "Index runs of triplet events, cut where they complete a group (two of their plain value)."
  [bar]
  (loop [i 0 cur [] acc 0 out []]
    (if (= i (count bar))
      (cond-> out (seq cur) (conj cur))
      (let [e (bar i)]
        (if (:trip e)
          (let [acc (+ acc (ticks e))
                cur (conj cur i)
                group (* 2 (values (:v (bar (first cur)))))]
            (if (zero? (mod acc group))
              (recur (inc i) [] 0 (conj out cur))
              (recur (inc i) cur acc out)))
          (recur (inc i) [] 0 (cond-> out (seq cur) (conj cur))))))))

;; ---------------------------------------------------------------- syllables

(def ^:private takadimi {0 "ta" 12 "ka" 24 "di" 36 "mi" 16 "ki" 32 "da"})
(def ^:private counting {12 "e" 24 "&" 36 "a" 16 "trip" 32 "let"})

(defn- kodaly [e pos trip-k]
  (cond trip-k (["tri" "o" "la"] (mod trip-k 3))
        (= :16th (:v e)) (if (#{0 24} pos) "ti" "ri")
        (and (= :eighth (:v e)) (:dot e)) "tim"
        (= :eighth (:v e)) "ti"
        (and (= :quarter (:v e)) (:dot e)) "ta-i"
        (= :half (:v e)) (if (:dot e) "ta-a-a" "ta-a")
        (= :whole (:v e)) "ta-a-a-a"
        :else "ta"))

(defn syllables
  "One syllable per event of `bar` (nil for rests) in `system`: :takadimi, :kodaly or :counting."
  [system bar]
  (let [on (onsets bar)
        trip-k (into {} (for [g (triplet-groups bar) [k i] (map-indexed vector g)] [i k]))]
    (vec (for [i (range (count bar))
               :let [e (bar i) o (on i) pos (mod o per-beat)]]
           (when-not (:rest e)
             (case system
               :takadimi (takadimi pos "ta")
               :kodaly (kodaly e pos (trip-k i))
               :counting (if (zero? pos) (str (inc (quot o per-beat))) (counting pos "·"))
               nil))))))
