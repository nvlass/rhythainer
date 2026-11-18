(ns rhythainer.grade)

;; Times are seconds. `expected` onsets count from the first downbeat at the set tempo. A fit
;; [offset scale] maps a tap to score time: (t - offset) / scale. Strict with a count-in fits nothing
;; (taps are already relative to the downbeat), strict without one fits the offset, relaxed fits both.

(def tolerances
  "Half-width of the hit window: the larger of `ms` and `frac` of a beat. Easy timing uses `easy`, of a beat."
  {:young {:ms 120 :frac 0.18 :easy 0.4 :tempo 72}
   :kids {:ms 90 :frac 0.12 :easy 0.33 :tempo 84}
   :teens {:ms 60 :frac 0.085 :easy 0.28 :tempo 90}
   :pro {:ms 40 :frac 0.06 :easy 0.2 :tempo 100}})

(defn windows
  "Each onset's window, kept under half the gap to its neighbours so windows never overlap."
  [expected beat-s {:keys [ms frac]}]
  (let [base (max (/ ms 1000) (* frac beat-s))
        n (count expected)]
    (vec (for [i (range n)
               :let [gaps (cond-> []
                            (pos? i) (conj (- (expected i) (expected (dec i))))
                            (< (inc i) n) (conj (- (expected (inc i)) (expected i))))]]
           (if (seq gaps) (min base (* 0.45 (apply min gaps))) base)))))

(defn align
  "Pairs [i j] of note and tap, by edit distance: a match inside its window costs (err / window)²,
   a missed note or an extra tap costs 1. Both inputs sorted."
  [expected ws taps]
  (let [n (count expected)
        m (count taps)
        cost (fn [i j] (let [e (/ (- (taps j) (expected i)) (ws i))]
                         (when (<= (Math/abs e) 1) (* e e))))
        d (vec (reductions
                (fn [prev i]
                  (reduce (fn [row j]
                            (conj row (if (zero? j)
                                        i
                                        (min (inc (prev j))
                                             (inc (row (dec j)))
                                             (if-let [c (cost (dec i) (dec j))] (+ (prev (dec j)) c) ##Inf)))))
                          [] (range (inc m))))
                (vec (range (inc m)))
                (range 1 (inc n))))
        at (fn [i j] (get-in d [i j]))]
    (loop [i n j m out ()]
      (cond (and (zero? i) (zero? j)) (vec out)
            (and (pos? i) (pos? j)
                 (when-let [c (cost (dec i) (dec j))] (== (at i j) (+ (at (dec i) (dec j)) c))))
            (recur (dec i) (dec j) (conj out [(dec i) (dec j)]))
            (and (pos? i) (== (at i j) (inc (at (dec i) j))))
            (recur (dec i) j out)
            :else (recur i (dec j) out)))))

(defn- mean [xs] (when (seq xs) (/ (reduce + xs) (count xs))))

(defn- sd [xs]
  (when (> (count xs) 1)
    (let [m (mean xs)] (Math/sqrt (mean (map #(let [d (- % m)] (* d d)) xs))))))

(defn- lsq
  "Least-squares [offset scale] for t ≈ offset + scale · e over [e t] pairs."
  [pairs]
  (let [mx (mean (map first pairs))
        my (mean (map second pairs))
        sxx (reduce + (map #(let [d (- (first %) mx)] (* d d)) pairs))
        sxy (reduce + (map #(* (- (first %) mx) (- (second %) my)) pairs))
        b (if (pos? sxx) (/ sxy sxx) 1)]
    [(- my (* b mx)) b]))

(defn- sane-scale? [s] (< 0.5 s 2))

(defn- initial-fit [expected taps anchored? relaxed?]
  (cond (and anchored? (not relaxed?)) [0 1]
        (empty? taps) [0 1]
        (and relaxed? (> (count taps) 1) (> (count expected) 1))
        (let [s (/ (- (peek taps) (first taps)) (- (peek expected) (first expected)))
              s (if (sane-scale? s) s 1)]
          [(- (first taps) (* s (first expected))) s])
        :else [(- (first taps) (first expected)) 1]))

(defn- refit [fit pairs expected taps anchored? relaxed?]
  (cond (and anchored? (not relaxed?)) fit
        (not relaxed?) (if (seq pairs) [(mean (for [[i j] pairs] (- (taps j) (expected i)))) 1] fit)
        (>= (count pairs) 2) (let [f (lsq (for [[i j] pairs] [(expected i) (taps j)]))]
                               (if (sane-scale? (second f)) f fit))
        :else fit))

(defn stars [score]
  (cond (>= score 0.9) 3 (>= score 0.7) 2 (>= score 0.45) 1 :else 0))

(defn passed?
  "Good enough to move on in memory mode: every tap landed on a note and at most a quarter of the notes
   are missing. Forgetting a note is forgiven more than tapping in the wrong place."
  [{:keys [n missed extra]}]
  (and (empty? extra) (<= (* 4 missed) n)))

(defn memory-stars
  "Stars for the longest length (bars) held in memory mode."
  [best]
  (cond (>= best 6) 3 (>= best 4) 2 (>= best 2) 1 :else 0))

(defn- long-gaps
  "Mean change, in seconds, of the tapped interval over each gap longer than a beat (rests, long notes):
   negative when the student rushes through them. A rushed rest pushes taps out of their windows, so taps
   pair with notes by order when the counts agree and every matched tap is already in order (a miss and an
   extra elsewhere would shift the pairs between them), otherwise by the alignment. Nil under two gaps."
  [expected s-taps notes beat-s]
  (let [in-order? (and (= (count s-taps) (count expected))
                       (every? (fn [[i {:keys [tap]}]] (or (nil? tap) (= i tap))) (map-indexed vector notes)))
        at (if in-order?
             s-taps
             (into {} (keep-indexed (fn [i {:keys [tap]}] (when tap [i (s-taps tap)])) notes)))
        ds (for [i (range 1 (count expected))
                 :let [a (at (dec i)) b (at i) gap (- (expected i) (expected (dec i)))]
                 :when (and a b (> gap (* 1.01 beat-s)))]
             (- (- b a) gap))]
    (when (>= (count ds) 2) (mean ds))))

(defn- tally
  "Counts, score and timing figures of marked `notes` against `expected` onsets and scored `taps`."
  [{:keys [expected taps notes extra] :as r} beat-s]
  (let [cnt #(count (filter (fn [x] (= % (:mark x))) notes))
        n (count expected)
        score (if (zero? n) 0 (max 0 (min 1 (/ (- (+ (cnt :perfect) (* 0.7 (cnt :good))) (* 0.5 (count extra))) n))))
        errs (keep :err notes)]
    (assoc r
           :n n :perfect (cnt :perfect) :good (cnt :good) :missed (cnt :missed)
           :score score :stars (stars score)
           :mean (mean errs) :spread (sd errs)
           :gaps (long-gaps expected taps notes beat-s))))

(defn- easy-align
  "Pairs [i j] of note and tap judged on the gaps between them, so tempo may drift: from the previous pair,
   the tapped gap over the note's tempo scale `ss` against the notated gap, within the note's window. A skipped
   note or tap costs 1, at most 3 in a row, and |i - j| stays within 8 of the count difference."
  [expected ws ss taps]
  (let [n (count expected)
        m (count taps)
        band (+ 8 (Math/abs (- n m)))
        idx (fn [i j] (+ (* i m) j))
        best (js/Float64Array. (* n m))
        prev (js/Int32Array. (* n m))
        cost (fn [i' j' i j]
               (let [e (/ (- (/ (- (taps j) (taps j')) (ss i)) (- (expected i) (expected i'))) (ws i))]
                 (when (<= (Math/abs e) 1) (* e e))))]
    (doseq [i (range n) j (range m)
            :let [k (idx i j)]]
      (aset best k (if (> (Math/abs (- i j)) band) ##Inf (+ i j)))
      (aset prev k -1)
      (when (<= (Math/abs (- i j)) band)
        (doseq [i' (range (max 0 (- i 4)) i)
                j' (range (max 0 (- j 4)) j)
                :let [b (aget best (idx i' j'))]
                :when (< b ##Inf)
                :let [c (cost i' j' i j)]
                :when c
                :let [v (+ b (- i i' 1) (- j j' 1) c)]
                :when (< v (aget best k))]
          (aset best k v)
          (aset prev k (idx i' j')))))
    (let [[_ end] (reduce (fn [[c _ :as acc] k]
                            (let [v (+ (aget best k) (- n (quot k m) 1) (- m (mod k m) 1))]
                              (if (< v c) [v k] acc)))
                          [(+ n m) -1] (range (* n m)))]
      (loop [k end out ()]
        (if (neg? k) (vec out) (recur (aget prev k) (conj out [(quot k m) (mod k m)])))))))

(defn- local-scales
  "Each note's tempo scale, from the pairs up to 4 either side of the last pair at or before it."
  [expected taps pairs s0]
  (let [k (count pairs)
        sc (vec (for [q (range k)
                      :let [[i0 j0] (pairs (max 0 (- q 4)))
                            [i1 j1] (pairs (min (dec k) (+ q 4)))
                            s (when (> i1 i0) (/ (- (taps j1) (taps j0)) (- (expected i1) (expected i0))))]]
                  (if (and s (sane-scale? s)) s s0)))
        by-note (zipmap (map first pairs) sc)]
    (vec (rest (reductions (fn [s i] (get by-note i s)) (if (seq sc) (sc 0) s0) (range (count expected)))))))

(defn- grade-easy
  "Like `grade`, but each note is judged by the gap from the previous matched one at the local tempo, and
   windows are `easy` of a beat. Taps are shown at their note plus that error."
  [{:keys [expected taps beat-s tolerance]}]
  (let [n (count expected)
        ws (windows expected beat-s {:ms 0 :frac (:easy (tolerances tolerance))})
        s0 (let [s (when (and (> (count taps) 1) (> n 1))
                     (/ (- (peek taps) (first taps)) (- (peek expected) (first expected))))]
             (if (and s (sane-scale? s)) s 1))
        ss (local-scales expected taps (easy-align expected ws (vec (repeat n s0)) taps) s0)
        pairs (easy-align expected ws ss taps)
        errs (into {} (map (fn [prev [i j]]
                             [i [j (if-let [[i' j'] prev]
                                     (- (/ (- (taps j) (taps j')) (ss i)) (- (expected i) (expected i')))
                                     0)]])
                           (cons nil pairs) pairs))
        notes (vec (for [i (range n)]
                     (if-let [[j err] (errs i)]
                       {:err err :tap j :mark (if (<= (Math/abs err) (/ (ws i) 2)) :perfect :good)}
                       {:mark :missed})))
        shown (into {} (for [[i [j err]] errs] [j (+ (expected i) err)]))
        used (sort (keys shown))
        place (fn [j]
                (or (shown j)
                    (let [a (last (filter #(< % j) used))
                          b (first (filter #(> % j) used))]
                      (cond (and a b) (+ (shown a) (* (- (taps j) (taps a)) (/ (- (shown b) (shown a)) (- (taps b) (taps a)))))
                            a (+ (shown a) (/ (- (taps j) (taps a)) s0))
                            b (- (shown b) (/ (- (taps b) (taps j)) s0))
                            :else (/ (- (taps j) (first taps)) s0)))))
        fit (let [f (when (>= (count pairs) 2) (lsq (for [[i j] pairs] [(expected i) (taps j)])))]
              (if (and f (sane-scale? (second f))) f [0 s0]))
        gaps (keep (fn [[[i' _] [i _]]]
                     (when (and (= i' (dec i)) (> (- (expected i) (expected i')) (* 1.01 beat-s)))
                       (second (errs i))))
                   (partition 2 1 pairs))]
    (assoc (tally {:expected expected :notes notes :taps (mapv place (range (count taps)))
                   :extra (vec (remove (set used) (range (count taps)))) :windows ws :fit fit}
                  beat-s)
           :gaps (when (>= (count gaps) 2) (mean gaps)))))

(defn grade
  "Marks per note (:perfect inside half the window, :good inside it, :missed), extra taps, the fit,
   a 0–1 score and timing figures in seconds (mean and spread of the errors, `long-gaps`).
   With `easy?`, `grade-easy`."
  [{:keys [expected taps anchored? relaxed? easy? beat-s tolerance] :as opts}]
  (let [expected (vec expected)
        taps (vec (sort taps))
        ws (windows expected beat-s (tolerances tolerance))
        to-score (fn [[off s]] (mapv #(/ (- % off) s) taps))]
    (if easy?
      (grade-easy (assoc opts :expected expected :taps taps))
      (let [fit (loop [fit (initial-fit expected taps anchored? relaxed?) k 0]
                  (let [fit' (refit fit (align expected ws (to-score fit)) expected taps anchored? relaxed?)]
                    (if (or (= fit fit') (= k 5)) fit' (recur fit' (inc k)))))
            s-taps (to-score fit)
            pairs (align expected ws s-taps)
            matched (into {} pairs)
            used (set (map second pairs))
            notes (vec (for [i (range (count expected))]
                         (if-let [j (matched i)]
                           (let [err (- (s-taps j) (expected i))]
                             {:err err :tap j :mark (if (<= (Math/abs err) (/ (ws i) 2)) :perfect :good)})
                           {:mark :missed})))]
        (tally {:expected expected :notes notes :taps s-taps :extra (vec (remove used (range (count taps))))
                :windows ws :fit fit}
               beat-s)))))

(defn bar-taps
  "The taps that belong to a bar with onsets `on`: from halfway back to `prev`, the previous bar's last onset,
   to halfway on to `next`, the next bar's first (either may be nil)."
  [taps prev on next]
  (let [lo (if prev (/ (+ prev (first on)) 2) ##-Inf)
        hi (if next (/ (+ (peek on) next) 2) ##Inf)]
    (filterv #(and (>= % lo) (< % hi)) taps)))

(defn combine
  "One result from results graded one after another (each bar of a stream), on the same clock."
  [rs beat-s]
  (let [offs (reductions + 0 (map (comp count :taps) rs))
        shift (fn [k r] (map #(+ % (nth offs k)) (:extra r)))]
    (tally {:expected (vec (mapcat :expected rs))
            :taps (vec (mapcat :taps rs))
            :windows (vec (mapcat :windows rs))
            :notes (vec (mapcat (fn [k r] (map #(cond-> % (:tap %) (update :tap + (nth offs k))) (:notes r)))
                                (range) rs))
            :extra (vec (mapcat shift (range) rs))}
           beat-s)))

(defn clean? [{:keys [missed extra]}] (and (zero? missed) (empty? extra)))
