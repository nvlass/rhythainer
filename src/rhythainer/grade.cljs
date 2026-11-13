(ns rhythainer.grade)

;; Times are seconds. `expected` onsets count from the first downbeat at the set tempo. A fit
;; [offset scale] maps a tap to score time: (t - offset) / scale. Strict with a count-in fits nothing
;; (taps are already relative to the downbeat), strict without one fits the offset, relaxed fits both.

(def tolerances
  "Half-width of the hit window: the larger of `ms` and `frac` of a beat."
  {:young {:ms 120 :frac 0.18 :tempo 72}
   :kids {:ms 90 :frac 0.12 :tempo 84}
   :teens {:ms 70 :frac 0.10 :tempo 90}
   :pro {:ms 40 :frac 0.06 :tempo 100}})

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

(defn grade
  "Marks per note (:perfect inside half the window, :good inside it, :missed), extra taps, the fit,
   a 0–1 score and timing figures (mean and spread of the errors, in seconds)."
  [{:keys [expected taps anchored? relaxed? beat-s tolerance]}]
  (let [expected (vec expected)
        taps (vec (sort taps))
        ws (windows expected beat-s (tolerances tolerance))
        to-score (fn [[off s]] (mapv #(/ (- % off) s) taps))
        fit (loop [fit (initial-fit expected taps anchored? relaxed?) k 0]
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
                       {:mark :missed})))
        extra (vec (remove used (range (count taps))))
        cnt #(count (filter (fn [x] (= % (:mark x))) notes))
        n (count expected)
        score (if (zero? n) 0 (max 0 (min 1 (/ (- (+ (cnt :perfect) (* 0.7 (cnt :good))) (* 0.5 (count extra))) n))))
        errs (keep :err notes)]
    {:notes notes :taps s-taps :extra extra :windows ws :fit fit
     :n n :perfect (cnt :perfect) :good (cnt :good) :missed (cnt :missed)
     :score score :stars (stars score)
     :mean (mean errs) :spread (sd errs)}))
