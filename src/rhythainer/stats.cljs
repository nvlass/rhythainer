(ns rhythainer.stats)

(defn- mean [xs] (when (seq xs) (/ (reduce + xs) (count xs))))

(defn by-mode
  "{mode {:n attempts :stars average}}."
  [attempts]
  (into {} (for [[m as] (group-by :mode attempts)]
             [m {:n (count as) :stars (mean (map :stars as))}])))

(defn timing
  "Average error and spread (seconds) over the last `n` attempts that had a fixed downbeat in strict time."
  [attempts n]
  (let [as (->> attempts (filter #(and (:anchored %) (= :strict (:timing %)) (:mean %))) (take-last n))]
    (when (seq as)
      {:n (count as) :mean (mean (map :mean as)) :spread (mean (keep :spread as))})))

(defn cells
  "{cell [hits notes]} summed over the last `n` attempts."
  [attempts n]
  (apply merge-with (partial mapv +) {} (map :cells (take-last n attempts))))

(defn best-session [sessions mode level]
  (some->> sessions (filter #(and (= mode (:mode %)) (= level (:level %)))) seq (map :score) (apply max)))
