(ns rhythainer.grade-test
  (:require [cljs.test :refer [deftest is]]
            [rhythainer.grade :as g]))

(def beat 0.6)
(def expected [0 0.6 0.9 1.2 1.8 2.4])

(defn- run [taps & {:as opts}]
  (g/grade (merge {:expected expected :taps taps :anchored? true :relaxed? false :beat-s beat :tolerance :kids} opts)))

(deftest exact-taps-are-perfect
  (let [r (run expected)]
    (is (= (count expected) (:perfect r)))
    (is (= 3 (:stars r)))
    (is (empty? (:extra r)))))

(deftest small-errors-still-count
  (let [r (run (map + expected [0.01 -0.02 0.015 0.03 -0.01 0.02]))]
    (is (zero? (:missed r)))
    (is (= 3 (:stars r)))))

(deftest missing-and-extra-taps
  (let [r (run [0 0.6 1.2 1.5 1.8 2.4])]
    (is (= 1 (:missed r)))
    (is (= :missed (:mark ((:notes r) 2))))
    (is (= 1 (count (:extra r))))))

(deftest one-missing-tap-doesnt-shift-the-rest
  (let [r (run [0 0.6 1.2 1.8 2.4])]
    (is (= 1 (:missed r)))
    (is (= 5 (:perfect r)))))

(deftest slower-tempo-strict-vs-relaxed
  (let [slow (map #(+ 5 (* 1.25 %)) expected)]
    (is (< (:score (run slow :anchored? false)) 0.5))
    (let [r (run slow :anchored? false :relaxed? true)]
      (is (= (count expected) (:perfect r)))
      (is (< (Math/abs (- 1.25 (second (:fit r)))) 1e-6)))))

(deftest strict-without-count-in-fits-the-offset
  (let [r (run (map #(+ 12.3 %) expected) :anchored? false)]
    (is (= (count expected) (:perfect r)))))

(deftest rushing-shows-in-the-mean
  (let [r (run (map #(- % 0.04) expected))]
    (is (< (Math/abs (+ 0.04 (:mean r))) 1e-9))))

(deftest windows-never-overlap
  (let [e [0 0.075 0.15 0.225 0.3]
        ws (g/windows e 0.3 (g/tolerances :young))]
    (is (every? #(< (* 2 %) 0.075) ws))))

(deftest no-taps
  (let [r (run [])]
    (is (= (count expected) (:missed r)))
    (is (zero? (:score r)))))

(deftest memory-forgives-a-forgotten-note-but-not-a-stray-tap
  (is (g/passed? (run (butlast expected))) "one of six missing")
  (is (not (g/passed? (run (take 3 expected)))) "half missing")
  (is (not (g/passed? (run (sort (conj expected 1.5))))) "one stray tap"))

(deftest rushing-the-rests-shows-in-gaps
  (let [q-rest [0 1.2 2.4 3.6]
        steady (run (map + q-rest [-0.06 -0.06 -0.06 -0.06]) :expected q-rest)
        rushed (run (map + q-rest [-0.06 -0.12 -0.18 -0.23]) :expected q-rest)]
    (is (< (Math/abs (:gaps steady)) 1e-9))
    (is (< -0.07 (:gaps rushed) -0.05))
    (is (pos? (:missed rushed)) "the late taps fall outside their windows, the gaps still count")
    (is (nil? (:gaps (run expected))) "no gap longer than a beat")))

(deftest bar-taps-split-halfway-between-bars
  (let [taps [-0.1 0.05 0.55 1.1 1.3 1.75]]
    (is (= [-0.1 0.05 0.55] (g/bar-taps taps nil [0 0.6] 1.2)) "a first bar takes everything before it")
    (is (= [1.1 1.3] (g/bar-taps taps 0.6 [1.2 1.5] 1.8)) "an early tap for the next bar goes with it")
    (is (= [1.1 1.3 1.75] (g/bar-taps taps 0.6 [1.2 1.5] nil)) "a last bar takes everything after it")))

(deftest combining-bars-matches-their-sum
  (let [bar1 [0 0.6 1.2 1.8]
        bar2 [2.4 3.0 3.6]
        r1 (run [0.01 0.6 1.0 1.25 1.8] :expected bar1)
        r2 (run [2.42 3.6] :expected bar2)
        c (g/combine [r1 r2] beat)]
    (is (= 7 (:n c)))
    (is (= (+ (:perfect r1) (:perfect r2)) (:perfect c)))
    (is (= (+ (:missed r1) (:missed r2)) (:missed c)))
    (is (= (+ (count (:extra r1)) (count (:extra r2))) (count (:extra c))))
    (is (= 2.42 ((:taps c) (:tap ((:notes c) 4)))) "tap indices point into the joined taps")
    (is (= (count (:extra r1)) (count (filter #(< % 5) (:extra c)))))
    (is (not (g/clean? r2)) "a missed note isn't clean")))

(deftest a-miss-and-a-distant-extra-dont-look-like-rushing
  (let [q-rest [0 1.2 2.4 3.6 4.8 6.0]
        r (run [0 2.4 3.6 4.8 5.0 6.0] :expected q-rest)]
    (is (= 1 (:missed r)))
    (is (= 1 (count (:extra r))))
    (is (< (Math/abs (:gaps r)) 1e-9))))
