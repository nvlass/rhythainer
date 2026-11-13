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
