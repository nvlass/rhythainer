(ns rhythainer.rhythm-test
  (:require [cljs.test :refer [deftest is testing]]
            [rhythainer.rhythm :as rh]))

(deftest every-exercise-fills-its-bars
  (doseq [level (range 1 (inc rh/max-level)) beats [2 3 4] bars [1 2 4] focus [false true]
          _ (range 15)
          :let [ex (rh/exercise rand {:level level :beats beats :bars bars :focus focus})]]
    (testing [level beats bars]
      (is (= bars (count ex)))
      (is (every? #(= (rh/bar-ticks beats) (rh/total %)) ex))
      (is (every? #(<= (:level (rh/cell-by-id (:cell %))) level) (apply concat ex)))
      (is (not (:rest (ffirst ex))) "starts with a note")
      (is (every? #(some (complement :rest) %) ex) "no silent bars"))))

(deftest tour-covers-each-cell-once
  (doseq [beats [2 3 4] level (range 1 (inc rh/max-level))
          :let [t (rh/tour beats level)]]
    (is (every? (fn [[a b]] (= a b)) t))
    (is (every? #(= (rh/bar-ticks beats) (rh/total (first %))) t))
    (is (every? #(some (complement :rest) (first %)) t))
    (is (apply distinct? (map (fn [[bar]] (or (some #(when (not= :q (:cell %)) (:cell %)) bar) :q)) t)))))

(deftest hits-skip-rests
  (let [q {:v :quarter :cell :q} r {:v :quarter :rest true :cell :q-rest}]
    (is (= [0 96 192 288] (map :at (rh/hits [[q r q r] [q r q r]] 4))))))

(deftest takadimi-syllables
  (let [e {:v :eighth} s {:v :16th} q {:v :quarter} t {:v :eighth :trip true}]
    (is (= ["ta" "di" "ta" "ka" "di" "mi" "ta"] (rh/syllables :takadimi [e e s s s s q])))
    (is (= ["ta" "ki" "da"] (rh/syllables :takadimi [t t t])))
    (is (= ["1" "&" "2" "3"] (rh/syllables :counting [e e q q])))))
