(ns rhythainer.i18n-test
  (:require [cljs.test :refer [deftest is]]
            [rhythainer.i18n :as i18n]
            [rhythainer.i18n.en :as en]
            [rhythainer.i18n.el :as el]
            [rhythainer.rhythm :as rh]))

(defn- flat-keys [m prefix]
  (mapcat (fn [[k v]]
            (let [k (if prefix (keyword (str prefix "." (name k))) k)]
              (if (map? v) (flat-keys v (name k)) [k])))
          m))

(deftest same-keys
  (is (= (set (flat-keys en/dict nil)) (set (flat-keys el/dict nil)))))

(deftest cheers-and-levels-exist
  (doseq [lang [:en :el] :let [loc (i18n/locale lang)]]
    (doseq [s (range 4) k (range 4)]
      (is (string? (i18n/t loc :cheer/attempt s k)))
      (is (string? (i18n/t loc :cheer/session s k))))
    (doseq [l (range 1 (inc rh/max-level))]
      (is (string? (i18n/t loc (keyword "level" (str "l" l))))))))

(deftest count-lines-exist
  (doseq [lang [:en :el] :let [loc (i18n/locale lang)] n [1 2 8] k [0 1 7 9 12]]
    (is (string? (i18n/t loc :count/fewer n k)))
    (is (string? (i18n/t loc :count/more n k)))
    (is (string? (i18n/t loc :count/misplaced n)))))
