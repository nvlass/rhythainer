(ns rhythainer.notation
  (:require [rhythainer.rhythm :as rh]))

;; One-line percussion staff: every head sits on the line, stems point up and beams are flat.
;; One SVG per system (line of bars), bars of equal width, notes spaced by duration inside a bar.

(def line-y 46)
(def tip-y 16)
(def syl-y 84)

(defn- ellipse-path [x y rx ry phi]
  (let [c (Math/cos (* phi (/ Math/PI 180))) s (Math/sin (* phi (/ Math/PI 180)))]
    (str "M" (- x (* rx c)) "," (- y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (+ x (* rx c)) "," (+ y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (- x (* rx c)) "," (- y (* rx s)) " Z ")))

(defn- head-path [x y v]
  (case v
    :whole (str (ellipse-path x y 7.2 5 0) (ellipse-path x y 3.4 2.6 -35))
    :half (str (ellipse-path x y 6.4 4.5 -22) (ellipse-path x y 5.4 2.1 -22))
    (ellipse-path x y 6.2 4.4 -22)))

(defn- stem-x [x] (+ x 5.6))

(defn- flag-path [sx k]
  (let [y (+ tip-y (* 7 k))]
    (str "M" sx "," y " c 0.5,7 10,9 8,20")))

(defn- rest-el [v x]
  (let [y line-y]
    (case v
      :whole [:rect.rest {:x (- x 6) :y y :width 12 :height 5}]
      :half [:rect.rest {:x (- x 6) :y (- y 5) :width 12 :height 5}]
      :quarter [:path.rest-stroke {:d (str "M" (- x 2) "," (- y 15) " l6,8 l-7,7 l7,8"
                                           " M" (+ x 4) "," (+ y 8) " q-8,-4 -5,7")}]
      (let [k (if (= v :16th) 2 1)]
        [:g
         [:path.rest-stroke {:d (str "M" (+ x 4) "," (- y 7) " l" (- (* 5 k) 1) "," (+ 16 (* 6 (dec k))))}]
         (for [i (range k)
               :let [yy (+ y -7 (* 7 i)) xx (- (+ x 4) (* 1.5 i))]]
           ^{:key i}
           [:g
            [:circle.rest {:cx (- xx 4) :cy (- yy 1) :r 2.4}]
            [:path.rest-stroke {:d (str "M" (- xx 4) "," yy " q3,2 6,-1")}]])]))))

(defn- beam-els [g bar sx]
  (let [poly (fn [x1 x2 y] (str x1 "," y " " x2 "," y " " x2 "," (+ y 4.5) " " x1 "," (+ y 4.5)))
        sixteenth? #(= :16th (:v (bar (g %))))
        n (count g)]
    (into [:g [:polygon.beam {:points (poly (sx (first g)) (sx (peek g)) tip-y)}]]
          (for [k (range n) :when (sixteenth? k)
                :let [prev? (and (pos? k) (sixteenth? (dec k)))
                      next? (and (< (inc k) n) (sixteenth? (inc k)))]
                :when (not prev?)
                :let [x1 (sx (g k))
                      x2 (cond next? (sx (g (inc k)))
                               (zero? k) (+ x1 8)
                               :else (- x1 8))]]
            [:polygon.beam {:points (poly (min x1 x2) (max x1 x2) (+ tip-y 7))}]))))

(defn- voice
  "Hiccup for one bar of events at x positions `xs`; `cls` gives an event's class, `syl` its syllable."
  [bar xs cls syl]
  (let [groups (rh/beam-groups bar)
        beamed (set (mapcat #(when (> (count %) 1) %) groups))
        sx (fn [i] (stem-x (xs i)))]
    (concat
     (for [i (range (count bar))
           :let [{:keys [v dot rest]} (bar i)
                 x (xs i)]]
       ^{:key (str "n" i)}
       [:g {:class (str (if rest "rest-ev " "note ") (cls i))}
        (if rest
          (rest-el v x)
          [:<>
           [:path.head {:d (head-path x line-y v) :fill-rule "evenodd"}]
           (when (not= v :whole)
             [:line.stem {:x1 (sx i) :x2 (sx i) :y1 (- line-y 1) :y2 tip-y}])
           (when (and (#{:eighth :16th} v) (not (beamed i)))
             (for [k (range (if (= v :16th) 2 1))]
               ^{:key k} [:path.flag {:d (flag-path (sx i) k)}]))])
        (when dot [:circle.dot {:cx (+ x 11) :cy (- line-y 4) :r 1.9}])
        (when-let [s (syl i)]
          [:text.syl {:x x :y syl-y :text-anchor "middle"} s])])
     (for [g groups :when (> (count g) 1)]
       ^{:key (str "b" (first g))}
       [:g {:class (str "beam-group " (cls (first g)))} (beam-els g bar sx)])
     (for [g (rh/triplet-groups bar)
           :let [x1 (- (sx (first g)) 2)
                 x2 (+ (sx (peek g)) 2)
                 mid (/ (+ x1 x2) 2)
                 y (- tip-y 8)
                 bracket? (not (some #(= g %) groups))]]
       ^{:key (str "t" (first g))}
       [:g
        (when bracket?
          [:path.bracket {:d (str "M" x1 "," (+ y 4) " V" y " H" (- mid 6) " M" (+ mid 6) "," y " H" x2 " V" (+ y 4))}])
        [:text.tuplet {:x mid :y (+ y 4) :text-anchor "middle"} "3"]]))))

(defn- spacing
  "Onset → x inside a bar of width `w`, spaced by the square root of each duration."
  [bar w]
  (let [pts (rh/onsets bar)
        len (rh/total bar)
        gaps (map - (concat (rest pts) [len]) pts)
        nat (map #(+ 14 (* 4 (Math/sqrt (/ % 4)))) gaps)
        scale (/ (- w 26) (reduce + nat))]
    (mapv #(+ 16 (* scale %)) (reductions + 0 nat))))

(def height 96)

(defn bar-width
  "Natural width of a bar: more room for busy bars."
  [bar]
  (max 130 (+ 40 (* 26 (count bar)))))

(defn system
  "Bars with a time signature when `tsig` is set. `cls` maps [bar i] to a class, `syl` to a syllable;
   `from` numbers the first bar, `cur` is a bar number to shade."
  [{:keys [bars beats bar-w tsig final? from cls syl cur] :or {from 0}}]
  (let [cls (or cls (constantly nil))
        syl (or syl (constantly nil))
        x0 (if tsig 34 4)
        w (+ x0 (* bar-w (count bars)) 4)]
    [:svg.notation {:viewBox (str "0 0 " w " " height) :width w :height height}
     [:line.staff {:x1 0 :x2 (- w 2) :y1 line-y :y2 line-y}]
     (when tsig
       [:g.tsig
        [:text {:x 17 :y (- line-y 3) :text-anchor "middle"} beats]
        [:text {:x 17 :y (+ line-y 19) :text-anchor "middle"} 4]])
     (for [[k bar] (map-indexed vector bars)
           :let [b (+ from k)
                 bx (+ x0 (* k bar-w))
                 xs (spacing bar bar-w)
                 last? (and final? (= k (dec (count bars))))]]
       ^{:key k}
       [:g
        (when (= b cur) [:rect.cur-bar {:x bx :y 4 :width bar-w :height (- height 8) :rx 6}])
        [:g {:transform (str "translate(" bx ",0)")}
         (voice bar xs #(cls [b %]) #(syl [b %]))]
        [:line {:class (if last? "bar final" "bar")
                :x1 (+ bx bar-w) :x2 (+ bx bar-w) :y1 (- line-y 14) :y2 (+ line-y 14)}]])]))

(defn score
  "Wraps `bars` into systems that fit `avail` px."
  [{:keys [bars avail] :as props}]
  (let [natural (apply max (map bar-width bars))
        per-line (max 1 (min (count bars) (quot (- avail 40) natural)))
        bar-w (max natural (quot (- avail 40) per-line))]
    [:div.systems
     (for [from (range 0 (count bars) per-line)
           :let [chunk (subvec bars from (min (count bars) (+ from per-line)))]]
       ^{:key from}
       [system (assoc props :bars chunk :from from :bar-w bar-w
                      :tsig (zero? from)
                      :final? (>= (+ from per-line) (count bars)))])]))

(defn cell-icon
  "A rhythm cell on its own, for legends and stats."
  [{:keys [notes]}]
  (let [bar-w (+ 24 (* 26 (count notes)))]
    [:svg.notation.icon {:viewBox (str "0 4 " bar-w " 64") :width bar-w :height 64}
     [:line.staff {:x1 0 :x2 bar-w :y1 line-y :y2 line-y}]
     (voice notes (spacing notes bar-w) (constantly nil) (constantly nil))]))
