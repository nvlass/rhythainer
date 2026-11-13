(ns rhythainer.views
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [rhythainer.rhythm :as rh]
            [rhythainer.grade :as grade]
            [rhythainer.stats :as stats]
            [rhythainer.audio :as audio]
            [rhythainer.events :as ev]
            [rhythainer.notation :as notation]
            [rhythainer.i18n :refer [tr]]))

(defn <sub [q] @(rf/subscribe q))
(defn >evt [e] (rf/dispatch e))

(defn seg [options value on-change]
  [:div.seg
   (for [[v label] options]
     ^{:key (str v)}
     [:button {:class (when (= v value) "on") :on-click #(on-change v)} label])])

(defn toggle [label value on-change & [hint]]
  [:label.toggle
   [:input {:type "checkbox" :checked (boolean value) :on-change #(on-change (not value))}]
   [:span label (when hint [:small hint])]])

(defn field [label & body] (into [:div.field [:span label]] body))

(defn slider [label value lo hi step on-change]
  [:label.slider
   [:span label [:b value]]
   [:input {:type "range" :min lo :max hi :step step :value value
            :on-change #(on-change (js/parseInt (.. % -target -value)))}]])

(defn- setter [k] #(>evt [:settings/set k %]))

(defn avail [] (- (min (<sub [:width]) 760) 66))

(defn star-row [n]
  [:span.stars (for [k (range 3)] ^{:key k} [:span {:class (when (< k n) "on")} "★"])])

(defn- tap-pad
  "Taps are timed from the event's own timestamp, so re-frame's dispatch delay doesn't matter."
  [active? label]
  [:button.pad {:class (when active? "active")
                :on-pointer-down (fn [e]
                                   (.preventDefault e)
                                   (>evt [:tap (audio/tap-time (.. e -nativeEvent -timeStamp))]))}
   label])

;; ---------------------------------------------------------------- setup

(defn level-legend [level]
  [:div.legend
   (for [c (rh/level-cells level)]
     ^{:key (:id c)}
     [:div.cell [notation/cell-icon c]])
   [:span.hint (tr (keyword "level" (str "l" level)))]])

(defn setup []
  (let [s (<sub [:settings])
        dive? (= :dive (:mode s))
        memory? (= :memory (:mode s))]
    [:div.card.setup
     [seg (vec (for [m [:listen :read :both :memory :dive]] [m (tr (keyword "mode" (name m)))])) (:mode s) (setter :mode)]
     [:p.hint.mode-hint (tr (keyword "mode-hint" (name (:mode s))))]
     (when memory?
       [field nil
        [seg [[:grow (tr :memory/grow)] [:span (tr :memory/span)]] (:memory-kind s) (setter :memory-kind)]
        [:small.hint (tr (keyword "memory" (str (name (:memory-kind s)) "-hint")))]])
     [field (tr :ui/level)
      [seg (vec (for [l (range 1 (inc rh/max-level))] [l (str l)])) (:level s) (setter :level)]]
     [level-legend (:level s)]
     [:details.options
      [:summary (tr :ui/options)]
      [:div.options-body
       [:div.row2
        [field (tr :ui/meter)
         [seg [[2 "2/4"] [3 "3/4"] [4 "4/4"]] (:beats s) (setter :beats)]]
        (when-not (or dive? memory?)
          [field (tr :ui/bars)
           [seg [[1 "1"] [2 "2"] [4 "4"]] (:bars s) (setter :bars)]])]
       [slider (tr :ui/tempo) (:tempo s) 40 160 2 (setter :tempo)]
       (when-not dive?
         [:<>
          [field (tr :ui/timing)
           [seg [[:strict (tr :ui/strict)] [:relaxed (tr :ui/relaxed)]] (:timing s) (setter :timing)]
           [:small.hint (tr (if (= :strict (:timing s)) :ui/strict-hint :ui/relaxed-hint))]]
          [toggle (tr :ui/lead-in) (:lead-in s) (setter :lead-in) (tr :ui/lead-in-hint)]
          [toggle (tr :ui/focus) (:focus s) (setter :focus) (tr :ui/focus-hint)]
          [toggle (tr :ui/syl-read) (:syl-read s) (setter :syl-read)]
          (when-not memory?
            [field (tr :ui/session-len)
             [seg [[5 "5"] [10 "10"]] (:session-len s) (setter :session-len)]])])]]]))

;; ---------------------------------------------------------------- feedback

(defn timeline
  "Your taps over the expected onsets: windows as bands, beats as a grid."
  [{:keys [expected result beat-s beats bars]}]
  (let [{:keys [windows taps notes extra]} result
        total (* beat-s beats bars)
        lo (- (* 0.5 beat-s))
        hi (+ total (* 0.25 beat-s))
        w (avail)
        h 64
        mid 32
        pad 8
        x (fn [t] (+ pad (* (- w (* 2 pad)) (/ (- (max lo (min hi t)) lo) (- hi lo)))))
        tap-mark (into {} (for [{:keys [tap mark]} notes :when tap] [tap mark]))
        extra (set extra)]
    [:svg.timeline {:viewBox (str "0 0 " w " " h) :width w :height h}
     (for [k (range (inc (* beats bars)))]
       ^{:key (str "g" k)}
       [:line {:class (if (zero? (mod k beats)) "grid bar" "grid") :x1 (x (* k beat-s)) :x2 (x (* k beat-s)) :y1 6 :y2 (- h 6)}])
     (for [[i e] (map-indexed vector expected)]
       ^{:key (str "e" i)}
       [:g
        [:rect.win {:x (x (- e (windows i))) :width (- (x (+ e (windows i))) (x (- e (windows i))))
                    :y (- mid 12) :height 24 :rx 4}]
        [:line.exp {:x1 (x e) :x2 (x e) :y1 (- mid 15) :y2 (+ mid 15)}]
        (when (= :missed (:mark (notes i)))
          [:circle.missed {:cx (x e) :cy mid :r 6}])])
     (for [[j t] (map-indexed vector taps)]
       ^{:key (str "t" j)}
       [:circle {:class (str "tap " (if (extra j) "extra" (some-> (tap-mark j) name))) :cx (x t) :cy mid :r 5}])]))

(defn- tendency [mean-s]
  (let [m (Math/round (* 1000 mean-s))]
    (cond (< (Math/abs m) 15) (tr :ui/on-time)
          (neg? m) (tr :ui/early (- m))
          :else (tr :ui/late m))))

(defn- count-line
  "How many taps against how many notes, said playfully; nothing when every note was hit."
  [{:keys [n taps missed]}]
  (let [k (count taps)]
    (cond (< k n) (tr :count/fewer n k)
          (> k n) (tr :count/more n k)
          (pos? missed) (tr :count/misplaced n)
          :else nil)))

(defn result-panel []
  (let [{:keys [stars msg perfect good missed extra relaxed? anchored? mean fit] :as res} (<sub [:result])
        s (<sub [:settings])
        {:keys [beats bars]} (<sub [:ex])]
    [:div.result
     [:div.cheer {:class (str "s" stars)}
      [star-row stars]
      [:b (tr :cheer/attempt stars msg)]]
     (when-let [line (count-line res)] [:p.count-line line])
     (when (and (= :memory (:mode (<sub [:session]))) (grade/passed? res))
       [:p.count-line.passed (tr :memory/passed)])
     [timeline {:expected (<sub [:expected]) :result res :beat-s (<sub [:beat-s]) :beats beats :bars (count bars)}]
     [:div.legend-row
      [:span.k.perfect (tr :ui/perfect perfect)]
      [:span.k.good (tr :ui/good good)]
      [:span.k.missed (tr :ui/missed missed)]
      [:span.k.extra (tr :ui/extra (count extra))]]
     (when relaxed?
       [:p.hint (tr :ui/your-tempo (Math/round (/ (:tempo s) (second fit))) (:tempo s))])
     (when (and anchored? (not relaxed?) mean)
       [:p.hint (tendency mean)])]))


;; ---------------------------------------------------------------- exercise

(defn- progress []
  (let [{:keys [k n scores]} (<sub [:session])]
    [:div.progress
     (for [i (range n)]
       ^{:key i}
       [:span.pip {:class [(when (= i k) "cur") (when-let [sc (scores i)] (str "s" (grade/stars sc)))]}])]))

(defn- memory-head []
  (let [{:keys [len tries best]} (<sub [:session])]
    [:div.memory-head
     [:b (tr :memory/length len)]
     [:span.tries (for [i (range ev/memory-tries)] ^{:key i} [:span {:class (when (< i tries) "on")} "♥"])]
     [:span.hint (tr :memory/best best)]]))

(defn- status []
  (let [{:keys [phase]} (<sub [:run])
        cnt (<sub [:count])
        mode (:mode (<sub [:session]))]
    [:div.status
     (cond cnt [:span.count-in cnt]
           (= phase :ready) (tr (keyword "status" (str "ready-" (name mode))) (:len (<sub [:session])))
           (= phase :listening) (tr :status/listening)
           (= phase :recording) [:b (tr (if (= mode :dive) :status/play-along :status/your-turn))]
           :else nil)]))

(defn exercise-card []
  (let [s (<sub [:settings])
        {:keys [mode k n]} (<sub [:session])
        {:keys [bars beats]} (<sub [:ex])
        {:keys [phase]} (<sub [:run])
        result (<sub [:result])
        hl (<sub [:hl])
        marks (when result (into {} (map (fn [h nt] [[(:bar h) (:i h)] (name (:mark nt))]) (<sub [:hits]) (:notes result))))
        show? (or (not (#{:listen :memory} mode)) (= phase :done))
        syl-sys (:syllables s)
        syls (when (and (not= syl-sys :none) (or (= mode :dive) (:syl-read s)))
               (mapv #(rh/syllables syl-sys %) bars))
        busy? (#{:listening :recording} phase)]
    [:div.card.exercise
     (if (= mode :memory)
       [memory-head]
       [:div.ex-head
        [progress]
        [:span.hint (tr :ui/item (inc k) n)]])
     [status]
     [:div.score-wrap
      (if show?
        [notation/score {:bars bars :beats beats :avail (avail)
                         :cls (fn [p] (str (when (= hl p) "playing ") (get marks p)))
                         :syl (when syls (fn [[b i]] (get-in syls [b i])))}]
        [:div.hidden-score {:class (when hl "pulse")} "🎧"])]
     (when result [result-panel])
     (when-not (= phase :done)
       [tap-pad (= phase :recording) (tr :ui/tap)])
     [:div.actions
      (case phase
        :ready [:button.primary.big {:on-click #(>evt [:start])} (tr :ui/start)]
        (:listening :recording)
        [:<>
         [:button {:on-click #(>evt [:stop])} (tr :ui/stop)]
         (when (and (= phase :recording) (not= mode :dive))
           [:button.primary {:on-click #(>evt [:finish (:token (<sub [:run]))])} (tr :ui/done)])]
        :done
        [:<>
         [:button {:on-click #(>evt [:hear])} (tr :ui/hear)]
         [:button {:on-click #(>evt [:hear-yours])} (tr :ui/hear-yours)]
         (when-not (= mode :memory)
           [:button {:on-click #(>evt [:again])} (tr :ui/again)])
         [:button.primary {:on-click #(>evt [:next])}
          (if (= mode :memory)
            (case (<sub [:memory-step])
              :longer (tr :memory/longer (inc (:len (<sub [:session]))))
              :retry (tr :memory/retry)
              (tr :ui/finish))
            (tr (if (= (inc k) n) :ui/finish :ui/next)))]]
        nil)]
     (when-not busy? [:p.hint.keys-hint (tr :ui/keys-hint)])]))

(defn summary-card []
  (let [{:keys [summary mode level kind]} (<sub [:session])
        {:keys [score stars msg record? best]} summary]
    [:div.card.summary {:class (str "s" stars)}
     [:div.big-stars [star-row stars]]
     [:h2 (tr :cheer/session stars msg)]
     (when record? [:div.record (tr :ui/new-record)])
     (if (= mode :memory)
       [:p (tr :memory/longest best) " · " (tr (keyword "memory" (name kind))) " · " (tr :ui/level) " " level]
       [:p (tr :ui/session-score (Math/round (* 100 score)) (tr (keyword "mode" (name mode))) level)])
     [:div.actions.center
      [:button.primary.big {:on-click #(>evt [:session/new])} (tr :ui/new-session)]]]))

(defn practice-view []
  [:<>
   [setup]
   (if (:summary (<sub [:session]))
     [summary-card]
     [exercise-card])])

;; ---------------------------------------------------------------- stats

(defn- reset-button []
  (let [armed (r/atom false)]
    (fn []
      [:button {:class (when @armed "bad on")
                :on-click #(if @armed (do (reset! armed false) (>evt [:stats/reset])) (reset! armed true))
                :on-blur #(reset! armed false)}
       (tr (if @armed :stats/reset-sure :stats/reset))])))

(defn stats-view []
  (let [attempts (<sub [:attempts])
        sessions (<sub [:sessions])
        modes (stats/by-mode attempts)
        timing (stats/timing attempts 30)
        cells (stats/cells attempts 500)]
    (if (empty? attempts)
      [:div.card [:p.hint (tr :stats/empty)]]
      [:<>
       [:div.card
        [:h3 (tr :stats/overview)]
        [:table.stats
         [:tbody
          (for [m [:listen :read :both :memory :dive] :let [{:keys [n] st :stars} (modes m)] :when n]
            ^{:key m}
            [:tr [:td (tr (keyword "mode" (name m)))] [:td.num (tr :stats/attempts n)]
             [:td [star-row (Math/round st)]]])]]]
       [:div.card
        [:h3 (tr :stats/timing)]
        (if timing
          [:<>
           [:p (tendency (:mean timing))]
           (when (:spread timing)
             [:p.hint (tr :stats/spread (Math/round (* 1000 (:spread timing))) (:n timing))])]
          [:p.hint (tr :stats/timing-none)])]
       [:div.card
        [:h3 (tr :stats/cells)]
        [:div.cell-rows
         (for [c rh/cells :let [[hit n] (cells (:id c))] :when n]
           ^{:key (:id c)}
           [:div.cell-row
            [notation/cell-icon c]
            [:div.meter [:div {:style {:width (str (Math/round (* 100 (/ hit n))) "%")}}]]
            [:span.num (str (Math/round (* 100 (/ hit n))) "%")]
            [:small.hint (str hit "/" n)]])]]
       (when (seq sessions)
        [:div.card
        [:h3 (tr :stats/sessions)]
        [:ul.sessions
         (for [{:keys [t mode level score best]} (reverse (take-last 10 sessions))]
           ^{:key t}
           [:li [star-row (if best (grade/memory-stars best) (grade/stars score))]
            [:span (tr (keyword "mode" (name mode))) " · " (tr :ui/level) " " level
             (when best (str " · " (tr :memory/longest best)))]
            [:small.hint (.toLocaleDateString (js/Date. t))]])]])
       [:div.actions [reset-button]]])))

;; ---------------------------------------------------------------- settings

(defn calibration []
  (let [{:keys [phase result]} (<sub [:calib])
        s (<sub [:settings])]
    [:div.calib
     [slider (tr :ui/latency) (:latency s) -100 300 5 (setter :latency)]
     [:small.hint (tr :ui/latency-hint)]
     (if (= phase :running)
       [tap-pad true (tr :ui/tap-along)]
       [:div.actions
        [:button {:on-click #(>evt [:calib/start])} (tr :ui/calibrate)]
        (case phase
          :ok [:span.hint (tr :ui/calib-ok result)]
          :failed [:span.hint (tr :ui/calib-failed)]
          nil)])]))

(defn settings-view []
  (let [s (<sub [:settings])]
    [:div.card.settings
     [field (tr :ui/tolerance)
      [seg (vec (for [t [:young :kids :teens :pro]] [t (tr (keyword "tol" (name t)))])) (:tolerance s) (setter :tolerance)]
      [:small.hint (let [{:keys [ms frac]} (grade/tolerances (:tolerance s))]
                     (tr :ui/tolerance-hint ms (Math/round (* 100 frac))))]]
     [field (tr :ui/syllables)
      [seg (vec (for [y [:takadimi :kodaly :counting :none]] [y (tr (keyword "syl" (name y)))])) (:syllables s) (setter :syllables)]]
     [toggle (tr :ui/tap-sound) (:tap-sound s) (setter :tap-sound) (tr :ui/tap-sound-hint)]
     [calibration]]))

;; ---------------------------------------------------------------- app

(defn app []
  (let [view (<sub [:view])
        s (<sub [:settings])]
    [:div.app
     [:header
      [:h1 "Rhythainer"]
      [:div.lang [seg [[:en "EN"] [:el "ΕΛ"]] (:lang s) (setter :lang)]]]
     [:nav
      (for [v [:practice :stats :settings]]
        ^{:key v}
        [:button {:class (when (= v view) "on") :on-click #(>evt [:set-view v])} (tr (keyword "nav" (name v)))])]
     [:main
      (case view
        :practice [practice-view]
        :stats [stats-view]
        :settings [settings-view])]]))
