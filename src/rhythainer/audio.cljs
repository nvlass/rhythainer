(ns rhythainer.audio)

(defonce ctx (atom nil))
(defonce out (atom nil))
(defonce noise (atom nil))
(defonce voices (atom []))

(defn- context []
  (or @ctx
      (let [Ctor (or (.-AudioContext js/window) (.-webkitAudioContext js/window))
            c (Ctor. #js {:latencyHint "interactive"})
            comp (.createDynamicsCompressor c)
            master (.createGain c)]
        (set! (.. master -gain -value) 0.9)
        (.connect master comp)
        (.connect comp (.-destination c))
        (reset! out master)
        (reset! ctx c))))

(defn install-unlock!
  "iOS only resumes audio inside a user-gesture call stack, so do it synchronously."
  []
  (when-let [session (.-audioSession js/navigator)]
    (set! (.-type session) "playback"))
  (let [unlock (fn []
                 (let [c (context)]
                   (when (not= "running" (.-state c))
                     (.resume c)
                     (let [src (.createBufferSource c)]
                       (set! (.-buffer src) (.createBuffer c 1 1 22050))
                       (.connect src (.-destination c))
                       (.start src 0)))))]
    (doseq [ev ["pointerdown" "touchend" "keydown"]]
      (.addEventListener js/document ev unlock #js {:capture true}))))

(defn now [] (.-currentTime (context)))

(defn tap-time
  "An event timeStamp (the performance.now clock) on the audio clock, at the moment it is heard:
   a tap that lands exactly on a scheduled sound gets that sound's time."
  [ts]
  (let [^js c (context)
        ^js o (when (.-getOutputTimestamp c) (.getOutputTimestamp c))]
    (if (and o (pos? (.-performanceTime o)))
      (+ (.-contextTime o) (/ (- ts (.-performanceTime o)) 1000))
      (- (+ (.-currentTime c) (/ (- ts (js/performance.now)) 1000))
         (or (.-outputLatency c) (.-baseLatency c) 0)))))

(defn- env!
  "Gain node with a fast attack to `peak` at `t` and an exponential decay over `len` seconds."
  [c t peak len]
  (let [g (.createGain c)]
    (.setValueAtTime (.-gain g) 0.0001 t)
    (.exponentialRampToValueAtTime (.-gain g) peak (+ t 0.0015))
    (.exponentialRampToValueAtTime (.-gain g) 0.0001 (+ t len))
    (.connect g @out)
    g))

(defn- noise-buffer [c]
  (or @noise
      (let [n (int (* 0.05 (.-sampleRate c)))
            b (.createBuffer c 1 n (.-sampleRate c))
            d (.getChannelData b 0)]
        (dotimes [i n] (aset d i (- (* 2 (js/Math.random)) 1)))
        (reset! noise b))))

(defn- woodblock!
  "A pitched knock: two sine partials with a quick downward chirp, plus a short band-passed click."
  [t accent?]
  (let [c (context)
        f (if accent? 1040 900)
        gain (if accent? 0.7 0.55)]
    (doseq [[mult peak len] [[1 gain 0.09] [2.71 (* 0.35 gain) 0.035]]]
      (let [osc (.createOscillator c)
            g (env! c t peak len)]
        (set! (.-type osc) "sine")
        (.setValueAtTime (.-frequency osc) (* f mult 1.25) t)
        (.exponentialRampToValueAtTime (.-frequency osc) (* f mult) (+ t 0.01))
        (.connect osc g)
        (.start osc t)
        (.stop osc (+ t len 0.02))
        (swap! voices conj [osc g])))
    (let [src (.createBufferSource c)
          bp (.createBiquadFilter c)
          g (env! c t (* 0.4 gain) 0.015)]
      (set! (.-buffer src) (noise-buffer c))
      (set! (.-type bp) "bandpass")
      (set! (.. bp -frequency -value) 3200)
      (set! (.. bp -Q -value) 1.2)
      (.connect src bp)
      (.connect bp g)
      (.start src t)
      (.stop src (+ t 0.04))
      (swap! voices conj [src g]))))

(defn- click! [t accent?]
  (let [c (context)
        osc (.createOscillator c)
        g (env! c t (if accent? 0.45 0.3) 0.05)]
    (set! (.-type osc) "triangle")
    (set! (.. osc -frequency -value) (if accent? 1760 1320))
    (.connect osc g)
    (.start osc t)
    (.stop osc (+ t 0.07))
    (swap! voices conj [osc g])))

(defn stop-all! []
  (when-let [c @ctx]
    (let [now (.-currentTime c)]
      (doseq [[src g] @voices]
        (try
          (.cancelScheduledValues (.-gain g) now)
          (.setTargetAtTime (.-gain g) 0 now 0.01)
          (.stop src (+ now 0.05))
          (catch :default _)))))
  (reset! voices []))

(defn schedule!
  "`clicks` are [time accent?] metronome ticks, `blocks` [time accent?] woodblock knocks, on the audio clock."
  [{:keys [clicks blocks]}]
  (let [c (context)
        soon (+ (.-currentTime c) 0.005)]
    (doseq [[t accent?] clicks] (click! (max soon t) accent?))
    (doseq [[t accent?] blocks] (woodblock! (max soon t) accent?))))

(defn knock! [] (woodblock! (+ (now) 0.002) false))
