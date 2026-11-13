(ns rhythainer.i18n.en)

(def ^:private cheers
  {3 ["Excellent!!" "Bravo!" "Perfect!" "Superb!"]
   2 ["Great job!" "Very good!" "Well done!" "Nice!"]
   1 ["Good try!" "Getting there!" "Nice effort!" "Keep it up!"]
   0 ["Almost! Try again" "Keep going!" "Let's try once more!" "You'll get it!"]})

(def ^:private session-cheers
  {3 ["Excellent!! You're a rhythm star!" "Bravo! Superb session!" "Perfect timing, well done!" "Amazing!"]
   2 ["Great session!" "Very good work!" "Well done, nice rhythm!" "Good job!"]
   1 ["Good effort! Keep practising" "Getting better!" "Nice work, keep going!" "Well tried!"]
   0 ["Keep practising, you'll get there!" "Every try makes you better!" "Let's go again!" "Don't give up!"]})

(defn- n-of [n one many] (str n " " (if (= n 1) one many)))

(def dict
  {:nav {:practice "Practice" :stats "Stats" :settings "Settings"}

   :mode {:listen "Listen" :read "Read" :both "Listen + read" :memory "Memory" :dive "Quick dive"}

   :mode-hint {:listen "Listen to a rhythm, then tap it back from memory."
               :read "Read the rhythm, then tap it."
               :both "See and hear the rhythm, then tap it back."
               :memory "Listen and tap it back; each time you manage, the rhythm gets longer. How long can you remember?"
               :dive "Every pattern of the level in turn: read along and tap with the woodblock."}

   :level {:l1 "Quarter notes and quarter rests"
           :l2 "Half, dotted half and whole notes, half rests"
           :l3 "Pairs of eighths"
           :l4 "Eighth rests: off-beat eighths"
           :l5 "Dotted quarter + eighth"
           :l6 "Syncopation: eighth, quarter, eighth"
           :l7 "Sixteenths"
           :l8 "Dotted eighth + sixteenth, sixteenth–eighth–sixteenth"
           :l9 "Triplets"}

   :tol {:young "Ages 5–8" :kids "Ages 9–12" :teens "Teens & adults" :pro "Advanced"}

   :syl {:takadimi "Ta-ka-di-mi" :kodaly "Kodály (ta ti-ti)" :counting "1 e & a" :none "None"}

   :status {:ready-listen "Press Start, listen, then tap the rhythm back."
            :ready-read "Read the rhythm. Press Start, then tap it."
            :ready-both "Press Start, follow the notes, then tap the rhythm back."
            :ready-dive "Press Start and tap along with the woodblock."
            :ready-memory (fn [_ n] (str "Press Start, listen to " (if (= n 1) "1 bar" (str n " bars")) ", then tap it back."))
            :listening "Listen…"
            :your-turn "Your turn: tap!"
            :play-along "Tap along!"}

   :memory {:grow "One bar at a time"
            :span "N bars at once"
            :grow-hint "The same rhythm gets one bar longer after each success."
            :span-hint "A new rhythm every time, one bar longer after each success."
            :length (fn [_ n] (str "Length: " n (if (= n 1) " bar" " bars")))
            :best (fn [_ n] (str "Best: " n))
            :longer (fn [_ n] (str "Next: " n " bars"))
            :retry "Try again"
            :passed "You remembered it! 🎉"
            :longest (fn [_ n] (str "Longest: " n (if (= n 1) " bar" " bars")))}

   :count {:fewer (fn [_ n k] (if (zero? k)
                                (str "There were " n " notes, but I didn't hear a single tap! Is the pad asleep? 😴")
                                (str "There were " n " notes, but I only heard " k ". "
                                     (if (= 1 (- n k)) "One got away! 👂" "A few got away! 👂"))))
           :more (fn [_ n k] (str "I heard " k " taps, but there " (if (= n 1) "was only 1 note" (str "were only " n " notes"))
                                  ". Some extra ones sneaked in! 🐾"))
           :misplaced (fn [_ n] (str n " notes, " n " taps: right count! But some landed in the wrong place. 🎯"))}

   :cheer {:attempt (fn [_ stars k] (get-in cheers [stars k]))
           :session (fn [_ stars k] (get-in session-cheers [stars k]))}

   :ui
   {:level "Level"
    :options "Options"
    :meter "Meter"
    :bars "Bars"
    :tempo "Tempo (bpm)"
    :timing "Timing"
    :strict "Strict"
    :relaxed "Relaxed"
    :strict-hint "Keep the tempo you heard."
    :relaxed-hint "Any steady tempo counts: only the proportions between notes are checked."
    :lead-in "Count-in"
    :lead-in-hint "a bar of clicks before your turn"
    :focus "Focus on the level"
    :focus-hint "mostly this level's patterns, fewer of the earlier ones"
    :syl-read "Show syllables while reading"
    :session-len "Exercises per round"
    :item (fn [_ k n] (str k " of " n))
    :tap "Tap"
    :start "Start"
    :stop "Stop"
    :done "Done"
    :hear "Hear it"
    :hear-yours "Hear yours"
    :again "Again"
    :next "Next"
    :finish "Finish"
    :keys-hint "Keyboard: Space or Enter starts and goes on; while it's your turn, Space (or F, J) taps."
    :perfect (fn [_ n] (str n " perfect"))
    :good (fn [_ n] (str n " good"))
    :missed (fn [_ n] (str n " missed"))
    :extra (fn [_ n] (str n " extra"))
    :your-tempo (fn [_ bpm set] (str "Your tempo: " bpm " bpm (set: " set ")."))
    :on-time "Right on time."
    :early (fn [_ ms] (str "On average " ms " ms early: you're rushing a little."))
    :late (fn [_ ms] (str "On average " ms " ms late: you're dragging a little."))
    :rushed-gaps (fn [_ ms] (str "You cut the rests and long notes short, by " ms " ms each: count them out to the end."))
    :dragged-gaps (fn [_ ms] (str "You held the rests and long notes too long, by " ms " ms each."))
    :new-record "New record!"
    :session-score (fn [_ pct mode level] (str "Score " pct "% · " mode " · level " level))
    :new-session "New round"
    :tolerance "Tolerance"
    :tolerance-hint (fn [_ ms pct] (str "A tap counts within ±" ms " ms or ±" pct "% of a beat, whichever is wider. Changing it also sets a suitable tempo."))
    :syllables "Rhythm syllables"
    :tap-sound "Sound on tap"
    :tap-sound-hint "a woodblock knock for every tap; in quick dive a short tick instead, so you hear whether you land with the rhythm"
    :latency "Tap delay correction (ms)"
    :latency-hint "Bluetooth headphones and some devices play sound late. Calibrate: tap along with 12 clicks."
    :tap-along "Tap along with the clicks"
    :calibrate "Calibrate"
    :calib-ok (fn [_ ms] (str "Set to " ms " ms."))
    :calib-failed "Not enough taps near the clicks. Try again."}

   :stats
   {:empty "No exercises yet. Your results will show up here."
    :overview "Overview"
    :attempts (fn [_ n] (n-of n "exercise" "exercises"))
    :timing "Timing"
    :timing-none "Play some exercises in strict time with a count-in to see whether you rush or drag."
    :spread (fn [_ ms n] (str "Spread ±" ms " ms over " (if (= n 1) "the last exercise" (str "the last " n " exercises")) "."))
    :cells "Patterns"
    :sessions "Recent rounds"
    :reset "Reset stats"
    :reset-sure "Sure? Tap again"}})
