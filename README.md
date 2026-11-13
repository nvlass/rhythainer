# Rhythainer

Rhythm training in the browser (rhythm + retainer): listen to or read a rhythm and tap it back. For children and adults; English and Greek. No backend; it installs as a PWA and works offline.

Sibling of [Chord Trainer](https://github.com/nvlass/chordtrainer), [Counterpointer](https://github.com/nvlass/counterpointer), [Dicteeineer](https://github.com/nvlass/dicteeineer), [Harmoneer](https://github.com/nvlass/harmoneer) and [Theoricalizer](https://github.com/nvlass/theoricalizer), at [music-trainer.app](https://music-trainer.app).

## Modes

- **Listen**: a rhythm plays on a woodblock (after an optional bar of count-in), then you tap it back from memory. The notation is shown afterwards.
- **Read**: the rhythm is shown; press Start and tap it.
- **Listen + read**: both, with the notes lighting up as they play.
- **Memory**: listen and tap it back; after each success the rhythm gets a bar longer. *One bar at a time* keeps extending the same rhythm; *N bars at once* plays a new rhythm of the new length each time. A try passes when every tap lands on a note and no more than a quarter of the notes are missing: forgetting a note is forgiven more than tapping in the wrong place. Two tries per length; the round ends with the longest length you held.
- **Quick dive**: every pattern of the level in turn, with rhythm syllables under the notes. Tap along while it plays.

Tap the big pad, or press Space / F / J. A round is 5 or 10 exercises (quick dive: one per pattern) and ends with stars and a cheer.

## Timing

- **Strict**: keep the tempo. With a count-in the downbeat is fixed; without one your first tap sets it.
- **Relaxed**: any steady tempo counts; the app fits your tempo and compares proportions, and tells you the tempo you kept.

Tolerance presets by age (5–8, 9–12, teens & adults, advanced) set the hit window (the larger of a fixed number of ms and a share of the beat) and a suitable tempo. Each note is perfect, good or missed, and extra taps count against you. After each exercise a timeline shows your taps over the expected notes, and in strict time whether you rush or drag.

A calibration in Settings measures how late your device plays sound (Bluetooth headphones especially) and corrects for it.

## Levels

Each level adds to the ones before:

1. Quarter notes and quarter rests
2. Half, dotted half and whole notes, half rests
3. Pairs of eighths
4. Eighth rests (off-beat eighths)
5. Dotted quarter + eighth
6. Syncopation: eighth, quarter, eighth
7. Sixteenths
8. Dotted eighth + sixteenth, sixteenth–eighth–sixteenth
9. Eighth and quarter triplets

Meter 2/4, 3/4 or 4/4, 1, 2 or 4 bars, tempo 40–160. "Focus on the level" draws mostly the level's own patterns. Rhythm syllables: ta-ka-di-mi, Kodály or counting.

Stats show results per mode, your timing tendency, and accuracy per pattern.

## Development

```sh
npm install
npx shadow-cljs watch app      # http://localhost:8285
npx shadow-cljs compile test   # tests on node
npm run dist                   # release build into dist/
```

## License

MIT, see [LICENSE](LICENSE).
