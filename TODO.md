# TODO

## v2

- [ ] Clap input: AudioWorklet onset detector. High-pass at ~1.5 kHz, rectify, fast envelope (~1 ms attack, ~10 ms release) against a slow one (~100 ms); fire when `fast > k × slow + floor`, with a ~50 ms minimum gap. Onset time from the worklet's sample position, so it is on the audio clock.
  - `getUserMedia` with echoCancellation, noiseSuppression and autoGainControl off.
  - Calibration screen: "clap 4 times" sets k and the floor; clapping along with clicks measures mic latency (separate from the tap latency).
  - iOS: switch `navigator.audioSession.type` to `play-and-record` while the mic is on, back to `playback` after.
  - Not in quick dive (mic + speaker at once). A visual metronome (pulsing dot) instead of audible clicks while clapping.
- [ ] Memory mode: Simon-style. Start with one bar, add a bar after each success; the score is the longest sequence held.
- [ ] Read mode: option to hide the notation after N seconds and play from memory.
- [ ] Adaptive selection: drill the cells with the worst recent accuracy more often (`stats/cells` already tallies per cell).
- [ ] Link from the music-trainer.app front page and add to its `deploy.sh`.

## Later

- [ ] Ties and syncopation across the beat; compound meter (6/8).
- [ ] Per-bar fit in relaxed mode ("drift allowed"), for young children.
- [ ] Recorded syllable samples for quick dive (speech synthesis lags too much).
- [ ] 5-line staff option.
- [ ] Export / import stats between devices.
- [ ] `navigator.storage.persist()` so IndexedDB is less likely to be evicted.
