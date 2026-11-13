# TODO

## v2

- [ ] Clap input: AudioWorklet onset detector. High-pass at ~1.5 kHz, rectify, fast envelope (~1 ms attack, ~10 ms release) against a slow one (~100 ms); fire when `fast > k × slow + floor`, with a ~50 ms minimum gap. Onset time from the worklet's sample position, so it is on the audio clock.
  - `getUserMedia` with echoCancellation, noiseSuppression and autoGainControl off.
  - Calibration screen: "clap 4 times" sets k and the floor; clapping along with clicks measures mic latency (separate from the tap latency).
  - iOS: switch `navigator.audioSession.type` to `play-and-record` while the mic is on, back to `playback` after.
  - Not in quick dive (mic + speaker at once). A visual metronome (pulsing dot) instead of audible clicks while clapping.
- [ ] Read mode: option to hide the notation after N seconds and play from memory.
- [ ] Drop-out / gap exercises (as drummers practise): a rhythm or groove plays with the student tapping along, then the sound (clicks included) drops out for X bars while they keep going, and comes back to show whether they're still in time. Options: bars on / bars off (e.g. 2 on 2 off, 4 on 1 off), gaps that grow each cycle. Grade the taps in the silent bars against the strict grid; show drift per gap on the timeline.
- [ ] Calibration nudge: when `:latency` was never calibrated and the recent anchored attempts are steadily off (e.g. |mean| > 40 ms with a small spread over the last 10), suggest calibrating, with a link to Settings. Shown once, dismissable. Open question: a steady early offset is often the student anticipating rather than the device, and calibration would hide it, so word it as "if your device plays late…" or offer it only for late offsets / adults.
- [ ] Adaptive selection: drill the cells with the worst recent accuracy more often (`stats/cells` already tallies per cell).

## Later

- [ ] Ties and syncopation across the beat; compound meter (6/8).
- [ ] Per-bar fit in relaxed mode ("drift allowed"), for young children.
- [ ] Recorded syllable samples for quick dive (speech synthesis lags too much).
- [ ] 5-line staff option.
- [ ] Export / import stats between devices.
- [ ] `navigator.storage.persist()` so IndexedDB is less likely to be evicted.
