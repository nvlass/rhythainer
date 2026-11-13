# Rhythainer

Static re-frame/shadow-cljs SPA for rhythm training (rhythm + retainer), no backend. Sibling of `../theoricalizer` (shell, `guard`, `storage`, i18n pattern, stylesheet base) and `../dicteeiner` (the tick model, cells and notation were adapted from its `rhythm` and `score`). See README.

## Commands

- `npx shadow-cljs watch app`: dev server on :8285
- `npx shadow-cljs compile test`: runs `test/` on node
- `npm run dist`: release build into `dist/`

## Conventions

- `dist/` is committed on purpose. Run `npm run dist` before committing changes to `src/` or `resources/public/`.
- `rhythm`, `grade` and `stats` are pure. Durations are ticks, 48 per quarter (`rh/per-beat`). An event is `{:v :dot :trip :rest :cell}`; `:cell` is the id of the rhythm cell it came from, used for per-pattern stats, so keep cell ids stable. `rh/cells` lists the cells by level; the test `every-exercise-fills-its-bars` runs every level × meter × length: run it after touching cells or the generator.
- Timing: everything is on the AudioContext clock. Taps take the DOM event's own `timeStamp` and convert it with `audio/tap-time` (via `getOutputTimestamp`, i.e. when the sound is heard), so dispatch delay doesn't matter. Never time taps with `js/Date` or by when a handler runs. `:latency` (ms, user-calibrated) is subtracted before grading.
- Grading (`grade/grade`): DP alignment of taps to onsets, then a fit. Strict with a count-in fits nothing (taps relative to the downbeat), strict without one fits the offset, relaxed fits offset + tempo. Windows are max(ms, frac × beat) per tolerance preset, capped below half the gap to neighbouring onsets. Grading is onset-only: rests are gaps.
- Run state is `:run {:token :phase :t0 :taps}`, phases `:ready → (:listening) → :recording → :done`. Every delayed event (`:later` fx) carries the token and must check it. `:t0` is the downbeat on the audio clock when a count-in fixed it, else nil.
- Sessions: `:session {:mode :level :k :n :items :scores :summary}`. Quick dive's items are `rh/tour` (one item per cell); other modes generate each exercise on load. Memory mode has no `:n`: it adds `:kind` (`:grow` extends `:grown` by a bar, `:span` generates `:len` fresh bars), `:len`, `:tries` and `:best`; `events/memory-step` decides what Next does (pass = `grade/passed?`: no stray taps, at most a quarter of the notes missed). Memory sessions are stored with `:best` (bars) instead of `:score`.
- Persistence: IndexedDB store `rhythainer`, keys `settings`, `attempts`, `sessions` (EDN strings). New setting keys need a default in `events/default-settings`. Attempt records feed stats; keep their shape backward compatible.
- The woodblock is synthesized (`audio/woodblock!`), so there are no samples; `sw.js` lists only the app shell. Bump `VERSION` when the cached set changes.
- Translations: tongue, `i18n/en.cljs` and `i18n/el.cljs` with the same keys (`i18n-test` checks). In Greek don't use `;` as a separator.
- Destructuring `:or` defaults don't apply to keys passed as nil; views pass optional fns like `:syl` as nil, so components use `(or x default)`.
