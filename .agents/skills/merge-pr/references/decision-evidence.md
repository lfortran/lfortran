# Decision evidence

The rules in `SKILL.md` were extracted from the merge/close decisions on
`lfortran/lfortran` PRs up to 2026-10-26: the 50 most recently closed PRs,
the open PRs adopted or left pending by the maintainer, and older external
PRs with maintainer comments (about 85 PRs in total). The cases below are
the precedents; cite them when a decision is borderline.

## Who decides, and on what

- The maintainer (certik) merges most PRs. Members also merge (assem2002 merges
  his own and some external PRs; bonachea owns coarray decisions, #12466).
- Formal GitHub reviews are rare. What substitutes for them is the PR body:
  Summary/Scope/Verification/Rationale, fail-on-main/pass-on-branch with the
  base SHA, local LLVM 11 suite counts, normal/`--fast`/`--std=f23`/GFortran
  runs, a "Not covered" list with filed issues (#15122, #15588, #15960,
  #15987). Honest statements of what CI cannot cover are accepted (#15969,
  #15979, #15948).
- The hard gate is the twelve required Quick jobs green on the head; merges
  follow within seconds to hours of green. Exhaustive is not waited for
  (#15948, #15969 broke Exhaustive on LLVM 7/8 and was fixed forward by
  #15976).
- Fork PRs need workflow approval before Quick runs; approval is selective
  and is itself part of the decision (#15978, #15980, #15806, #12773).

## Content judgment precedents

### Small, local, right layer → merged as is
- #15588 (+44, one guarded semantics branch), #15701 (two visitor overrides),
  #15960 (one function in one pass), #15945 (one generator file), #15979,
  #15969 (runtime symbols only), #15980 (test-only), #15978 (10-line check).
- #15122 removed a guard in the shared predicate `is_temporary_needed` and
  waited 11 days before merging without comment: non-local, but tests and
  the zero-iteration reasoning were in the body.

### Non-local but disciplined → merged
- #15323 (84 files, `use` imports of generics, 30 references): 12 ordered
  commits, a CI-found side bug split into #15960 and merged first.
- #15870 → #15873 → #15874: move allocation decisions from the LLVM backend
  into an ASR pass, stacked and merged bottom-up; the schema churn
  (~890 references) went in its own no-behavior-change commit.
- #15875: four verifier rules, each landing after the fix that makes it hold,
  each with a negative test.
- #15888: DO CONCURRENT scoping in AST→ASR, ~246 references.
- #15806: `ASR.asdl` + runtime ABI change on every unformatted I/O, merged as
  is because it was complete and well evidenced ("The PR looks good, thanks
  for this!").
- #15351: shared helper and `.mod` layout for logical arrays only; the
  maintainer rebased and merged ("I am rebasing it and taking care of it.").
- #15918: +8.7k/−8.1k pure code move, judged by `-Werror` CI and the measured
  build-time gain, not by line count.
- #15896, #15902: +7k/+3k lines, but gated behind `--gpu`, declining loops
  instead of miscompiling.

### Too large → split into a stack
- #14615 (828 files, 9 layers, green repeatedly) → closed for 10 PRs: schema
  only (#15877, 590 references), runtime interface with inert defaults
  (#15880), independent prerequisites (#15876), then behavior PRs (#15881,
  #15884, #15903, #15897, #15913, #15917, #15920), merged bottom-up.
- #15760 → split; low-risk parts merged (#15904, #15818).

### Design / policy → closed or refused regardless of CI
- #15819, #15820: green and clean, closed "Superseded by #15934" because they
  added a custom protocol, platform-specific IPC, an optional dependency and
  a CI configuration, and duplicated code in pi-repl.
- #12293: one-line change to parse `.f` as fixed form by default: refused as a
  product choice ("we want to keep the door open to use `.f` for modern
  Fortran").
- #8572: "This should not print a leading space without
  `--print-leading-space`".
- #15093 vs #15118: a default-semantics change accepted because the standard
  requires it.

### Wrong layer, incomplete, or a trade-off → redone
- #11414 removed a `free` in the backend, trading a double free for a leak:
  "Leaks memory." / "We can't have memory leaks." → #11417 in semantics.
- #15962 added `PassOptions`/CLI plumbing and kept a stale special case →
  rewritten as #15990 (+23/−25, one file), keeping the contributor's test.
- #15924 stopped folding kind-16 constants and missed integer/complex →
  rewritten as #16002.
- #12403 also changed neighbouring intrinsics ("A lot of changes that need to
  be carefully reviewed.") → #14648, which removed the duplicate instead.
- #15093: "The fix in #15118 seem a lot simpler, why is that? Do we need all
  the boiler plate code in this PR?"
- #12478: a hot-path default change with a Metal workaround, no test, and red
  LLVM 19 → parked ("we need to debug it why it fails").

### Adoption of sound external work
- #15941 (+ duplicate #15940) → #15987: cherry-picked unchanged, authorship
  preserved, the more complete test kept, a leftover bug filed (#15988).
- #15965 → #16001 (LLVM 11 references regenerated in a follow-up commit),
  #15878 (+ narrower #15879) → #15993 (overflow ICE fixed in a separate
  commit with test `matmul_09`).
- #12886 → #14695 (joined the continuation in the single tokenizer function
  that feeds every consumer); #12800 → #14647 (also needed `array_op`).

## Gate precedents

- Conflicts and red CI: the templated comments and draft marking used in the
  2026-10-02/03 sweep (#12442, #13877, #12773, #12478, #12470, #12403,
  #11414). When main was at fault: "Main was broken, I fixed it in #13983, so
  you need to rebase." (#12773).
- Freshness: #15868 merged on green CI from before the #15877 schema change;
  its new `.asr` inputs lacked the new fields and broke main. Two agents opened
  identical fixes (#15912, #15919); the maintainer merged his own copy
  (#15925) before CI and closed both.
- Red main: minimal fixes merged within 20–80 minutes, test-only or CI-only,
  sometimes before their own CI (#15925, #15939, #15976). #15939 then left
  main red because of GCC/Clang argument-evaluation order until #15945.
- Flaky tests: disabled only with an issue quoted in the disable comment
  (#15957 / #15956), re-enabled only with a deterministic replacement
  (#15959).
- CI-only `-Werror` failures fixed with added commits, not weaker flags
  (#15875, #15896, #15880, #15918).
- Infrastructure failures: shown to be infrastructure (the same job passed on
  sibling PRs), re-run once, then rebased onto the CI fix (#15873 onto
  #15948).
- Test placement: "Please add this into continue compilation." (#13877);
  "This is not used?" (#12293); "This test should be removed, or registered
  correctly." and "This should not be in the root directory." (#8572).
- Diagnostics: the primary label goes on the offending entity ("highlight the
  variable `out_intent` itself, not the term `intent(out)`", #12773).
- Regression after merge: fixed forward with the smallest scoped PR
  (#15870 → #15949 → #15951, with a `c`-labelled test); revert only if a fix
  is not near ("Let's figure out what is actually wrong and fix it.", #15508
  → #15510/#15511).
- Bulk close on 2026-10-14 (69 PRs, by a member): the linked issue was
  closed, "linked issue is closed, hence closing this PR (AI automated)".
  Weak spot: a reporter-closed issue (#12416) took an unmerged fix with it,
  hence the skill's "verify the fix is on `main`" step.

## Where the skill deliberately tightens observed practice

- **Closing and adoption comments.** Adopted or duplicate external PRs were
  often left open or closed with no comment (#15940, #15941, #15962, #15881,
  #15912, #15919), and contributors were sometimes not @-mentioned (#15993,
  #16001). The skill always posts T4/T5/T6.
- **Freshness after schema changes.** G4 turns the #15868 breakage into a rule.
- **No pre-CI merges of compiler source.** Pre-CI merges were only ever
  test-, reference- or CI-only main fixes; the skill does those only when the
  user says so.
- **No rewriting of contributors' branches.** The maintainer occasionally
  force-pushed a rebase (#15351) or merged main into a contributor branch
  (#12439, #13877); the skill adopts into the user's fork instead.
- **Stale descriptions.** #12439 merged with a body that described a change no
  longer in the diff; the skill treats a mismatched description as a
  hygiene finding.
