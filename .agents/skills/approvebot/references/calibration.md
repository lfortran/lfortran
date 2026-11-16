# Calibration

The policy improves only from the maintainer's judgments on real PRs. The
target is **zero wrong `bot-approved` verdicts**. Sending an easy PR to
review is a much smaller cost; reduce it only once a category has a clean
record.

## What counts as a judgment

`status` reports a human reaction to the current verdict, and `ack` records
it in `.approvebot/feedback.jsonl`:

| Human action | Default outcome |
| --- | --- |
| `auto-merge` applied to a `bot-approved` PR, or the PR merged | agree |
| `auto-merge` applied to a `needs-review` PR | disagree: the bot was too cautious |
| `bot-approved` replaced by `needs-review`, or removed | disagree: a wrong approval |
| `needs-review` replaced by `bot-approved` | disagree: the bot was too cautious |
| A `bot-approved` PR closed without merging | disagree |
| A `needs-review` PR merged or closed | agree, unless the maintainer said otherwise |

The maintainer may explain a disagreement in a PR comment. That comment is
the reason. If there is none, record `no reason given`. Do not guess the
maintainer's reason.

## Turning disagreements into calibration PRs

When `python3 $BOT feedback` lists uncaptured disagreements:

1. Work in a separate worktree from upstream `main`, on one branch named
   `approvebot/calibration`, never in the bot's checkout or its main
   worktree. Keep at most one calibration PR open, and add new cases to it
   as new commits.
2. For each disagreement:
   - Append a case to `evals/cases.jsonl` (format below).
   - Make the **smallest** policy change that would have produced the
     maintainer's verdict, without changing any earlier case's expected
     verdict. A wrong approval narrows a kind, or adds an exclusion or an
     out-of-scope item. A too-cautious verdict may widen a kind only if
     the maintainer's reason supports it.
   - If you cannot state the reason as a rule, add only the case. Say so in
     the PR body, and ask the maintainer.
3. Re-judge every case in `cases.jsonl` with the changed policy, each in a
   fresh subagent that is given only the policy and that case's PR at its
   recorded head. Report any case whose verdict changed. A change that
   breaks an earlier case needs a different rule.
4. Push the branch to the maintainer's fork and open or update a **draft**
   PR against `lfortran/lfortran` `main`. Title it
   `approvebot: calibrate policy from #A, #B`. The body lists each case,
   the maintainer's reason, the rule change, and the replay result. Follow
   the commit rules in `AGENTS.md`, with no AI attribution.
5. Run `python3 $BOT feedback --capture <PR URL> --prs A B`.

Never mark a calibration PR ready, label it, or merge it. Never classify it
with a verdict other than `needs-review`: it changes agent policy, which is
out of scope. The policy in effect stays the one on the bot's checkout until
the maintainer merges.

Agreements are cases too. Add them to `cases.jsonl` in the next calibration
PR, so that the replay protects good verdicts as well as bad ones.

## Promotion to auto-merge

`python3 $BOT stats` counts verdicts and outcomes per label and kind. When
a kind of `bot-approved` reaches about 25 agreements with no disagreements
since its last rule change, you may propose, in a calibration PR, that this
kind also apply `auto-merge`. The proposal states the count, the cases and
the residual risks. Do not change the script's behavior until the maintainer
merges that proposal. Then it covers only the kinds that PR names.

A wrong approval in a kind that already applies `auto-merge` suspends that
kind's promotion in the same calibration PR.

## Case format

One JSON object per line in `evals/cases.jsonl`:

```json
{"pr": 17972, "head": "709bc38172", "policy": 1, "bot": "low-risk C2",
 "maintainer": "bot-approved", "scope": "S2", "kind": "C2",
 "reason": "the maintainer's words", "lesson": "the rule this case pins"}
```

`bot` is the verdict the bot gave, and `maintainer` is the correct one. A
case with `"bot": null` was added by hand.
