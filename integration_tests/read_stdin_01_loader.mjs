// Drive a read_stdin_01 program (Emscripten wasm under node, or a native
// binary fed through a pipe) while emulating an interactive terminal.
// Each scenario spawns a fresh
// process (one read kind each, so a guarded first read cannot mask
// another kind's buffering bug).  Lazy scenarios answer every prompt the
// program prints with its queued record and never close stdin -- exactly
// like a browser prompt with no further input pending.  A read that
// over-fetches waits for a record that is never sent and this loader
// times out (#12108).  Feeding the record up front would let buffered
// stdio hide that bug, so the lazy supply and the never-closed stdin are
// essential there.  eager scenarios instead queue all their records at
// once (node's synchronous stdin gives spurious EOFs when a blocking
// one-character read finds an empty pipe, which is not what #12108 is
// about); they guard the record boundaries between several reads of one
// process.  "multi" additionally closes stdin after the final record,
// covering the end-of-file abort that the issue reported.
// Usage: node read_stdin_01_loader.mjs <prog> [wasm|native]
import { spawn } from "node:child_process";

const prog = process.argv[2];
if (!prog) {
    console.error("usage: node read_stdin_01_loader.mjs <prog> [wasm|native]");
    process.exit(2);
}

// "wasm" is an Emscripten JS driver and must be launched through node;
// "native" (the default) is spawned directly, so that
// get_command_argument(1) receives the scenario mode.
const jsDriver = process.argv[3] === "wasm";

// prompts: [pattern, record] or [pattern, record, times]
const scenarios = [
    { mode: "formatted", prompts: [[/INPUT\?/, "A\n"]], expect: ["GOTC:A"] },
    { mode: "list", prompts: [[/LIST\?/, "42\n"]], expect: ["GOTN:42"] },
    { mode: "unit5", prompts: [[/U5\?/, "42\n"]], expect: ["GOTU:42"] },
    { mode: "array", prompts: [[/ARR\?/, "1 2 3\n"]], expect: ["GOTA:6"] },
    { mode: "real", prompts: [[/REAL\?/, "1.5\n"]], expect: ["GOTR:1.5"] },
    { mode: "logical", prompts: [[/LOG\?/, ".true.\n"]], expect: ["GOTL:T"] },
    // list then formatted in one process (#12656's adjacency bug): eager,
    // both records buffered, so these guard record advancement, not prompts
    { mode: "mixed", feed: "eager", records: ["42\n", "A\n"],
      expect: ["GOTN:42", "GOTC:A"] },
    { mode: "u5mix", feed: "eager", records: ["42\n", "A\n"],
      expect: ["GOTN:42", "GOTC:A"] },
    { mode: "bare", feed: "eager", records: ["junk\n", "42\n"],
      expect: ["GOTN:42"] },
    { mode: "advmix", feed: "eager", records: ["42\n"], expect: ["GOTN:42"] },
    { mode: "list2", feed: "eager", records: ["42\n", "43\n"],
      expect: ["GOTN:42", "GOTN2:43"] },
    { mode: "multi", feed: "eager", records: ["A\n", "A\n", "A\n"],
      close: true, expect: ["GOT3:done"] },
];

async function run(scenario) {
    return new Promise((resolve, reject) => {
        // argv for a node-run JS driver repeats the script path as argv[0];
        // a native binary is spawned as `<prog> <mode>`.
        const child = jsDriver
            ? spawn(process.execPath, [prog, scenario.mode],
                    {stdio: ["pipe", "pipe", "inherit"]})
            : spawn(prog, [scenario.mode],
                    {stdio: ["pipe", "pipe", "inherit"]});
        const queue = scenario.prompts
            ? scenario.prompts.map(([re, data, times]) =>
                ({ re, data, times: times ?? 1, fed: 0 }))
            : [];
        let out = "";
        let settled = false;
        let allFed = scenario.feed === "eager" && !scenario.close;

        function finish(err) {
            if (settled) return;
            settled = true;
            clearTimeout(timer);
            if (err) reject(err);
            else {
                console.log(`${scenario.mode}: ok`);
                resolve();
            }
        }

        function feedLazy() {
            while (queue.length > 0) {
                const q = queue[0];
                const seen = (out.match(new RegExp(q.re.source, "g"))
                    || []).length;
                while (q.fed < Math.min(seen, q.times)) {
                    child.stdin.write(q.data);
                    q.fed += 1;
                }
                if (q.fed >= q.times) {
                    queue.shift();
                    if (queue.length === 0) {
                        allFed = true;
                        if (scenario.close) child.stdin.end();
                    }
                } else {
                    break;
                }
            }
        }

        const timer = setTimeout(() => {
            child.kill();
            finish(new Error(
                `${scenario.mode}: no completion after `
                + `${scenario.feed === "eager" ? "its input records" : "its input lines"}`
                + ` (stdin ${scenario.close ? "closed" : "never closed"}; `
                + `stdout so far: ${JSON.stringify(out)}`
                + (allFed ? "" : ", input not fully fed") + ")"));
        }, 10000);

        child.stdout.on("data", (chunk) => {
            out += chunk;
            if (scenario.feed !== "eager") feedLazy();
        });
        child.on("error", (e) => finish(e));
        // 'close', not 'exit': node may deliver the last stdout chunks
        // after the 'exit' event fires
        child.on("close", (code) => {
            if (settled) return;
            if (code !== 0) {
                finish(new Error(`${scenario.mode}: exited with code ${code}`
                    + ` (stdout: ${JSON.stringify(out)})`));
                return;
            }
            const missing = scenario.expect.filter(e => !out.includes(e));
            if (missing.length > 0) {
                finish(new Error(`${scenario.mode}: missing `
                    + `${JSON.stringify(missing)} from stdout: `
                    + `${JSON.stringify(out)}`));
            } else {
                finish();
            }
        });

        if (scenario.feed === "eager") {
            for (const r of scenario.records) child.stdin.write(r);
            if (scenario.close) child.stdin.end();
        }
    });
}

const failures = [];
for (const s of scenarios) {
    try {
        await run(s);
    } catch (e) {
        failures.push(e.message);
    }
}
if (failures.length > 0) {
    for (const f of failures) console.error(`error: ${f}`);
    process.exit(1);
}
