// Run a LFortran wasm32-unknown-emscripten program under node while
// emulating an interactive terminal: each input record is written only
// after the program asks for it, and stdin is never closed, exactly like
// a browser prompt with no input pending. Each read (formatted and
// list-directed, one fresh process each so one kind cannot mask the other)
// must stop at the newline that terminates its record; a read that
// over-fetches waits for a record that is never sent and this loader times
// out (issue #12108). Feeding the lines up front would let a buffered
// over-reading stdio hide the bug, so the lazy, prompt-driven supply and
// the never-closed stdin are essential.
// Usage: node read_stdin_01_loader.mjs <prog>
import { spawn } from "node:child_process";

const prog = process.argv[2];
if (!prog) {
    console.error("usage: node read_stdin_01_loader.mjs <prog>");
    process.exit(2);
}

// mode -> [prompt regex, input line, required output]
const scenarios = [
    ["formatted", /INPUT\?/, "A\n", "GOTC:A"],
    ["list", /LIST\?/, "42\n", "GOTN:42"],
];

async function run([mode, prompt, input, want]) {
    return new Promise((resolve, reject) => {
        const child = spawn(process.execPath, [prog, mode], {
            stdio: ["pipe", "pipe", "inherit"],
        });
        let out = "";
        let fed = false;
        const timer = setTimeout(() => {
            child.kill();
            reject(new Error(
                `${mode}: no completion after one input line ` +
                `(stdout so far: ${JSON.stringify(out)})`));
        }, 10000);
        child.stdout.on("data", (chunk) => {
            out += chunk;
            if (!fed && prompt.test(out)) {
                // stdin stays open on purpose, like an unanswered prompt
                child.stdin.write(input);
                fed = true;
            }
        });
        child.on("error", (e) => { clearTimeout(timer); reject(e); });
        child.on("exit", (code) => {
            clearTimeout(timer);
            if (code !== 0) {
                reject(new Error(`${mode}: exited with code ${code} ` +
                    `(stdout: ${JSON.stringify(out)})`));
            } else if (!out.includes(want)) {
                reject(new Error(`${mode}: '${want}' missing from stdout ` +
                    `(${JSON.stringify(out)})`));
            } else {
                console.log(`${mode}: ok`);
                resolve();
            }
        });
    });
}

for (const s of scenarios) {
    try {
        await run(s);
    } catch (e) {
        console.error(`error: ${e.message}`);
        process.exit(1);
    }
}
