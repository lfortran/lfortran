// Run a LFortran wasm32-unknown-emscripten program under node while
// emulating an interactive terminal: one complete input record is written
// to stdin but stdin is deliberately kept open afterwards, exactly like a
// browser prompt with no input pending. A formatted read must finish on
// the record-terminating newline; if it swallows the newline and waits for
// more data (issue #12108), the program hangs and this loader fails.
// Usage: node formatted_read_stdin_01_loader.mjs <prog.js>
import { spawn } from "node:child_process";

const prog = process.argv[2];
if (!prog) {
    console.error("usage: node formatted_read_stdin_01_loader.mjs <prog.js>");
    process.exit(2);
}

const child = spawn(process.execPath, [prog], {
    stdio: ["pipe", "pipe", "inherit"],
});

let out = "";
child.stdout.on("data", (chunk) => { out += chunk; });

const timer = setTimeout(() => {
    console.error("error: the program did not finish after one input line");
    console.error(`error: stdout so far: ${JSON.stringify(out)}`);
    child.kill();
    process.exit(1);
}, 10000);

child.on("exit", (code) => {
    clearTimeout(timer);
    if (code !== 0) {
        console.error(`error: the program exited with code ${code}`);
        process.exit(code ?? 1);
    }
    if (!out.includes("GOT:A")) {
        console.error(`error: 'GOT:A' missing from stdout (${JSON.stringify(out)})`);
        process.exit(1);
    }
});

child.stdin.write("A\n"); // stdin stays open on purpose
