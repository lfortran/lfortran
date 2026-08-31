// Run an `-sMODULARIZE -sEXPORT_ES6` build (see #12603): call the exported
// module factory; main() runs at startup, so an `error stop` fails the test
// with a non-zero exit code. Assert the expected stdout as well, so a
// toolchain that stops invoking main() at factory call cannot pass silently.
// Finally assert the STACK_SIZE=64mb sentinel, proving the `-s=KEY=VALUE`
// spelling reached emcc instead of being silently ignored.
// Usage: node loader.mjs <module.mjs>.
import { readFile } from "node:fs/promises";
let out = "";
const write = process.stdout.write.bind(process.stdout);
process.stdout.write = (chunk, ...args) => { out += String(chunk); return write(chunk, ...args); };
const url = new URL(process.argv[2], `file://${process.cwd()}/`);
const { default: createModule } = await import(url.href);
await createModule();
process.stdout.write = write;
if (!out.includes("PASS")) {
    console.error("error: main() did not produce the expected output");
    process.exit(1);
}
const moduleText = await readFile(url, "utf8");
if (!moduleText.includes("67108864")) {
    console.error("error: -s=STACK_SIZE=64mb was not applied");
    process.exit(1);
}
