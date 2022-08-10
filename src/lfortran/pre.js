function define_imports(memory, outputBuffer, stdout_print) {
    function printNum(num) {
        outputBuffer.push(num.toString());
    }

    function printStr(startIdx, strSize) {
        var bytes = new Uint8Array(memory.buffer, startIdx, strSize);
        var string = new TextDecoder("utf8").decode(bytes);
        outputBuffer.push(string);
    }

    function flushBuffer() {
        stdout_print(outputBuffer.join(" ") + "\n");
        outputBuffer = [];
    }

    var imports = {
        js: {
            memory: memory,

            /* functions */
            print_i32: printNum,
            print_i64: printNum,
            print_f32: printNum,
            print_f64: printNum,
            print_str: printStr,
            flush_buf: flushBuffer,
        },
    };

    return imports;
}

function saveString(str)
{
    return str;
}

Module["executeWasm"] = function(buffer) {
    var outputBuffer = [];
    var memory = new WebAssembly.Memory({ initial: 10, maximum: 100 }); // initial 640Kb and max 6.4Mb
    var stdout_print = Module["stdout_redirector"];
    var imports = define_imports(memory, outputBuffer, stdout_print);

    const mod = new WebAssembly.Module(buffer);
    const instance = new WebAssembly.Instance(mod, imports);
    try {
        instance.exports._lcompilers_main();
    }
    catch (e) {
        console.log(e);
        return 1;
    }
    return 0;
}