set pagination off
set confirm off
set print thread-events off
set print elements 64
set disable-randomization off
set debuginfod enabled off
set auto-load python-scripts off
set startup-with-shell off
handle SIGSEGV stop print nopass
handle SIGBUS stop print nopass
handle SIGABRT stop print nopass
handle SIGILL stop print nopass
handle SIGFPE stop print nopass
python
import gdb
import hashlib
import os

def command(text):
    try:
        gdb.execute(text)
    except gdb.error as error:
        print("DIAGNOSTIC_COMMAND_FAILED {}: {}".format(text, error))

hashes = {}
def snapshot(label):
    print("\n=== RUNTIME SNAPSHOT: {} ===".format(label))
    command("info sharedlibrary")
    command("info proc mappings")
    for obj in gdb.objfiles():
        path = os.path.realpath(obj.filename)
        if path not in hashes and os.path.isfile(path):
            digest = hashlib.sha256()
            with open(path, "rb") as library:
                for chunk in iter(lambda: library.read(1024 * 1024), b""):
                    digest.update(chunk)
            hashes[path] = digest.hexdigest()
        print("OBJECT={} BUILD_ID={} SHA256={}".format(
            path, obj.build_id or "absent", hashes.get(path, "unavailable")))
    pid = gdb.selected_inferior().pid
    with open("/proc/{}/maps".format(pid)) as maps:
        with open(os.path.join(os.environ["LFORTRAN_UNIT_DIAGNOSTICS"],
                               label + ".maps"), "w") as output:
            output.write(maps.read())
end
start
python
snapshot("main")
end
continue
python
if gdb.selected_inferior().pid:
    print("\n=== FIRST FAULT: BEFORE APPLICATION SIGNAL HANDLER ===")
    command("p $_siginfo")
    command("thread apply all bt full")
    command("info registers")
    command("info symbol $pc")
    command("x/24i $pc")
    command("disassemble /r $pc-32, $pc+96")
    command("x/64gx $sp")
    snapshot("first-fault")
    gdb.execute("quit 97")
else:
    status = int(gdb.parse_and_eval("$_exitcode"))
    print("INFERIOR_EXIT={}".format(status))
    gdb.execute("quit {}".format(status))
end
