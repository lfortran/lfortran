#!/usr/bin/env python3
"""Keep the mtime of regenerated files whose content did not change.

build0.sh regenerates many sources (asr.h, ast.h, the parser, ...) and CMake
runs it on every (re)configure. The generators rewrite their output each time,
so every file gets a new mtime and the build system recompiles everything that
includes them, even when the content is byte-identical.

    keep_unchanged_mtime.py save STATE_FILE PATH...   # before generating
    keep_unchanged_mtime.py restore STATE_FILE        # after generating

`save` records the mtime and a hash of every source file below the given paths.
`restore` puts the old mtime back on each file that was rewritten with
identical content. Files whose content really changed keep their new mtime.
"""
import hashlib
import json
import os
import sys

EXTENSIONS = (".h", ".hh", ".cpp", ".cc")
SKIP_DIRS = {"CMakeFiles", "__pycache__", ".git"}


def source_files(paths):
    for path in paths:
        if os.path.isfile(path):
            yield path
            continue
        for root, dirs, files in os.walk(path):
            dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
            for name in files:
                if name.endswith(EXTENSIONS):
                    yield os.path.join(root, name)


def digest(path):
    with open(path, "rb") as f:
        return hashlib.sha1(f.read()).hexdigest()


def save(state_file, paths):
    state = {p: [os.stat(p).st_mtime_ns, digest(p)] for p in source_files(paths)}
    with open(state_file, "w") as f:
        json.dump(state, f)


def restore(state_file):
    with open(state_file) as f:
        state = json.load(f)
    kept = 0
    for path, (mtime_ns, sha1) in state.items():
        if not os.path.isfile(path):
            continue
        st = os.stat(path)
        if st.st_mtime_ns != mtime_ns and digest(path) == sha1:
            os.utime(path, ns=(st.st_atime_ns, mtime_ns))
            kept += 1
    print(f"kept the old mtime of {kept} regenerated file(s) with unchanged content")


if __name__ == "__main__":
    if len(sys.argv) >= 4 and sys.argv[1] == "save":
        save(sys.argv[2], sys.argv[3:])
    elif len(sys.argv) == 3 and sys.argv[1] == "restore":
        restore(sys.argv[2])
    else:
        sys.exit(__doc__)
