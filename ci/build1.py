"""Run build1.sh with the Prefix `shell`, forwarding CMake arguments.

    python ci/build1.py [cmake_args...]

`shell` does not pass command-line arguments to scripts, so they are passed
as a JSON list in LFORTRAN_CMAKE_ARGS. build1.sh configures with
`python ci/build1.py --cmake <args>`, which runs CMake with its own arguments
followed by the forwarded ones, so the forwarded ones take precedence.
"""

import json
import os
import subprocess
import sys


def main(args):
    if args[:1] == ["--cmake"]:
        cmd = ["cmake"] + args[1:] \
            + json.loads(os.environ.get("LFORTRAN_CMAKE_ARGS", "[]"))
        print("+ " + subprocess.list2cmdline(cmd), flush=True)
        return subprocess.call(cmd)
    env = dict(os.environ, LFORTRAN_CMAKE_ARGS=json.dumps(args))
    return subprocess.call(["shell", "--norc", "build1.sh"], env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
