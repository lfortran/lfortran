#!/usr/bin/env python3
"""Reject package-cache aliases before running macOS CI build tools."""

import argparse
from pathlib import Path
import subprocess


def private_tools(prefix):
    prefix = prefix.resolve()
    if not prefix.is_dir():
        raise RuntimeError(f"environment prefix is missing: {prefix}")
    tools = []
    for name in ("cmake", "ctest", "cpack"):
        tool = prefix / "bin" / name
        if not tool.is_file():
            raise RuntimeError(f"required tool is missing: {tool}")
        resolved = tool.resolve()
        if not resolved.is_relative_to(prefix):
            raise RuntimeError(f"tool resolves outside the environment: {tool} -> {resolved}")
        links = resolved.stat().st_nlink
        if links != 1:
            raise RuntimeError(
                f"tool is hard-linked ({links} links): {tool}; "
                "macOS environments must use always_copy: true"
            )
        tools.append(tool)
    return tools


def check_environment(prefix):
    for tool in private_tools(prefix):
        print(f"{tool}: private environment copy, link count 1", flush=True)
        subprocess.run([str(tool), "--version"], check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("prefix", type=Path, help="active macOS environment prefix")
    args = parser.parse_args()
    check_environment(args.prefix)


if __name__ == "__main__":
    main()
