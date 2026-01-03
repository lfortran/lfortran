#!/usr/bin/env python3

import argparse
import os
import sys
import traceback
from pathlib import Path

from llanguage_server.cxx.lsp_code_generator import CPlusPlusLspCodeGenerator

EXIT_SUCCESS: int = 0
EXIT_FAILURE: int = 1

def is_debug_enabled() -> bool:
    debug: str = os.environ.get("DEBUG_LSP_CODE_GENERATOR", "false")
    return debug.lower() in ["true", "t", "yes", "y", "on", "1"]

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog="LSPCodeGenerator",
        description="Generates code in a target language from Microsoft's LSP spec (Language Server Protocol).",
    )
    parser.add_argument(
        "-s", "--schema",
        help="Path to the metaModel.json file describing the current LSP spec (e.g. version 3.17).",
        type=argparse.FileType(mode="r", encoding="UTF-8"),
    )
    parser.add_argument(
        "-l", "--target-language",
        help="Target language for the generated code.",
        choices=["c++"],
    )
    parser.add_argument(
        "-o", "--output-dir",
        help="Path to the directory into which to write the generated files.",
        type=Path,
    )
    return parser.parse_args()

def main() -> int:
    args: argparse.Namespace = parse_args()
    match args.target_language:
        case "c++":
            code_generator = CPlusPlusLspCodeGenerator(args)
        case _:
            raise ValueError(f"Unsupported target language: {args.target_language}")
    code_generator.generate_code()
    return EXIT_SUCCESS

if __name__ == "__main__":
    try:
        exit_code = main()
        sys.exit(exit_code)
    except SystemExit as system_exit:
        if system_exit.code is not None:
            if system_exit.code != 0 and is_debug_enabled():
                traceback.print_exc()
            sys.exit(system_exit.code)
    except:
        print("Rescued unhandled exception:", file=sys.stderr)
        traceback.print_exc()
        sys.exit(EXIT_FAILURE)
