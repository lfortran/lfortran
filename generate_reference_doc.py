#!/usr/bin/env python3
"""
ASR Reference Documentation Generator

This script generates and maintains reference documentation for ASR nodes by:
1. Parsing ASR.asdl for node definitions
2. Parsing asr_verify.cpp for verification constraints
3. Updating ONLY the auto-generated sections in markdown files
4. Preserving all human-written documentation

Auto-generated sections are marked with:
  <!-- BEGIN AUTO: section_name -->
  ... auto-generated content ...
  <!-- END AUTO: section_name -->

Everything outside these markers is preserved across regeneration.

Usage:
  python3 generate_reference_doc.py       # Check if docs are up-to-date (CI mode)
  python3 generate_reference_doc.py -u    # Update docs in place
"""

import sys
from pathlib import Path

from ci.asr_docs.asdl_parser import parse_asdl
from ci.asr_docs.cpp_parser import parse_cpp_verifiers
from ci.asr_docs.markdown_gen import (
    generate_node_content,
    generate_enum_content,
    generate_struct_content,
)
from ci.asr_docs.doc_parser import parse_existing_doc
from ci.asr_docs.utils import (
    write_if_changed,
    snapshot_directory_hashes,
    show_diff,
    read_file_content,
    RED, GREEN, BLUE, RESET,
)


ASDL_PATH = "src/libasr/ASR.asdl"
VERIFY_CPP_PATH = "src/libasr/asr_verify.cpp"
DOCS_DIR = Path("doc/reference")


def generate_reference_docs():
    """Generate all reference documentation files."""
    nodes, enums, structs = parse_asdl(ASDL_PATH)
    restrictions = parse_cpp_verifiers(VERIFY_CPP_PATH)

    generated_files = set()
    changed_files = []

    # Generate node files
    for category, node_list in nodes.items():
        for name, sig in node_list:
            node_dir = DOCS_DIR / category
            path = node_dir / f"{name}.md"
            generated_files.add(path)

            existing = parse_existing_doc(path)
            content = generate_node_content(
                category, name, sig, restrictions.get(name, []), existing
            )

            if write_if_changed(path, content):
                changed_files.append(path)

    # Generate enum files
    for enum_type, values in enums.items():
        path = DOCS_DIR / f"{enum_type}.md"
        generated_files.add(path)

        existing = parse_existing_doc(path)
        content = generate_enum_content(enum_type, values, existing)

        if write_if_changed(path, content):
            changed_files.append(path)

    # Generate struct files
    for struct_name, fields in structs.items():
        path = DOCS_DIR / f"{struct_name}.md"
        generated_files.add(path)

        existing = parse_existing_doc(path)
        content = generate_struct_content(struct_name, fields, existing)

        if write_if_changed(path, content):
            changed_files.append(path)

    # Find and remove obsolete files
    removed_files = []
    if DOCS_DIR.exists():
        for path in DOCS_DIR.rglob("*.md"):
            if path not in generated_files:
                path.unlink()
                removed_files.append(path)
                # Remove empty parent directories
                try:
                    path.parent.rmdir()
                except OSError:
                    pass

    return changed_files, removed_files


def main():
    update_mode = "-u" in sys.argv

    print("Taking snapshot of documentation files...")
    before_hashes = snapshot_directory_hashes(DOCS_DIR)
    before_contents = {p: read_file_content(p) for p in before_hashes}

    print("Generating documentation...")
    try:
        changed_files, removed_files = generate_reference_docs()
    except Exception as e:
        print(RED + f"Documentation generation failed: {e}" + RESET)
        import traceback
        traceback.print_exc()
        sys.exit(1)

    print("Checking for differences...")
    after_hashes = snapshot_directory_hashes(DOCS_DIR)

    # Find all changes
    all_changed = set()
    for p in after_hashes:
        if before_hashes.get(p) != after_hashes[p]:
            all_changed.add(p)
    for p in before_hashes:
        if p not in after_hashes:
            all_changed.add(p)

    if all_changed or removed_files:
        print(RED + f"\nDocumentation divergence detected.\n" + RESET)

        if all_changed:
            print(BLUE + "Changed files:" + RESET)
            for path in sorted(all_changed):
                old = before_contents.get(path, "")
                new = read_file_content(path) if path.exists() else ""
                show_diff(old, new, str(path))

        if removed_files:
            print(BLUE + "\nRemoved obsolete files:" + RESET)
            for path in removed_files:
                print(f"  - {path}")

        if not update_mode:
            print(RED + "\nDocumentation is outdated. Run with -u to update." + RESET)
            sys.exit(1)
        else:
            print(GREEN + "\nDocumentation updated successfully." + RESET)
            sys.exit(0)
    else:
        print(GREEN + "Documentation is up-to-date." + RESET)
        sys.exit(0)


if __name__ == "__main__":
    main()
