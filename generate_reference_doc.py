import os
import re
import hashlib
import sys
import difflib

from pathlib import Path
from collections import defaultdict

ASDL_PATH = "src/libasr/ASR.asdl"
VERIFY_CPP_PATH = "src/libasr/asr_verify.cpp"
DOCS_DIR = Path("doc/reference")
NODE_TYPES = ["unit", "symbol", "stmt", "expr", "ttype", "omp_clause", "schedule_type"]

RED = "\033[91m"
GREEN = "\033[92m"
CYAN = "\033[96m"
BOLD = "\033[1m"
RESET = "\033[0m"
BLUE = "\033[94m"


def parse_asdl(asdl_text):
    nodes = defaultdict(list)     # concrete nodes with parentheses
    enums = defaultdict(list)     # enum values (no parentheses)
    structs = {}                  # structs

    current_section = None

    lines = asdl_text.splitlines()

    line_index = 0
    while line_index < len(lines):
        line = lines[line_index].strip()
        # print(f"Processing line: {line}")
        if not line or line.startswith("--") or line.startswith("module "):
            line_index += 1
            continue

        # if current_section is not None:
        #     print(f"current section hai")
        # else:
        #     print(f"current section nahi hai")
        # if "|" in line:
        #     print(f"Found section: {line}")
        # else:
        #     print("Nahi mili bhai")

        # check if line starts with any of NODE_TYPES
        if any(line.startswith(node_type) for node_type in NODE_TYPES):
            current_section = line.split()[0]
            line_index += 1
            continue

        if "=" in line:
            lhs, rhs = line.split("=", 1)
            lhs = lhs.strip()
            rhs = rhs.strip()
            if lhs != "":
                current_section = lhs

            # if it has `(` and no `)`, iterate to find the closing `)`
            if "(" in rhs and ")" not in rhs:
                while line_index + 1 < len(lines):
                    line_index += 1
                    next_line = lines[line_index].strip()
                    rhs += " " + next_line
                    if ")" in next_line:
                        break

            # print(f"Processing: {lhs} = {rhs}")
            if rhs.startswith("(") and rhs.endswith(")"):
                # tuple-like type
                fields = [arg.strip() for arg in rhs[1:-1].split(",") if arg.strip()]
                structs[current_section] = fields
                line_index += 1
                continue

            # print(f"Found section: {current_section} with line: {line}")
            for part in rhs.split("|"):
                # print(f"Processing part: {part}")
                part = part.strip()
                if "(" in part:
                    name = part.split("(")[0].strip()
                    # print(f"Found node: {name} with part: {part}")
                    nodes[current_section].append((name, part))
                else:
                    enums[current_section].append(part)
        elif current_section is not None and "|" in line:
            # print(f"Continuing section: {current_section} with line: {line}")
            for part in line.strip("| ").split("|"):
                part = part.strip()
                # if it has `(` and no `)`, iterate to find the closing `)`
                if "(" in part and ")" not in part:
                    # print(f"Part has `(` but no `)`: {part}")
                    while line_index + 1 < len(lines):
                        line_index += 1
                        next_line = lines[line_index].strip()
                        part += " " + next_line
                        # print(f"Continuing part: {part}")
                        if ")" in next_line:
                            break
                if "(" in part:
                    name = part.split("(")[0].strip()
                    nodes[current_section].append((name, part))
                else:
                    enums[current_section].append(part)
        line_index += 1

    return nodes, enums, structs


def parse_cpp_restrictions(cpp_text):
    restriction_map = defaultdict(list)
    lines = cpp_text.splitlines()
    i = 0
    while i < len(lines):
        line = lines[i]
        match = re.match(r'\s*void\s+visit_(\w+)\s*\([^)]*\)\s*\{', line)
        if match:
            node_name = match.group(1)
            brace_count = 0
            body_lines = []
            while i < len(lines):
                line = lines[i]
                brace_count += line.count('{')
                brace_count -= line.count('}')
                body_lines.append(line)
                i += 1
                if brace_count == 0:
                    break
            body = '\n'.join(body_lines)

            # Find all require(...) calls, including multiline
            require_calls = re.findall(r'require\s*\((.+?),\s*(.+?)\);', body, re.DOTALL)

            for cond, msg_expr in require_calls:
                # Extract all string literals from the message expression
                string_literals = re.findall(r'"([^"]*)"', msg_expr)
                if string_literals:
                    full_msg = ''.join(s.strip() for s in string_literals)
                    # remove `''` from full_msg
                    full_msg = full_msg.replace("''", "")
                    # strip double spaces
                    full_msg = re.sub(r'\s+', ' ', full_msg).strip()
                    restriction_map[node_name].append(full_msg)
        else:
            i += 1

    return restriction_map



def generate_enum_file(lhs, values):
    output_path = DOCS_DIR / f"{lhs}.md"
    output_path.parent.mkdir(parents=True, exist_ok=True)

    md = [
        "<!-- This is an automatically generated file. Do not edit it manually. -->",
        "",
        f"# {lhs}",
        "",
        f"`{lhs}` is an enum-like type with the following values:",
        "",
        ""
    ]
    for val in values:
        md.append(f"- `{val}`")

    with open(output_path, "w") as f:
        f.write("\n".join(md))


def generate_node_file(category, name, signature, restrictions):
    node_dir = DOCS_DIR / category
    node_dir.mkdir(parents=True, exist_ok=True)
    path = node_dir / f"{name}.md"

    args_match = re.search(r"\((.*?)\)", signature)
    args_list = []
    if args_match:
        for arg in args_match.group(1).split(","):
            arg = arg.strip()
            if not arg:
                continue
            parts = arg.split()
            if len(parts) >= 2:
                arg_type = " ".join(parts[:-1])
                arg_name = parts[-1]
                args_list.append(f"`{arg_name}` of type {arg_type}")

    md = [
        "<!-- This is an automatically generated file. Do not edit it manually. -->",
        "",
        f"# {name}",
        "",
        f"{name}, a **{category}** node.",
        "",
        "## Declaration",
        "",
        "### Syntax",
        "",
        signature,
        "",
        "### Arguments",
    ]
    if args_list:
        md.append("Input argument" + ("s are " if len(args_list) > 1 else " is ") +
                  ", ".join(args_list) + ".")
    else:
        md.append("None.")

    md += [
        "",
        "### Return values",
        "",
        "None.",
        "",
        "## ASR",
        "",
        "<!-- Generate ASR using pickle. -->",
        "",
        "## Restrictions",
        "",
        "<!-- Generated from asr_verify.cpp. -->",
    ]
    if restrictions:
        for r in restrictions:
            md.append(f"* {r}")
    else:
        md.append("None.")

    with open(path, "w") as f:
        f.write("\n".join(md))


def generate_struct_file(name, fields):
    path = DOCS_DIR / f"{name}.md"
    path.parent.mkdir(parents=True, exist_ok=True)

    parsed_fields = []
    for field in fields:
        parts = field.split()
        if len(parts) >= 2:
            arg_type = " ".join(parts[:-1])
            arg_name = parts[-1]
            parsed_fields.append(f"- `{arg_name}` of type `{arg_type}`")
        else:
            parsed_fields.append(f"- `{field}`")

    md = [
        "<!-- This is an automatically generated file. Do not edit it manually. -->",
        f"# {name}",
        "",
        f"`{name}` is a struct with the following fields:",
        "",
        *parsed_fields
    ]

    with open(path, "w") as f:
        f.write("\n".join(md))


def generate_reference_docs():
    with open(ASDL_PATH, "r") as f:
        asdl_text = f.read()
    with open(VERIFY_CPP_PATH, "r") as f:
        cpp_text = f.read()

    nodes, enums, structs = parse_asdl(asdl_text)
    restrictions = parse_cpp_restrictions(cpp_text)

    for category, node_list in nodes.items():
        for name, sig in node_list:
            generate_node_file(category, name, sig, restrictions.get(name, []))

    for enum_type, values in enums.items():
        generate_enum_file(enum_type, values)

    for tuple_name, fields in structs.items():
        generate_struct_file(tuple_name, fields)


def file_hash(path):
    hasher = hashlib.sha256()
    with open(path, "rb") as f:
        while chunk := f.read(8192):
            hasher.update(chunk)
    return hasher.hexdigest()


def snapshot_directory_hashes(directory):
    hashes = {}
    for path in directory.rglob("*.md"):
        hashes[path] = file_hash(path)
    return hashes


def read_file_lines(path):
    try:
        with open(path, "r", encoding="utf-8") as f:
            return f.readlines()
    except Exception as e:
        print(f"Error reading {path}: {e}")
        return []


def show_diff(old_lines, new_lines, filename):
    print(f"\n{BOLD}--- Diff for: {filename} ---{RESET}")
    diff = difflib.unified_diff(
        old_lines,
        new_lines,
        fromfile=f"{filename} (before)",
        tofile=f"{filename} (after)",
        lineterm=""
    )

    for line in diff:
        if line.startswith("+") and not line.startswith("+++"):
            print(GREEN + line + RESET)
        elif line.startswith("-") and not line.startswith("---"):
            print(RED + line + RESET)
        elif line.startswith("@@"):
            print(CYAN + line + RESET)
        else:
            print(line)


def main():
    print("🔍 Taking snapshot of documentation files...")
    before_hashes = snapshot_directory_hashes(DOCS_DIR)
    before_contents = {p: read_file_lines(p) for p in before_hashes}

    print("⚙️  Running documentation generation script...")

    try:
        generate_reference_docs()
    except Exception as e:
        print(RED + f"❌ Documentation generation script failed: {e}" + RESET)
        sys.exit(1)

    print("🔎 Checking for differences after generation...")
    after_hashes = snapshot_directory_hashes(DOCS_DIR)

    changed_files = [p for p in after_hashes if before_hashes.get(p) != after_hashes[p]]

    if changed_files:
        print(RED + "Document was outdated 🚨.\n" + RESET)
        print(BLUE + "The following files were changed:\n" + RESET)
        for path in changed_files:
            old_lines = before_contents.get(path, [])
            new_lines = read_file_lines(path)
            show_diff(old_lines, new_lines, path)
        
        # Files are already updated, but if user don't provide `-u` flag, we exit with a message
        if not sys.argv[1:] or sys.argv[1] != '-u':
            print(RED + "❌ Documentation is outdated or was regenerated but not committed.\n" + RESET)
            sys.exit(1)
        else:
            print(GREEN + "✅ Documentation was outdated but has been updated." + RESET)
            sys.exit(0)
    else:
        print(GREEN + "✅ Documentation is up-to-date." + RESET)
        sys.exit(0)


if __name__ == "__main__":
    main()
