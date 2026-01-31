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

import os
import re
import sys
import hashlib
import difflib
from pathlib import Path
from collections import defaultdict

import src.libasr.asdl as asdl


ASDL_PATH = "src/libasr/ASR.asdl"
VERIFY_CPP_PATH = "src/libasr/asr_verify.cpp"
DOCS_DIR = Path("doc/reference")

RED = "\033[91m"
GREEN = "\033[92m"
CYAN = "\033[96m"
BOLD = "\033[1m"
RESET = "\033[0m"
BLUE = "\033[94m"


class ASDLParserVisitor(asdl.VisitorBase):
    def __init__(self):
        super().__init__()
        self.nodes = defaultdict(list)
        self.enums = defaultdict(list)
        self.structs = {}

    def visitModule(self, mod):
        for df in mod.dfns:
            self.visit(df)

    def visitType(self, tp):
        self.current_type = tp.name
        self.visit(tp.value)

    def visitSum(self, sum_):
        for cons in sum_.types:
            self.visit(cons)

    def visitConstructor(self, cons):
        if cons.fields:
            field_string = f"{cons.name}({', '.join(f'{f.type}' + ('*' if f.seq else '') + ('?' if f.opt else '') + ' ' + f.name for f in cons.fields)})"
            self.nodes[self.current_type].append((cons.name, field_string))
        else:
            self.enums[self.current_type].append(cons.name)

    def visitProduct(self, prod):
        field_string_list = []
        for f in prod.fields:
            type_str = f"{f.type}" + ('*' if f.seq else '') + ('?' if f.opt else '')
            field_string_list.append(type_str + " " + f.name)
        self.structs[self.current_type] = field_string_list


def parse_asdl():
    mod = asdl.parse(ASDL_PATH)
    visitor = ASDLParserVisitor()
    visitor.visit(mod)
    return visitor.nodes, visitor.enums, visitor.structs


def find_top_level_comma(s):
    """Find the first comma that is not inside parentheses."""
    paren_count = 0
    for i, c in enumerate(s):
        if c == '(':
            paren_count += 1
        elif c == ')':
            paren_count -= 1
        elif c == ',' and paren_count == 0:
            return i
    return -1


def extract_require_calls(body):
    """Extract require(condition, message) calls handling nested parentheses.

    The require macro does not have a semicolon after it, so we must count
    parentheses to find the matching close paren rather than looking for ';'.
    """
    results = []
    i = 0
    while i < len(body):
        match = re.search(r'require\s*\(', body[i:])
        if not match:
            break

        start = i + match.end()  # Position after 'require('

        # Count parentheses to find matching )
        paren_count = 1
        j = start
        while j < len(body) and paren_count > 0:
            if body[j] == '(':
                paren_count += 1
            elif body[j] == ')':
                paren_count -= 1
            j += 1

        if paren_count == 0:
            content = body[start:j-1]  # Everything inside require(...)
            # Split on first top-level comma (condition, message)
            comma_pos = find_top_level_comma(content)
            if comma_pos != -1:
                cond = content[:comma_pos].strip()
                msg = content[comma_pos+1:].strip()
                results.append((cond, msg))

        i = j

    return results


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
            require_calls = extract_require_calls(body)
            for cond, msg_expr in require_calls:
                full_msg = build_message_with_placeholders(msg_expr)
                if full_msg:
                    restriction_map[node_name].append(full_msg)
        else:
            i += 1
    return restriction_map


def unescape_cpp_string(text):
    """
    Unescape C++ string escape sequences.
    Converts \" to ", \n to newline, \\ to backslash, etc.
    """
    result = []
    i = 0
    while i < len(text):
        if text[i] == '\\' and i + 1 < len(text):
            next_char = text[i + 1]
            if next_char == '"':
                result.append('"')
            elif next_char == 'n':
                result.append('\n')
            elif next_char == 't':
                result.append('\t')
            elif next_char == '\\':
                result.append('\\')
            else:
                result.append(next_char)
            i += 2
        else:
            result.append(text[i])
            i += 1
    return ''.join(result)


def build_message_with_placeholders(msg_expr):
    """
    Build a restriction message from C++ code, inserting [...] placeholders
    where runtime string concatenation occurs.

    For example:
      '"Variable " + std::string(x.m_name) + " depends on " + std::string(y)'
    becomes:
      'Variable [...] depends on [...]'
    """
    # Find all string literals with their positions
    # Pattern handles escape sequences: [^"\\] matches non-quote/non-backslash,
    # \\. matches backslash followed by any char (escape sequence like \")
    literal_pattern = re.compile(r'"((?:[^"\\]|\\.)*)"')
    literals = [(m.start(), m.end(), unescape_cpp_string(m.group(1))) for m in literal_pattern.finditer(msg_expr)]

    if not literals:
        return None

    # Check if there are + operators between literals (indicating runtime concatenation)
    has_concatenation = False

    # Check for trailing runtime concatenation after last (or only) literal
    last_end = literals[-1][1]
    trailing = msg_expr[last_end:].strip()
    if '+' in trailing:
        trailing_after_plus = trailing.split('+', 1)[-1].strip()
        if trailing_after_plus and not trailing_after_plus.startswith(')'):
            has_concatenation = True

    # Check between literals for runtime concatenation
    for i in range(len(literals) - 1):
        between = msg_expr[literals[i][1]:literals[i + 1][0]]
        if '+' in between:
            # Check if there's non-whitespace content between + and next literal
            # that would indicate runtime concatenation (not just "str1" + "str2")
            between_stripped = between.replace('+', '').strip()
            if between_stripped:
                has_concatenation = True
                break

    if not has_concatenation:
        # Simple case: just string literal concatenation like "str1" "str2" or "str1" + "str2"
        full_msg = ''.join(lit[2] for lit in literals)
        full_msg = full_msg.replace("''", "")
        full_msg = re.sub(r'\s+', ' ', full_msg).strip()
        return full_msg if full_msg else None

    # Complex case: runtime concatenation - insert [...] placeholders
    result_parts = []
    prev_end = 0

    for start, end, text in literals:
        # Check what's between previous literal and this one
        between = msg_expr[prev_end:start] if prev_end > 0 else ""

        if prev_end > 0 and '+' in between:
            # There's a + between literals
            between_content = between.replace('+', '').strip()
            if between_content:
                # Non-trivial content between literals means runtime value
                result_parts.append('[...]')

        result_parts.append(text)
        prev_end = end

    # Check if there's trailing runtime content after last literal
    trailing = msg_expr[prev_end:].strip()
    if trailing and '+' in trailing:
        trailing_after_plus = trailing.split('+', 1)[-1].strip()
        if trailing_after_plus and not trailing_after_plus.startswith(')'):
            result_parts.append('[...]')

    full_msg = ''.join(result_parts)
    full_msg = full_msg.replace("''", "")
    full_msg = re.sub(r'\s+', ' ', full_msg).strip()
    return full_msg if full_msg else None


def parse_existing_doc(path):
    """Parse existing markdown file and extract sections."""
    if not path.exists():
        return None

    content = path.read_text()
    sections = {}

    # Extract auto-generated sections
    pattern = r'<!-- BEGIN AUTO: (\w+) -->\n(.*?)<!-- END AUTO: \1 -->'
    for match in re.finditer(pattern, content, re.DOTALL):
        section_name = match.group(1)
        sections[f"auto_{section_name}"] = match.group(2)

    # Extract documentation section (between ## Documentation and next ##)
    doc_match = re.search(r'## Documentation\n\n(.*?)(?=\n## |\Z)', content, re.DOTALL)
    if doc_match:
        doc_content = doc_match.group(1).strip()
        if doc_content and doc_content != "_No documentation yet._":
            sections["documentation"] = doc_content

    return sections


def generate_node_content(category, name, signature, restrictions, existing_sections=None):
    """Generate markdown content for a node, preserving existing documentation."""
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
                args_list.append(f"`{arg_name}` of type `{arg_type}`")

    if args_list:
        args_text = "Input argument" + ("s are " if len(args_list) > 1 else " is ") + ", ".join(args_list) + "."
    else:
        args_text = "None."

    if restrictions:
        restrictions_text = "\n".join(f"* {r}" for r in restrictions)
    else:
        restrictions_text = "None."

    # Get existing documentation or use placeholder
    doc_text = "_No documentation yet._"
    if existing_sections and "documentation" in existing_sections:
        doc_text = existing_sections["documentation"]

    md = f"""# {name}

{name}, a **{category}** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
{signature}
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
{args_text}
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

{doc_text}

## Restrictions

<!-- BEGIN AUTO: restrictions -->
{restrictions_text}
<!-- END AUTO: restrictions -->
"""
    return md


def generate_enum_content(name, values, existing_sections=None):
    """Generate markdown content for an enum, preserving existing documentation."""
    values_text = "\n".join(f"* `{val}`" for val in values)

    doc_text = "_No documentation yet._"
    if existing_sections and "documentation" in existing_sections:
        doc_text = existing_sections["documentation"]

    md = f"""# {name}

`{name}` is an **enum**.

## Values

<!-- BEGIN AUTO: values -->
{values_text}
<!-- END AUTO: values -->

## Documentation

{doc_text}
"""
    return md


def generate_struct_content(name, fields, existing_sections=None):
    """Generate markdown content for a struct, preserving existing documentation."""
    fields_text_lines = []
    for field in fields:
        parts = field.split()
        if len(parts) >= 2:
            arg_type = " ".join(parts[:-1])
            arg_name = parts[-1]
            fields_text_lines.append(f"* `{arg_name}` of type `{arg_type}`")
        else:
            fields_text_lines.append(f"* `{field}`")
    fields_text = "\n".join(fields_text_lines)

    doc_text = "_No documentation yet._"
    if existing_sections and "documentation" in existing_sections:
        doc_text = existing_sections["documentation"]

    md = f"""# {name}

`{name}` is a **struct**.

## Fields

<!-- BEGIN AUTO: fields -->
{fields_text}
<!-- END AUTO: fields -->

## Documentation

{doc_text}
"""
    return md


def write_if_changed(path, content):
    """Write content to file only if it differs from existing content."""
    path.parent.mkdir(parents=True, exist_ok=True)

    if path.exists():
        existing = path.read_text()
        if existing == content:
            return False

    path.write_text(content)
    return True


def generate_reference_docs():
    """Generate all reference documentation files."""
    with open(VERIFY_CPP_PATH, "r") as f:
        cpp_text = f.read()

    nodes, enums, structs = parse_asdl()
    restrictions = parse_cpp_restrictions(cpp_text)

    generated_files = set()
    changed_files = []

    # Generate node files
    for category, node_list in nodes.items():
        for name, sig in node_list:
            node_dir = DOCS_DIR / category
            path = node_dir / f"{name}.md"
            generated_files.add(path)

            existing = parse_existing_doc(path)
            content = generate_node_content(category, name, sig, restrictions.get(name, []), existing)

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


def file_hash(path):
    hasher = hashlib.sha256()
    with open(path, "rb") as f:
        while chunk := f.read(8192):
            hasher.update(chunk)
    return hasher.hexdigest()


def snapshot_directory_hashes(directory):
    hashes = {}
    if directory.exists():
        for path in directory.rglob("*.md"):
            hashes[path] = file_hash(path)
    return hashes


def read_file_content(path):
    try:
        return path.read_text()
    except Exception:
        return ""


def show_diff(old_content, new_content, filename):
    old_lines = old_content.splitlines(keepends=True)
    new_lines = new_content.splitlines(keepends=True)

    print(f"\n{BOLD}--- {filename} ---{RESET}")
    diff = difflib.unified_diff(old_lines, new_lines, fromfile="before", tofile="after", lineterm="")

    for line in diff:
        if line.startswith("+") and not line.startswith("+++"):
            print(GREEN + line.rstrip() + RESET)
        elif line.startswith("-") and not line.startswith("---"):
            print(RED + line.rstrip() + RESET)
        elif line.startswith("@@"):
            print(CYAN + line.rstrip() + RESET)
        else:
            print(line.rstrip())


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
