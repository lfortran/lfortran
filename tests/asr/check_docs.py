#!/usr/bin/env python3
"""Check that the ASR node documentation still describes the ASR we have.

Documentation goes stale silently: a field is added to `ASR.asdl`, the pages
keep the old signature, and nothing notices. This script makes that a test
failure. It checks three things:

- every example under `doc/src/asr/examples/` is a valid ASR text document
  that the initial verifier accepts and that round-trips byte-exactly through
  `lfortran --from-asr ... --show-asr --clojure`;
- every ```clojure excerpt on a page is a verbatim subtree of the example
  the page includes, so an excerpt cannot drift from the document it came
  from;
- every declaration in a page's "Syntax" section matches `ASR.asdl`, and
  every constructor and enumeration value of `ASR.asdl` appears in one of
  those sections, so a new ASR node cannot be added without documenting it.
"""

import argparse
import os
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
DOCS = ROOT / "doc" / "src" / "asr"
EXAMPLES = DOCS / "examples"
ASDL = ROOT / "src" / "libasr" / "ASR.asdl"

FENCE = re.compile(r"^```(\w+)\n(.*?)^```", re.M | re.S)
INCLUDE = re.compile(r"^```\{literalinclude\}\s*(\S+)", re.M)


def run(command):
    result = subprocess.run(command, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, check=False)
    return (result.returncode,
            result.stdout.decode("utf-8", "replace").replace("\r\n", "\n"),
            result.stderr.decode("utf-8", "replace"))


# --------------------------------------------------------------- examples
def check_examples(lfortran, failures):
    documents = sorted(EXAMPLES.glob("*.asr"))
    if not documents:
        failures.append("no ASR examples found in %s" % EXAMPLES)
        return documents
    for path in documents:
        code, out, err = run([str(lfortran), "--verify-asr", str(path),
                              "--no-color"])
        if code != 0:
            failures.append("%s: the ASR verifier rejects it:\n%s"
                            % (path.name, (out + err).strip()))
            continue
        code, out, err = run([str(lfortran), "--from-asr", str(path),
                              "--show-asr", "--clojure", "--no-color"])
        if code != 0:
            failures.append("%s: cannot be printed back:\n%s"
                            % (path.name, (out + err).strip()))
            continue
        if out != path.read_text(encoding="utf-8"):
            failures.append("%s: does not round-trip. Regenerate it with "
                            "`lfortran --from-asr %s --show-asr --clojure "
                            "--no-color`." % (path.name, path.name))
    return documents


# --------------------------------------------------------------- excerpts
def flatten(text):
    """Compare ASR text ignoring how deeply it happens to be indented."""
    return "\n".join(line.strip() for line in text.strip().split("\n"))


def check_excerpts(page, text, failures):
    included = []
    for target in INCLUDE.findall(text):
        path = (page.parent / target).resolve()
        if not path.exists():
            failures.append("%s: includes %s, which does not exist"
                            % (page.relative_to(ROOT), target))
            continue
        included.append(flatten(path.read_text(encoding="utf-8")))
    if not included:
        return
    for language, block in FENCE.findall(text):
        if language != "clojure":
            continue
        excerpt = flatten(block)
        if not any(excerpt in document for document in included):
            failures.append(
                "%s: the excerpt starting %r is not a verbatim part of the "
                "example the page includes"
                % (page.relative_to(ROOT), block.strip().split("\n")[0]))


# ------------------------------------------------------------ declarations
def asdl_source():
    text = "\n".join(line for line in ASDL.read_text(encoding="utf-8").split("\n")
                     if not line.strip().startswith("--"))
    body = text[text.index("module ASR {") + len("module ASR {"):]
    return body[:body.rindex("}")]


def split_top(text, separator):
    out, depth, current = [], 0, ""
    for character in text:
        if character in "([":
            depth += 1
        elif character in ")]":
            depth -= 1
        if character == separator and depth == 0:
            out.append(current)
            current = ""
        else:
            current += character
    out.append(current)
    return out


def declarations():
    """Every constructor and enumeration value declared in ASR.asdl."""
    constructors, values = {}, {}
    entries, current = [], ""
    for line in asdl_source().split("\n"):
        if not line.strip():
            continue
        if line[0] not in " \t":
            if current:
                entries.append(current)
            current = line
        else:
            current += " " + line.strip()
    if current:
        entries.append(current)

    for entry in entries:
        name, rest = entry.split("=", 1)
        name = name.strip()
        for alternative in split_top(" ".join(rest.split()), "|"):
            alternative = alternative.strip()
            match = re.match(r"^(\w+)\((.*)\)$", alternative)
            if match:
                constructors[match.group(1)] = normalise(alternative)
            elif alternative.startswith("("):
                constructors[name] = normalise(name + " = " + alternative)
            else:
                values.setdefault(name, set()).add(alternative)
    return constructors, values


def normalise(text):
    return " ".join(text.split())


def documented(text):
    """The constructors and enumeration values the Syntax sections declare."""
    constructors, values = {}, set()
    for block in syntax_blocks(text):
        for entry in entries(block):
            constructors, values = read_entry(entry, constructors, values)
    return constructors, values


def syntax_blocks(text):
    """The fenced block that follows each `### Syntax` heading."""
    out = []
    for section in text.split("\n### Syntax\n")[1:]:
        match = FENCE.search(section)
        if match and section[:match.start()].strip() == "":
            out.append(match.group(2))
    return out


def entries(block):
    """Split an ASDL block into its top level `name = ...` entries."""
    out, current = [], ""
    for line in block.split("\n"):
        if not line.strip():
            continue
        if line[0] not in " \t":
            if current:
                out.append(current)
            current = line
        else:
            current += " " + line.strip()
    if current:
        out.append(current)
    return out


def read_entry(block, constructors, values):
        block = normalise(block)
        match = re.match(r"^(\w+)\s*=\s*(.*)$", block)
        if not match:
            head = re.match(r"^(\w+)\((.*)\)$", block)
            if head:
                constructors[head.group(1)] = block
            return constructors, values
        name, rest = match.group(1), match.group(2)
        for alternative in split_top(rest, "|"):
            alternative = alternative.strip()
            if not alternative:
                continue
            head = re.match(r"^(\w+)\((.*)\)$", alternative)
            if head:
                constructors[head.group(1)] = alternative
            elif alternative.startswith("("):
                constructors[name] = name + " = " + alternative
            else:
                values.add((name, alternative))
        return constructors, values


def check_declarations(pages, failures):
    declared, enum_values = declarations()
    seen_constructors, seen_values = set(), set()
    for page, text in pages:
        found, values = documented(text)
        for name, block in found.items():
            if name not in declared:
                failures.append("%s: declares %s, which ASR.asdl does not"
                                % (page.relative_to(ROOT), name))
                continue
            seen_constructors.add(name)
            if normalise(block) != declared[name]:
                failures.append(
                    "%s: the declaration of %s does not match ASR.asdl\n"
                    "  page:      %s\n  ASR.asdl:  %s"
                    % (page.relative_to(ROOT), name, normalise(block),
                       declared[name]))
        for name, value in values:
            if name in enum_values and value in enum_values[name]:
                seen_values.add((name, value))
            elif name in enum_values:
                failures.append("%s: %s is not a value of %s in ASR.asdl"
                                % (page.relative_to(ROOT), value, name))

    for name in sorted(set(declared) - seen_constructors):
        failures.append("%s is declared in ASR.asdl but no page documents it"
                        % name)
    for name, values in sorted(enum_values.items()):
        for value in sorted(values - {v for n, v in seen_values if n == name}):
            failures.append("%s of %s is declared in ASR.asdl but no page "
                            "documents it" % (value, name))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--lfortran", required=True, type=pathlib.Path)
    args = parser.parse_args()

    failures = []
    documents = check_examples(args.lfortran, failures)

    pages = [(path, path.read_text(encoding="utf-8"))
             for path in sorted(DOCS.rglob("*.md"))]
    for page, text in pages:
        check_excerpts(page, text, failures)
    check_declarations(pages, failures)

    if failures:
        print("\n".join(failures), file=sys.stderr)
        print("FAIL: %d problem(s) in the ASR documentation" % len(failures))
        return 1
    print("%d ASR examples round-trip, %d pages agree with ASR.asdl"
          % (len(documents), len(pages)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
