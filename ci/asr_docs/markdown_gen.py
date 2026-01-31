"""
Markdown Generator for ASR reference documentation.

Generates properly formatted markdown for nodes, enums, and structs.
"""

import re


def generate_node_content(category, name, signature, restrictions, existing_sections=None):
    """
    Generate markdown content for a node, preserving existing documentation.

    Args:
        category: Node category (e.g., 'stmt', 'expr', 'ttype')
        name: Node name
        signature: ASR signature string
        restrictions: List of formatted restriction messages
        existing_sections: Dict of preserved sections from existing doc

    Returns:
        str: Complete markdown content
    """
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

    # Get existing ASR example or use placeholder
    asr_text = "_No ASR example yet._"
    if existing_sections and "asr" in existing_sections:
        asr_text = existing_sections["asr"]

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

## ASR

{asr_text}

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
