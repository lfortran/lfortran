"""
Markdown Generator for ASR reference documentation.

Generates properly formatted markdown for nodes, enums, and structs.
Following certik's format from issue #7006.
"""

import re


def generate_node_content(category, name, signature, restrictions, existing_sections=None):
    """
    Generate markdown content for a node, preserving existing documentation.

    Format follows certik's example from issue #7006:
    - ## ASR: the ASR signature (auto-generated)
    - ## Documentation: human-written content (preserved)
    - ## Verify: restrictions from asr_verify.cpp (auto-generated)

    Args:
        category: Node category (e.g., 'stmt', 'expr', 'ttype')
        name: Node name
        signature: ASR signature string
        restrictions: List of formatted restriction messages
        existing_sections: Dict of preserved sections from existing doc

    Returns:
        str: Complete markdown content
    """
    if restrictions:
        verify_text = "\n".join(f"* {r}" for r in restrictions)
    else:
        verify_text = "None."

    # Get existing documentation or use placeholder
    doc_text = "_No documentation yet._"
    if existing_sections and "documentation" in existing_sections:
        doc_text = existing_sections["documentation"]

    md = f"""# {name}

## ASR

<!-- BEGIN AUTO: asr -->
```
{signature}
```
<!-- END AUTO: asr -->

## Documentation

{doc_text}

## Verify

<!-- BEGIN AUTO: verify -->
{verify_text}
<!-- END AUTO: verify -->
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
