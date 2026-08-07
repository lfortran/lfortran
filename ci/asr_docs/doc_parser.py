"""
Documentation Parser for existing markdown files.

Parses existing documentation to preserve human-written content.
"""

import re
from pathlib import Path


def parse_existing_doc(path):
    """
    Parse existing markdown file and extract sections to preserve.

    Extracts:
    - Auto-generated sections (marked with BEGIN/END AUTO comments)
    - Documentation section (human-written)

    Args:
        path: Path to the markdown file

    Returns:
        dict: Mapping of section names to content, or None if file doesn't exist
    """
    if isinstance(path, str):
        path = Path(path)

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
