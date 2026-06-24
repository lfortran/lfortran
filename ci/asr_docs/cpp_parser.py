"""
C++ Parser for asr_verify.cpp

Extracts verifier functions and restriction messages from asr_verify.cpp.
"""

import re
from collections import defaultdict


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


def unescape_cpp_string(text):
    """Unescape C++ string escape sequences."""
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
    """
    # Find all string literals with their positions
    literal_pattern = re.compile(r'"((?:[^"\\]|\\.)*)"')
    literals = [(m.start(), m.end(), unescape_cpp_string(m.group(1)))
                for m in literal_pattern.finditer(msg_expr)]

    if not literals:
        return None

    has_concatenation = False

    # Check for trailing runtime concatenation after last literal
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
            between_stripped = between.replace('+', '').strip()
            if between_stripped:
                has_concatenation = True
                break

    if not has_concatenation:
        # Simple case: just string literal concatenation
        full_msg = ''.join(lit[2] for lit in literals)
        full_msg = full_msg.replace("''", "")
        full_msg = re.sub(r'\s+', ' ', full_msg).strip()
        return full_msg if full_msg else None

    # Complex case: runtime concatenation - insert [...] placeholders
    result_parts = []
    prev_end = 0

    for start, end, text in literals:
        between = msg_expr[prev_end:start] if prev_end > 0 else ""

        if prev_end > 0 and '+' in between:
            between_content = between.replace('+', '').strip()
            if between_content:
                result_parts.append('[...]')

        result_parts.append(text)
        prev_end = end

    # Check for trailing runtime content
    trailing = msg_expr[prev_end:].strip()
    if trailing and '+' in trailing:
        trailing_after_plus = trailing.split('+', 1)[-1].strip()
        if trailing_after_plus and not trailing_after_plus.startswith(')'):
            result_parts.append('[...]')

    full_msg = ''.join(result_parts)
    full_msg = full_msg.replace("''", "")
    full_msg = re.sub(r'\s+', ' ', full_msg).strip()
    return full_msg if full_msg else None


def format_code_elements(text):
    """
    Format code elements in restriction messages with backticks.

    Identifies and wraps:
    - C++ identifiers with :: (e.g., Node::m_member)
    - C++ member variables (m_name)
    - Type names ending in _t (e.g., Struct_t)
    - Keywords: nullptr, true, false
    - Quoted strings that look like type names
    """
    if not text:
        return text

    result = text

    # First, handle qualified names (Node::member) - replace with placeholder
    # to prevent member variable pattern from matching the part after ::
    placeholders = {}
    placeholder_idx = 0

    def save_qualified(m):
        nonlocal placeholder_idx
        key = f"__QUALIFIED_{placeholder_idx}__"
        placeholders[key] = f"`{m.group(1)}`"
        placeholder_idx += 1
        return key

    # Qualified names: Node::member or ASR::Type_t
    result = re.sub(r'\b([A-Z][A-Za-z0-9_]*::[A-Za-z0-9_:]+)\b', save_qualified, result)

    # Now apply remaining patterns
    patterns = [
        # Type names ending in _t: Struct_t, Array_t, etc.
        (r'\b([A-Z][A-Za-z0-9]*_t)\b', r'`\1`'),
        # Member variables: m_name, n_dims
        (r'\b(m_[a-z_][a-z0-9_]*|n_[a-z_][a-z0-9_]*)\b', r'`\1`'),
        # C++ keywords
        (r'\b(nullptr|true|false)\b', r'`\1`'),
        # Quoted type/physical names: "FixedSizeArray", "DescriptorString"
        (r'"([A-Z][A-Za-z0-9]+)"', r'`\1`'),
    ]

    for pattern, replacement in patterns:
        result = re.sub(pattern, replacement, result)

    # Restore placeholders
    for key, value in placeholders.items():
        result = result.replace(key, value)

    return result


def parse_cpp_verifiers(cpp_path):
    """
    Parse asr_verify.cpp and extract verifier information.

    Returns:
        dict: mapping node_name -> list of formatted restriction messages
    """
    with open(cpp_path, 'r') as f:
        cpp_text = f.read()

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
                    # Format code elements with backticks
                    formatted_msg = format_code_elements(full_msg)
                    restriction_map[node_name].append(formatted_msg)
        else:
            i += 1

    return restriction_map
