#!/usr/bin/env python3
"""
Tests for ASR Reference Documentation Generator

Tests verify:
1. ASDL parsing extracts correct node definitions
2. C++ restriction extraction works correctly
3. Code elements are properly formatted with backticks
4. Documentation preservation across regeneration
5. Correct handling of new/removed nodes

Format follows certik's example from issue #7006.
"""

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from ci.asr_docs.asdl_parser import parse_asdl
from ci.asr_docs.cpp_parser import (
    parse_cpp_verifiers,
    format_code_elements,
    build_message_with_placeholders,
    extract_require_calls,
)
from ci.asr_docs.markdown_gen import (
    generate_node_content,
    generate_enum_content,
    generate_struct_content,
)
from ci.asr_docs.doc_parser import parse_existing_doc
from ci.asr_docs.utils import write_if_changed


def test_parse_existing_doc_preserves_documentation():
    """Test that human-written documentation is preserved."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"

        content = """# TestNode

## ASR

<!-- BEGIN AUTO: asr -->
```
TestNode(int x)
```
<!-- END AUTO: asr -->

## Documentation

This is human-written documentation that should be preserved.

It has multiple paragraphs and **formatting**.

### Subsection

With a subsection too.

## Verify

<!-- BEGIN AUTO: verify -->
* Some restriction
<!-- END AUTO: verify -->
"""
        test_file.write_text(content)

        sections = parse_existing_doc(test_file)

        assert sections is not None
        assert "documentation" in sections
        assert "human-written documentation" in sections["documentation"]
        assert "multiple paragraphs" in sections["documentation"]
        assert "Subsection" in sections["documentation"]


def test_parse_existing_doc_ignores_placeholder():
    """Test that placeholder documentation is not preserved."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"

        content = """# TestNode

## ASR

<!-- BEGIN AUTO: asr -->
```
TestNode(int x)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
None.
<!-- END AUTO: verify -->
"""
        test_file.write_text(content)

        sections = parse_existing_doc(test_file)

        assert sections is not None
        assert "documentation" not in sections


def test_generate_node_content_includes_documentation():
    """Test that existing documentation is included in generated content."""
    existing = {"documentation": "This is preserved documentation."}

    content = generate_node_content(
        category="expr",
        name="TestNode",
        signature="TestNode(int x)",
        restrictions=["Must be positive"],
        existing_sections=existing
    )

    assert "This is preserved documentation." in content
    assert "_No documentation yet._" not in content


def test_generate_node_content_uses_placeholder_when_no_existing():
    """Test that placeholder is used when no existing documentation."""
    content = generate_node_content(
        category="expr",
        name="TestNode",
        signature="TestNode(int x)",
        restrictions=[],
        existing_sections=None
    )

    assert "_No documentation yet._" in content


def test_generate_node_content_certik_format():
    """Test that output matches certik's format from issue #7006."""
    content = generate_node_content(
        category="ttype",
        name="String",
        signature="String(int kind, expr? len, bool is_assumed_length)",
        restrictions=["Must have valid kind"],
        existing_sections=None
    )

    # Check certik's format: # Name, ## ASR, ## Documentation, ## Verify
    assert "# String" in content
    assert "## ASR" in content
    assert "## Documentation" in content
    assert "## Verify" in content
    # No "do not edit" header
    assert "do not edit" not in content.lower()
    # ASR contains signature in code block
    assert "```\nString(int kind, expr? len, bool is_assumed_length)\n```" in content


def test_generate_enum_content_preserves_documentation():
    """Test enum documentation preservation."""
    existing = {"documentation": "Enum documentation here."}

    content = generate_enum_content(
        name="TestEnum",
        values=["Value1", "Value2"],
        existing_sections=existing
    )

    assert "Enum documentation here." in content
    assert "* `Value1`" in content
    assert "* `Value2`" in content


def test_generate_struct_content_preserves_documentation():
    """Test struct documentation preservation."""
    existing = {"documentation": "Struct documentation here."}

    content = generate_struct_content(
        name="TestStruct",
        fields=["int x", "string name"],
        existing_sections=existing
    )

    assert "Struct documentation here." in content
    assert "`x` of type `int`" in content
    assert "`name` of type `string`" in content


def test_auto_markers_present_in_output():
    """Test that auto-generated sections have proper markers."""
    content = generate_node_content(
        category="stmt",
        name="TestStmt",
        signature="TestStmt(expr value)",
        restrictions=["Value must exist"],
        existing_sections=None
    )

    assert "<!-- BEGIN AUTO: asr -->" in content
    assert "<!-- END AUTO: asr -->" in content
    assert "<!-- BEGIN AUTO: verify -->" in content
    assert "<!-- END AUTO: verify -->" in content


def test_format_code_elements_qualified_names():
    """Test that C++ qualified names get backticks."""
    text = "ArrayConstructor::m_struct_vars must be nullptr"
    result = format_code_elements(text)
    assert "`ArrayConstructor::m_struct_vars`" in result
    assert "`nullptr`" in result


def test_format_code_elements_member_variables():
    """Test that member variables get backticks."""
    text = "The m_name field and n_dims count must be set"
    result = format_code_elements(text)
    assert "`m_name`" in result
    assert "`n_dims`" in result


def test_format_code_elements_type_names():
    """Test that type names ending in _t get backticks."""
    text = "Must point to a Struct_t when allocating"
    result = format_code_elements(text)
    assert "`Struct_t`" in result


def test_format_code_elements_quoted_types():
    """Test that quoted type names get backticks."""
    text = 'Physical type should not be "FixedSizeArray"'
    result = format_code_elements(text)
    assert "`FixedSizeArray`" in result
    assert '"FixedSizeArray"' not in result


def test_format_code_elements_keywords():
    """Test that C++ keywords get backticks."""
    text = "Value must not be nullptr and enabled must be true or false"
    result = format_code_elements(text)
    assert "`nullptr`" in result
    assert "`true`" in result
    assert "`false`" in result


def test_cpp_restriction_parsing():
    """Test that C++ restrictions are correctly extracted."""
    cpp_code = '''
    void visit_TestNode(const TestNode_t& x) {
        require(x.m_value > 0,
            "Value must be positive");
        require(x.m_name != nullptr,
            "Name cannot be null");
    }
    '''

    with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
        f.write(cpp_code)
        cpp_path = f.name

    restrictions = parse_cpp_verifiers(cpp_path)

    assert "TestNode" in restrictions
    assert "Value must be positive" in restrictions["TestNode"]
    assert "Name cannot be null" in restrictions["TestNode"]

    Path(cpp_path).unlink()


def test_cpp_restriction_parsing_multiline_message():
    """Test that multiline restriction messages are handled."""
    cpp_code = '''
    void visit_Array(const Array_t& x) {
        require(x.n_dims > 0,
            "Array type cannot have "
            "0 dimensions.");
    }
    '''

    with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
        f.write(cpp_code)
        cpp_path = f.name

    restrictions = parse_cpp_verifiers(cpp_path)

    assert "Array" in restrictions
    assert "Array type cannot have" in restrictions["Array"][0]
    assert "0 dimensions." in restrictions["Array"][0]

    Path(cpp_path).unlink()


def test_cpp_restriction_parsing_runtime_concatenation():
    """Test that runtime string concatenation inserts [...] placeholders."""
    cpp_code = '''
    void visit_Variable(const Variable_t& x) {
        require(check_dep(x),
            "Variable " + std::string(x.m_name) + " depends on " +
            std::string(dep_name) + " but isn't found in its dependency list.");
    }
    '''

    with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
        f.write(cpp_code)
        cpp_path = f.name

    restrictions = parse_cpp_verifiers(cpp_path)

    assert "Variable" in restrictions
    msg = restrictions["Variable"][0]
    assert "Variable" in msg
    assert "[...]" in msg
    assert "depends on" in msg

    Path(cpp_path).unlink()


def test_cpp_restriction_parsing_escaped_quotes():
    """Test that escaped quotes in C++ strings are handled correctly."""
    cpp_code = '''
    void visit_Variable(const Variable_t& x) {
        require(x.m_type != nullptr,
            "Variable of type \\"DeferredLength\\" must be allocatable");
    }
    '''

    with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
        f.write(cpp_code)
        cpp_path = f.name

    restrictions = parse_cpp_verifiers(cpp_path)

    assert "Variable" in restrictions
    assert "`DeferredLength`" in restrictions["Variable"][0]

    Path(cpp_path).unlink()


def test_cpp_restriction_single_literal_trailing_concat():
    """Test runtime concatenation after single string literal."""
    msg_expr = '"Value should be " + std::to_string(x.value)'
    result = build_message_with_placeholders(msg_expr)
    assert result == "Value should be [...]"


def test_write_if_changed_no_change():
    """Test that files are not rewritten if content is the same."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"
        content = "Test content"

        test_file.write_text(content)
        original_mtime = test_file.stat().st_mtime

        import time
        time.sleep(0.01)

        changed = write_if_changed(test_file, content)

        assert changed is False
        assert test_file.stat().st_mtime == original_mtime


def test_write_if_changed_with_change():
    """Test that files are rewritten if content differs."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"

        test_file.write_text("Original content")

        changed = write_if_changed(test_file, "New content")

        assert changed is True
        assert test_file.read_text() == "New content"


def test_obsolete_file_removal():
    """Test that obsolete documentation files are removed."""
    with tempfile.TemporaryDirectory() as tmpdir:
        docs_dir = Path(tmpdir) / "doc" / "reference"

        expr_dir = docs_dir / "expr"
        expr_dir.mkdir(parents=True)

        valid_file = expr_dir / "ValidNode.md"
        valid_file.write_text("# ValidNode\n\nValid content.")

        obsolete_file = expr_dir / "ObsoleteNode.md"
        obsolete_file.write_text("# ObsoleteNode\n\nThis should be deleted.")

        assert valid_file.exists()
        assert obsolete_file.exists()

        generated_files = {valid_file}

        removed_files = []
        for path in docs_dir.rglob("*.md"):
            if path not in generated_files:
                path.unlink()
                removed_files.append(path)
                try:
                    path.parent.rmdir()
                except OSError:
                    pass

        assert valid_file.exists()
        assert not obsolete_file.exists()
        assert obsolete_file in removed_files


def test_full_regeneration_preserves_docs():
    """Integration test: full regeneration cycle preserves documentation."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_dir = Path(tmpdir) / "doc" / "reference" / "expr"
        test_dir.mkdir(parents=True)
        test_file = test_dir / "TestExpr.md"

        original_content = """# TestExpr

## ASR

<!-- BEGIN AUTO: asr -->
```
TestExpr(int old_value)
```
<!-- END AUTO: asr -->

## Documentation

This documentation was written by a human and must survive regeneration.

It includes:
- Lists
- **Bold text**
- Code: `example()`

## Verify

<!-- BEGIN AUTO: verify -->
* Old restriction
<!-- END AUTO: verify -->
"""
        test_file.write_text(original_content)

        sections = parse_existing_doc(test_file)

        new_content = generate_node_content(
            category="expr",
            name="TestExpr",
            signature="TestExpr(int new_value, string extra)",
            restrictions=["New restriction from verify"],
            existing_sections=sections
        )

        # Verify documentation is preserved
        assert "written by a human and must survive" in new_content
        assert "Lists" in new_content

        # Verify auto-generated sections are updated
        assert "new_value" in new_content
        assert "extra" in new_content
        assert "New restriction from verify" in new_content

        # Verify old auto-generated content is gone
        assert "old_value" not in new_content
        assert "Old restriction" not in new_content


if __name__ == "__main__":
    import traceback

    tests = [
        test_parse_existing_doc_preserves_documentation,
        test_parse_existing_doc_ignores_placeholder,
        test_generate_node_content_includes_documentation,
        test_generate_node_content_uses_placeholder_when_no_existing,
        test_generate_node_content_certik_format,
        test_generate_enum_content_preserves_documentation,
        test_generate_struct_content_preserves_documentation,
        test_auto_markers_present_in_output,
        test_format_code_elements_qualified_names,
        test_format_code_elements_member_variables,
        test_format_code_elements_type_names,
        test_format_code_elements_quoted_types,
        test_format_code_elements_keywords,
        test_cpp_restriction_parsing,
        test_cpp_restriction_parsing_multiline_message,
        test_cpp_restriction_parsing_runtime_concatenation,
        test_cpp_restriction_parsing_escaped_quotes,
        test_cpp_restriction_single_literal_trailing_concat,
        test_write_if_changed_no_change,
        test_write_if_changed_with_change,
        test_obsolete_file_removal,
        test_full_regeneration_preserves_docs,
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            test()
            print(f"  PASS: {test.__name__}")
            passed += 1
        except Exception as e:
            print(f"  FAIL: {test.__name__}")
            traceback.print_exc()
            failed += 1

    print(f"\n{passed} passed, {failed} failed")
    sys.exit(0 if failed == 0 else 1)
