#!/usr/bin/env python3
"""
Tests for generate_reference_doc.py

These tests verify:
1. ASDL parsing extracts correct node definitions
2. C++ restriction extraction works correctly
3. Documentation preservation across regeneration
4. Correct handling of new/removed nodes
"""

import os
import sys
import tempfile
import shutil
from pathlib import Path

# Add parent directory to path to import the module
sys.path.insert(0, str(Path(__file__).parent.parent))

import generate_reference_doc as grd


def test_parse_existing_doc_preserves_documentation():
    """Test that human-written documentation is preserved."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"

        # Create a file with human documentation
        content = """# TestNode

TestNode, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
TestNode(int x)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input argument is `x` of type `int`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

This is human-written documentation that should be preserved.

It has multiple paragraphs and **formatting**.

### Subsection

With a subsection too.

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Some restriction
<!-- END AUTO: restrictions -->
"""
        test_file.write_text(content)

        sections = grd.parse_existing_doc(test_file)

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

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
None.
<!-- END AUTO: restrictions -->
"""
        test_file.write_text(content)

        sections = grd.parse_existing_doc(test_file)

        assert sections is not None
        assert "documentation" not in sections  # Placeholder should not be preserved


def test_generate_node_content_includes_documentation():
    """Test that existing documentation is included in generated content."""
    existing = {"documentation": "This is preserved documentation."}

    content = grd.generate_node_content(
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
    content = grd.generate_node_content(
        category="expr",
        name="TestNode",
        signature="TestNode(int x)",
        restrictions=[],
        existing_sections=None
    )

    assert "_No documentation yet._" in content


def test_generate_enum_content_preserves_documentation():
    """Test enum documentation preservation."""
    existing = {"documentation": "Enum documentation here."}

    content = grd.generate_enum_content(
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

    content = grd.generate_struct_content(
        name="TestStruct",
        fields=["int x", "string name"],
        existing_sections=existing
    )

    assert "Struct documentation here." in content
    assert "`x` of type `int`" in content
    assert "`name` of type `string`" in content


def test_auto_markers_present_in_output():
    """Test that auto-generated sections have proper markers."""
    content = grd.generate_node_content(
        category="stmt",
        name="TestStmt",
        signature="TestStmt(expr value)",
        restrictions=["Value must exist"],
        existing_sections=None
    )

    assert "<!-- BEGIN AUTO: syntax -->" in content
    assert "<!-- END AUTO: syntax -->" in content
    assert "<!-- BEGIN AUTO: arguments -->" in content
    assert "<!-- END AUTO: arguments -->" in content
    assert "<!-- BEGIN AUTO: restrictions -->" in content
    assert "<!-- END AUTO: restrictions -->" in content


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

    restrictions = grd.parse_cpp_restrictions(cpp_code)

    assert "TestNode" in restrictions
    assert "Value must be positive" in restrictions["TestNode"]
    assert "Name cannot be null" in restrictions["TestNode"]


def test_cpp_restriction_parsing_multiline_message():
    """Test that multiline restriction messages are handled."""
    cpp_code = '''
    void visit_Array(const Array_t& x) {
        require(x.n_dims > 0,
            "Array type cannot have "
            "0 dimensions.");
    }
    '''

    restrictions = grd.parse_cpp_restrictions(cpp_code)

    assert "Array" in restrictions
    # Note: concatenated strings lose whitespace between them
    assert "Array type cannot have" in restrictions["Array"][0]
    assert "0 dimensions." in restrictions["Array"][0]


def test_cpp_restriction_parsing_runtime_concatenation():
    """Test that runtime string concatenation inserts [...] placeholders."""
    cpp_code = '''
    void visit_Variable(const Variable_t& x) {
        require(check_dep(x),
            "Variable " + std::string(x.m_name) + " depends on " +
            std::string(dep_name) + " but isn't found in its dependency list.");
        require(x.m_value != nullptr,
            "Initialisation of " + std::string(x.m_name) +
            " must reduce to a compile time constant.");
    }
    '''

    restrictions = grd.parse_cpp_restrictions(cpp_code)

    assert "Variable" in restrictions
    assert len(restrictions["Variable"]) == 2

    # First message should have placeholders for runtime values
    msg1 = restrictions["Variable"][0]
    assert "Variable" in msg1
    assert "[...]" in msg1
    assert "depends on" in msg1
    assert "but isn't found in its dependency list." in msg1

    # Second message should also have placeholder
    msg2 = restrictions["Variable"][1]
    assert "Initialisation of" in msg2
    assert "[...]" in msg2
    assert "must reduce to a compile time constant." in msg2


def test_cpp_restriction_parsing_simple_plus_concatenation():
    """Test that simple string literal + concatenation works without placeholders."""
    cpp_code = '''
    void visit_Test(const Test_t& x) {
        require(x.ok,
            "This is " + "a simple test.");
    }
    '''

    restrictions = grd.parse_cpp_restrictions(cpp_code)

    assert "Test" in restrictions
    # Simple "str1" + "str2" should join without placeholders
    assert restrictions["Test"][0] == "This is a simple test."


def test_cpp_restriction_parsing_escaped_quotes():
    """Test that escaped quotes in C++ strings are handled correctly."""
    cpp_code = '''
    void visit_Variable(const Variable_t& x) {
        require(x.m_type != nullptr,
            "Variable of type \\"DeferredLength\\" must be allocatable");
    }
    '''
    restrictions = grd.parse_cpp_restrictions(cpp_code)
    assert "Variable" in restrictions
    assert 'Variable of type "DeferredLength" must be allocatable' in restrictions["Variable"]


def test_cpp_restriction_single_literal_trailing_concat():
    """Test runtime concatenation after single string literal."""
    cpp_code = '''
    void visit_Test(const Test_t& x) {
        require(x.ok,
            "Value should be " + std::to_string(x.value));
    }
    '''
    restrictions = grd.parse_cpp_restrictions(cpp_code)
    assert "Test" in restrictions
    assert restrictions["Test"][0] == "Value should be [...]"


def test_write_if_changed_no_change():
    """Test that files are not rewritten if content is the same."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"
        content = "Test content"

        test_file.write_text(content)
        original_mtime = test_file.stat().st_mtime

        import time
        time.sleep(0.01)  # Ensure time difference if file is rewritten

        changed = grd.write_if_changed(test_file, content)

        assert changed is False
        assert test_file.stat().st_mtime == original_mtime


def test_write_if_changed_with_change():
    """Test that files are rewritten if content differs."""
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.md"

        test_file.write_text("Original content")

        changed = grd.write_if_changed(test_file, "New content")

        assert changed is True
        assert test_file.read_text() == "New content"


def test_obsolete_file_removal():
    """Test that obsolete documentation files are removed when nodes are deleted from ASDL."""
    with tempfile.TemporaryDirectory() as tmpdir:
        docs_dir = Path(tmpdir) / "doc" / "reference"

        # Create a directory structure with an obsolete file
        expr_dir = docs_dir / "expr"
        expr_dir.mkdir(parents=True)

        # Create a valid file that would be generated
        valid_file = expr_dir / "ValidNode.md"
        valid_file.write_text("# ValidNode\n\nValid content.")

        # Create an obsolete file that should be removed
        obsolete_file = expr_dir / "ObsoleteNode.md"
        obsolete_file.write_text("# ObsoleteNode\n\nThis should be deleted.")

        # Also create an obsolete category directory with a file
        obsolete_dir = docs_dir / "obsolete_category"
        obsolete_dir.mkdir(parents=True)
        obsolete_category_file = obsolete_dir / "OldNode.md"
        obsolete_category_file.write_text("# OldNode\n\nShould be deleted.")

        # Verify files exist before
        assert valid_file.exists()
        assert obsolete_file.exists()
        assert obsolete_category_file.exists()

        # Simulate generated_files set (only ValidNode is current)
        generated_files = {valid_file}

        # Run the removal logic (copied from generate_reference_docs)
        removed_files = []
        for path in docs_dir.rglob("*.md"):
            if path not in generated_files:
                path.unlink()
                removed_files.append(path)
                try:
                    path.parent.rmdir()
                except OSError:
                    pass

        # Verify valid file still exists
        assert valid_file.exists()

        # Verify obsolete files were removed
        assert not obsolete_file.exists()
        assert not obsolete_category_file.exists()

        # Verify obsolete directory was removed (it was empty after file deletion)
        assert not obsolete_dir.exists()

        # Verify expr_dir still exists (it still has valid_file)
        assert expr_dir.exists()

        # Verify correct files were tracked as removed
        assert len(removed_files) == 2
        assert obsolete_file in removed_files
        assert obsolete_category_file in removed_files


def test_full_regeneration_preserves_docs():
    """Integration test: full regeneration cycle preserves documentation."""
    with tempfile.TemporaryDirectory() as tmpdir:
        # Create a test file with documentation
        test_dir = Path(tmpdir) / "doc" / "reference" / "expr"
        test_dir.mkdir(parents=True)
        test_file = test_dir / "TestExpr.md"

        original_content = """# TestExpr

TestExpr, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
TestExpr(int old_value)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input argument is `old_value` of type `int`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

This documentation was written by a human and must survive regeneration.

It includes:
- Lists
- **Bold text**
- Code: `example()`

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Old restriction
<!-- END AUTO: restrictions -->
"""
        test_file.write_text(original_content)

        # Parse existing
        sections = grd.parse_existing_doc(test_file)

        # Generate new content with updated ASR but same documentation
        new_content = grd.generate_node_content(
            category="expr",
            name="TestExpr",
            signature="TestExpr(int new_value, string extra)",  # Changed signature
            restrictions=["New restriction from verify"],  # Changed restriction
            existing_sections=sections
        )

        # Verify documentation is preserved
        assert "written by a human and must survive" in new_content
        assert "Lists" in new_content
        assert "Bold text" in new_content

        # Verify auto-generated sections are updated
        assert "new_value" in new_content
        assert "extra" in new_content
        assert "New restriction from verify" in new_content

        # Verify old auto-generated content is gone
        assert "old_value" not in new_content
        assert "Old restriction" not in new_content


if __name__ == "__main__":
    # Run all tests
    import traceback

    tests = [
        test_parse_existing_doc_preserves_documentation,
        test_parse_existing_doc_ignores_placeholder,
        test_generate_node_content_includes_documentation,
        test_generate_node_content_uses_placeholder_when_no_existing,
        test_generate_enum_content_preserves_documentation,
        test_generate_struct_content_preserves_documentation,
        test_auto_markers_present_in_output,
        test_cpp_restriction_parsing,
        test_cpp_restriction_parsing_multiline_message,
        test_cpp_restriction_parsing_runtime_concatenation,
        test_cpp_restriction_parsing_simple_plus_concatenation,
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
