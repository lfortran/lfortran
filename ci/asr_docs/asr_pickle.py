#!/usr/bin/env python3
"""
ASR Pickle Generator

Generates compact ASR "pickle" notation from Fortran code.
This shows concrete ASR instances like: String(1, (), True, False, DescriptorString)

Usage:
    python -m ci.asr_docs.asr_pickle "character(len=*), intent(in) :: x"
    python -m ci.asr_docs.asr_pickle path/to/file.f90
"""

import subprocess
import re
import sys
import tempfile
from pathlib import Path


def fortran_to_asr(fortran_code, lfortran_path="lfortran"):
    """
    Convert Fortran code to ASR using lfortran --show-asr.

    Args:
        fortran_code: Fortran source code string
        lfortran_path: Path to lfortran binary

    Returns:
        str: ASR output from lfortran
    """
    # Wrap snippet in a minimal program if needed
    if "program" not in fortran_code.lower() and "subroutine" not in fortran_code.lower():
        # It's a declaration snippet, wrap it
        fortran_code = f"""
subroutine test_snippet()
{fortran_code}
end subroutine
"""

    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(fortran_code)
        temp_path = f.name

    try:
        result = subprocess.run(
            [lfortran_path, "--show-asr", temp_path],
            capture_output=True,
            text=True,
            timeout=30
        )
        return result.stdout
    except subprocess.TimeoutExpired:
        return None
    except FileNotFoundError:
        return None
    finally:
        Path(temp_path).unlink(missing_ok=True)


def parse_asr_node(asr_text, node_name):
    """
    Extract a specific node from ASR output and convert to pickle format.

    The pickle format is compact: NodeName(arg1, arg2, ...)
    """
    # This is a simplified parser - full implementation would need proper ASR parsing
    # For now, we extract the key information

    lines = asr_text.split('\n')
    result = []

    # Find Variable nodes and extract their type info
    in_variable = False
    variable_info = {}
    indent_level = 0

    for line in lines:
        stripped = line.strip()

        if 'Variable' in stripped and not in_variable:
            in_variable = True
            variable_info = {'name': '', 'type': ''}
            indent_level = len(line) - len(line.lstrip())

        elif in_variable:
            current_indent = len(line) - len(line.lstrip())
            if current_indent <= indent_level and stripped and not stripped.startswith('('):
                in_variable = False
            else:
                # Extract variable name (usually second field)
                if stripped and not stripped.startswith('(') and not stripped.startswith('['):
                    if not variable_info['name']:
                        variable_info['name'] = stripped

    return asr_text


def asr_to_pickle(asr_output):
    """
    Convert verbose ASR output to compact pickle notation.

    Input (verbose):
        (String 1 () AssumedLength DescriptorString)

    Output (pickle):
        String(1, (), AssumedLength, DescriptorString)
    """
    if not asr_output:
        return None

    # Remove ANSI color codes
    asr_clean = re.sub(r'\x1b\[[0-9;]*m', '', asr_output)

    result_parts = []

    # Find String type patterns (new format with length_kind)
    # (String 1 () AssumedLength DescriptorString)
    # (String 1 (IntegerConstant 10 ...) ExpressionLength DescriptorString)
    string_match = re.search(
        r'\(String\s+(\d+)\s+(\([^)]*(?:\([^)]*\)[^)]*)*\))\s+([A-Za-z]+)\s+([A-Za-z]+)\s*\)',
        asr_clean
    )
    if string_match:
        kind = string_match.group(1)
        length_raw = string_match.group(2)
        length_kind = string_match.group(3)
        physical_type = string_match.group(4)

        # Simplify length expression
        if length_raw == "()":
            length = "()"
        else:
            # Extract constant value if it's IntegerConstant
            const_match = re.search(r'IntegerConstant\s+(\d+)', length_raw)
            if const_match:
                length = const_match.group(1)
            else:
                # It's a variable reference or expression
                var_match = re.search(r'Var\s+\d+\s+(\w+)', length_raw)
                if var_match:
                    length = var_match.group(1)
                else:
                    length = "<expr>"

        result_parts.append(f"String({kind}, {length}, {length_kind}, {physical_type})")

    # Find Integer type patterns
    int_match = re.search(r'\(Integer\s+(\d+)\s*\)', asr_clean)
    if int_match:
        kind = int_match.group(1)
        result_parts.append(f"Integer({kind})")

    # Find Real type patterns
    real_match = re.search(r'\(Real\s+(\d+)\s*\)', asr_clean)
    if real_match:
        kind = real_match.group(1)
        result_parts.append(f"Real({kind})")

    # Find Array type patterns
    array_match = re.search(
        r'\(Array\s+\(([^)]+)\)\s+\[([^\]]*)\]\s+([A-Za-z]+)\s*\)',
        asr_clean,
        re.DOTALL
    )
    if array_match:
        elem_type = array_match.group(1).strip()
        dims = array_match.group(2).strip()
        physical_type = array_match.group(3)
        result_parts.append(f"Array({elem_type}, [{dims}], {physical_type})")

    # Find Allocatable wrapper
    if 'Allocatable' in asr_clean:
        # Find what's inside Allocatable
        alloc_match = re.search(r'\(Allocatable\s+(\([^)]+\))\s*\)', asr_clean)
        if alloc_match:
            inner = alloc_match.group(1)
            # Try to parse inner type
            inner_pickle = asr_to_pickle(inner)
            if inner_pickle:
                result_parts = [f"Allocatable({p})" for p in inner_pickle]

    return result_parts if result_parts else None


def generate_pickle_for_fortran(fortran_snippet, lfortran_path="lfortran"):
    """
    Generate pickle notation for a Fortran code snippet.

    Args:
        fortran_snippet: Fortran code (can be a declaration or full program)
        lfortran_path: Path to lfortran binary

    Returns:
        str: Pickle notation or None if failed
    """
    asr = fortran_to_asr(fortran_snippet, lfortran_path)
    if not asr:
        return None

    pickled = asr_to_pickle(asr)
    return pickled


def main():
    """Command-line interface."""
    if len(sys.argv) < 2:
        print("Usage: python -m ci.asr_docs.asr_pickle <fortran_code_or_file>")
        print()
        print("Examples:")
        print('  python -m ci.asr_docs.asr_pickle "integer :: x = 5"')
        print('  python -m ci.asr_docs.asr_pickle test.f90')
        sys.exit(1)

    arg = sys.argv[1]

    # Check if it's a file
    if Path(arg).exists():
        fortran_code = Path(arg).read_text()
    else:
        fortran_code = arg

    # Try to find lfortran
    lfortran_path = "build/src/bin/lfortran"
    if not Path(lfortran_path).exists():
        lfortran_path = "lfortran"

    print(f"Fortran code:\n{fortran_code}\n")

    asr = fortran_to_asr(fortran_code, lfortran_path)
    if asr:
        print(f"ASR output:\n{asr}\n")

        pickled = asr_to_pickle(asr)
        if pickled:
            print(f"Pickle notation:")
            for p in pickled:
                print(f"  {p}")
    else:
        print("Failed to generate ASR (lfortran not found or error)")


if __name__ == "__main__":
    main()
