"""
Utility functions for ASR documentation generator.

File operations, hashing, and diff display.
"""

import hashlib
import difflib
from pathlib import Path

# ANSI color codes
RED = "\033[91m"
GREEN = "\033[92m"
CYAN = "\033[96m"
BOLD = "\033[1m"
RESET = "\033[0m"
BLUE = "\033[94m"


def write_if_changed(path, content):
    """
    Write content to file only if it differs from existing content.

    Args:
        path: Path to the file
        content: Content to write

    Returns:
        bool: True if file was written, False if unchanged
    """
    if isinstance(path, str):
        path = Path(path)

    path.parent.mkdir(parents=True, exist_ok=True)

    if path.exists():
        existing = path.read_text()
        if existing == content:
            return False

    path.write_text(content)
    return True


def file_hash(path):
    """Compute SHA256 hash of a file."""
    hasher = hashlib.sha256()
    with open(path, "rb") as f:
        while chunk := f.read(8192):
            hasher.update(chunk)
    return hasher.hexdigest()


def snapshot_directory_hashes(directory):
    """
    Create a snapshot of file hashes in a directory.

    Args:
        directory: Path to directory

    Returns:
        dict: Mapping of file paths to their hashes
    """
    if isinstance(directory, str):
        directory = Path(directory)

    hashes = {}
    if directory.exists():
        for path in directory.rglob("*.md"):
            hashes[path] = file_hash(path)
    return hashes


def read_file_content(path):
    """Safely read file content, returning empty string on error."""
    try:
        return Path(path).read_text()
    except Exception:
        return ""


def show_diff(old_content, new_content, filename):
    """
    Display a colored unified diff between old and new content.

    Args:
        old_content: Previous file content
        new_content: New file content
        filename: Name to display in diff header
    """
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
