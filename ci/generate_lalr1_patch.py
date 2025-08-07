#!/usr/bin/env python3

"""
This script generates the parser.yy.patch that makes the GLR parser a LALR(1)
parser.

Usage:

    ci/generate_lalr1_patch.py

"""

import subprocess

# Check if git diff shows any changes using return code
result = subprocess.run(['git', 'diff'], capture_output=True, text=True)
if result.returncode != 0:
    print("Git diff shows changes. Please commit the changes first.")
    exit(1)

# Define file paths
file_path = 'src/lfortran/parser/parser.yy'
patch_path = 'ci/parser.yy.patch'

# Read the file
with open(file_path, 'r') as f:
    lines = f.readlines()

# Modify common_block_list section
for i in range(len(lines) - 4):
    if lines[i].strip() == 'common_block_list' and \
       'common_block_list TK_COMMA common_block' in lines[i+1]:
        # Delete the first two alternative lines
        del lines[i+1:i+3]
        # Replace '| ' with ': ' in the now-next line
        lines[i+1] = lines[i+1].replace('| ', ': ', 1)
        break

# Modify id section
for j in range(len(lines) - 1):
    if lines[j].strip() == 'id' and \
       ': TK_NAME { $$ = SYMBOL($1, @$); }' in lines[j+1]:
        # Delete all subsequent | KW_* lines until ;
        k = j + 2
        while k < len(lines) and lines[k].strip() != ';':
            stripped_line = lines[k].strip()
            if not stripped_line.startswith('| KW'):
                raise ValueError(f"Unexpected line in id section: {stripped_line}")
            del lines[k]
            # No k += 1, as del shifts the list
        break

# Write modified lines back to the file
with open(file_path, 'w') as f:
    f.writelines(lines)

# Generate the patch using git diff
with open(patch_path, 'w') as patch_file:
    subprocess.run(['git', 'diff', '--', file_path], stdout=patch_file, check=True)

# Restore the original file to remove changes
subprocess.run(['git', 'restore', file_path], check=True)

print(f"Patch generated and saved to {patch_path}")
