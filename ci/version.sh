#!/usr/bin/env bash

# This script extracts the project's current version from git using
# `git describe`, which determines the version based on the latest tag, such as:
#
#     $ git describe --tags --dirty
#     v0.6.0-37-g3878937f-dirty
#
# Each tag starts with "v", so we strip the "v", and the final version becomes:
#
#     0.6.0-37-g3878937f-dirty
#

set -ex

# Set default Git identity if not configured
git config user.name "GitHub Actions"
git config user.email "runner@github.com"

# Check if there are uncommitted changes
if [[ -n $(git status --porcelain) ]]; then
  echo "There are uncommitted changes. Committing them..."
  git add .    # Stage all changes
  git commit -m "Auto commit before tagging"  # Commit with a default message
fi

# Check if tags exist; if not, create a default tag
if ! git tag -l | grep -q "v"; then
  git tag v0.0.0
  echo "No tags found. Created default tag v0.0.0"
else
  echo "Tag already exists."
fi

# Extract version using git describe
version=$(git describe --tags --dirty)
version="${version:1}"

# Save version to a file
echo $version > version