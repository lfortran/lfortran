#!/usr/bin/env bash

set -ex

dest="$1"
lfortran_version="$2"
cmake -E make_directory $dest

# Copy Directories:
cmake -E copy_directory src $dest/src
cmake -E copy_directory share $dest/share
cmake -E copy_directory cmake $dest/cmake
cmake -E copy_directory examples $dest/examples
cmake -E copy_directory doc/man $dest/doc/man

# Copy Files:
cmake -E copy CMakeLists.txt README.md LICENSE $dest
printf '%s\n' "$lfortran_version" > "$dest/version"

# Create the tarball
cmake -E make_directory dist
cmake -E tar cfz dist/$dest.tar.gz $dest
cmake -E remove_directory $dest
