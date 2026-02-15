#!/usr/bin/env bash

set -ex

dest="$1"
cmake -E make_directory $dest

# Copy Directories:
cmake -E copy_directory src $dest/src

# Remove build artifacts from src/runtime to avoid CMakeCache.txt path conflicts
# Use nullglob so globs that match nothing expand to nothing (not the literal pattern)
# Use || true to prevent failure when no files exist (e.g., on Windows)
shopt -s nullglob
rm -rf $dest/src/runtime/CMakeCache.txt \
    $dest/src/runtime/CMakeFiles \
    $dest/src/runtime/cmake_install.cmake \
    $dest/src/runtime/build.ninja \
    $dest/src/runtime/.ninja_log \
    $dest/src/runtime/*.mod \
    $dest/src/runtime/*.so \
    $dest/src/runtime/*.so.* \
    $dest/src/runtime/lfortran_compiler.stamp || true
shopt -u nullglob
cmake -E copy_directory share $dest/share
cmake -E copy_directory cmake $dest/cmake
cmake -E copy_directory examples $dest/examples
cmake -E copy_directory doc/man $dest/doc/man

# Copy Files:
cmake -E copy CMakeLists.txt README.md LICENSE version $dest

# Create the tarball
cmake -E make_directory dist
cmake -E tar cfz dist/$dest.tar.gz $dest
cmake -E remove_directory $dest
