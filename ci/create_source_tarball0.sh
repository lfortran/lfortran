#!/usr/bin/env bash

set -ex

dest="$1"
cmake -E make_directory $dest

# Copy Directories:
cmake -E copy_directory src $dest/src
cmake -E copy_directory share $dest/share
cmake -E copy_directory cmake $dest/cmake
cmake -E copy_directory examples $dest/examples
cmake -E copy_directory doc/man $dest/doc/man
# tests/asr/check_docs.py reads the ASR node documentation, so the
# tarball has to carry it for `ctest` to be able to run that test.
cmake -E copy_directory doc/src/asr $dest/doc/src/asr
cmake -E copy_directory tests/asr $dest/tests/asr
# test_lfortran pins the launch geometry, and the arrays the launches
# overwrite, of these integration tests.
cmake -E make_directory $dest/integration_tests
cmake -E copy integration_tests/gpu_metal_485.f90 \
    integration_tests/gpu_metal_490.f90 integration_tests/gpu_metal_491.f90 \
    integration_tests/gpu_metal_527.f90 \
    integration_tests/gpu_tile_high_bounds_i4.f90 \
    integration_tests/gpu_tile_high_bounds_i8.f90 \
    $dest/integration_tests

# Copy Files:
cmake -E copy CMakeLists.txt README.md LICENSE version $dest

# Create the tarball
cmake -E make_directory dist
cmake -E tar cfz dist/$dest.tar.gz $dest
cmake -E remove_directory $dest
