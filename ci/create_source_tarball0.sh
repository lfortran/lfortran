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
# overwrite, of these integration tests; its interactive stdin cases run
# runtime_input_records.f90.
# test_kokkos_standard compiles kokkos_cpp_standard.f90 with installed Kokkos.
cmake -E make_directory $dest/integration_tests
cmake -E copy integration_tests/gpu_metal_485.f90 \
    integration_tests/gpu_metal_490.f90 integration_tests/gpu_metal_491.f90 \
    integration_tests/gpu_metal_527.f90 \
    integration_tests/gpu_tile_high_bounds_i4.f90 \
    integration_tests/gpu_tile_high_bounds_i8.f90 \
    integration_tests/runtime_input_records.f90 \
    integration_tests/kokkos_cpp_standard.f90 \
    $dest/integration_tests
# test_scoped_attribute_roundtrip regenerates these kernels and links their
# separately compiled pending operations.
cmake -E copy integration_tests/noalias_dummy_06.f90 \
    integration_tests/noalias_dummy_06_pending.f90 \
    integration_tests/dead_array_store_03.f90 \
    integration_tests/dead_array_store_03_pending.f90 \
    $dest/integration_tests

# Copy Files:
cmake -E copy CMakeLists.txt README.md LICENSE version $dest

# Create the tarball
cmake -E make_directory dist
cmake -E tar cfz dist/$dest.tar.gz $dest
cmake -E remove_directory $dest
