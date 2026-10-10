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

# Copy Files:
cmake -E copy CMakeLists.txt README.md LICENSE version $dest

# Runtime trait CTests need these drivers and their Fortran/C fixtures,
# not the whole integration test suite.
cmake -E make_directory $dest/integration_tests
cmake -E copy \
    integration_tests/traits_runtime_separate_01.py \
    integration_tests/traits_runtime_separate_01*.f90 \
    integration_tests/traits_runtime_owning_separate_01*.f90 \
    integration_tests/traits_runtime_pointer_separate_01*.f90 \
    integration_tests/traits_runtime_owning_failure_01.py \
    integration_tests/traits_runtime_owning_failure_01.f90 \
    integration_tests/traits_runtime_owning_failure_01.c \
    integration_tests/traits_runtime_result_02.py \
    integration_tests/traits_runtime_result_02.f90 \
    integration_tests/traits_runtime_factory_01.py \
    integration_tests/traits_runtime_factory_01*.f90 \
    integration_tests/traits_runtime_05*.f90 \
    integration_tests/traits_runtime_combination_01*.f90 \
    integration_tests/traits_runtime_inspection_separate_01*.f90 \
    integration_tests/traits_runtime_generic_01*.f90 \
    integration_tests/traits_runtime_numeric_separate_01.py \
    integration_tests/traits_runtime_numeric_separate_01*.f90 \
    integration_tests/traits_runtime_inspection_state_01.f90 \
    integration_tests/traits_runtime_07*.f90 \
    integration_tests/traits_paper_functional.py \
    integration_tests/traits_type_adoption.py \
    integration_tests/traits_type_adoption_02*.f90 \
    integration_tests/traits_type_adoption_04*.f90 \
    integration_tests/traits_paper_printy.py \
    integration_tests/traits_paper_printy.f90 \
    integration_tests/traits_intrinsic_02*.f90 \
    integration_tests/traits_paper_manual_values.f90 \
    $dest/integration_tests
cmake -E copy_directory integration_tests/traits_paper_functional \
    $dest/integration_tests/traits_paper_functional
cmake -E copy_directory integration_tests/traits_paper_dynamic \
    $dest/integration_tests/traits_paper_dynamic
cmake -E copy_directory integration_tests/traits_paper_type_adoption \
    $dest/integration_tests/traits_paper_type_adoption

# Create the tarball
cmake -E make_directory dist
cmake -E tar cfz dist/$dest.tar.gz $dest
cmake -E remove_directory $dest
