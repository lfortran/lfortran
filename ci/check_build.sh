#!/usr/bin/env shell
# This is a cross-platform `shell` script.

# `ci/check_build.sh <build_dir>`: install and check the compiler built from
# the source tarball (ci/source_tarball.sh) in <build_dir>, then
# copy it into src/ for the test scripts that run the in-tree compiler. Pass
# the build directory relative to the project root: forward slashes keep the
# globs below working on Windows.

set -ex

build_dir="$1"
root=$(pwd)
source_dir="$root/lfortran-src"

cmake --build "$build_dir" --target install
cd "$build_dir"
./src/lfortran/tests/test_lfortran
./src/bin/lfortran < "$source_dir/src/bin/example_input.txt"
ctest --output-on-failure
cpack -V
cd "$root"

jupyter kernelspec list --json
python ci/test_fortran_kernel.py -v

cd share/lfortran/nb
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output Demo1_out.ipynb Demo1.ipynb
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output Demo2_out.ipynb Demo2.ipynb
cat Demo1_out.ipynb
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output "Hello World_out.ipynb" "Hello World.ipynb"
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output "Operators Control Flow_out.ipynb" "Operators Control Flow.ipynb"
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.timeout=120 --output Variables_out.ipynb Variables.ipynb
cd "$root"

if [[ $WIN == "1" ]]; then # Windows
    cp "$build_dir/src/bin/lfortran.exe" src/bin
    cp "$build_dir"/src/runtime/lfortran_runtime* src/runtime/
else # Linux or macOS
    cp "$build_dir/src/bin/lfortran" src/bin
    cp "$build_dir"/src/runtime/liblfortran_runtime* src/runtime/
fi
cp "$build_dir"/src/runtime/*.mod src/runtime/
