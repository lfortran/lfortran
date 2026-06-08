#!/usr/bin/env bash

set -ex

echo "CONDA_PREFIX=$CONDA_PREFIX"

# Use freshly built LFortran

export PATH="$PWD/src/bin:$PATH"

which lfortran
lfortran --version

micromamba install -c conda-forge fpm=0.12.0

which fpm
fpm --version

micromamba install -y -c conda-forge openmpi

git clone https://github.com/sourceryinstitute/OpenCoarrays.git
cd OpenCoarrays

cmake -B build \
  -DCMAKE_INSTALL_PREFIX="$HOME/opencoarrays"

cmake --build build -j2
cmake --install build

export PATH="$HOME/opencoarrays/bin:$PATH"

which caf
caf --version

cd ..

# Clone caffeine

git clone -b main https://github.com/BerkeleyLab/caffeine.git
cd caffeine

git checkout 0388cf70cd193214952d8be9a00e968c4c5061e2

# Toolchain setup

export FC=lfortran
export CC=clang
export CXX=clang++

echo "FC=${FC}"
echo "CC=${CC}"
echo "CXX=${CXX}"
which clang
clang --version

which caf
caf --version

which cafrun
cafrun --version

# GASNet debug options

export GASNET_CONFIGURE_ARGS="--enable-rpath --enable-debug"

# Build caffeine

./install.sh --yes --prefix=$PWD/inst --verbose

cd ..

# Make caffeine launcher available

export PATH="$PWD/caffeine/inst/bin:$PATH"

# Number of coarray images

CAF_IMAGES=${CAF_IMAGES:-2}

echo "Using CAF_IMAGES=$CAF_IMAGES"

# Remove benign STOP output differences

normalize_output() {
    sed '/^STOP$/d' | awk '{$1=$1;print}' | sort
}

# Find all coarray-enabled tests

tests=$(python3 -c '
filenames = []
current_file = None

for line in open("tests/tests.toml"):
    line = line.strip()

    if line.startswith("[[test]]"):
        current_file = None

    elif line.startswith("filename ="):
        current_file = "tests/" + line.split("\"")[1]

    elif "--coarray=true" in line:
        if current_file:
            filenames.append(current_file)

print(" ".join(filenames))
')

if [ -z "$tests" ]; then
echo "No coarray tests found"
exit 1
fi

for testfile in $tests; do
echo "========================================="
echo "Running coarray test: $testfile"
echo "========================================="


base=$(basename "$testfile" .f90)

# ----------------------------------------
# Compile with LFortran + caffeine
# ----------------------------------------

lfortran "$testfile" \
    --coarray=true \
    -o "${base}_lf.out" \
    -L$PWD/caffeine/inst/lib \
    -lcaffeine \
    -lgasnet-smp-seq

# ----------------------------------------
# Compile with gfortran/OpenCoarrays
# ----------------------------------------

caf "$testfile" -o "${base}_gf.out"

# ----------------------------------------
# Run LFortran executable
# ----------------------------------------

gasnetrun_smp -n "$CAF_IMAGES" ./"${base}_lf.out"


# ----------------------------------------
# Run gfortran executable
# ----------------------------------------

cafrun -np "$CAF_IMAGES" ./"${base}_gf.out"

echo "PASS: $testfile"

rm -f "${base}_lf.out" "${base}_gf.out"


done

echo
echo "All coarray runtime tests passed"

rm -rf caffeine
