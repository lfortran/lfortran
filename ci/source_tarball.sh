#!/usr/bin/env shell
# This is a cross-platform `shell` script.

# Generate the sources, create the source tarball and unpack it into
# lfortran-src/. CI then builds it with `pixi run build`
# (LFORTRAN_SOURCE_DIR=lfortran-src), so the release tarball itself is tested.

set -ex

shell build0.sh

pandoc --standalone --to man doc/man/lfortran.md -o doc/man/lfortran.1

# using debugging option i.e. `-x` causes a bug with `cat` command here,
# and hence we turned off command tracing
set +x
lfortran_version=$(cat version)
# we re-enable command tracing
set -x

bash ci/create_source_tarball.sh "$lfortran_version"
tar xzf dist/lfortran-$lfortran_version.tar.gz
# Build from a fixed directory name: the version changes with every commit,
# and a versioned path would make every compiler cache entry miss.
source_dir=lfortran-src
rm -rf "$source_dir"
mv "lfortran-$lfortran_version" "$source_dir"
