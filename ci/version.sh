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
# In case of a fresh clone without any tag information, a default version is returned.

set -ex

version=$(git describe --tags --dirty || echo "-0.0.0-devel")
version="${version:1}"
echo $version > version
