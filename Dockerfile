FROM ubuntu:22.04 AS build

USER root

RUN apt update
RUN apt install curl git build-essential binutils-dev zlib1g-dev clang libunwind-dev -y

# fix version of miniforge to 24.11.0-0 as that fixes the version of mamba
RUN curl -L -O "https://github.com/conda-forge/miniforge/releases/download/24.11.0-0/Miniforge3-$(uname)-$(uname -m).sh"
RUN bash Miniforge3-$(uname)-$(uname -m).sh -b

RUN /root/miniforge3/bin/mamba init bash

WORKDIR /lfortran_build

COPY . .

RUN /root/miniforge3/bin/mamba env create -f environment_linux.yml -y
SHELL ["/root/miniforge3/bin/mamba", "run", "-n", "lf", "/bin/bash", "-c"]

RUN ./build_release.sh

RUN ctest
RUN LFORTRAN_TEST_ENV_VAR='STATUS OK!' python integration_tests/run_tests.py
RUN python run_tests.py

FROM ubuntu:22.04 AS app

RUN apt update
RUN apt install binutils clang --no-install-recommends -y

COPY --from=build /lfortran_build/inst /app
