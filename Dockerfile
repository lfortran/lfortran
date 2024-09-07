FROM ubuntu:22.04 AS build

USER root

RUN apt update
RUN apt install curl git build-essential binutils-dev zlib1g-dev clang -y

RUN curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
RUN bash Miniforge3-$(uname)-$(uname -m).sh -b

RUN /root/miniforge3/bin/mamba init bash

WORKDIR /lfortran

COPY . .

RUN /root/miniforge3/bin/mamba env create -f environment_linux.yml -y
SHELL ["/root/miniforge3/bin/mamba", "run", "-n", "lf", "/bin/bash", "-c"]

RUN ./build_release.sh

RUN ctest
RUN python integration_tests/run_tests.py
RUN python run_tests.py --no-llvm

FROM ubuntu:22.04 AS app

RUN apt update
RUN apt install binutils clang --no-install-recommends -y

COPY --from=build /lfortran/inst /app
