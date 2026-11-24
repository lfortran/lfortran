FROM ubuntu:22.04 AS build

USER root

# Pixi provides the compilers and binutils; the system provides only what
# downloads Pixi and the sources.
RUN apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates curl git && \
    rm -rf /var/lib/apt/lists/*

# Pixi provides every build and test dependency (see pixi.toml).
ARG PIXI_VERSION=v0.83.0
RUN curl -fsSL -o /usr/local/bin/pixi \
        "https://github.com/prefix-dev/pixi/releases/download/${PIXI_VERSION}/pixi-$(uname -m)-unknown-linux-musl" && \
    chmod +x /usr/local/bin/pixi

WORKDIR /lfortran_build

COPY . .

RUN pixi install -e llvm11

# Pixi's GCC 15 libstdc++ is newer than the app image's, so link it
# statically; the compiler then needs only the system's C libraries.
RUN pixi run -e llvm11 build-release \
        "-DCMAKE_EXE_LINKER_FLAGS=-static-libstdc++ -static-libgcc" \
        -DCMAKE_INSTALL_PREFIX=/lfortran_build/inst

# The release build is in build/release, so the suites need its directory;
# the tasks take build/llvm11 from the environment.
RUN pixi run -e llvm11 ctest --test-dir /lfortran_build/build/release
RUN LFORTRAN_TEST_ENV_VAR='STATUS OK!' pixi run -e llvm11 integration_tests \
        --compiler-dir /lfortran_build/build/release/src/bin \
        --build-dir /lfortran_build/build/release/integration_tests
RUN pixi run -e llvm11 tests \
        --compiler-dir /lfortran_build/build/release/src/bin \
        --build-dir /lfortran_build/build/release/reference_tests

FROM ubuntu:22.04 AS app

RUN apt-get update && \
    apt-get install -y --no-install-recommends binutils clang && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /lfortran_build/inst /app

# The compiler must run, and link programs, without the Pixi environment.
RUN printf 'program hello\nprint *, "Hello"\nend program\n' > /tmp/hello.f90 && \
    /app/bin/lfortran --version && \
    /app/bin/lfortran /tmp/hello.f90 -o /tmp/hello && \
    /tmp/hello && \
    rm /tmp/hello.f90 /tmp/hello
