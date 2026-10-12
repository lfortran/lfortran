# LFortran

[![project chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://lfortran.zulipchat.com/)

LFortran is a modern open-source (BSD licensed) interactive Fortran compiler
built on top of LLVM. It can execute user's code interactively to allow
exploratory work (much like Python, MATLAB or Julia) as well as compile to
binaries with the goal to run user's code on modern architectures such as
multi-core CPUs and GPUs.

Website: https://lfortran.org/

Try online: https://dev.lfortran.org/

Try LFortran in a JupyterLite notebook:
[![JupyterLite](https://jupyterlite.rtfd.io/en/latest/_static/badge.svg)](https://lfortran.github.io/lfortran/)

To build and run that JupyterLite site locally, use `pixi run lab` and open
<http://localhost:8000/lab/index.html>. See
[doc/src/jupyterlite.md](doc/src/jupyterlite.md) for details and for how to
write tests for bugs found in the lab.

# Install and build with Pixi

[Pixi](https://pixi.sh/) is the recommended workflow for dependencies, builds,
and tests. It installs the dependencies declared by the repository and runs
the build; no separate package installation is needed. Native tasks default
to LLVM 11:

```bash
git clone https://github.com/lfortran/lfortran.git
cd lfortran
pixi run build
pixi run start --version
pixi run start examples/expr2.f90
```

The last command compiles and runs the program. To keep an executable, pass
`-o <filename>` to `start`.

On macOS, install Xcode Command Line Tools first; on Windows, use an initialized
MSVC developer shell with Git Bash. Linux C/C++ compilers are provided by Pixi.
See the [Pixi installation guide](doc/src/installation.md#pixi-recommended)
for platform details. AI contributors must follow the
[required Pixi toolchain policy](AGENTS.md#required-agent-toolchain-pixi);
use Pixi to supply project dependencies rather than installing packages with
system or global package managers.

After building, run all three test suites with LLVM 11 from the repository root:

```bash
pixi run ctest -j8 > unit.log 2>&1
pixi run tests -j8 > reference.log 2>&1
pixi run integration_tests -j8 > integration.log 2>&1
```

Inspect the saved test logs. **Reference tests and updates require LLVM 11**;
the `tests` task verifies the compiler's LLVM version before running. For a
focused test, append `-t <pattern>` to `tests` or `integration_tests`.

Other Pixi configurations build separately under `build/<environment>`:

```bash
pixi run -e llvm22 build
pixi run -e llvm22 start --version
pixi run -e llvm22 integration_tests -j8 > integration-llvm22.log 2>&1
```

Use `-e <environment>` consistently to select another build.
`pixi run clean` cleans the default configuration's build targets.
Dependency and build details live in `pixi.toml` and its scripts.

# Documentation

All documentation, installation instructions, motivation, design, ... is
available at:

https://docs.lfortran.org/

Which is generated using the files in the `doc` directory.


# Development

We welcome all contributions.
The main development repository is at GitHub:

https://github.com/lfortran/lfortran

Please send Pull Requests (PRs) and open issues there.

See the [CONTRIBUTING](CONTRIBUTING.md) document for more information.

Main mailinglist:

https://groups.io/g/lfortran

You can also chat with us on Zulip ([![project chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://lfortran.zulipchat.com/)).

Note: We moved to the above GitHub repository from GitLab on July 18, 2022.

# Donations

You can support LFortran's development by donating to NumFOCUS or Open
Collective as well as GitHub Sponsors:

* https://numfocus.org/donate-to-lfortran
* https://opencollective.com/lfortran
* https://github.com/sponsors/lfortran

All donations will be used strictly to fund LFortran development, by supporting
tasks such as paying developers to implement features, sprints, improved
documentation, fixing bugs, etc.

The donations to LFortran are managed by the NumFOCUS foundation. NumFOCUS is a
501(c)3 non-profit foundation, so if you are subject to US Tax law, your
contributions will be tax-deductible.

If you want to discuss another way to fund or help with the development, feel
free to contact Ondřej Čertík (ondrej@certik.us).

# Star History

[![Star History Chart](https://star-history.dera.page/svg?repos=lfortran/lfortran&type=Date)](https://star-history.dera.page/#lfortran/lfortran&Date)

# Optional installation alternatives

For packaging or an existing non-Pixi installation, see the
[optional alternatives](doc/src/installation.md#optional-installation-alternatives)
at the end of the installation guide. Use Pixi above for development and tests.
