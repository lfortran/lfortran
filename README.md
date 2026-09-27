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

[Pixi](https://pixi.sh/) is the recommended way to set up LFortran from Git.
It installs the dependencies declared by the repository and runs the build:

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
See the [installation guide](doc/src/installation.md) for platform details and
the supported Conda, source-tarball, and manual build alternatives.

Native tasks default to LLVM 11, matching the reference tests. Environments
build separately under `build/<environment>`, so different LLVM versions and
configurations can coexist:

```bash
pixi run -e llvm22 build
pixi run -e llvm22 start --version
pixi run ctest -j8 > unit.log 2>&1
pixi run tests -j8 > reference.log 2>&1
pixi run integration_tests -j8 > integration.log 2>&1
```

Inspect the saved test logs. Use `-e <environment>` on each command to select
another build; reference tests require LLVM 11. `pixi run -e llvm22 clean`
cleans only that configuration's build targets. Dependency and build details
live in `pixi.toml` and its scripts, so these setup commands remain stable as
the project evolves.

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
