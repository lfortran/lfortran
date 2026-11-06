# Documentation

The documentation for LFortran is built with sphinx

## Installing dependencies

To build the documentation you will need the following dependencies

- [sphinx-build](https://www.sphinx-doc.org) for building the pages
- [sphinx-intl](https://www.sphinx-doc.org/en/master/usage/advanced/intl.html) for translations
- [sphinx-material](https://bashtage.github.io/sphinx-material/) for the page theme
- [sphinx-copybutton](https://sphinx-copybutton.readthedocs.io/en/latest/) to allow copying of code-blocks
- [myst-parser](https://myst-parser.readthedocs.io/en/latest/) for markdown support
- [nbsphinx](https://nbsphinx.readthedocs.io/en/latest/) for converting notebooks
- LFortran with jupyter kernel for the notebook conversion

The `docs` environment of `pixi.toml` provides all of them, as in the
Exhaustive "Documentation" job.


## Building the documentation

First build LFortran with the Jupyter kernel and install it into the `docs`
environment, from the repository root:

```console
pixi shell -e docs
pixi run -e docs build -DWITH_XEUS=yes -DCMAKE_INSTALL_PREFIX="$CONDA_PREFIX"
cmake --build build/docs --target install
```

To build the documentation run the `build.py` script, in the same shell:

```
cd doc
python build.py
```

To view the rendered pages start a http server:

```
python -m http.server -d site
```

And go to: http://localhost:8000/en/

You can also pass the language as an argument to only build part of the site

```
python build.py en de
```

## Updating the documentation

To update the documentation edit the markdown files in the `doc/src` directory.
If you add new files in Markdown (`md`) or Notebook (`ipynb`) format, make sure to include them in a _toctree_ directive.

To update the translation files after an addition to the docs, run

```
python intl.py
```

New translations can be added in `doc/src/_static/languages.json`.
Make sure the default language (English) is always first.

## Translating the documentation

Translations can be contributed via [weblate](https://hosted.weblate.org/projects/fortran-lang/lfortran-docs/).

[![Translation status](https://hosted.weblate.org/widgets/fortran-lang/-/lfortran-docs/horizontal-auto.svg)](https://hosted.weblate.org/engage/fortran-lang/)

The documentation can also be translated by editing the _po_-files in `doc/locale`.
