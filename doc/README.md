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

You can install all required dependencies using the mamba package manager with the following enviroment file.

```yaml
# environment.yml
channels:
  - conda-forge
dependencies:
  - lfortran
  - myst-parser
  - nbsphinx
  - sphinx
  - sphinx-intl
  - sphinx-copybutton
  - sphinx-material
```

## Building the documentation

To build the documentation run the `build.py` script:

```
python3 doc/build.py
```

To view the rendered pages start a http server:

```
python3 -m http.server -d doc/site
```

You can also pass the language as an argument to only build part of the site

```
python3 doc/build.py en de
```

## Updating the documentation

To update the documentation edit the markdown files in the `doc/src` directory.
If you add new files in Markdown (`md`) or Notebook (`ipynb`) format, make sure to include them in a _toctree_ directive.

To update the translation files after an addition to the docs, run

```
python3 doc/intl.py
```

New translations can be added in `doc/src/_static/languages.json`.
Make sure the default language (English) is always first.

## Translating the documentation

Translations can be contributed via [weblate](https://hosted.weblate.org/projects/fortran-lang/lfortran-docs/).

[![Translation status](https://hosted.weblate.org/widgets/fortran-lang/-/lfortran-docs/horizontal-auto.svg)](https://hosted.weblate.org/engage/fortran-lang/)

The documentation can also be translated by editing the _po_-files in `doc/locale`.
