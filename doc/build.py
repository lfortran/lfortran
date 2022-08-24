"""
Script for conveniently building the documentation in all supported languages.

Run this script with:

.. code::

    python3 build.py


You can also pass the language as an argument:

.. code::

    python3 build.py en de


The first language will be handled as the default language.
"""

import json
import sys
import subprocess
from pathlib import Path
from typing import List, Dict

root = Path(__file__).parent
"""
Make sure to run this script from the root of the documenation repository.
"""

template = """
<!DOCTYPE HTML>
 
<meta charset="UTF-8">
<meta http-equiv="refresh" content="1; url={0}">
 
<script>
  window.location.href = "{0}"
</script>
 
<title>Page Redirection</title>
 
If you are not redirected automatically, follow the <a href='{0}'>link</a>.
"""

all_redirects = {
    "index.html": "{0}/",
    **{
        f"{name}/index.html": f"../{{0}}/{name}/"
        for name in (
            "installation",
            "language",
            "progress",
            "design",
            "developer_tutorial",
            "ast_and_asr",
            "contributing",
        )
    },
    **{
        f"intrinsics/{name}/index.html": f"../../../{{0}}/intrinsics/{name}/"
        for name in (
            "array/allocated",
            "array/cshift",
            "array/size",
            "bit/bge",
            "bit/bgt",
            "bit/bit_size",
            "bit/ble",
            "bit/blt",
            "bit/btest",
            "bit/shiftl",
            "bit/shiftr",
            "character/achar",
            "character/adjustl",
            "character/adjustr",
            "character/char",
            "character/lge",
            "character/len_trim",
            "kind-type/kind",
            "mathematical/acos",
            "mathematical/acosh",
            "mathematical/asin",
            "mathematical/asinh",
            "mathematical/atan",
            "mathematical/atan2",
            "mathematical/atanh",
            "mathematical/fraction",
            "misc/command_argument_count",
            "misc/cpu_time",
            "misc/date_and_time",
            "misc/new_line",
            "numeric/abs",
            "numeric/aimag",
            "numeric/aint",
            "numeric/anint",
            "numeric/ceiling",
            "numeric/cmplx",
            "numeric/conjg",
            "numeric/digits",
            "numeric/dim",
            "numeric/epsilon",
            "numeric/erf",
            "numeric/erfc",
            "numeric/exp",
            "numeric/floor",
            "numeric/gamma",
            "numeric/mod",
        )
    },
}
"""
All redirects from the original site without language component.
"""

all_languages: List[str]
"""
List of currently supported languages, taken from ``doc/src/_static/languages.json``.

To support a new language add its `language code`_ to the list.

.. _language code: https://www.sphinx-doc.org/en/master/usage/configuration.html#intl-options
"""

with open(root / "src" / "_static" / "languages.json") as fd:
    all_languages = list(json.load(fd).values())


def build_docs(language: str) -> None:
    """
    Build the documentation for a single language.

    Parameters
    ----------
    language : str
        The language to build the documentation for.
    """

    subprocess.run(
        [
            "sphinx-build",
            "-b",
            "dirhtml",
            str(root / "src"),
            str(root / "site" / language),
            f"-Dlanguage={language}",
        ],
        cwd=root,
    )


def build_redirects(redirects: Dict[str, str], language: str) -> None:
    """
    Build the redirects for a single language.

    Parameters
    ----------
    redirects : Dict[str, str]
        Page redirects to build.
    language : str
        The language to build the redirects for.
    """

    for source, target in redirects.items():
        source_path = root / "site" / source
        redirect = template.format(target.format(language))
        if not source_path.parent.exists():
            source_path.parent.mkdir(parents=True)
        with open(source_path, "w", encoding="utf-8") as fd:
            fd.write(redirect)


def build_all(redirects: Dict[str, str], languages: List[str]) -> None:
    """
    Build the documentation for all languages.

    Parameters
    ----------
    redirects : Dict[str, str]
        Page redirects to build.
    languages : List[str]
        List of languages to build the documentation for.
    """

    for language in languages:
        build_docs(language)

    build_redirects(redirects, languages[0])


if __name__ == "__main__":
    languages = sys.argv[1:] if len(sys.argv) > 1 else all_languages

    build_all(all_redirects, languages)

    print()
    print("Preview the documentation using")
    print()
    print(f"    python3 -m http.server -d \"{root / 'site'}\"")
    print()
