import json
from pathlib import Path

root = Path(__file__).parent

with open(root / "_static" / "languages.json", "r", encoding="utf-8") as fd:
    all_languages = json.load(fd)

if "language" not in globals():
    language = "en"

project = "LFortran"
author = f"{project} contributors"
copyright = f"2022 {author}"

extensions = [
    "myst_parser",
    "nbsphinx",
    "sphinx_copybutton",
]
myst_enable_extensions = [
    "dollarmath",
]

html_theme = "sphinx_material"
html_title = project
html_logo = "_static/img/lfortran-logo.svg"
html_favicon = html_logo
html_css_files = ["css/custom.css", "css/ansi.css"]
html_static_path = ["_static"]
html_show_sourcelink = False

html_theme_options = {
    "nav_title": f"{project} Documentation",
    "base_url": f"https://docs.lfortran.org/{language}/",
    "repo_name": project,
    "color_primary": "amber",
    "color_accent": "orange",
    "repo_url": "https://github.com/lfortran/lfortran",
    "nav_links": [
        dict(href="installation", internal=True, title="Getting started"),
        dict(href="progress", internal=True, title="LFortran Development Status"),
        dict(href="design", internal=True, title="Developers's Guide"),
        dict(href="intrinsics/array", internal=True, title="Intrinsics"),
    ],
    "version_info": {
        lang: f"/../{code}/"
        for lang, code in all_languages.items()
    },
    "globaltoc_depth": 2,
}

html_sidebars = {
    "**": ["logo-text.html", "globaltoc.html", "localtoc.html", "searchbox.html"]
}

templates_path = ["_templates"]
locale_dirs = ["../locale"]
gettext_compact = "docs"
