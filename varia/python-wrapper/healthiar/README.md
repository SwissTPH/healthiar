# healthiar

[![PyPI - Version](https://img.shields.io/pypi/v/healthiar.svg)](https://pypi.org/project/healthiar)
[![PyPI - Python Version](https://img.shields.io/pypi/pyversions/healthiar.svg)](https://pypi.org/project/healthiar)

-----

## Table of Contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Documentation](#documentation)
- [License](#license)

## Requirements

To use this Python package, you need [R](https://www.r-project.org/), with the [`healthiar`](https://cran.r-project.org/web/packages/healthiar/) package installed.

| Software | Versions |
|----------|----------|
| Python   | >=3.9    |
| R        | >=4.5    |

## Installation

1.  Install the `healthiar` wrapper package with `pip`:

    ```console
    pip install healthiar
    ```
    Be aware that this does **not** install the `healthiar` R package.

2.  Add the path of your R installation as environment variable:

    ```console
    import os

    os.environ['R_HOME'] = "/path/to/R/R-4.6.0"
    # or
    os.environ["PATH"] = "/path/to/R/R-4.6.0/bin/x64"
    ```

## Documentation

For the documentation on using `healthiar`, we refer to the R package documentation:
- [Intro to healthiar](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html) vignette;
- [healthiar with Python](https://swisstph.github.io/healthiar/articles/healthiar_with_python.html) vignette;
- [Reference page](https://swisstph.github.io/healthiar/reference/index.html) of the package website.

## License

`healthiar` is distributed under the terms of the [GPL-3.0-only](https://spdx.org/licenses/GPL-3.0-only.html) license.
