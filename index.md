# intersimR

`intersimR` provides tools to detect and quantify local interaction
events between marine animals and vessels using tracking data. The
package implements the simulation-based framework described in *A
simulation-based framework to detect marine animal–vessel interactions
using tracking data*, allowing users to evaluate attraction and
following behaviour through null-model simulations and event-level
metrics.

The package is designed to support reproducible analysis pipelines,
making it easy to apply the framework to new datasets across marine
species and vessel types.

------------------------------------------------------------------------

## Installation

You can install the development version of the package from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("spatialmarine/intersimR")
```

Once installed, load the package with:

``` r
library(intersimR)
```

## Overview

`intersimR` includes tools for:

- Extracting local interaction events from animal and vessel tracks

- Generating simulation-based null models

- Computing attraction and following behaviour

- Estimating p-values based on repeated simulations

- Summarising and visualising interaction events

The workflow mirrors the structure of the original methodological paper,
with generic functions that can be applied to any time-synchronised
animal–vessel tracking dataset.

## Documentation

The full reference documentation and vignettes will be available at:

<https://spatialmarine.github.io/intersimR/>

## Contributing

Issues, questions, and pull requests are welcome. Please open an issue
to report bugs or suggest improvements.

## License

This package is distributed under the MIT license. See the `LICENSE`
file for details.

## Citation

If you use `intersimR` in your research, please cite the methodological
paper associated with this framework once it is published. A `CITATION`
file will be added when a formal reference is available.
