actuaRE: Handling Single-Level and Hierarchically Structured Risk Factors using Credibility and Random Effects Models
====
Fits random effects models for multi-level/high-cardinality factors using credibility theory (Buhlmann-Straub for single-level, Jewell for hierarchical structures), GLM extensions following Ohlsson (2008) <doi:10.1080/03461230701878612>, or Tweedie generalized linear mixed models. Provides functions for model fitting, visualization, and prediction. See Campo, B.D.C. and Antonio, K. (2023) <doi:10.1080/03461238.2022.2161413>.

<p align="left">
  <img src="vignettes/actuaRE.png" width="25%">
</p>


## Installation

### On current R (>= 3.0.0)
* Development version from Github:
```
library("devtools"); install_github("BavoDC/actuaRE", dependencies = TRUE, build_vignettes = TRUE)
```
(This requires `devtools` >= 1.6.1, and installs the "master" (development) branch.)
This approach builds the package from source, i.e. `make` and compilers must be installed on your system -- see the R FAQ for your operating system; you may also need to install dependencies manually. Specify `build_vignettes=FALSE` if you have trouble because your system is missing some of the `LaTeX/texi2dvi` tools.

## Documentation
The basic functionality of the package is explained and demonstrated in the vignette, which you can access using
```
vignette("actuaRE")
```
or via the [homepage](https://bavodc.github.io/websiteactuaRE/articles/actuaRE.html) of the package.

## Citation
If you use this package, please cite:

- Campo, B.D.C. and Antonio, Katrien (2023). Insurance pricing with hierarchically structured data an illustration with a workers' compensation insurance portfolio. _Scandinavian Actuarial Journal_, doi: 10.1080/03461238.2022.2161413
- Campo, B.D.C. (2026). _The actuaRE package: Handling Single-Level and Hierarchically Structured Risk Factors using Credibility and Random Effects Models_. R package version 1.0.0, [https://cran.r-project.org/package=actuaRE](https://cran.r-project.org/package=actuaRE)
