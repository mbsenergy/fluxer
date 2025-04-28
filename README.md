
[![MBSENERGY](https://img.shields.io/badge/MBS-ENERGY-037f8c?style=for-the-badge)](mbsconsulting.com)
[![Made with R](https://img.shields.io/badge/Made%20with-rstats-88c0d0?style=for-the-badge&logo=r)](https://cran.r-project.org/)


# fluxer <a href="https://mbsconsulting.com"><img src="man/figures/fluxer.png" align="right" height="180" /></a>

**FLUX collection of functions and utilities**

## Installation

Install the package using `remotes`:

````r
install.packages("remotes")
remotes::install_github("mbsenergy/eikondata")
````

## Develop

### Functions
Remember to document the functions using `roxygen-like` code, naming and importing dependencies correctly and testing it.
- Use `roxygen2` for documentation (`#' @description`, `#' @param`, `#' @return`,  `#' @export`, etc.).
- Ensure proper naming conventions and dependency management (`Imports:` in `DESCRIPTION`).
```R
#' @title Dataset x
#' @description Brief description of x.
#' @format A data frame with n rows and m columns.
"x"
```

### Add datasets 
Load or create the dataset in R. then use the `usethis::use_data(x)`. It will be stored in `data/x.rda`.
Remember to create the documentation in the `R` directory named `x.R`

## Build the package

```R
devtools::document()
devtools::build()

## If needed testing
devtools::load_all()

```
