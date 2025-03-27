# fluxer
FLUX collection of functions and utilities

## Installation
```R
# Install devtools if not already installed
install.packages("devtools")

# Install fluxer from GitHub (if applicable)
devtools::install_github("your-username/fluxer")
```

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
