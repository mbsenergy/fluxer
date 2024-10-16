# fluxer
FLUX collection of functions and utilities

## Develop

### Functions
Remember to document the functions using `roxygen-like` code, naming and importing dependencies correctly and testing it.

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
