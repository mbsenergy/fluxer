# fluxer
DATAFLUX collection of functions and utilities

## Notion Page
DATAFLUX/Backend/fluxer package



## Develop

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
