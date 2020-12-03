# NpiEurope

This [research compendium](https://doi.org/10.1080/00031305.2017.1375986) allows
you to reproduce the results and figures from 
[Garchitorena *et al.* (2020)](https://doi.org/10.1101/2020.08.17.20174821).

## Live automatic updates

The results and figures are automatically updated on a daily basis using the new
data posted on the ECDC website.

## Compendium structure

- The script in `analysis` is used for the Bayesian estimation of the 
epidemiological parameters. Because these analyses may be very time consuming,
it's not advised to run it yourself if you just want to get a feeling of how
the code works. By default, this code will run sequentially but can be readily
converted to run in parallel thanks to the 
[future](https://cran.r-project.org/package=future) package. Here is for example
what I use for my local computer:

```r
library(future)
library(parallel)
cl <- makeCluster(4)
plan(cluster, workers = cl)
source('analysis/01-estimate_efficiency_parameters.R', echo=TRUE)
```

- The output from the script in `analysis` is saved in `MCMC_NpiEurope`.

- From the data in `MCMC_NpiEurope`, the figures of the article are produced 
with the Rmarkdown files in `vignettes`. You can see the output of these
vignettes by visiting 
[this compendium website](https://bisaloo.github.io/NpiEurope_compendium).

## Do it yourself

To run the code yourself instead of relying of the automatic updates, you will
need to first install the functions defined here as well as the package
dependencies.

This is done in two steps:

1. Copy the project and open it
1. Install the functions and dependencies

```r
create_from_github("Bisaloo/NpiEurope_compendium")
remotes::install()
```

You can now run any code from the `analysis/` or the `vignettes/` folder.
