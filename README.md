# NpiEurope

This [research compendium](https://doi.org/10.1080/00031305.2017.1375986) allows
you to reproduce the results and figures from 
[Garchitorena *et al.* (2020)](https://doi.org/10.1101/2020.08.17.20174821)

## Compendium structure

- The script in `analysis` is used for the Bayesian estimation of the 
epidemiological parameters. Because these analyses may be very time consuming,
it's not advised to run it yourself if you just want to get a feeling of how
the code works. By default, this code will run sequentially but can be readily
converted to run parallelly thanks to the 
[future](https://cran.r-project.org/package=future) package. Here is for example
what I use for my local computer:

```r
library(future)
library(parallel)
cl <- makeCluster(4)
plan(cluster, workers = cl)
source('analysis/01-estimate_efficiency_parameters.R', echo=TRUE)
```

- The output from the scripts in `analysis` is saved in `data-final`.

- From the data in `data-final`, the figures of the article are produced with 
the Rmarkdown files in `vignettes`. You can see the output of these vignettes
by visiting 
[this compendium website](https://bisaloo.github.io/NpiEurope_compendium).
