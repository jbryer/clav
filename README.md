
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clav: Cluster Analysis Validation

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/clav?color=orange)](https://cran.r-project.org/package=clav)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/jbryer/clav)
[![R build
status](https://github.com/jbryer/clav/workflows/R-CMD-check/badge.svg)](https://github.com/jbryer/clav/actions)
<!-- badges: end -->

The `clav` package provides utilities for conducting cluster (profile)
analysis with an emphasis on the validating the stability of the
profiles both within a given data set as well as across data sets.
Unlike supervised models where the known class is measured, validation
of unsupervised models where there is no known class can be challenging.
The approach implemented here attempts to compare the cluster results
across many random samples.

## Installation

You can install the development version of clav like so:

``` r
remotes::install_github('jbryer/clav')
```

## Development

The following commands are useful for working with the package source
locally.

``` r
# Prep the PISA data set. This will take a while to run the first time.
source('data-raw/data-prep-pisa-2015.R')
# Generate the package documentation
usethis::use_tidy_description()
devtools::document()
# Install the package
devtools::install()
# Run CRAN check for any issues
devtools::check(cran = TRUE)
```

## Example

``` r
library(clav)
data(pisa2015, package = 'clav')

cluster_vars <- c('interest', 'enjoyment', 'motivation', 'efficacy', "belonging")
outcome_vars <- c('science_score', 'principals')

pisa_usa <- pisa2015 |> dplyr::filter(country == 'UNITED STATES')
```

Finding the optimal number of clusters.

``` r
optimal <- optimal_clusters(pisa_usa[,cluster_vars], max_k = 5)
optimal
#>   k      wss silhoutte       gap calinski_harabasz davies_bouldin
#> 1 1 22099.00        NA 0.9201448               NaN            NaN
#> 2 2 17389.55 0.2004559 0.8984924         1255.2547       1.921547
#> 3 3 15575.47 0.2090841 0.8877103          991.8627       2.135113
#> 4 4 14254.57 0.1649883 0.8770958          864.8128       1.859489
#> 5 5 13116.20 0.1637914 0.8792677          873.2673       1.811864
plot(optimal, ncol = 2)
```

<img src="man/figures/README-optimal-clusters-1.png" width="100%" />

Validating cluster profiles using random samples of 50%. Out-of-bag uses
the remaining 50% to predict cluster membership.

``` r
pisa_cv_random <- pisa_usa |> 
    dplyr::select(dplyr::all_of(cluster_vars)) |>
    clav::cluster_validation(
        n_clusters = 3,
        sample_size = 0.5 * nrow(pisa_usa),
        replace = FALSE,
        n_samples = 100,
        seed = 42
)
plot(pisa_cv_random)
```

<img src="man/figures/README-random-validation-1.png" width="100%" />

Re-estimate the clusters using the OOB sample instead of predicting
using the in sample model.

``` r
pisa_cv_random2 <- pisa_usa |> 
    dplyr::select(dplyr::all_of(cluster_vars)) |>
    clav::cluster_validation(
        n_clusters = 3,
        oob_predict_fun = function(fit, newdata) {
            newfit <- stats::kmeans(newdata, 3)
            newfit$cluster
        },
        sample_size = 0.5 * nrow(pisa_usa),
        replace = FALSE,
        n_samples = 100,
        seed = 42
)
plot(pisa_cv_random2)
```

<img src="man/figures/README-oob-reestimate-validation-1.png" width="100%" />

Bootstrap approach to validation.

``` r
pisa_cv_bootstrap <- pisa_usa |> 
    dplyr::select(dplyr::all_of(cluster_vars)) |>
    clav::cluster_validation(
        n_clusters = 3,
        sample_size = nrow(pisa_usa),
        replace = TRUE,
        n_samples = 100,
        seed = 42
)
plot(pisa_cv_bootstrap)
```

<img src="man/figures/README-bootstrap-validation-1.png" width="100%" />

Using latent profile analysis for estimating clusters.

``` r
lpa <- estimate_profiles(pisaUSA15, 3)
lpa_predict <- predict(lpa, pisaUSA15[sample(nrow(pisaUSA15), 100),])
# lpa_estimates <- get_estimates(lpa)
lpa_data <- get_data(lpa)

plot_profiles(lpa)
clav::profile_plot(pisaUSA15,
             clusters = lpa_data$Class)

lpa_cv_random <- cluster_validation(
    pisaUSA15,
    n_clusters = 3,
    cluster_fun = estimate_profiles,
    oob_predict_fun = function(fit, newdata) {
        estimate_profiles(newdata, n_clusters)
    },
    sample_size = 0.5 * nrow(pisaUSA15),
    replace = FALSE,
    n_samples = 50,
    seed = 42
)
plot(lpa_cv_random)
```

## Profile Plot

``` r
fit <- pisa_usa |> 
    dplyr::select(dplyr::all_of(cluster_vars)) |>
    stats::kmeans(centers = 3)
clav::profile_plot(pisa_usa[,cluster_vars],
                   clusters = fit$cluster,
                   df_dep = pisa_usa[,outcome_vars],
                   center_band = 0.33,
                   cluster_order = cluster_vars)
```

<img src="man/figures/README-profile-plot-1.png" width="100%" />
