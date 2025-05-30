
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <a href='https://github.com/jbryer/clav'><img src='man/figures/clav1.png' align="right" width="200" /></a> Cluster Analysis Validation (`clav`)

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/clav?color=orange)](https://cran.r-project.org/package=clav)
[![](https://img.shields.io/badge/devel%20version-0.1.1-blue.svg)](https://github.com/jbryer/clav)
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
# Run CRAN check
devtools::check(cran = TRUE)
# Build the pkgdown site
pkgdown::build_site()
```

## Example

``` r
library(clav)
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
data(pisa2015, package = 'clav')

cluster_vars <- c('interest', 'enjoyment', 'motivation', 'efficacy', "belonging")
outcome_vars <- c('science_score', 'principals')

pisa_usa <- pisa2015 |> dplyr::filter(country == 'UNITED STATES')
```

Finding the optimal number of clusters.

``` r
optimal <- optimal_clusters(pisa_usa[,cluster_vars], max_k = 5)
optimal
#>   k   wss silhoutte  gap calinski_harabasz davies_bouldin
#> 1 1 22099        NA 0.92               NaN            NaN
#> 2 2 17390      0.20 0.90              1255            1.9
#> 3 3 15575      0.21 0.89               970            1.7
#> 4 4 14255      0.16 0.88               844            1.8
#> 5 5 12768      0.17 0.88               857            1.8
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
summary(pisa_cv_bootstrap)
#>    cluster   variable   mean    sd median   min   max range     se
#> 1        A   interest -0.085 0.208 -0.019 -0.71  0.18  0.88 0.0208
#> 4        A  enjoyment -0.826 0.933 -0.472 -3.18 -0.14  3.05 0.0933
#> 7        A motivation  0.034 0.138  0.042 -0.37  0.31  0.68 0.0138
#> 10       A   efficacy  0.228 0.172  0.253 -0.75  0.46  1.21 0.0172
#> 13       A  belonging -0.088 0.332 -0.155 -0.62  0.84  1.46 0.0332
#> 2        B   interest -0.778 0.135 -0.794 -1.11 -0.20  0.91 0.0135
#> 5        B  enjoyment -0.190 0.085 -0.176 -0.38  0.26  0.64 0.0085
#> 8        B motivation -0.721 0.122 -0.735 -1.18 -0.42  0.76 0.0122
#> 11       B   efficacy -0.854 0.177 -0.921 -1.07  0.17  1.24 0.0177
#> 14       B  belonging -0.127 0.204 -0.066 -1.07  0.21  1.27 0.0204
#> 3        C   interest  0.745 0.084  0.771  0.53  0.89  0.35 0.0084
#> 6        C  enjoyment  0.688 0.132  0.735  0.32  0.85  0.53 0.0132
#> 9        C motivation  0.670 0.123  0.659  0.44  0.92  0.48 0.0123
#> 12       C   efficacy  0.555 0.071  0.535  0.44  0.76  0.32 0.0071
#> 15       C  belonging  0.254 0.190  0.293 -0.14  0.55  0.69 0.0190
plot(pisa_cv_bootstrap)
```

<img src="man/figures/README-bootstrap-validation-1.png" width="100%" />

Using latent profile analysis for estimating clusters.

``` r
library(tidyLPA)
lpa <- pisa_usa |> 
    dplyr::select(dplyr::all_of(cluster_vars)) |>
    tidyLPA::estimate_profiles(3)
# lpa_predict <- predict(lpa, pisaUSA15[sample(nrow(pisaUSA15), 100),])
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
