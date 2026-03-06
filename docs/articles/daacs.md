# College Readiness Profiles

This vignette outlines the analysis for the paper Identifying and
Distinguishing College Readiness Profiles Across Academic Outcomes: The
Importance of Integrating Academic Skills and Self-Regulated Learning by
Timothy J. Cleary, Jason Bryer, Elie ChingYen Yu presented at AERA 2025.

``` r
data(daacs)
cluster_vars <- c('Motivation', 'Metacognition', 'Strategies', 'Mathematics', 'Reading', 'Writing')
daacs <- daacs |>
    dplyr::mutate(LogFeedbackViews = log(daacs$FeedbackViews)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cluster_vars), clav::scale_this))
```

## Finding the desired number of clusters

``` r
optimal <- optimal_clusters(daacs[,cluster_vars], max_k = 6)
optimal
#>   k      wss silhoutte       gap calinski_harabasz davies_bouldin rand_index
#> 1 1 38250.00        NA 0.9135164               NaN            NaN         NA
#> 2 2 29868.66 0.2000828 0.8821611          1788.684       1.885763  0.5002855
#> 3 3 25052.91 0.2024985 0.8980284          1678.540       1.634979  0.7523985
#> 4 4 22790.00 0.1806161 0.8936067          1441.385       1.780377  0.7415528
#> 5 5 20747.34 0.1634359 0.9039425          1343.682       1.631170  0.8858106
#> 6 6 19198.02 0.1651062 0.9157272          1264.308       1.574902  0.9058986
```

``` r
plot(optimal, ncol = 2)
```

## Validating cluster solution

``` r
cv <- cluster_validation(daacs[,cluster_vars],
                         n_clusters = 5)
plot(cv, facet = FALSE)
```

![](daacs_files/figure-html/cluster-validation-1.png)

``` r
plot_distributions(cv, plot_in_sample = TRUE, plot_oob_sample = TRUE)
```

![](daacs_files/figure-html/plot-distributions-1.png)

``` r
cv_boot <- cluster_validation(daacs[,cluster_vars],
                         n_clusters = 5,
                         sample_size = nrow(daacs),
                         replace = TRUE)
plot(cv_boot, facet = FALSE)
```

![](daacs_files/figure-html/bootstrap-validation-1.png)

## Profile plots

``` r
fit <- stats::kmeans(daacs[,cluster_vars], centers = 5)
profile_plot(daacs[,cluster_vars],
             clusters = fit$cluster,
             df_dep = daacs[,c('LogFeedbackViews', 'TermSuccess')],
             cluster_order = cluster_vars)
```

![](daacs_files/figure-html/profile-plot-1.png)
