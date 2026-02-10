# Basic summary statistics by group

Calculate basic summary statistics by a grouping variable. This function
is inspired by the
[`psych::describeBy()`](https://rdrr.io/pkg/psych/man/describe.by.html)
function but has been implemented to always return a data frame using
the tidyverse. The `funs` parameter provides additional flexibility
regarding what summary statistics are calculated.

## Usage

``` r
describe_by(
  df,
  group,
  group_name = "group",
  funs = list(n = length, mean = mean, sd = sd, se = function(x, ...) {
     sd(x,
    ...)/sqrt(length(x))
 }, median = median, min = min, max = max, range = function(x,
    ...) {
     diff(range(x, ...))
 })
)
```

## Arguments

- df:

  a data frame to summarize.

- group:

  a grouping vector.

- group_name:

  name of the grouping variable in the returned data frame.

- funs:

  a named list of functions. The names of the list will be used as the
  column names in the returned data frame.

## Value

a data frame with a column `group` corresponding to the group variable
and column `variable` corresponding to the names of the data frame.
Additional columns for each function in the `funs` parameter.

## Examples

``` r
data(pisa2015, package = 'clav')
describe_by(df = pisa2015[,c('science_score', 'efficacy', 'motivation')], group = pisa2015$country)
#>           group      variable     n       mean         sd          se   median
#> 1 UNITED STATES      efficacy  4637   1.870309  0.6662445 0.009783969   2.0000
#> 2 UNITED STATES    motivation  4637   1.926785  0.7561965 0.011104937   2.0000
#> 3 UNITED STATES science_score  4637 506.398045 91.8862771 1.349373286 504.8727
#> 4        CANADA      efficacy 15934   1.875698  0.6669602 0.005283692   2.0000
#> 5        CANADA    motivation 15934   2.046865  0.7794135 0.006174553   2.0000
#> 6        CANADA science_score 15934 526.652038 84.6720478 0.670776218 529.0316
#>        min      max    range
#> 1   0.0000   3.0000   3.0000
#> 2   0.0000   3.0000   3.0000
#> 3 234.1381 817.7677 583.6296
#> 4   0.0000   3.0000   3.0000
#> 5   0.0000   3.0000   3.0000
#> 6 225.5391 795.2875 569.7484
```
