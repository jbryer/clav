# Utility function to print p-values

This function will print a pretty p-value. If the p-value is less than
10^(-digits), for example 0.01 with digits = 2, then the result will be
"p \< 0.01".

## Usage

``` r
print_p_value(p_value, digits = 2)
```

## Arguments

- p_value:

  the p-value.

- digits:

  number of digits to print.

## Value

a character version of the p-value.
