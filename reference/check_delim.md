# Checks If Two or More Species Delimitation Outputs are (Nearly) Equal

`check_delim()` checks if two or more species delimitation outputs have
differences in its dimensions, labels, and values.

## Usage

``` r
check_delim(list)
```

## Arguments

- list:

  a [list](https://rdrr.io/r/base/list.html) containing two or more
  species delimitation outputs to check.

## Value

A single logical value, `TRUE` or `FALSE`.

## Details

`check_delim()` will check if two or more species delimitation outputs
have different dimensions (rows, columns), if labels are the same or if
there are any duplicated or absent labels, and if there are any NA
values or if partitions were set using non numeric values. Each table is
compared against the first table in `list`. Any of the cases listed
above triggers a
[`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
describing the issue, and `check_delim()` returns `FALSE`. If none of
the cases are found for any pairwise comparison, `check_delim()` returns
`TRUE`.

## Author

Pedro S. Bittencourt, Rupert A. Collins, Tomas Hrbek.

## Examples

``` r

# create dummy delimitation outputs
delim_1 <- tibble::tibble(
  labels = paste0("seq", 1:10),
  method_A = c(rep(1, 5), rep(2, 5))
)

delim_2 <- tibble::tibble(
  labels = paste0("seq", 1:10),
  method_B = c(rep(1, 3), rep(2, 2), rep(3, 5))
)

delim_3 <- tibble::tibble(
  labels = paste0("seq", 1:10),
  method_C = c(rep(1, 3), rep(2, 2), rep(3, 3), rep(4, 2))
)

# check outputs
check_delim(list(delim_1, delim_2, delim_3))
#> Checking species delimitation tables...
#> Checking table 1 against table 2...
#> Checking table 1 against table 3...
#> Checking table 1 against table 3...
#> ✔ Checking complete! All tables are consistent.
#> Checking table 1 against table 3...
#> Checking table 1 against table 3...
#> [1] TRUE
```
