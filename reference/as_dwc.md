# Rename Columns using Darwin Core Standard Terms

`as_dwc()` rename columns in a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html) using
a vector of terms defined by Darwin Core Standard.

## Usage

``` r
as_dwc(dwc, data, terms)
```

## Arguments

- dwc:

  a list of standard terms and definitions created using
  [`get_dwc()`](get_dwc.md).

- data:

  a [tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

- terms:

  a vector or list of terms to be used as replacement.

## Value

an object of class
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

`as_dwc()` will replace current column names by the ones defined in
`terms`. For each column in `data`, Darwin Core equivalent terms must be
informed in the same order by the user. If `terms` and column names do
not match in length or if `terms` used are not found in Darwin Core
standard, an error will be printed on `Console`.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
# create a data frame with sample metadata
my_df <- tibble::tibble(
  species = c("sp1", "sp2", "sp3"),
  location = c("loc1", "loc2", "loc3"),
  voucher = c("M01", "M02", "M03"),
  collector = c("John", "Robert", "David")
)

if (FALSE) { # \dontrun{
# get_dwc() reads from a remote repository, so this part of the example
# is not run automatically.
dwc <- get_dwc(type = "simple")

# rename columns
as_dwc(dwc, my_df, terms = c("scientificName", "locality", "catalogNumber", "recordedBy"))
} # }
```
