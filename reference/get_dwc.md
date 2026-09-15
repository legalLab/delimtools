# Get Darwin Core Terms and Definitions

`get_dwc()` returns a list of standardized terms and definitions used by
the Darwin Core Maintenance Interest Group <https://dwc.tdwg.org/>.

## Usage

``` r
get_dwc(type)
```

## Arguments

- type:

  Which type of distribution files to download. Available options are:

  - simple Simple Darwin Core Terms.

  - all All Darwin Core Terms.

## Value

a list.

## Details

`get_dwc()` reads Darwin Core distribution documents and terms from
Github repository <https://github.com/tdwg/dwc> directly into
`Environment`. This function will return a list containing the most
recent accepted terms as a vector and a
[tbl_df](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing terms, definitions, examples and details about each one of
them.

## Author

Pedro S. Bittencourt, Rupert A. Collins

## Examples

``` r
dwc <- get_dwc(type= "simple") 
```
