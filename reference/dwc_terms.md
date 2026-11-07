# Print Darwin Core Terms, Definitions and Examples as Bullet Lists

`dwc_terms()` checks a vector or list of terms and return definitions
and examples for each one of them.

## Usage

``` r
dwc_terms(dwc, terms)
```

## Arguments

- dwc:

  a list of standard terms and definitions created using
  [get_dwc](get_dwc.md).

- terms:

  a vector or list of terms to check.

## Value

a bullet list.

## Details

For each term in a vector or list, `dwc_terms` will return a bullet list
containing the term, followed by its definition and examples.

## Author

Pedro S. Bittencourt, Rupert A. Collins.

## Examples

``` r
if (FALSE) { # \dontrun{
# get_dwc() reads from a remote repository, so this example is not run
# automatically.
dwc <- get_dwc(type = "simple")
dwc_terms(dwc, c("genus", "scientificName"))
} # }
```
