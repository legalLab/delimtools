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
dwc <- get_dwc(type= "simple")
dwc_terms(dwc, c("genus", "scientificName"))
#> • genus: The full scientific name of the genus in which the dwc:Taxon is
#> classified. Examples: `Puma`; `Monoclea`
#> • scientificName: The full scientific name, with authorship and date
#> information if known. When forming part of a dwc:Identification, this should be
#> the name in lowest level taxonomic rank that can be determined. Examples:
#> `Coleoptera` (order); `Vespertilionidae` (family); `Manis` (genus); `Ctenomys
#> sociabilis` (genus + specificEpithet); `Ambystoma tigrinum diaboli` (genus +
#> specificEpithet + infraspecificEpithet); `Roptrocerus typographi (Györfi,
#> 1952)` (genus + specificEpithet + scientificNameAuthorship); `Quercus agrifolia
#> var. oxyadenia (Torr.) J.T. Howell` (genus + specificEpithet + taxonRank +
#> infraspecificEpithet + scientificNameAuthorship); `×Agropogon littoralis (Sm.)
#> C. E. Hubb.`; `Mentha ×smithiana R. A. Graham`; `Agrostis stolonifera L. ×
#> Polypogon monspeliensis (L.) Desf.`
```
