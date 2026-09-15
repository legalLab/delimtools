# Geophagus Eartheaters Species Partitions

This is a data frame containing species delimitation partitions for all
the 137 unique haplotypes of [geophagus](geophagus.md) generated using
functions contained in this package. Use [report_delim](report_delim.md)
to check number of lineages per method.

## Usage

``` r
geophagus_delims
```

## Format

A dataframe with 137 rows and 9 columns:

- labels:

  Unique haplotype labels

- abgd:

  species partitions for `ABGD` method

- asap:

  species partitions for `ASAP` method

- bgmyc:

  species partitions for `bGMYC` method

- gmyc:

  species partitions for `GMYC` method

- locmin:

  species partitions for `locmin` method

- morph:

  species partitions following NCBI taxonomy

- mptp:

  species partitions for `mPTP` method

- ptp:

  species partitions for `PTP` method
