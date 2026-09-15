# Select the Best Partition from a Delimitation Result

Generic function to extract the most informative partition from an ABGD
or ASAP result object.

## Usage

``` r
best_partition(x, ...)

# S3 method for class 'abgd'
best_partition(x, pass = c("recursive", "initial"), ...)

# S3 method for class 'asap'
best_partition(x, rank = NULL, ...)
```

## Arguments

- x:

  A delimitation result object (`"abgd"` or `"asap"`).

- ...:

  Additional arguments passed to methods.

- pass:

  Which pass to use: `"recursive"` (default) or `"initial"`.

- rank:

  Integer. Rank of the partition to return (1 = best ASAP score). If
  `NULL` (default), returns rank 1.

## Value

A named list describing the selected partition. See method documentation
for details.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- abgd("geophagus.fasta", model = "simple")
best   <- best_partition(result)
best$n_groups
} # }
```
