# Add a per-animal sequential fix identifier

Add a per-animal sequential fix identifier

## Usage

``` r
add_fix_id(x, overwrite = FALSE)
```

## Arguments

- x:

  A `track_tbl` with columns `id`, `lon`, `lat`, `time`.

- overwrite:

  Logical; if `FALSE` (default) and `fixID` already exists, an error is
  thrown to avoid accidental reassignment. Set `TRUE` to recompute.

## Value

The same `track_tbl` with an integer column `fixID`: 1, 2, 3, ... within
each `id`, ordered by `time` (ties broken by `lon`, `lat`).
