# Add a per-track sequential fix index and a globally-unique fix identifier

Add a per-track sequential fix index and a globally-unique fix
identifier

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

The same `track_tbl` with:

- `fixN`: integer 1,2,3,... within each `id` ordered by time (ties by
  lon/lat)

- `fixID`: character unique identifier `paste(id, fixN, sep = "_")`
