# Coerce inputs to a canonical movement track table

`as_track()` converts a data.frame/tibble to a validated `track_tbl`
with columns `id`, `time` (POSIXct, UTC), `lon`, and `lat` (decimal
degrees), sorted by `(id, time)`. This canonical structure is what all
intersimR functions expect internally.

## Usage

``` r
as_track(x, ...)

# S3 method for class 'data.frame'
as_track(x, lon, lat, time, id, tz = "UTC", crs = "OGC:CRS84", ...)
```

## Arguments

- x:

  Input object.

- ...:

  Reserved for future method-specific arguments; unused here.

- lon, lat, time, id:

  Columns (tidy-eval) identifying longitude, latitude, timestamp, and
  track identifier in `x`.

- tz:

  Character timezone for timestamps (default `"UTC"`). Passed to
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) during
  coercion.

- crs:

  Coordinate reference string stored as an attribute (default
  `"OGC:CRS84"`). Informational; positions are assumed to be lon/lat
  (EPSG:4326).

## Value

A `track_tbl`.

## Examples

``` r
df <- data.frame(
  id   = c("a","a","a"),
  time = as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + c(0, 60, 120),
  lon  = c(0.00, 0.01, 0.02),
  lat  = c(0.00, 0.01, 0.02)
)
trk <- as_track(df, lon = lon, lat = lat, time = time, id = id)
is_track_tbl(trk)
#> [1] TRUE
```
