# Create a land/ocean mask raster for track simulations

Creates a raster mask with values:

- 0 = ocean

- 1 = land

## Usage

``` r
create_oceanmask(
  bbox,
  res,
  polygon,
  polygon_type = c("land", "ocean"),
  crs = "EPSG:4326",
  touches = TRUE
)
```

## Arguments

- bbox:

  Numeric vector of length 4: c(xmin, xmax, ymin, ymax) in lon/lat.

- res:

  Numeric. Raster resolution in degrees (e.g., 0.01).

- polygon:

  Spatial polygon defining either land or ocean. Either a
  terra::SpatVector or an sf object.

- polygon_type:

  Character. One of c("land","ocean") indicating what `polygon`
  represents.

- crs:

  Character. Target CRS; default is EPSG:4326.

- touches:

  Logical. Passed to terra::rasterize(); if TRUE, cells touched by
  polygon boundaries are included (often preferable near coastlines).

## Value

A terra::SpatRaster with values 0 (ocean) and 1 (land).

## Details

The input polygon can represent either land or ocean; set `polygon_type`
accordingly. Areas not covered by the input polygon are assigned the
complementary class (land if polygon is ocean; ocean if polygon is
land).
