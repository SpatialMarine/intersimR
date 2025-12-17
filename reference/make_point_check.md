# Create a point.check function for availability::surrogateAR()

Create a point.check function for availability::surrogateAR()

## Usage

``` r
make_point_check(oceanmask)
```

## Arguments

- oceanmask:

  terra::SpatRaster with values 0=ocean, 1=land

## Value

function(tm, pt) -\> logical (TRUE if at sea)
