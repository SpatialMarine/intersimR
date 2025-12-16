# Create a point.check function for availability::surrogateAR()

Returns a function with signature function(tm, pt) that evaluates
whether a candidate point is valid (at sea) using a 0/1 oceanmask.

## Usage

``` r
make_point_check(oceanmask, outside = c("land", "sea"))
```

## Arguments

- oceanmask:

  A terra::SpatRaster with values 0=ocean, 1=land.

- outside:

  How to treat points outside the mask extent or returning NA. One of
  c("land","sea"). Default "land" (conservative; matches your current
  logic).

## Value

A function(tm, pt) -\> logical, TRUE if the point is at sea.
