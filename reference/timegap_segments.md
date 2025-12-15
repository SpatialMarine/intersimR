# Segment a sorted time series by a maximum gap (in minutes)

Segment a sorted time series by a maximum gap (in minutes)

## Usage

``` r
timegap_segments(time, max_gap_min)
```

## Arguments

- time:

  POSIXct vector, already sorted

- max_gap_min:

  numeric, gap threshold in minutes

## Value

integer segment index starting at 1
