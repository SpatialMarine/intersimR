# Detect proximity-based events between animal and vessel

Proximity events are contiguous periods within a dyad (matchID) where
paired fixes remain within `min_dist_m`, allowing internal gaps up to
`max_gap_min`. Events shorter than `min_duration_min` are discarded.

## Usage

``` r
detect_proximity_events(pairs, min_dist_m, min_duration_min, max_gap_min)
```

## Arguments

- pairs:

  data.frame/tibble from match_animal_vessels(); requires columns:
  matchID, fixID, animalID, vesselID, animal_time, dist_m

- min_dist_m:

  numeric. Maximum distance (meters) to consider proximity.

- min_duration_min:

  numeric. Minimum event duration (minutes).

- max_gap_min:

  numeric. Maximum allowed gap (minutes) within an event.

## Value

input `pairs` augmented with `eventID` (NA if not in a selected event)
