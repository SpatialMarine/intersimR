# Match animal fixes to vessel positions within time and distance thresholds

Match animal fixes to vessel positions within time and distance
thresholds

## Usage

``` r
match_animal_vessels(animal, vessel, dist_thr_m, time_thr_min)
```

## Arguments

- animal:

  track_tbl with columns: id, lon, lat, time, fixID

- vessel:

  track_tbl with columns: id, lon, lat, time

- dist_thr_m:

  numeric. Distance threshold in meters

- time_thr_min:

  numeric. Half time window in minutes (±)

## Value

A tibble with columns: matchID, fixID, animalID, vesselID, animal_time,
vessel_time, animal_lon, animal_lat, vessel_lon, vessel_lat, dt_min,
dist_m Attributes: dist_thr_m, time_thr_min
