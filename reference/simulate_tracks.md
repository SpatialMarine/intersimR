# Simulate surrogate tracks for an animal segment

Fits a surrogate AR movement model (availability) to a single animal
segment and generates `sim_n` simulated tracks with the same timestamps.
Optionally constrains simulated points to sea using an ocean mask
(terra).

## Usage

``` r
simulate_tracks(
  animal,
  sim_n = 1000L,
  oceanmask = NULL,
  min_locs = 6L,
  anchor = c("start", "start_end"),
  cores = 1L,
  seed = NULL
)
```

## Arguments

- animal:

  A track_tbl for a single segment. Requires columns: id, lon, lat,
  time.

- sim_n:

  Integer. Number of simulations.

- oceanmask:

  Optional terra::SpatRaster with values 0=ocean, 1=land.

- min_locs:

  Integer. If animal has \< min_locs rows, it will be resampled to
  `min_locs`.

- anchor:

  One of c("start","start_end"). Fix first point only, or first+last.

- cores:

  Integer. Number of cores. On macOS/Linux uses fork parallelism.

- seed:

  Optional integer seed for reproducibility.

## Value

A tibble with columns: animalID, simID, time, lon, lat
