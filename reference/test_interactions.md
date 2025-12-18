# Test animal-vessel interactions (attraction / follow) using simulations

Given an animal and vessel segment (track_tbl) extracted around an
observed event, simulate animal tracks, pair them with the vessel,
detect proximity events, and compute a Monte Carlo p-value for:

- "attract": probability that a null simulation produces \>= 1 proximity
  event

- "follow" : probability that the earliest null event has duration \>=
  observed duration

## Usage

``` r
test_interactions(
  animal,
  vessel,
  method = c("attract", "follow"),
  min_dist_m,
  min_duration_min,
  max_gap_min,
  sim_n = 1000,
  obs_duration_min = NULL,
  seed = NULL,
  oceanmask = NULL,
  min_locs = 6,
  anchor = "start_end",
  cores = 1,
  return_simdata = FALSE
)
```

## Arguments

- animal:

  track_tbl. Animal segment.

- vessel:

  track_tbl. Vessel segment.

- method:

  character. "attract" or "follow".

- min_dist_m:

  numeric. Proximity threshold in meters.

- min_duration_min:

  numeric. Minimum event duration in minutes.

- max_gap_min:

  numeric. Maximum allowed internal gap within an event (minutes).

- sim_n:

  integer. Target number of simulations.

- obs_duration_min:

  numeric. Observed duration (minutes) for follow test. REQUIRED if
  method == "follow". Should come from
  extract_event_segments()\$meta\$duration_min.

- seed:

  integer or NULL. Random seed.

- oceanmask:

  optional. Passed to simulate_tracks().

- min_locs:

  integer. Minimum number of locations; if nrow(animal) \<= min_locs,
  interpolate to min_locs + 1.

- anchor:

  character. Passed to simulate_tracks() (e.g., "start" or "start_end").

- cores:

  integer. Passed to simulate_tracks() if relevant.

- return_simdata:

  logical. If TRUE, return paired sim data and sim events.

## Value

list with:

- result: tibble (one row) with p_value and summaries

- meta: list with n_valid, n_failed, interpolated, etc.

- (optional) sim_pairs, sim_events: for inspection if return_simdata =
  TRUE
