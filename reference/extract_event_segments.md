# Extract animal and vessel segments (track_tbl) for a selected proximity event

Operates on the output of detect_proximity_events() and assumes paired
data produced by match_animal_vessels(), with a fixed internal schema.

## Usage

``` r
extract_event_segments(x, eventID, lead_secs = 0L, lag_secs = 0L)
```

## Arguments

- x:

  list from detect_proximity_events(); contains \$data and \$events

- eventID:

  character(1). Event identifier to extract

- lead_secs:

  integer(1). Seconds to extend window before event start (\>=0)

- lag_secs:

  integer(1). Seconds to extend window after event end (\>=0)

## Value

list with:

- animal: track_tbl

- vessel: track_tbl

- meta: list with event metadata and window bounds
