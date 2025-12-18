#' Extract animal and vessel segments (track_tbl) for a selected proximity event
#'
#' Operates on the output of detect_proximity_events() and assumes paired data
#' produced by match_animal_vessels(), with a fixed internal schema.
#'
#' @param x list from detect_proximity_events(); contains $data and $events
#' @param eventID character(1). Event identifier to extract
#' @param lead_secs integer(1). Seconds to extend window before event start (>=0)
#' @param lag_secs integer(1). Seconds to extend window after event end (>=0)
#'
#' @return list with:
#'   - animal: track_tbl
#'   - vessel: track_tbl
#'   - meta: list with event metadata and window bounds
#'
#' @export
extract_event_segments <- function(x,
                                   eventID,
                                   lead_secs = 0L,
                                   lag_secs  = 0L) {

  # ---- input validation ----
  if (!is.list(x) || !all(c("data", "events") %in% names(x))) {
    stop("`x` must be the list returned by detect_proximity_events(): list(data=..., events=...)", call. = FALSE)
  }
  if (!is.character(eventID) || length(eventID) != 1L || is.na(eventID)) {
    stop("`eventID` must be a single, non-missing character string.", call. = FALSE)
  }

  lead_secs <- as.integer(lead_secs)
  lag_secs  <- as.integer(lag_secs)
  if (lead_secs < 0L || lag_secs < 0L) {
    stop("`lead_secs` and `lag_secs` must be non-negative.", call. = FALSE)
  }

  pairs  <- x$data
  events <- x$events

  # ---- enforce internal schema ----
  req_ev <- c("matchID", "eventID", "animalID", "vesselID",
              "start", "end", "duration_min")
  if (!all(req_ev %in% names(events))) {
    stop("x$events must contain columns: ", paste(req_ev, collapse = ", "), call. = FALSE)
  }

  req_pairs <- c("animalID", "vesselID",
                 "animal_time",
                 "animal_lon", "animal_lat",
                 "vessel_lon", "vessel_lat")
  if (!all(req_pairs %in% names(pairs))) {
    stop("x$data must contain columns: ", paste(req_pairs, collapse = ", "), call. = FALSE)
  }

  # vessel_time is optional but recommended
  has_vessel_time <- "vessel_time" %in% names(pairs)

  # ---- select event metadata ----
  ev <- events[events$eventID == eventID, , drop = FALSE]
  if (nrow(ev) == 0L) stop("eventID not found in x$events: ", eventID, call. = FALSE)
  if (nrow(ev) > 1L) {
    warning("Multiple rows found for eventID; using the first.", call. = FALSE)
    ev <- ev[1, , drop = FALSE]
  }

  if (!inherits(ev$start, "POSIXct") || !inherits(ev$end, "POSIXct")) {
    stop("x$events `start` and `end` must be POSIXct.", call. = FALSE)
  }

  # ---- define window ----
  window_start <- ev$start - lead_secs
  window_end   <- ev$end   + lag_secs

  # ---- slice paired data on animal_time ----
  seg_pairs <- pairs[pairs$animal_time >= window_start &
                       pairs$animal_time <= window_end, , drop = FALSE]

  if (nrow(seg_pairs) == 0L) {
    stop("No paired fixes found within window for eventID: ", eventID, call. = FALSE)
  }

  # ---- build tracks from identical rows ----
  animal_df <- seg_pairs |>
    dplyr::transmute(
      id   = animalID,
      time = animal_time,
      lon  = animal_lon,
      lat  = animal_lat
    )

  vessel_df <- seg_pairs |>
    dplyr::transmute(
      id   = vesselID,
      time = if (has_vessel_time) vessel_time else animal_time,
      lon  = vessel_lon,
      lat  = vessel_lat
    )

  animal_trk <- as_track(animal_df, lon = lon, lat = lat, time = time, id = id)
  vessel_trk <- as_track(vessel_df, lon = lon, lat = lat, time = time, id = id)

  meta <- list(
    matchID      = ev$matchID,
    eventID      = ev$eventID,
    animalID     = ev$animalID,
    vesselID     = ev$vesselID,
    start        = ev$start,
    end          = ev$end,
    duration_min = ev$duration_min,   # taken from input; not recalculated
    window_start = window_start,
    window_end   = window_end,
    lead_secs    = lead_secs,
    lag_secs     = lag_secs,
    n_pairs      = nrow(seg_pairs),
    vessel_time  = has_vessel_time
  )

  list(animal = animal_trk, vessel = vessel_trk, meta = meta)
}
