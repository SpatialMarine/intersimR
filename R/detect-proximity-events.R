#' Detect proximity-based events between animal and vessel
#'
#' Proximity events are contiguous periods within a dyad (matchID) where
#' paired fixes remain within `min_dist_m`, allowing internal gaps up to
#' `max_gap_min`. Events shorter than `min_duration_min` are discarded.
#'
#' @param pairs data.frame/tibble from match_animal_vessels(); requires
#'   columns: matchID, fixID, animalID, vesselID, animal_time, dist_m
#' @param min_dist_m numeric. Maximum distance (meters) to consider proximity.
#' @param min_duration_min numeric. Minimum event duration (minutes).
#' @param max_gap_min numeric. Maximum allowed gap (minutes) within an event.
#'
#' @return input `pairs` augmented with `eventID` (NA if not in a selected event)
#'
#' @export
detect_proximity_events <- function(pairs,
                                    min_dist_m,
                                    min_duration_min,
                                    max_gap_min) {

  req <- c("matchID","fixID","animalID","vesselID","animal_time","dist_m")
  if (!all(req %in% names(pairs))) {
    stop("`pairs` must contain columns: ", paste(req, collapse = ", "), call. = FALSE)
  }

  DT <- data.table::as.data.table(pairs)

  # 1) Keep only within-distance observations for segmentation
  cand <- DT[dist_m <= min_dist_m]
  if (nrow(cand) == 0L) {
    # Return input with eventID = NA and empty events table
    out_data <- tibble::as_tibble(DT)
    out_data$eventID <- NA_character_

    out_events <- tibble::tibble(matchID=character(), eventID=character(),
                                 animalID=character(), vesselID=character(),
                                 nloc=integer(), start=as.POSIXct(character()),
                                 end=as.POSIXct(character()),
                                 duration_min=double(), dist_min=double())
    return(list(data = out_data, events = out_events))
  }

  # 2) Order within dyad and segment by max gap
  data.table::setorder(cand, matchID, animal_time)
  cand[, seg := timegap_segments(animal_time, max_gap_min), by = matchID]

  # 3) Summarize segments and filter by duration/dist
  ev <- cand[, list(
    nloc = .N,
    start = min(animal_time),
    end   = max(animal_time),
    duration_min = as.numeric(difftime(max(animal_time), min(animal_time), units = "mins")),
    dist_min = min(dist_m),
    animalID = animalID[1],
    vesselID = vesselID[1]
  ), by = list(matchID, seg)]

  ev <- ev[duration_min >= min_duration_min & dist_min <= min_dist_m]
  if (nrow(ev) == 0L) {
    out_data <- tibble::as_tibble(DT)
    out_data$eventID <- NA_character_

    out_events <- tibble::tibble(matchID = character(), eventID = character(),
                                 animalID = character(), vesselID = character(),
                                 nloc = integer(), start = as.POSIXct(character()),
                                 end = as.POSIXct(character()),
                                 duration_min = double(), dist_min = double())
    return(list(data = out_data, events = out_events))
  }

  # 4) Construct stable human-readable event IDs
  ev[, eventID := paste(matchID, sprintf("%03d", seg), sep = "_")]

  # 5) Attach eventID to candidate rows in selected segments only
  cand_sel <- ev[cand, on = list(matchID, seg), nomatch = 0L]
  # Keep only keys needed for joining back
  idmap <- cand_sel[, list(matchID, fixID, eventID)]

  # 6) Augment the full pairs table with eventID (NA otherwise)
  out_data <- idmap[DT, on = list(matchID, fixID)]
  # idmap[DT,] places idmap as 'x' and DT as 'i'; re-order columns and coerce tidy
  data.table::setnames(out_data, old = c("i.matchID","i.fixID"), new = c("matchID","fixID"), skip_absent = TRUE)
  if (!"eventID" %in% names(out_data)) out_data[, eventID := NA_character_]
  data.table::setcolorder(out_data, c(names(DT), "eventID"))
  out_data <- tibble::as_tibble(out_data)

  out_events <- tibble::as_tibble(
    ev[, list(matchID, eventID, animalID, vesselID, nloc, start, end, duration_min, dist_min)]
  )

  list(data = out_data, events = out_events)

}
