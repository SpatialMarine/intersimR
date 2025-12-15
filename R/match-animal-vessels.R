#' Match animal fixes to vessel positions within time and distance thresholds
#'
#' @param animal track_tbl with columns: id, lon, lat, time, fixID
#' @param vessel track_tbl with columns: id, lon, lat, time
#' @param dist_thr_m numeric. Distance threshold in meters
#' @param time_thr_min numeric. Half time window in minutes (±)
#'
#' @return A tibble with columns:
#'   matchID, fixID, animalID, vesselID,
#'   animal_time, vessel_time,
#'   animal_lon, animal_lat, vessel_lon, vessel_lat,
#'   dt_min, dist_m
#' Attributes:
#'   dist_thr_m, time_thr_min
#'
#' @export
match_animal_vessels <- function(animal, vessel, dist_thr_m, time_thr_min) {
  stopifnot(inherits(animal, "track_tbl"),
            inherits(vessel, "track_tbl"),
            "fixID" %in% names(animal))

  animal_df <- data.table::as.data.table(animal)
  vessel_df <- data.table::as.data.table(vessel)
  tz_an <- attr(animal_df$time, "tzone")

  .empty_matches <- function() {
    tibble::tibble(
      matchID = character(),
      fixID = integer(),
      animalID = character(),
      vesselID = character(),
      animal_time = as.POSIXct(character(), tz = tz_an),
      vessel_time = as.POSIXct(character(), tz = tz_an),
      animal_lon = double(), animal_lat = double(),
      vessel_lon = double(), vessel_lat = double(),
      dt_min = double(), dist_m = double()
    )
  }

  # Rename explicitly for semantic clarity
  animal_df[, `:=`(
    animalID    = id,
    animal_lon  = lon,
    animal_lat  = lat,
    animal_time = time,
    time_start  = time - time_thr_min * 60,
    time_end    = time + time_thr_min * 60
  )]

  vessel_df[, `:=`(
    vesselID    = id,
    vessel_lon  = lon,
    vessel_lat  = lat,
    vessel_time = time,
    time_start  = time,
    time_end    = time
  )]

  # Global time pre-filter on vessels
  vessel_df <- vessel_df[
    time >= min(animal_df$animal_time) - time_thr_min * 60 &
      time <= max(animal_df$animal_time) + time_thr_min * 60
  ]
  if (nrow(vessel_df) == 0L) {
    out <- .empty_matches()
    attr(out, "dist_thr_m") <- dist_thr_m
    attr(out, "time_thr_min") <- time_thr_min
    return(out)
  }

  # Interval join on time
  data.table::setkey(vessel_df, time_start, time_end)
  cand <- data.table::foverlaps(
    x = animal_df[, list(fixID, animalID, animal_lon, animal_lat,
                      animal_time, time_start, time_end)],
    y = vessel_df[, list(vessel_time, time_start, time_end, vesselID, vessel_lon, vessel_lat)],
    by.x = c("time_start", "time_end"),
    by.y = c("time_start", "time_end"),
    nomatch = 0L
  )
  if (nrow(cand) == 0L) {
    out <- .empty_matches()
    attr(out, "dist_thr_m") <- dist_thr_m
    attr(out, "time_thr_min") <- time_thr_min
    return(out)
  }

  # Time difference (min) and geodesic distance (m)
  cand[, `:=`(
    dt_min = abs(as.numeric(difftime(vessel_time, animal_time, units = "mins"))),
    dist_m = geosphere::distGeo(
      p1 = cbind(animal_lon, animal_lat),
      p2 = cbind(vessel_lon, vessel_lat)
    )
  )]

  # Thresholds
  cand <- cand[dt_min <= time_thr_min & dist_m <= dist_thr_m]
  if (nrow(cand) == 0L) {
    out <- .empty_matches()
    attr(out, "dist_thr_m") <- dist_thr_m
    attr(out, "time_thr_min") <- time_thr_min
    return(out)
  }

  # Best vessel fix per (fixID, vesselID): min |Δt|, then min distance
  data.table::setorder(cand, fixID, vesselID, dt_min, dist_m)
  cand <- cand[, .SD[1L], by = list(fixID, vesselID)]

  # Human-readable dyad ID
  cand[, matchID := paste(animalID, vesselID, sep = "_")]

  out <- tibble::as_tibble(
    cand[, list(
      matchID,
      fixID,
      animalID, vesselID,
      animal_time, vessel_time,
      animal_lon, animal_lat,
      vessel_lon, vessel_lat,
      dt_min, dist_m
    )]
  )
  attr(out, "dist_thr_m") <- dist_thr_m
  attr(out, "time_thr_min") <- time_thr_min
  out
}
