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
      fixID = character(),
      animalID = character(),
      vesselID = character(),
      animal_time = as.POSIXct(character(), tz = tz_an),
      vessel_time = as.POSIXct(character(), tz = tz_an),
      animal_lon = double(), animal_lat = double(),
      vessel_lon = double(), vessel_lat = double(),
      dt_min = double(), dist_m = double()
    )
  }

  # --- rename for semantic clarity ---
  animal_df[, `:=`(
    animalID    = id,
    animal_lon  = lon,
    animal_lat  = lat,
    animal_time = time
  )]

  vessel_df[, `:=`(
    vesselID    = id,
    vessel_lon  = lon,
    vessel_lat  = lat,
    vessel_time = time
  )]

  # --- global time prefilter for vessels (expanded by time_thr) ---
  tmin <- min(animal_df$animal_time) - time_thr_min * 60
  tmax <- max(animal_df$animal_time) + time_thr_min * 60
  vessel_df <- vessel_df[vessel_time >= tmin & vessel_time <= tmax]

  if (nrow(vessel_df) == 0L) {
    out <- .empty_matches()
    attr(out, "dist_thr_m") <- dist_thr_m
    attr(out, "time_thr_min") <- time_thr_min
    return(out)
  }

  # --- internal helper: interpolate lon/lat at animal times and compute dt_min ---
  .interp_vessel_at_animal_time <- function(vdt, animal_times) {
    # vdt: data.table for ONE vesselID with vessel_time, vessel_lon, vessel_lat
    # animal_times: POSIXct vector
    vdt <- vdt[order(vessel_time)]
    t_obs <- as.numeric(vdt$vessel_time)
    if (length(t_obs) < 2L) return(NULL)  # cannot interpolate safely

    t_out <- as.numeric(animal_times)

    # No extrapolation: approx returns NA outside range (rule = 1)
    lon_i <- stats::approx(t_obs, vdt$vessel_lon, xout = t_out, rule = 1, ties = "ordered")$y
    lat_i <- stats::approx(t_obs, vdt$vessel_lat, xout = t_out, rule = 1, ties = "ordered")$y

    # nearest-observed time distance for dt_min
    idx <- findInterval(t_out, t_obs)
    # clamp indices
    i1 <- pmax(idx, 1L)
    i2 <- pmin(idx + 1L, length(t_obs))

    dt_sec <- pmin(abs(t_out - t_obs[i1]), abs(t_out - t_obs[i2]))
    dt_min <- dt_sec / 60

    data.table::data.table(
      vessel_lon = lon_i,
      vessel_lat = lat_i,
      dt_min = dt_min
    )
  }

  # --- build matches by vesselID (keeps multiple vessels per fix) ---
  animal_times <- animal_df$animal_time
  # ensure deterministic order on animal side
  data.table::setorder(animal_df, animalID, animal_time, animal_lon, animal_lat)

  out_list <- vector("list", length = data.table::uniqueN(vessel_df$vesselID))
  v_ids <- sort(unique(vessel_df$vesselID))

  k <- 0L
  for (vid in v_ids) {
    vdt <- vessel_df[vesselID == vid]
    interp <- .interp_vessel_at_animal_time(vdt, animal_times)
    if (is.null(interp)) next

    # combine animal fixes with interpolated vessel positions at same timestamps
    tmp <- data.table::data.table(
      fixID = animal_df$fixID,
      animalID = animal_df$animalID,
      vesselID = vid,
      animal_time = animal_df$animal_time,
      vessel_time = animal_df$animal_time,   # evaluated at animal timestamps
      animal_lon = animal_df$animal_lon,
      animal_lat = animal_df$animal_lat,
      vessel_lon = interp$vessel_lon,
      vessel_lat = interp$vessel_lat,
      dt_min = interp$dt_min
    )

    # enforce time threshold (nearest observed vessel fix)
    tmp <- tmp[dt_min <= time_thr_min]

    # drop rows where interpolation not possible (outside vessel time range)
    tmp <- tmp[!is.na(vessel_lon) & !is.na(vessel_lat)]

    if (nrow(tmp) == 0L) next

    # distance threshold
    tmp[, dist_m := geosphere::distGeo(
      p1 = cbind(animal_lon, animal_lat),
      p2 = cbind(vessel_lon, vessel_lat)
    )]
    tmp <- tmp[dist_m <= dist_thr_m]

    if (nrow(tmp) == 0L) next

    # dyad ID (animalID includes sim identity if you set it that way)
    tmp[, matchID := paste(animalID, vesselID, sep = "_")]

    k <- k + 1L
    out_list[[k]] <- tmp
  }

  if (k == 0L) {
    out <- .empty_matches()
    attr(out, "dist_thr_m") <- dist_thr_m
    attr(out, "time_thr_min") <- time_thr_min
    return(out)
  }

  cand <- data.table::rbindlist(out_list[seq_len(k)], use.names = TRUE)

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
