#' Test animal-vessel interactions (attraction / follow) using simulations
#'
#' Given an animal and vessel segment (track_tbl) extracted around an observed event,
#' simulate animal tracks, pair them with the vessel, detect proximity events, and
#' compute a Monte Carlo p-value for:
#'   - "attract": probability that a null simulation produces >= 1 proximity event
#'   - "follow" : probability that the earliest null event has duration >= observed duration
#'
#' @param animal track_tbl. Animal segment.
#' @param vessel track_tbl. Vessel segment.
#' @param method character. "attract" or "follow".
#' @param min_dist_m numeric. Proximity threshold in meters.
#' @param min_duration_min numeric. Minimum event duration in minutes.
#' @param max_gap_min numeric. Maximum allowed internal gap within an event (minutes).
#' @param sim_n integer. Target number of simulations.
#' @param obs_duration_min numeric. Observed duration (minutes) for follow test.
#'   REQUIRED if method == "follow". Should come from extract_event_segments()$meta$duration_min.
#' @param seed integer or NULL. Random seed.
#' @param oceanmask optional. Passed to simulate_tracks().
#' @param min_locs integer. Minimum number of locations; if nrow(animal) <= min_locs,
#'   interpolate to min_locs + 1.
#' @param anchor character. Passed to simulate_tracks() (e.g., "start" or "start_end").
#' @param cores integer. Passed to simulate_tracks() if relevant.
#' @param return_simdata logical. If TRUE, return paired sim data and sim events.
#'
#' @return list with:
#'   - result: tibble (one row) with p_value and summaries
#'   - meta: list with n_valid, n_failed, interpolated, etc.
#'   - (optional) sim_pairs, sim_events: for inspection if return_simdata = TRUE
#'
#' @export
test_interactions <- function(animal,
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
                              return_simdata = FALSE) {

  method <- match.arg(method)

  if (!is.null(seed)) set.seed(seed)

  if (!is.numeric(sim_n) || length(sim_n) != 1L || sim_n < 1) {
    stop("`sim_n` must be a positive integer.", call. = FALSE)
  }
  sim_n <- as.integer(sim_n)

  if (method == "follow") {
    if (is.null(obs_duration_min) || !is.numeric(obs_duration_min) || length(obs_duration_min) != 1L) {
      stop("For method='follow', `obs_duration_min` must be provided as a single numeric value.", call. = FALSE)
    }
  }

  # ------------------------------------------------------------------
  # Helper: interpolate animal track to n points (base approx)
  # ------------------------------------------------------------------
  interpolate_track_minlocs <- function(trk, n_out) {
    df <- as.data.frame(trk)

    # Expect at least id, time, lon, lat columns from your as_track() design
    req <- c("id", "time", "lon", "lat")
    if (!all(req %in% names(df))) {
      stop("Animal track_tbl must contain columns: id, time, lon, lat.", call. = FALSE)
    }
    if (nrow(df) < 2L) {
      stop("Cannot interpolate track with < 2 fixes.", call. = FALSE)
    }

    # Ensure sorted by time
    df <- df[order(df$time), ]
    t_num <- as.numeric(df$time)

    t_new <- seq(min(df$time), max(df$time), length.out = n_out)
    t_new_num <- as.numeric(t_new)

    lon_new <- stats::approx(t_num, df$lon, xout = t_new_num, method = "linear", ties = "ordered")$y
    lat_new <- stats::approx(t_num, df$lat, xout = t_new_num, method = "linear", ties = "ordered")$y

    out <- data.frame(
      id   = df$id[1],
      time = as.POSIXct(t_new, tz = attr(df$time, "tzone")),
      lon  = lon_new,
      lat  = lat_new
    )

    as_track(out, lon = lon, lat = lat, time = time, id = id)
  }

  # ------------------------------------------------------------------
  # 1) Interpolate animal if needed
  # ------------------------------------------------------------------
  interpolated <- FALSE
  if (nrow(animal) <= min_locs) {
    animal <- interpolate_track_minlocs(animal, n_out = min_locs + 1L)
    interpolated <- TRUE
  }

  # ------------------------------------------------------------------
  # 2) Simulate tracks (may return fewer valid simulations)
  # ------------------------------------------------------------------
  sim_tracks <- simulate_tracks(
    animal,
    sim_n = sim_n,
    seed = seed,
    oceanmask = oceanmask,
    anchor = anchor,
    cores = cores
  )

  # Expect simulate_tracks() to return a data.frame/track_tbl with nsim or simid.
  sim_df <- as.data.frame(sim_tracks)
  if (!("simID" %in% names(sim_df))) {
    stop("simulate_tracks() output must include `nsim` and/or `simid` to identify simulations.", call. = FALSE)
  }

  n_valid <- length(unique(sim_df$simID))
  n_failed <- sim_n - n_valid
  if (n_valid < 1L) {
    stop("No valid simulations were generated (n_valid = 0).", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 3) Pair simulated tracks with vessel
  # ------------------------------------------------------------------
  # convert to track_tbl and add fixID
  sim_tracks <- as_track(sim_tracks, lon = lon, lat = lat, time = time, id = id)
  sim_tracks <- add_fix_id(sim_tracks)

  # pair with vessel
  sim_pairs <- match_animal_vessels(sim_tracks, vessel, dist_thr_m = 30000, time_thr_min = 5)

  # ------------------------------------------------------------------
  # 4) Detect proximity events in simulated pairs
  # ------------------------------------------------------------------
  sim_ev <- detect_proximity_events(
    pairs = sim_pairs,
    min_dist_m = min_dist_m,
    min_duration_min = min_duration_min,
    max_gap_min = max_gap_min
  )

  sim_events <- sim_ev$events

  # ------------------------------------------------------------------
  # 5–7) Compute method-specific statistic and p-value
  # ------------------------------------------------------------------
  if (method == "attract") {

    # observed statistic is always 1 (we are testing within an observed-event window)
    obs_stat <- 1L

    # count simulations with >= 1 event
    if (nrow(sim_events) == 0L) {
      n_event_sims <- 0L
    } else {
      # Each event row belongs to a matchID; ensure matchID encodes simulation identity.
      # We treat ">= 1 event in simulation" as ">= 1 matchID for that simulation".
      # If matchID is unique per simulation, n_distinct(matchID) == n_event_sims.
      n_event_sims <- dplyr::n_distinct(sim_events$matchID)
    }

    # Monte Carlo p-value; with obs_stat==1 this reduces to (n_event_sims + 1)/(n_valid + 1)
    p_value <- (n_event_sims + 1) / (n_valid + 1)

    result <- tibble::tibble(
      method = "attract",
      sim_n_target = sim_n,
      n_valid = n_valid,
      n_failed = n_failed,
      obs_stat = obs_stat,
      n_event_sims = n_event_sims,
      p_value = p_value
    )

  } else { # method == "follow"

    obs_stat <- as.numeric(obs_duration_min)

    # For each simulation, take the earliest event (by start time) and its duration.
    # If no event in a simulation, duration = 0.
    sims_all <- data.frame(matchID = unique(sim_pairs$matchID))

    if (nrow(sim_events) == 0L) {
      sim_dur <- sims_all |>
        dplyr::mutate(duration_min = 0)
    } else {
      sim_dur <- sim_events |>
        dplyr::arrange(matchID, start) |>
        dplyr::group_by(matchID) |>
        dplyr::slice(1L) |>
        dplyr::ungroup() |>
        dplyr::select(matchID, duration_min)

      sim_dur <- sims_all |>
        dplyr::left_join(sim_dur, by = "matchID") |>
        dplyr::mutate(duration_min = dplyr::coalesce(duration_min, 0))
    }

    n_ge <- sum(sim_dur$duration_min >= obs_stat)

    p_value <- (n_ge + 1) / (n_valid + 1)

    result <- tibble::tibble(
      method = "follow",
      sim_n_target = sim_n,
      n_valid = n_valid,
      n_failed = n_failed,
      obs_stat = obs_stat,
      null_mean = mean(sim_dur$duration_min),
      null_sd = stats::sd(sim_dur$duration_min),
      null_q95 = as.numeric(stats::quantile(sim_dur$duration_min, 0.95, names = FALSE)),
      n_ge = n_ge,
      p_value = p_value
    )
  }

  meta <- list(
    interpolated = interpolated,
    min_locs = min_locs,
    anchor = anchor,
    min_dist_m = min_dist_m,
    min_duration_min = min_duration_min,
    max_gap_min = max_gap_min
  )

  out <- list(result = result, meta = meta)

  if (return_simdata) {
    out$sim_pairs <- sim_pairs
    out$sim_events <- sim_events
  }

  out
}
