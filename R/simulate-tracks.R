#' Simulate surrogate tracks for an animal segment
#'
#' Fits a surrogate AR movement model (availability) to a single animal segment
#' and generates `sim_n` simulated tracks with the same timestamps.
#' Optionally constrains simulated points to sea using an ocean mask (terra).
#'
#' @param animal A track_tbl for a single segment. Requires columns: id, lon, lat, time.
#' @param sim_n Integer. Number of simulations.
#' @param oceanmask Optional terra::SpatRaster with values 0=ocean, 1=land.
#' @param min_locs Integer. If animal has < min_locs rows, it will be resampled to `min_locs`.
#' @param anchor One of c("start","start_end"). Fix first point only, or first+last.
#' @param cores Integer. Number of cores. On macOS/Linux uses fork parallelism.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A tibble with columns: animalID, simID, time, lon, lat
#' @export
simulate_tracks <- function(animal,
                            sim_n = 1000L,
                            oceanmask = NULL,
                            min_locs = 6L,
                            anchor = c("start", "start_end"),
                            cores = 1L,
                            seed = NULL) {

  # ---- dependency guard ---------------------------------------------------
  if (!requireNamespace("availability", quietly = TRUE)) {
    stop(
      "Package 'availability' is required for simulate_tracks().\n",
      "Install it with:\n",
      "  remotes::install_github('AustralianAntarcticDataCentre/availability')",
      call. = FALSE
    )
  }
  # ------------------------------------------------------------------------



  anchor  <- match.arg(anchor)

  stopifnot(inherits(animal, "track_tbl"))
  req <- c("id", "lon", "lat", "time")
  if (!all(req %in% names(animal))) {
    stop("`animal` must contain columns: id, lon, lat, time.", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  .empty_out <- function(animalID) {
    tibble::tibble(
      animalID = character(),
      simID = character(),
      time = as.POSIXct(character()),
      lon = double(),
      lat = double()
    )
  }

  .resample_track <- function(df, n) {
    df <- df[order(df$time), ]
    if (nrow(df) >= n) return(df)
    t0 <- min(df$time); t1 <- max(df$time)
    if (t0 == t1) return(df)

    t_num <- as.numeric(df$time)
    t_new <- seq(from = t0, to = t1, length.out = n)
    t_new_num <- as.numeric(t_new)

    lon_new <- stats::approx(t_num, df$lon, xout = t_new_num, ties = "ordered")$y
    lat_new <- stats::approx(t_num, df$lat, xout = t_new_num, ties = "ordered")$y

    data.frame(
      id   = df$id[1],
      lon  = lon_new,
      lat  = lat_new,
      time = as.POSIXct(t_new, origin = "1970-01-01", tz = attr(df$time, "tzone"))
    )
  }

  tr <- as.data.frame(animal)
  tr <- tr[order(tr$time), c("id","lon","lat","time")]
  animalID <- as.character(tr$id[1])

  if (nrow(tr) < 3L) return(.empty_out(animalID))
  if (nrow(tr) < min_locs) tr <- .resample_track(tr, n = min_locs)

  arfit <- tryCatch(
    availability::surrogateARModel(tr[, c("lon", "lat")]),
    error = function(e) NULL
  )
  if (is.null(arfit)) return(.empty_out(animalID))

  fixed_vec <- switch(
    anchor,
    "start"     = rep(c(TRUE, FALSE), c(1, nrow(tr) - 1)),
    "start_end" = rep(c(TRUE, FALSE, TRUE), c(1, nrow(tr) - 2, 1))
  )

  # checker (built internally only)
  pc_main <- if (!is.null(oceanmask)) {
    make_point_check(oceanmask)
  } else {
    function(tm, pt) TRUE
  }

  pad_w <- max(4L, nchar(sim_n))

  .sim_error <- function(s, msg) {
    structure(list(s = s, message = msg), class = "sim_error")
  }

  .one_sim <- function(s, pc) {
    tryCatch({
      simu <- availability::surrogateAR(
        arfit,
        xs = tr[, c("lon", "lat")],
        ts = tr[, c("time")],
        point.check = pc,
        fixed = fixed_vec,
        partial = FALSE
      )
      if (is.null(simu) || is.na(simu$xs[1])) return(NULL)

      tibble::tibble(
        animalID = animalID,
        simID = stringr::str_pad(s, width = pad_w, pad = "0"),
        id = paste(animalID, simID, sep = "_"),
        time  = as.POSIXct(simu$ts, origin = "1970-01-01", tz = attr(tr$time, "tzone")),
        lon   = simu$xs[, 1],
        lat   = simu$xs[, 2]
      )
    }, error = function(e) .sim_error(s, conditionMessage(e)))
  }

  if (cores <= 1L) {
    sim_list <- lapply(seq_len(sim_n), function(s) .one_sim(s, pc_main))
  } else if (.Platform$OS.type == "unix") {
    RNGkind("L'Ecuyer-CMRG")
    if (!is.null(seed)) set.seed(seed)
    sim_list <- parallel::mclapply(
      X = seq_len(sim_n),
      FUN = function(s) .one_sim(s, pc_main),
      mc.cores = cores,
      mc.preschedule = FALSE
    )
  } else {
    # Windows PSOCK fallback: wrap mask and rebuild checker in worker
    mask_w <- if (!is.null(oceanmask)) terra::wrap(oceanmask) else NULL

    RNGkind("L'Ecuyer-CMRG")
    if (!is.null(seed)) set.seed(seed)

    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    sim_list <- foreach::foreach(
      s = seq_len(sim_n),
      .packages = c("availability", "stringr", "tibble", "terra"),
      .export   = c("make_point_check")
    ) %dopar% {

      pc_local <- function(tm, pt) TRUE
      if (!is.null(mask_w)) {
        mask_local <- terra::unwrap(mask_w)
        pc_local <- make_point_check(mask_local)
      }

      # re-run locally
      .one_sim(s, pc_local)
    }
  }

  ok   <- Filter(function(x) is.data.frame(x), sim_list)
  errs <- Filter(function(x) inherits(x, "sim_error"), sim_list)

  if (length(ok) == 0L) {
    if (length(errs) > 0L) {
      stop("All simulations failed. First error: ", errs[[1]]$message, call. = FALSE)
    }
    return(.empty_out(animalID))
  }

  tibble::as_tibble(data.table::rbindlist(ok, fill = TRUE))
}
