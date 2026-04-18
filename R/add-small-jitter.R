#' Add a very small positional jitter to geographic coordinates
#'
#' Internal helper used as a numerical fallback when
#' [availability::surrogateARModel()] fails on segments with nearly singular
#' spatial geometry, such as very short or quasi-linear tracks. The function
#' adds a very small amount of random noise to longitude and latitude
#' coordinates expressed in decimal degrees.
#'
#' The jitter magnitude is defined in meters, but applied to lon/lat after
#' approximate conversion to degrees at the mean latitude of the segment. By
#' default, the jitter scale is computed as a small fraction of the segment's
#' median step length, subject to lower and upper bounds.
#'
#' This function is intended only as a fallback to break exact or near-exact
#' collinearity and should not be used to modify tracks in a way that changes
#' their ecological interpretation.
#'
#' @param xy A numeric matrix or data frame with exactly two columns
#'   representing longitude and latitude, in that order, in decimal degrees.
#' @param jitter_fraction Numeric scalar. Fraction of the median step length
#'   used to define the jitter magnitude. Default is `0.01`, corresponding to
#'   1\% of the median step length.
#' @param min_jitter_m Numeric scalar. Minimum jitter magnitude in meters.
#'   Default is `0.5`.
#' @param max_jitter_m Numeric scalar. Maximum jitter magnitude in meters.
#'   Default is `5`.
#' @param interior_only Logical. If `TRUE` (default), jitter is applied only to
#'   interior points, leaving the first and last coordinates unchanged. This is
#'   useful when start and end positions should be preserved.
#' @param seed Optional integer. If provided, sets the random seed before
#'   generating the jitter.
#'
#' @details
#' The conversion from meters to degrees is approximate:
#' \deqn{1^\circ \mathrm{latitude} \approx 111320 \mathrm{\ m}}
#' and
#' \deqn{1^\circ \mathrm{longitude} \approx 111320 \cos(\phi) \mathrm{\ m}}
#' where \eqn{\phi} is the mean latitude of the segment in radians.
#'
#' The function computes step lengths from successive coordinates using a local
#' planar approximation based on the mean latitude of the segment. The jitter
#' magnitude is then defined as:
#'
#' \deqn{
#' \mathrm{jitter\_m} =
#' \min(\mathrm{max\_jitter\_m},
#' \max(\mathrm{min\_jitter\_m},
#' \mathrm{jitter\_fraction} \times \mathrm{median\_step\_length}))
#' }
#'
#' If the median step length cannot be computed or is zero, the minimum jitter
#' value is used.
#'
#' The value of the final jitter magnitude in meters is stored as an attribute
#' named `"jitter_m"` in the returned object.
#'
#' @return
#' A numeric matrix with the same dimensions as `xy`, containing the jittered
#' longitude and latitude coordinates. The returned matrix includes an
#' attribute `"jitter_m"` giving the jitter magnitude used, in meters.
#'
#' @examples
#' xy <- matrix(
#'   c(2.8300, 41.6500,
#'     2.8305, 41.6501,
#'     2.8310, 41.6501,
#'     2.8315, 41.6502),
#'   ncol = 2,
#'   byrow = TRUE
#' )
#'
#' xy_j <- add_small_jitter(xy, seed = 42)
#' xy_j
#' attr(xy_j, "jitter_m")
#'
#' @keywords internal
#' @noRd
add_small_jitter <- function(xy,
                             jitter_fraction = 0.01,
                             min_jitter_m = 0.5,
                             max_jitter_m = 5,
                             interior_only = TRUE,
                             seed = NULL) {

  xy <- as.matrix(xy)

  if (!is.numeric(xy)) {
    stop("`xy` must be numeric.", call. = FALSE)
  }

  if (ncol(xy) != 2L) {
    stop("`xy` must have exactly two columns: lon and lat.", call. = FALSE)
  }

  if (nrow(xy) < 2L) {
    stop("`xy` must contain at least two rows.", call. = FALSE)
  }

  if (!is.numeric(jitter_fraction) || length(jitter_fraction) != 1L ||
      !is.finite(jitter_fraction) || jitter_fraction < 0) {
    stop("`jitter_fraction` must be a single non-negative numeric value.",
         call. = FALSE)
  }

  if (!is.numeric(min_jitter_m) || length(min_jitter_m) != 1L ||
      !is.finite(min_jitter_m) || min_jitter_m < 0) {
    stop("`min_jitter_m` must be a single non-negative numeric value.",
         call. = FALSE)
  }

  if (!is.numeric(max_jitter_m) || length(max_jitter_m) != 1L ||
      !is.finite(max_jitter_m) || max_jitter_m < min_jitter_m) {
    stop("`max_jitter_m` must be a single numeric value >= `min_jitter_m`.",
         call. = FALSE)
  }

  if (!is.logical(interior_only) || length(interior_only) != 1L ||
      is.na(interior_only)) {
    stop("`interior_only` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      stop("`seed` must be NULL or a single finite numeric value.",
           call. = FALSE)
    }
    set.seed(as.integer(seed))
  }

  lon <- xy[, 1]
  lat <- xy[, 2]

  if (any(!is.finite(lon)) || any(!is.finite(lat))) {
    stop("`xy` contains non-finite values.", call. = FALSE)
  }

  lat0 <- mean(lat, na.rm = TRUE)

  # approximate conversion factors
  m_per_deg_lat <- 111320
  m_per_deg_lon <- 111320 * cos(lat0 * pi / 180)

  if (!is.finite(m_per_deg_lon) || m_per_deg_lon <= 0) {
    stop("Could not compute longitude conversion factor from latitude.",
         call. = FALSE)
  }

  # approximate step lengths in meters using local planar conversion
  dlon_m <- diff(lon) * m_per_deg_lon
  dlat_m <- diff(lat) * m_per_deg_lat
  step_m <- sqrt(dlon_m^2 + dlat_m^2)

  ref_step <- stats::median(step_m, na.rm = TRUE)
  if (!is.finite(ref_step) || ref_step <= 0) {
    ref_step <- min_jitter_m
  }

  jitter_m <- jitter_fraction * ref_step
  jitter_m <- max(min_jitter_m, min(max_jitter_m, jitter_m))

  deg_per_m_lat <- 1 / m_per_deg_lat
  deg_per_m_lon <- 1 / m_per_deg_lon

  idx <- seq_len(nrow(xy))
  if (interior_only && nrow(xy) > 2L) {
    idx <- 2:(nrow(xy) - 1L)
  }

  xy_j <- xy
  xy_j[idx, 1] <- xy_j[idx, 1] +
    stats::rnorm(length(idx), mean = 0, sd = jitter_m * deg_per_m_lon)
  xy_j[idx, 2] <- xy_j[idx, 2] +
    stats::rnorm(length(idx), mean = 0, sd = jitter_m * deg_per_m_lat)

  attr(xy_j, "jitter_m") <- jitter_m

  xy_j
}
