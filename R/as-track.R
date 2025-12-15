#' Coerce inputs to a canonical movement track table
#'
#' `as_track()` converts a data.frame/tibble to a validated `track_tbl` with
#' columns `id`, `time` (POSIXct, UTC), `lon`, and `lat` (decimal degrees),
#' sorted by `(id, time)`. This canonical structure is what all intersimR
#' functions expect internally.
#'
#' @name as_track
#' @param x Input object.
#' @param ... Reserved for future method-specific arguments; unused here.
#' @return A `track_tbl`.
#' @examples
#' df <- data.frame(
#'   id   = c("a","a","a"),
#'   time = as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + c(0, 60, 120),
#'   lon  = c(0.00, 0.01, 0.02),
#'   lat  = c(0.00, 0.01, 0.02)
#' )
#' trk <- as_track(df, lon = lon, lat = lat, time = time, id = id)
#' is_track_tbl(trk)
#' @export
as_track <- function(x, ...) {
  UseMethod("as_track")
}

#' @rdname as_track
#' @param lon,lat,time,id Columns (tidy-eval) identifying longitude, latitude,
#'   timestamp, and track identifier in `x`.
#' @param tz Character timezone for timestamps (default `"UTC"`). Passed to
#'   `as.POSIXct()` during coercion.
#' @param crs Coordinate reference string stored as an attribute (default
#'   `"OGC:CRS84"`). Informational; positions are assumed to be lon/lat (EPSG:4326).
#' @export
as_track.data.frame <- function(x, lon, lat, time, id, tz = "UTC", crs = "OGC:CRS84", ...) {
  lon  <- rlang::ensym(lon)
  lat  <- rlang::ensym(lat)
  time <- rlang::ensym(time)
  id   <- rlang::ensym(id)

  out <- tibble::tibble(
    id   = x[[rlang::as_name(id)]],
    time = as.POSIXct(x[[rlang::as_name(time)]], tz = tz),
    lon  = as.numeric(x[[rlang::as_name(lon)]]),
    lat  = as.numeric(x[[rlang::as_name(lat)]])
  )

  # sort and validate
  ord <- order(out$id, out$time)
  out <- out[ord, , drop = FALSE]

  .validate_track_tbl_cols(out)
  .validate_sorted_unique(out)

  new_track_tbl(out, tz = tz, crs = crs)
}
