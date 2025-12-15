# Internal constructor -----------------------------------------------------

new_track_tbl <- function(x, tz = "UTC", crs = "OGC:CRS84") {
  # x must already contain id, time, lon, lat with correct types/sorting
  x <- tibble::as_tibble(x)
  structure(
    x,
    class = c("track_tbl", "tbl_df", "tbl", "data.frame"),
    tz = tz,
    crs = crs
  )
}

# Internal validators ------------------------------------------------------

.validate_track_tbl_cols <- function(x) {
  req <- c("id", "time", "lon", "lat")
  missing <- setdiff(req, names(x))
  if (length(missing)) {
    stop("`track_tbl` is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!inherits(x$time, "POSIXct")) stop("`time` must be POSIXct (UTC).", call. = FALSE)
  if (!is.numeric(x$lon) || !is.numeric(x$lat)) stop("`lon` and `lat` must be numeric.", call. = FALSE)
}

.validate_sorted_unique <- function(x) {
  # (id, time) must be unique and sorted
  if (any(duplicated(x[c("id", "time")]))) {
    stop("Duplicated (id, time) pairs detected; please deduplicate before converting.", call. = FALSE)
  }
  # sorted check (cheap)
  ord <- order(x$id, x$time)
  if (any(ord != seq_len(nrow(x)))) {
    warning("Rows are not sorted by (id, time); they will be sorted in `as_track()`.", call. = FALSE)
  }
}

# Public helpers -----------------------------------------------------------

#' Test if an object is a track table
#' @param x Any R object.
#' @return Logical scalar.
#' @export
is_track_tbl <- function(x) inherits(x, "track_tbl")

#' Print method for track tables
#' @param x A `track_tbl`.
#' @param ... Ignored.
#' @export
print.track_tbl <- function(x, ...) {
  tz  <- attr(x, "tz")
  crs <- attr(x, "crs")
  n_ids <- length(unique(x$id))
  rng <- range(x$time)
  cat(sprintf("<track_tbl> %d fixes across %d id(s)\n", nrow(x), n_ids))
  cat(sprintf("  time range: %s  to  %s  (%s)\n", format(rng[1]), format(rng[2]), tz %||% "UTC"))
  cat(sprintf("  CRS: %s\n\n", crs %||% "OGC:CRS84"))
  NextMethod()
}

# tiny internal `%||%` (avoid importing rlang just for this use here)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
