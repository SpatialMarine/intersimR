#' Add a per-animal sequential fix identifier
#'
#' @param x A \code{track_tbl} with columns \code{id}, \code{lon}, \code{lat}, \code{time}.
#' @param overwrite Logical; if \code{FALSE} (default) and \code{fixID} already exists,
#'   an error is thrown to avoid accidental reassignment. Set \code{TRUE} to recompute.
#'
#' @return The same \code{track_tbl} with an integer column \code{fixID}:
#'   1, 2, 3, ... within each \code{id}, ordered by \code{time} (ties broken by \code{lon}, \code{lat}).
#' @export
add_fix_id <- function(x, overwrite = FALSE) {
  stopifnot(inherits(x, "track_tbl"))
  req <- c("id", "lon", "lat", "time")
  if (!all(req %in% names(x))) {
    stop("track_tbl must contain columns: id, lon, lat, time.", call. = FALSE)
  }
  if ("fixID" %in% names(x) && !isTRUE(overwrite)) {
    stop("Column 'fixID' already exists. Use overwrite = TRUE to recompute.", call. = FALSE)
  }

  # Keep original attributes/classes to restore later
  orig_class <- class(x)
  orig_attr  <- attributes(x)
  # Work in data.table for speed; restore class/attrs on return
  dt <- data.table::as.data.table(x)

  # Deterministic ordering within id: time, lon, lat
  data.table::setorder(dt, id, time, lon, lat)

  # Assign sequential index per id
  dt[, fixID := seq_len(.N), by = id]
  dt[, fixID := as.integer(fixID)]

  # Restore original ordering of rows (by original row index), if present
  # If x had row.names or no stable index, we simply return ordered-by-time,
  # which is typically preferred for downstream pipelines.
  out <- as.data.frame(dt)

  # Restore class and non-dimension attributes (except names/row.names)
  class(out) <- orig_class
  # Preserve custom attributes other than names/row.names/dim/dimnames
  keep_attrs <- setdiff(names(orig_attr), c("names","row.names","dim","dimnames","class"))
  for (nm in keep_attrs) attr(out, nm) <- orig_attr[[nm]]

  out
}
