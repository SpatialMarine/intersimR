#' Add a per-track sequential fix index and a globally-unique fix identifier
#'
#' @param x A \code{track_tbl} with columns \code{id}, \code{lon}, \code{lat}, \code{time}.
#' @param overwrite Logical; if \code{FALSE} (default) and \code{fixID} already exists,
#'   an error is thrown to avoid accidental reassignment. Set \code{TRUE} to recompute.
#'
#' @return The same \code{track_tbl} with:
#'   - \code{fixN}: integer 1,2,3,... within each \code{id} ordered by time (ties by lon/lat)
#'   - \code{fixID}: character unique identifier \code{paste(id, fixN, sep = "_")}
#' @export
add_fix_id <- function(x, overwrite = FALSE) {
  stopifnot(inherits(x, "track_tbl"))
  req <- c("id", "lon", "lat", "time")
  if (!all(req %in% names(x))) {
    stop("track_tbl must contain columns: id, lon, lat, time.", call. = FALSE)
  }
  if (("fixID" %in% names(x) || "fixN" %in% names(x)) && !isTRUE(overwrite)) {
    stop("Column 'fixID' and/or 'fixN' already exists. Use overwrite = TRUE to recompute.", call. = FALSE)
  }

  orig_class <- class(x)
  orig_attr  <- attributes(x)

  dt <- data.table::as.data.table(x)

  # Deterministic ordering within id: time, lon, lat
  data.table::setorder(dt, id, time, lon, lat)

  # Per-track sequential index
  dt[, fixN := as.integer(seq_len(.N)), by = id]

  # Globally-unique fix identifier (unique across simulations because id differs)
  dt[, fixID := paste(id, fixN, sep = "_")]

  out <- as.data.frame(dt)
  class(out) <- orig_class

  keep_attrs <- setdiff(names(orig_attr), c("names","row.names","dim","dimnames","class"))
  for (nm in keep_attrs) attr(out, nm) <- orig_attr[[nm]]

  out
}
