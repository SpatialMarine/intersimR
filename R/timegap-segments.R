#' Segment a sorted time series by a maximum gap (in minutes)
#' @param time POSIXct vector, already sorted
#' @param max_gap_min numeric, gap threshold in minutes
#' @return integer segment index starting at 1
#' @export
timegap_segments <- function(time, max_gap_min) {
  if (length(time) == 0L) return(integer())
  d <- as.numeric(difftime(time, data.table::shift(time), units = "mins"))
  d[is.na(d)] <- 0
  as.integer(1L + cumsum(d > max_gap_min))
}
