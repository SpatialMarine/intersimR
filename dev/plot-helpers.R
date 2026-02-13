.map_bounds <- function(..., pad = 0.10) {
  dfs <- list(...)
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]

  all_lon <- unlist(lapply(dfs, function(d) d$lon), use.names = FALSE)
  all_lat <- unlist(lapply(dfs, function(d) d$lat), use.names = FALSE)

  xl <- range(all_lon, na.rm = TRUE)
  yl <- range(all_lat, na.rm = TRUE)

  dx <- diff(xl); dy <- diff(yl)
  if (!is.finite(dx) || dx == 0) dx <- 0.01
  if (!is.finite(dy) || dy == 0) dy <- 0.01

  xlim <- xl + c(-1, 1) * dx * pad
  ylim <- yl + c(-1, 1) * dy * pad

  list(xlim = xlim, ylim = ylim)
}


.format_pvalue <- function(p, digits = 3, prefix = "p") {
  if (is.null(p) || is.na(p)) return(paste0(prefix, " = NA"))
  if (!is.numeric(p)) stop("p must be numeric.", call. = FALSE)

  thr <- 10^(-digits)
  if (p < thr) {
    paste0(prefix, " < ", format(thr, scientific = FALSE, trim = TRUE))
  } else {
    paste0(prefix, " = ", format(round(p, digits), nsmall = digits, scientific = FALSE, trim = TRUE))
  }
}

.format_pvalues <- function(p_values, digits = 3) {
  if (is.null(p_values)) return(NULL)

  if (is.numeric(p_values) && length(p_values) == 1L) {
    return(.format_pvalue(p_values, digits = digits, prefix = "p"))
  }

  if (is.numeric(p_values) && length(p_values) == 2L) {
    nm <- names(p_values)
    if (is.null(nm) || any(nm == "")) {
      # fallback labels if not named
      nm <- c("attract", "follow")
    }

    labs <- vapply(seq_along(p_values), function(i) {
      .format_pvalue(p_values[[i]], digits = digits, prefix = nm[[i]])
    }, character(1))

    # Multi-line label
    return(paste(labs, collapse = "\n"))
  }

  stop("`p_values` must be NULL, a single numeric p-value, or a numeric vector of length 2 (ideally named).",
       call. = FALSE)
}



.repair_ggplot_labels <- function(p) {
  if (!inherits(p$labels, "labels")) {
    p$labels <- structure(p$labels, class = "labels")
  }
  p
}


