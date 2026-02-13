#' Plot observed animal–vessel tracks with optional simulations
#'
#' Plots track segments in lon/lat (EPSG:4326) with optional landmask (oceanmask)
#' and optional p-value annotation (single or two values: attract/follow).
#'
#' Supported types:
#' - "obs_vessel": observed animal + observed vessel
#' - "obs_vessel_sim": observed animal + observed vessel + simulated animal tracks
#'
#' @param animal track_tbl. Observed animal track segment (must include id, time, lon, lat).
#' @param vessel track_tbl. Observed vessel track segment (must include id, time, lon, lat).
#' @param sim_animal track_tbl or NULL. Simulated animal tracks (multiple ids/groups), required for type="obs_vessel_sim".
#' @param oceanmask RasterLayer or NULL. Optional raster mask with values 0=ocean, 1=land.
#' @param p_values NULL, numeric(1), or numeric(2). Optional p-value(s). If length 2, ideally named
#'   c(attract=..., follow=...) (or any names you prefer).
#' @param type character. "obs_vessel" or "obs_vessel_sim".
#' @param animate logical. If TRUE, returns a gganimate object (requires gganimate).
#'
#' @return A ggplot object (static) or a gganimate object (if animate=TRUE).
#'
plot_tracks <- function(animal,
                        vessel,
                        sim_animal = NULL,
                        oceanmask = NULL,
                        p_values = NULL,
                        type = c("obs_vessel", "obs_vessel_sim"),
                        animate = FALSE) {

  type <- match.arg(type)

  # ---- validate classes ----
  if (!inherits(animal, "track_tbl")) stop("`animal` must be a track_tbl.", call. = FALSE)
  if (!inherits(vessel, "track_tbl")) stop("`vessel` must be a track_tbl.", call. = FALSE)
  if (!is.null(sim_animal) && !inherits(sim_animal, "track_tbl")) {
    stop("`sim_animal` must be a track_tbl or NULL.", call. = FALSE)
  }

  if (type == "obs_vessel_sim" && is.null(sim_animal)) {
    stop("type = 'obs_vessel_sim' requires `sim_animal`.", call. = FALSE)
  }
  if (type == "obs_vessel" && !is.null(sim_animal)) {
    stop("type = 'obs_vessel' does not accept `sim_animal`.", call. = FALSE)
  }

  # ---- coerce for plotting ----
  animal_df <- as.data.frame(animal)
  vessel_df <- as.data.frame(vessel)
  sim_df    <- if (!is.null(sim_animal)) as.data.frame(sim_animal) else NULL

  req <- c("id", "time", "lon", "lat")
  if (!all(req %in% names(animal_df))) stop("`animal` must include columns: id, time, lon, lat.", call. = FALSE)
  if (!all(req %in% names(vessel_df))) stop("`vessel` must include columns: id, time, lon, lat.", call. = FALSE)
  if (!is.null(sim_df) && !all(req %in% names(sim_df))) stop("`sim_animal` must include columns: id, time, lon, lat.", call. = FALSE)

  # Ensure deterministic ordering
  animal_df <- animal_df[order(animal_df$time), , drop = FALSE]
  vessel_df <- vessel_df[order(vessel_df$time), , drop = FALSE]
  if (!is.null(sim_df)) sim_df <- sim_df[order(sim_df$id, sim_df$time), , drop = FALSE]

  # ---- bounds ----
  b <- .map_bounds(animal_df, vessel_df, sim_df, pad = 0.10)

  # ---- landmask (optional) ----
  land_df <- NULL
  if (!is.null(oceanmask)) {
    land_df <- raster::rasterToPoints(oceanmask) |> as.data.frame()
    names(land_df) <- c("lon", "lat", "mask")
  }

  # ---- last points ----
  animal_last <- animal_df[nrow(animal_df), , drop = FALSE]
  vessel_last <- vessel_df[nrow(vessel_df), , drop = FALSE]

  sim_last <- NULL
  if (!is.null(sim_df)) {
    sim_last <- dplyr::as_tibble(sim_df) |>
      dplyr::group_by(id) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  }

  # ---- p-value label (optional) ----
  p_lab <- .format_pvalues(p_values, digits = 3)

  # ---- build plot ----
  p <- ggplot2::ggplot()

  # landmask layer
  if (!is.null(land_df)) {
    p <- p +
      ggplot2::geom_raster(
        data = land_df,
        ggplot2::aes(x = lon, y = lat, fill = factor(mask)),
        alpha = 0.6
      ) +
      ggplot2::scale_fill_manual(values = c("0" = "white", "1" = "grey10"), guide = "none")
  }

  # simulations (only for obs_vessel_sim)
  if (type == "obs_vessel_sim") {
    p <- p +
      ggplot2::geom_path(
        data = sim_df,
        ggplot2::aes(x = lon, y = lat, group = id, colour = "Simulated animal track"),
        linewidth = 1,
        alpha = 0.30
      ) +
      ggplot2::geom_point(
        data = sim_last,
        ggplot2::aes(x = lon, y = lat, colour = "Simulated animal track"),
        size = 2,
        alpha = 0.30
      )
  }

  # observed animal
  p <- p +
    ggplot2::geom_path(
      data = animal_df,
      ggplot2::aes(x = lon, y = lat, colour = "Animal track"),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = animal_last,
      ggplot2::aes(x = lon, y = lat, colour = "Animal track"),
      size = 2
    )

  # observed vessel
  p <- p +
    ggplot2::geom_path(
      data = vessel_df,
      ggplot2::aes(x = lon, y = lat, colour = "Vessel track"),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = vessel_last,
      ggplot2::aes(x = lon, y = lat, colour = "Vessel track"),
      size = 2
    )

  # colours (kept close to your original scheme)
  p <- p +
    ggplot2::scale_colour_manual(
      values = c(
        "Vessel track" = "#EFC000FF",
        "Animal track" = "#0072B2",
        "Simulated animal track" = "grey70"
      ),
      breaks = c("Vessel track", "Animal track", "Simulated animal track")
    )

  # p-value annotation
  if (!is.null(p_lab)) {
    p <- p +
      ggplot2::annotate(
        "text",
        x = Inf, y = Inf,
        label = p_lab,
        hjust = 1.1, vjust = 1.5,
        size = 6
      )
  }

  # extent + scalebar + theme
  p <- p +
    ggplot2::coord_sf(crs = 4326, xlim = b$xlim, ylim = b$ylim, expand = TRUE) +
    ggspatial::annotation_scale(location = "br", style = "ticks") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),   # <-- FIX
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # ---- optional animation ----
  if (isTRUE(animate)) {
    if (!requireNamespace("gganimate", quietly = TRUE)) {
      stop("Package 'gganimate' is required for animate=TRUE.", call. = FALSE)
    }
    if (!requireNamespace("gifski", quietly = TRUE)) {
      stop("Package 'gifski' is required to render animations.", call. = FALSE)
    }

    p <- .repair_ggplot_labels(p)

    p <- p + gganimate::transition_reveal(along = time)

    return(
      gganimate::animate(
        p,
        renderer = gganimate::gifski_renderer()
      )
    )
  }

  p
}
