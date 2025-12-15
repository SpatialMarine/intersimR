
# load internal functions for dev
devtools::load_all()

# import track data
bird <- readr::read_csv("dev/data/608_bird.csv", show_col_types = FALSE)
ship <- readr::read_csv("dev/data/608_ship.csv", show_col_types = FALSE)

# convert to track_tbl class

bird$behaviour <- "resting"

bird_trk <- as_track(bird,
                     lon = longitude,
                     lat = latitude,
                     time = time,
                     id = organismID)
ship_trk <- as_track(ship,
                     lon = longitude,
                     lat = latitude,
                     time = time,
                     id = shipID)

## DEV only: check coertion worked
stopifnot(inherits(bird_trk, "track_tbl"))
stopifnot(inherits(ship_trk, "track_tbl"))
stopifnot(inherits(bird_trk$time, "POSIXct"))
stopifnot(all(c("lon", "lat", "time", "id") %in% names(bird_trk)))


## plot
library(ggplot2)
ggplot() +
  geom_path(data = ship_trk, aes(x = lon, y = lat, group = id),
            color = "#E69F00", linewidth = 0.6) +
  geom_path(data = bird_trk, aes(x = lon, y = lat, group = id),
            color = "#0072B2", linewidth = 0.6, linetype = "dashed") +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()


## add fix id
bird_trk <- add_fix_id(bird_trk)

## match animal and vessel
matched <- match_animal_vessels(
  animal = bird_trk,
  vessel = ship_trk,
  dist_thr_m = 30000,
  time_thr_min = 5
)

# detect encounter events
encounter <- detect_proximity_events(
  pairs = matched,
  min_dist_m = 30000,
  min_duration_min = 30,
  max_gap_min = 60
)

encounter_data <- encounter$data
encounter_events <- encounter$events


# detect association events
association <- detect_proximity_events(
  pairs = matched,
  min_dist_m = 1500,
  min_duration_min = 15,
  max_gap_min = 30
)

association_data <- association$data
association_events <- association$events


