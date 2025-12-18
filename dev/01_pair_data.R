
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



# extract segments to test attraction
event_seg <- extract_event_segments(x = association,
                                         eventID = association$events$eventID[1],
                                         lead_secs = 1800L,
                                         lag_secs  = 0L)





# create ocean mask
library(rnaturalearth)
library(sf)
library(terra)

# import land polygon
land_sf <- ne_download(
  scale = 10,          # 10, 50, or 110
  type = "land",
  category = "physical",
  returnclass = "sf"
)

# create ocean mask
oceanmask <- create_oceanmask(
  bbox = c(-6, 16, 34.5, 45),
  res = 0.01,
  polygon = land_sf,
  polygon_type = "land"
)

# plot mask
plot(oceanmask)


# simulate tracks
library(parallel)
library(doParallel)
library(foreach)

sim_bird <- simulate_tracks(
  animal = event_seg$animal,
  sim_n = 100L,
  oceanmask = oceanmask,
  min_locs = 6L,
  anchor = "start",
  cores = 10L,
  seed = 42)

# plot simulations
p <- ggplot() +
  geom_path(data=sim_bird, aes(x=lon, y=lat, group=simID), color="grey", linewidth=1, alpha = 0.3)


# test attraction
test_attraction <- test_interactions(
                    animal = event_seg$animal,
                    vessel = event_seg$vessel,
                    obs_duration_min = NULL,
                    min_dist_m = 1500, # rename using same naming as detect_proximity_events(!)
                    min_duration_min = 15,
                    max_gap_min = 30,
                    method = c("attract"),
                    sim_n = 10L,
                    oceanmask = oceanmask,
                    anchor = "start",
                    min_locs = 6L,
                    cores = 10L,
                    seed = 42,
                    return_simdata = TRUE)




## Test follow

# extract segments to test attraction
follow_seg <- extract_event_segments(x = association,
                                    eventID = association$events$eventID[1],
                                    lead_secs = 0L,
                                    lag_secs  = 0L)

# test attraction
test_follow <- test_interactions(
                      animal = follow_seg$animal,
                      vessel = follow_seg$vessel,
                      obs_duration_min = follow_seg$meta$duration_min,
                      min_dist_m = 1500, # rename using same naming as detect_proximity_events(!)
                      min_duration_min = 15,
                      max_gap_min = 30,
                      method = c("follow"),
                      sim_n = 100L,
                      oceanmask = oceanmask,
                      anchor = "start",
                      min_locs = 6L,
                      cores = 10L,
                      seed = 42,
                      return_simdata = TRUE)


# plot simulations
p <- ggplot() +
  geom_path(data=test_follow$sim_pairs, aes(x=animal_lon, y=animal_lat, group=animalID), color="grey", linewidth=1, alpha = 0.3)


