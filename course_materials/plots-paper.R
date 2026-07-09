

# plots for article
# one simplified version for figure 4
# one complete version for animation
# animations without simulations

# Load packages
library(intersimR)
library(terra)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(gganimate)
library(tidyterra)
library(gifski)
library(readr)
library(geodata)



# oceanmask
land_sf <- rnaturalearth::ne_download(
  scale = 10,
  type = "land",
  category = "physical",
  returnclass = "sf"
)

oceanmask <- create_oceanmask(
  bbox = c(-6, 16, 34.5, 45),
  res = 0.005,
  polygon = land_sf,
  polygon_type = "land"
)

plot(oceanmask)


# import world map
#world <- ne_countries(scale = "large", continent = c("africa", "europe"), returnclass = "sf")
world <- world(resolution = 1, path=tempdir())



# Load example data
# a: 339
# b: 444
# c: 608
# d:1500

bird <- readr::read_csv("data/273_bird.csv", show_col_types = FALSE)
ship <- readr::read_csv("data/273_ship.csv", show_col_types = FALSE)


# convert to track objects
bird_trk <- as_track(
  bird,
  lon = longitude,
  lat = latitude,
  time = time,
  id = organismID
)

ship_trk <- as_track(
  ship,
  lon = longitude,
  lat = latitude,
  time = time,
  id = shipID
)

# add idx
bird_trk <- add_fix_id(bird_trk)

# pair
matched <- match_animal_vessels(
  animal = bird_trk,
  vessel = ship_trk,
  dist_thr_m = 30000,
  time_thr_min = 5
)


# detect association
association <- detect_proximity_events(
  pairs = matched,
  min_dist_m = 1500,
  min_duration_min = 15,
  max_gap_min = 30
)

association_events <- association$events
event_id <- association_events$eventID[1]

# extract segment for attraction
attraction_seg <- extract_event_segments(
  x = association,
  eventID = event_id,
  lead_secs = 1800,
  lag_secs = 0
)


# attraction test
test_attraction <- test_interactions(
  animal = attraction_seg$animal,
  vessel = attraction_seg$vessel,
  obs_duration_min = NULL,
  min_dist_m = 1500,
  min_duration_min = 15,
  max_gap_min = 30,
  method = "attract",
  sim_n = 100,
  oceanmask = oceanmask,
  anchor = "start",
  min_locs = 6,
  cores = 2,
  seed = 123,
  return_simdata = TRUE
)


test_attraction$result

# get simulations
sim_animal_attract <- as_track(
  test_attraction$sim_pairs,
  lon = animal_lon,
  lat = animal_lat,
  time = animal_time,
  id = matchID
)



### define lon/lat bounds
# range from data
xl <- range(c(attraction_seg$animal$lon, attraction_seg$vessel$lon, sim_animal_attract$lon))
yl <- range(c(attraction_seg$animal$lat, attraction_seg$vessel$lat, sim_animal_attract$lat))
# get centroid
zoom_to <- c(mean(xl), mean(yl))  # center of the range
# define zoom level
lon_span <- xl[2]-xl[1]
lat_span <- yl[2]-yl[1]
zoom_lon <- floor(log2(360/lon_span))
zoom_lat <- floor(log2(180/lat_span))
zoom_level <- min(zoom_lon, zoom_lat)
# define span
lon_span <- 360 / 2^zoom_level
lat_span <- 180 / 2^zoom_level
# define boundaries
lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)



## Get last positions
vessel_last <- attraction_seg$vessel %>%
  arrange(time) %>%
  slice_tail(n = 1)

bird_last <- attraction_seg$animal %>%
  arrange(time) %>%
  slice_tail(n = 1)

sim_last <- sim_animal_attract %>%
  arrange(id, time) %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup()




# plot
p <- ggplot() +

  # plot simulated tracks
  geom_path(data=sim_animal_attract, aes(x=lon, y=lat, group=id, colour="Simulations"), linewidth=0.7, alpha = 0.3) +
  geom_point(data=sim_last, aes(x=lon, y=lat, group=id, color="Simulations"), size=1, alpha = 0.3) +
  # ship track
  geom_path(data = attraction_seg$vessel, aes(x = lon, y = lat, group = id, colour = "Vessel"), linewidth = 1.5) +
  geom_point(data=vessel_last, aes(x=lon, y=lat, color="Vessel"), size=2) +
  # plot animal track
  geom_path(data = attraction_seg$animal, aes(x = lon, y = lat, group = id, colour = "Animal"), linewidth = 1.5) +
  geom_point(data=bird_last, aes(x=lon, y=lat, color="Animal"), size=2) +
  # add land
  geom_sf(fill="grey80", colour="grey40", size=0.2, data=world) +
  # set colours
  scale_colour_manual(values = c("Animal" = "#0072B2", "Vessel" = "#EFC000FF", "Simulations" = "grey50"), name = NULL) +
  # aesthetics
  coord_sf(xlim = lon_bounds, ylim = lat_bounds,expand = FALSE) +
  # theme
  theme_bw(base_size = 14) +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

