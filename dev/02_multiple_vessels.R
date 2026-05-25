
# Example to analyse multiple birds and vessels
# The code is meant to match multiple bird against multiple vessels.


# load internal functions for dev
devtools::load_all()
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(terra)
library(parallel)
library(doParallel)
library(foreach)



## 1. Import data --------------------------------------------------------------

# import track data
bird <- readr::read_csv("dev/data/ampliamar/calbor.csv", show_col_types = FALSE)  # change input file
ship <- readr::read_csv("dev/data/ampliamar/ships.csv", show_col_types = FALSE)

# filter ship data to the same time period as bird data
ship <- ship %>%
  filter(time >= min(bird$Date_Time) & time <= max(bird$Date_Time))


# convert to track_tbl class
bird_trk <- as_track(bird,
                     lon = longitude,
                     lat = latitude,
                     time = Date_Time,
                     id = organismID)
ship_trk <- as_track(ship,
                     lon = longitude,
                     lat = latitude,
                     time = time,
                     id = cfr)


## plot
ggplot() +
  geom_path(data = ship_trk, aes(x = lon, y = lat, group = id),
            color = "#E69F00", linewidth = 0.6) +
  geom_path(data = bird_trk, aes(x = lon, y = lat, group = id),
            color = "#0072B2", linewidth = 0.6) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()



## 3. Create ocean mask--------------------------------------------------------

# import land polygon
land_sf <- ne_download(
  scale = 10,          # 10, 50, or 110
  type = "land",
  category = "physical",
  returnclass = "sf"
)

# create ocean mask
# note about spatial resolution: very high resolution may lead to long
# computation times when simulating tracks.
oceanmask <- create_oceanmask(
  bbox = c(min(bird_trk$lon)-1,  max(bird_trk$lon)+1, min(bird_trk$lat)-1,  max(bird_trk$lat)+1),
  res = 0.01,
  polygon = land_sf,
  polygon_type = "land"
)

# plot mask
plot(oceanmask)



## 4. Loop per bird  -----------------------------------------------------------

# Define bird id list
bird_ids <- unique(bird_trk$id)

# Create empty list of encounters and associations
encounters_list <- list()
associations_list <- list()


# Loop
for(i in 1:length(bird_ids)){

  print(paste("Processing bird", bird_ids[i], "(", i, "of", length(bird_ids), ")"))

  bird_trk_i <- bird_trk %>% filter(id == bird_ids[i])

  ## add fix id
  bird_trk_i <- add_fix_id(bird_trk_i)

  # match animal and vessel (filter like)
  # pair data and interpolate vessel to animal time points.
  matched <- match_animal_vessels(
    animal = bird_trk_i,
    vessel = ship_trk,
    dist_thr_m = 30000,
    time_thr_min = 5  # animal and vessel were previously interpolated to 5 min.
  )

  # detect encounter events
  # make eventID go first(!)
  encounter <- detect_proximity_events(
    pairs = matched,
    min_dist_m = 30000,
    min_duration_min = 30,
    max_gap_min = 60
  )

  encounter_data <- encounter$data
  encounter_events <- encounter$events

  # append encounter events to list
  encounters_list[[i]] <- encounter_events


  # detect association events
  association <- detect_proximity_events(
    pairs = matched,
    min_dist_m = 1500,
    min_duration_min = 15,
    max_gap_min = 30
  )

  association_data <- association$data
  association_events <- association$events


  ### If an association event is detected, then test for attraction and following
  if(nrow(association_events) > 0){

    # add columns for p-values
    association_events$attract_p_value <- NA_real_
    association_events$follow_p_value  <- NA_real_
    association_events$attract_error   <- NA_character_
    association_events$follow_error    <- NA_character_

    # loop for each association event
    for(j in 1:nrow(association_events)){

      # ------ attract -----------------------------------

      res_attract <- tryCatch({
        # extract segments to test attraction
        event_seg <- extract_event_segments(
          x = association,
          eventID = association$events$eventID[j],
          lead_secs = 1800L,
          lag_secs  = 0L
        )

        # test attraction
        test_interactions(
          animal = event_seg$animal,
          vessel = event_seg$vessel,
          obs_duration_min = NULL,
          min_dist_m = 1500,
          min_duration_min = 15,
          max_gap_min = 30,
          method = "attract",
          sim_n = 100L,
          oceanmask = oceanmask,
          anchor = "start",
          min_locs = 6L,
          cores = 10L,
          seed = 42,
          return_simdata = FALSE
        )
      }, error = function(e) e)

      # store values in association events table
      if (inherits(res_attract, "error")) {
        association_events$attract_p_value[j] <- NA_real_
        association_events$attract_error[j]   <- conditionMessage(res_attract)
      } else {
        association_events$attract_p_value[j] <- res_attract$result$p_value
      }



      # ------ follow -----------------------------------

      res_follow <- tryCatch({
        # extract segments to test follow
        follow_seg <- extract_event_segments(
          x = association,
          eventID = association$events$eventID[j],
          lead_secs = 0L,
          lag_secs  = 0L
        )

        # test follow
        test_interactions(
          animal = follow_seg$animal,
          vessel = follow_seg$vessel,
          obs_duration_min = follow_seg$meta$duration_min,
          min_dist_m = 1500,
          min_duration_min = 15,
          max_gap_min = 30,
          method = "follow",
          sim_n = 100L,
          oceanmask = oceanmask,
          anchor = "start",
          min_locs = 6L,
          cores = 10L,
          seed = 42,
          return_simdata = TRUE
        )
      }, error = function(e) e)

      # store values in association events table
      if (inherits(res_follow, "error")) {
        association_events$follow_p_value[j] <- NA_real_
        association_events$follow_error[j]   <- conditionMessage(res_follow)
      } else {
        association_events$follow_p_value[j] <- res_follow$result$p_value
      }

  }
    # append association events to list
    associations_list[[i]] <- association_events
  }
}



## combine all encounters and associations
encounters <- do.call(rbind, encounters_list)
associations <- do.call(rbind, associations_list)
