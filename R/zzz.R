utils::globalVariables(c(
  # columns used in j/expressions
  "id","lon","lat","time","time_start","time_end","fixID", "fixN",
  "animalID","animal_lon","animal_lat","animal_time",
  "vesselID","vessel_lon","vessel_lat","vessel_time",
  "dt_min","dist_m","matchID","seg","nloc","start","end",
  "duration_min","dist_min","eventID",
  # data.table pronouns
  ":=", ".N",".SD", "simID",
  # foreach
  "s"
))
