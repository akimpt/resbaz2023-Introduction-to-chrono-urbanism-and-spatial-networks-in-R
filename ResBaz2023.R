# ResBaz2023 
# Anthony Kimpton

## Setting up: the usual suspects

#install.packages(c("RColorBrewer", "viridis", "tidyverse", "sf", "data.table", "interp", "tidytransit","DiagrammeR", "elevatr", "stars", "gtfstools", "tmap", "reactable", "plotly", "remotes"))
library(RColorBrewer)
library(viridis)
library(tidyverse)
library(sf)
library(data.table)
library(interp)
library(tidytransit)
library(DiagrammeR)
library(elevatr)
library(stars)
library(gtfstools)
library(tmap)
library(reactable)
library(plotly)
library(remotes)

#install_github("bergant/datamodelr")
library(datamodelr)

wd <- getwd()
date <- format(Sys.time(), '%y%m%d')

date <- "231121"

download_dir <- file.path(wd, paste("downloads", date, sep = "_"))

if (!file.exists(download_dir)){
  dir.create(download_dir)
}

tmap_mode("view")
#tmap_mode("plot")
tmap_options(check.and.fix = TRUE)

## Setting up: the problem pair

#If it wont't run use this R version that has been compiled differently https://cran.r-project.org/bin/windows/base/rpatched.html
#This must run before loading rJava https://stackoverflow.com/questions/34624002/r-error-java-lang-outofmemoryerror-java-heap-space
#install.packages(c("rJava", "r5r"))
library(rJava)
library(r5r)

options(java.parameters = "-Xmx8000m")

.jinit()
ver <- .jcall("java.lang.System", "S", "getProperty", "java.version")
ver <- as.numeric(gsub("\\..*", "", ver))
ver

## Areas: Downloads

#local government areas
url_lga_2021 <- "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2021_AUST_GDA2020_SHP.zip"

download.file(url_lga_2021, destfile = paste0(download_dir, "/", basename(url_lga_2021)))

#Suburbs
url_sa2_2021 <- "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/SA2_2021_AUST_SHP_GDA2020.zip"

download.file(url_sa2_2021, destfile = paste0(download_dir, "/", basename(url_sa2_2021)))

#land use areas
url_meshblocks_2021 <- "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/MB_2021_AUST_SHP_GDA2020.zip"

download.file(url_meshblocks_2021, destfile = paste0(download_dir, "/", basename(url_meshblocks_2021)))

for (file in list.files(path = download_dir, pattern="*.zip")){
  print(file)
  unzip(zipfile = paste(download_dir, file, sep = "/"), exdir = download_dir, overwrite = TRUE)
}

for (file in list.files(path = download_dir, pattern="*.shp$")){
  print(file)
  assign(gsub(".shp", "",file), st_read(paste(download_dir, file, sep = "/")))
}

SA2_2021_AUST_GDA2020 <- SA2_2021_AUST_GDA2020 %>% 
  filter(GCC_NAME21 == "Greater Brisbane")

study_area <- SA2_2021_AUST_GDA2020 %>%
  filter(SA2_NAME21 == "Springfield Lakes")

land_use <- MB_2021_AUST_GDA2020 %>%
  filter(GCC_NAME21 == "Greater Brisbane")

point_of_origin_sf <- land_use %>%
  filter(MB_CODE21 == 30562523100) %>%
  st_centroid() %>% 
  st_transform(crs = "WGS84") %>% 
  transmute(id = "UniSQ")

point_of_origin_df <- point_of_origin_sf %>% 
  data.frame(st_coordinates(st_cast(.$geometry,"MULTIPOINT"))) %>%
  st_drop_geometry() %>%
  transmute(id, lon = X, lat = Y)

origins_and_destinations_df <- SA2_2021_AUST_GDA2020 %>% 
  st_centroid()  %>% 
  st_transform(crs = "WGS84") %>% 
  data.frame(st_coordinates(st_cast(.$geometry,"MULTIPOINT"))) %>%
  st_drop_geometry() %>%
  transmute(id = SA2_NAME21, lon = X, lat = Y)

# Areas: Mapped

tmap_mode("view")

tm_shape(land_use, bbox = st_bbox(study_area)) +
  tm_fill(col = "MB_CAT21") +
  tm_shape(study_area) +
  tm_borders(lwd = 2) +
  tm_text("SA2_NAME21") +
  tm_shape(point_of_origin_sf) +
  tm_symbols(shape = 23, col = "#FFB448", border.col = "#3C2D4D") +
  tm_text("id", col = "#3C2D4D")

## Elevation: Download

options(timeout = 8000) #for when it's a biggie or slow

elevation <- get_elev_raster(study_area, z = 12) %>% st_as_stars()

write_stars(elevation, paste0(download_dir, "/elevation.tif"))

#contours <- st_contour(elevation)

options(timeout = 60) #back to 1 min default

## Elevation: Mapped

tm_shape(elevation, bbox = st_bbox(study_area)) +
  tm_raster(palette = viridis(20, direction = 1), title = "Elevation (m)", style="cont") +
  # tm_shape(contours) +
  # tm_borders(lwd = 0.5, col = "grey") +
  tm_shape(study_area) +
  tm_borders(lwd = 2, col = "white") +
  tm_text("SA2_NAME21", col = "white") +
  tm_shape(point_of_origin_sf) +
  tm_symbols(shape = 23, col = "#FFB448", border.col = "#3C2D4D") +
  tm_text("id", col = "white")

## GTFS: Download

# Public Transport Routes, Stops, and Schedules
url_latest_gtfs <- "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip"

download.file(url_latest_gtfs, destfile = paste0(download_dir, "/", basename(url_latest_gtfs)))

latest_validator <- download_validator(tempdir())

gtfs <- read_gtfs(paste0(download_dir, "/SEQ_GTFS.zip"))

path_output_dir <- tempfile("validation_from_path")

validate_gtfs(gtfs, ".", latest_validator)

feed_info <- read.csv(paste(download_dir, "feed_info.txt", sep = "/"))

## GTFS: Datamodel

attach(gtfs)

dm1 <- dm_from_data_frames(agency, calendar, calendar_dates, feed_info, routes, shapes, stops, stop_times, trips)

dm1 <- dm_add_references(
  dm1,
  calendar$service_id == calendar_dates$service_id,
  calendar_dates$service_id == trips$service_id,
  calendar$service_id == trips$service_id,
  trips$route_id == routes$route_id,
  shapes$shape_id == trips$shape_id,
  stop_times$trip_id == trips$trip_id,
  stop_times$stop_id == stops$stop_id
)

table_segments <- list(
  meta = c("agency", "feed_info"),
  stops = c("stops", "stop_times"),
  services = c("trips", "routes", "shapes"),
  calendar = c("calendar", "calendar_dates"))

dm1 <- dm_set_segment(dm1, table_segments)

graph <- dm_create_graph(dm1, rankdir = "TB", col_attr = c("column", "type"))
dm_render_graph(graph)

## OSM: Download

# Roads and Points of Interest
#Australian OSM will throw out an unsolved error that most likely relates to Java' tricky memory management
#url_latest_osm <- "https://download.geofabrik.de/australia-oceania/australia-latest.osm.pbf"

url_latest_osm <- "https://download.bbbike.org/osm/bbbike/Brisbane/Brisbane.osm.pbf"

options(timeout = 8000) #for when it's a biggie or slow

download.file(url_latest_osm, destfile = paste0(download_dir, "/", basename(url_latest_osm)))

options(timeout = 60) #back to 1 min default

## r5r: Build Network

r5r_core <- setup_r5(data_path = download_dir,  verbose = TRUE)

gc()

street_net <- street_network_to_sf(r5r_core)
fast_roads <- subset(street_net$edges, street_class %like% 'MOTORWAY|PRIMARY') #"MOTORWAY|PRIMARY|OTHER|SECONDARY|TERTIARY" 
transit_net <- transit_network_to_sf(r5r_core)
rail_bus_net <- subset(transit_net$routes, mode %like% 'RAIL|BUS') #"BUS|FERRY|TRAM|RAIL"

## r5r: Mapped

tm_shape(fast_roads, bbox = st_bbox(study_area)) +
  tm_lines(lwd = 1, col = "street_class", palette = brewer.pal(3, "Pastel1")) +
  tm_shape(rail_bus_net) +
  tm_lines(lwd = 1, col = "mode", palette = brewer.pal(2, "Dark2")) +
  tm_shape(study_area) +
  tm_borders(lwd = 2) +
  tm_text("SA2_NAME21") +
  tm_shape(point_of_origin_sf) +
  tm_symbols(shape = 23, col = "#FFB448", border.col = "#3C2D4D") +
  tm_text("id", col = "#3C2D4D")

## Travel Time Matricies: from CBD calculated

mode <- c("WALK", "TRANSIT") 

time_of_departure <- as.POSIXct("22-11-2023 08:00:00",
                                 format = "%d-%m-%Y %H:%M:%S", tz = Sys.timezone(location = TRUE))

ttm_transit <- travel_time_matrix(r5r_core,   
                          origins = origins_and_destinations_df,
                          destinations = origins_and_destinations_df,    
                          mode = mode,
                          max_trip_duration = 60,
                          departure_datetime = time_of_departure) %>% 
  arrange(from_id) %>% 
  rename("transit_travel_time_p50" = "travel_time_p50")

mode <- c("CAR") 

ttm_driving <- travel_time_matrix(r5r_core,   
                          origins = origins_and_destinations_df,
                          destinations = origins_and_destinations_df,    
                          mode = mode,
                          max_trip_duration = 30,
                          departure_datetime = time_of_departure) %>% 
  arrange(from_id)  %>% 
  rename("driving_travel_time_p50" = "travel_time_p50")

ttm <- ttm_transit %>% 
  full_join(ttm_driving, by = c("from_id", "to_id")) %>% 
  mutate(id = paste0(from_id, " to ",to_id))

## Travel Time Matricies: from CBD tabled

reactable(ttm, filterable = TRUE, minRows = 10)

## Travel Time Matricies: from CBD plotted

p1 <- ttm %>% 
  filter(from_id == "Brisbane City") %>%
  mutate(triple = transit_travel_time_p50 > driving_travel_time_p50*3) %>% 
  ggplot(., aes(x = transit_travel_time_p50, y = driving_travel_time_p50, label = id, col = triple)) +
  geom_point() +
  ggtitle("From Brisbane City")

  ggplotly(p1, tooltip = "id")

# Isochrones: auto-centric vs. human-scale calculated

time_intervals <- c(10, 15, 20, 30)

mode <- c("WALK") 
walk <- isochrone(r5r_core,
                  cutoffs = time_intervals,
                  origins = point_of_origin_df,
                  mode = mode,
                  sample_size = 1,
                  departure_datetime = time_of_departure,
                  progress = TRUE) %>% 
  mutate(mins = as.character(isochrone),
         mins = factor(mins, levels = c("10", "15", "20", "30")))

gc()

mode <- c("CAR") 
drive <- isochrone(r5r_core,
                  cutoffs = time_intervals,
                  origins = point_of_origin_df,
                  mode = mode,
                  sample_size = 1,
                  departure_datetime = time_of_departure,
                  progress = TRUE) %>% 
  mutate(mins = as.character(isochrone),
         mins = factor(mins, levels = c("10", "15", "20", "30")))

gc()

# Isochrones: auto-centric vs. human-scale mapped

tm_shape(fast_roads, bbox = st_bbox(study_area)) +
  tm_lines(lwd = 1, col = "street_class", palette = brewer.pal(3, "Pastel1")) +
  tm_shape(rail_bus_net) +
  tm_lines(lwd = 1, col = "mode", palette = brewer.pal(3, "Dark2")) +
  tm_shape(study_area) +
  tm_borders(lwd = 2) +
  tm_text("SA2_NAME21") +
  tm_shape(drive) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Greys")) +
  tm_shape(walk) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Greens")) +
  tm_shape(point_of_origin_sf) +
  tm_symbols(shape = 23, col = "#FFB448", border.col = "#3C2D4D") +
  tm_text("id", col = "#3C2D4D")

## Isochrones: Cyclist road traffic tress calculated

mode <- c("BICYCLE") 
bike_lts1 <- isochrone(r5r_core,
                  cutoffs = time_intervals,
                  origins = point_of_origin_df,
                  mode = mode,
                  sample_size = 1,
                  departure_datetime = time_of_departure,
                  max_lts = 1,
                  progress = TRUE) %>% 
  mutate(mins = as.character(isochrone),
         mins = factor(mins, levels = c("10", "15", "20", "30")))

gc()

bike_lts4 <- isochrone(r5r_core,
                  cutoffs = time_intervals,
                  origins = point_of_origin_df,
                  mode = mode,
                  sample_size = 1,
                  departure_datetime = time_of_departure,
                  max_lts = 4,
                  progress = TRUE) %>% 
  mutate(mins = as.character(isochrone),
         mins = factor(mins, levels = c("10", "15", "20", "30")))

gc()

## Isochrones: Cyclist road traffic tress mapped

tm_shape(fast_roads, bbox = st_bbox(study_area)) +
  tm_lines(lwd = 1, col = "street_class", palette = brewer.pal(3, "Pastel1")) +
  tm_shape(rail_bus_net) +
  tm_lines(lwd = 1, col = "mode", palette = brewer.pal(3, "Dark2")) +
  tm_shape(study_area) +
  tm_borders(lwd = 2) +
  tm_text("SA2_NAME21") +
  tm_shape(bike_lts4) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Reds")) +
  tm_shape(bike_lts1) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Blues")) +
  tm_shape(point_of_origin_sf) +
  tm_symbols(shape = 23, col = "#FFB448", border.col = "#3C2D4D") +
  tm_text("id", col = "#3C2D4D")

## Isochrones: Car-dependent calculated

time_of_departure <- as.POSIXct("22-11-2023 08:00:00", #Tue
                                 format = "%d-%m-%Y %H:%M:%S", tz = Sys.timezone(location = TRUE))

mode <- c("WALK", "TRANSIT") 
transit_tue <- isochrone(r5r_core,
                  cutoffs = time_intervals,
                  origins = point_of_origin_df,
                  mode = mode,
                  sample_size = 1,
                  departure_datetime = time_of_departure,
                  progress = TRUE) %>% 
  mutate(mins = as.character(isochrone),
         mins = factor(mins, levels = c("10", "15", "20", "30")))

gc()

time_of_departure <- as.POSIXct("25-11-2023 08:00:00", #Sat
                                 format = "%d-%m-%Y %H:%M:%S", tz = Sys.timezone(location = TRUE))

transit_sat <- isochrone(r5r_core,
                  cutoffs = time_intervals,
                  origins = point_of_origin_df,
                  mode = mode,
                  sample_size = 1,
                  departure_datetime = time_of_departure,
                  progress = TRUE) %>% 
  mutate(mins = as.character(isochrone),
         mins = factor(mins, levels = c("10", "15", "20", "30")))

gc()

time_of_departure <- as.POSIXct("22-11-2023 08:00:00",
                                 format = "%d-%m-%Y %H:%M:%S", tz = Sys.timezone(location = TRUE))

#If it just resembles a walking area, ensure that you picked a date within the GTFS' feed_info$feed_start_date and feed_info$feed_end_date, and also that you have the correct time zone specified

## Isochrones: Car-dependent mapped

tm_shape(fast_roads, bbox = st_bbox(study_area)) +
  tm_lines(lwd = 1, col = "street_class", palette = brewer.pal(3, "Pastel1")) +
  tm_shape(rail_bus_net) +
  tm_lines(lwd = 1, col = "mode", palette = brewer.pal(3, "Dark2")) +
  tm_shape(study_area) +
  tm_borders(lwd = 2) +
  tm_text("SA2_NAME21") +
  tm_shape(drive) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Greys")) +
  tm_shape(transit_tue) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Reds")) +
  tm_shape(transit_sat) +
  tm_fill(col = "mins", alpha = 0.5, palette = brewer.pal(4, "Blues")) +
  tm_shape(point_of_origin_sf) +
  tm_symbols(shape = 23, col = "#FFB448", border.col = "#3C2D4D") +
  tm_text("id", col = "#3C2D4D")


