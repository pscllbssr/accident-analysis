OUTPUT_DIR <- "data/processed"
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)
}

national_roads_file <-
  "data/roads/ch.astra.nationalstrassenachsen.gdb"
st_layers(national_roads_file)

# read in geodata of national roads
national_roads_geodata <-
  st_read(national_roads_file, layer = "Stammachsen")

# read in accidents dataset
library(data.table)
accidents <-
  data.frame(fread("data/RoadTrafficAccidentLocations.csv"))

# read in preprocessed traffic dataset
traffic <- read.csv("data/processed/traffic.csv")
traffic <- traffic %>% 
  filter(!is.na(traffic$LV95_E)) %>% 
  filter(str_detect(Strasse, "A"))

traffic_as_sf <- st_as_sf(
  traffic[!is.na(traffic$LV95_E), ],
  coords = c("LV95_E", "LV95_N")
)

# filter accidents on motorways
accidents_motorways <- accidents %>%
  filter(RoadType_en == "Motorway")


# accidents on motorways make for about 9% of all accidents in the dataset
nrow(accidents_motorways) / nrow(accidents)

# convert accidents into sf geo-object
accidents_as_sf <-
  st_as_sf(
    accidents_motorways,
    coords = c("AccidentLocation_CHLV95_E", "AccidentLocation_CHLV95_N")
  )

# add road name and segment as a column to accidents
accidents_as_sf$road <-
  as.data.frame(national_roads_geodata)[st_nearest_feature(accidents_as_sf, national_roads_geodata), "Strassennummer"]
accidents_as_sf$road_segment <-
  as.data.frame(national_roads_geodata)[st_nearest_feature(accidents_as_sf, national_roads_geodata), "Segmentname"]

ggplot() +
  # road net
  geom_sf(data = national_roads_geodata,
          mapping = aes(colour = Strassennummer)) +
  labs(title = "Swiss Motorways with accidents") +
  # accidents
  geom_sf(data = accidents_as_sf,
          mapping = aes (colour = road),
          shape = 8)

# match accidents with nearest traffic counter id
accidents_as_sf$nearestCounter <-
  as.data.frame(traffic_as_sf)[st_nearest_feature(accidents_as_sf, traffic_as_sf), "No"]
head(accidents_as_sf)

ggplot() +
  geom_sf(data = accidents_as_sf,
          mapping = aes(colour = nearestCounter)) +
  scale_color_gradientn(colours = rainbow(5)) +
  geom_sf(data = traffic_as_sf,
          mapping = aes(colour = No), shape = 13, size = 6) +
  labs(title = "Check mapping of accidents (dots) & counters (cross)")

st_write(accidents_as_sf, file.path(OUTPUT_DIR, "motorway_accidents_geodata.csv"), layer_options = "GEOMETRY=AS_XY")
