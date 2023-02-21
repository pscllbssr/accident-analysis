library(sf)
library(ggplot2)

## 1.) road net
national_roads_file <- "data/roads/ch.astra.nationalstrassenachsen.gdb"
st_layers(national_roads_file)

# read in geodata
national_roads_geodata <- st_read(national_roads_file, layer = "Stammachsen")


### ----------------


# 2.) Traffic counters

library(readxl)
traffic_counters <- read_excel("data/traffic_counters/messstellenverzeichnis.xlsx", 
                               sheet = "SASVZ_CSACR", skip = 10)

traffic_counters <- traffic_counters %>% 
  rename("No" = "Nummer\r\nNuméro\r\nNuméro",
         "LV95_E" = "Koordinate Ost LV95\r\nCoordonnée est LV95\r\nCoordinata est LV95",
         "LV95_N" = "Koordinate Nord LV95\r\nCoordonnée nord LV95\r\nCoordinata nord LV95",
         "Route" = "Strasse\r\nRoute\r\nStrada") 
  # select(c("No", "LV95_E", "LV95_N"))
head(traffic_counters)



### ----------------


# 3.) Accidents

library(data.table)

accidents_df <- data.frame(fread("data/RoadTrafficAccidentLocations.csv"))
head(accidents_df)


### ----------------


# 4.) Plot

ggplot() +
  # road net
  geom_sf(data = national_roads_geodata) +
  geom_point(data = accidents_df[accidents_df$RoadType_de == 'Autobahn', ],
             mapping = aes(x = AccidentLocation_CHLV95_E,
                           y = AccidentLocation_CHLV95_N,
                           alpha = 0.001)) +
  labs(alpha="Accidents on Motorway") + 
  # traffic counters
  
  geom_point(data = traffic_counters, 
             mapping = aes(x = LV95_E, 
                           y = LV95_N, 
                           color = grepl("^H ",Route))) +
  labs(color="Traffic counter on Motorway", title = "Accidents (black) & traffic counters (red) on Motorways") 
  
