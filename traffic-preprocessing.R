library(openxlsx)
library(readxl)

OUTPUT_DIR <- "data/processed"
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)
}

# init empty data frame
traffic_data <- data.frame()

# 2021 - 2018
traffic_files_1 = c(
  list(c(2021, "Jahresergebnisse 2021.xlsx")),
  list(c(2020, "Bulletin_2020_12_de.xlsx")),
  list(c(2019, "Bulletin_2019_de.xlsx")),
  list(c(2018, "Bulletin_2018_de.xlsx"))
)

for(i in seq_along(traffic_files_1)){
  year <- traffic_files_1[[i]][1]
  file <- traffic_files_1[[i]][2]

  tryCatch(
    expr = {
      tra <- read.xlsx(xlsxFile = file.path("data/traffic", file), fillMergedCells = TRUE, startRow = 7, sheet = "DTV", check.names = TRUE)
      tra <- subset(tra, select = -c(Messstelle.1)) 
      tra <- tra %>% 
        rename("Messvariable" = "X6") %>% 
        mutate(No = strtoi(No))
      tra <- tra %>% 
        filter(Messvariable == "DTV") %>% 
        pivot_longer(cols = matches("X[0-9]{2}$"), names_to = "Month", values_to = "Count") %>% 
        mutate(Strasse = gsub(" ","",Strasse)) %>% 
        mutate(Month = gsub("X","",Month)) %>% 
        mutate(Year = strtoi(year))
      
      if(length(colnames(traffic_data)) != 0) {
        cols <- intersect(colnames(tra), colnames(traffic_data))
        traffic_data <- rbind(tra[,cols], traffic_data[,cols])
      }else{
        traffic_data <- rbind(traffic_data, tra)
      }
      
    },
    error = function(e){
      print(e)
      print(file)
    }
  )
}

# file <- read.xlsx(xlsxFile = file.path("data/traffic", "Jahresergebnisse-2017.xlsx"), fillMergedCells = TRUE, sheet = "Daten - Données", startRow = 10, check.names = TRUE)
# colnames(file)
# 
# 
# file <- read.xlsx(xlsxFile = file.path("data/traffic", "data/traffic/Jahresergebnisse-2016.xlsx"), fillMergedCells = TRUE, sheet = "Daten - Données", startRow = 10, check.names = TRUE)
# colnames(file)
# 
# 
# file <- read.xlsx(xlsxFile = file.path("data/traffic", "data/traffic/Jahresergebnisse2015.xlsx"), fillMergedCells = TRUE, sheet = "Daten - Données", startRow = 10, check.names = TRUE)
# colnames(file)


# read location list
traffic_counters <- read_excel("data/traffic_counters/messstellenverzeichnis.xlsx", 
                               sheet = "SASVZ_CSACR", skip = 10)

# rename
traffic_counters <- traffic_counters %>% 
  rename("No" = "Nummer\r\nNuméro\r\nNuméro",
         "LV95_E" = "Koordinate Ost LV95\r\nCoordonnée est LV95\r\nCoordinata est LV95",
         "LV95_N" = "Koordinate Nord LV95\r\nCoordonnée nord LV95\r\nCoordinata nord LV95"
         ) %>% 
  select(c("No", "LV95_E", "LV95_N")) 

# join traffic data with coordinates from counters list
traffic <- left_join(traffic_data, traffic_counters, by = "No") %>% 
  rename("Name" = "Messstelle",
         "Canton" = "Kt",
         "Variable" = "Messvariable")

head(traffic)

write.csv(x = traffic, file = file.path(OUTPUT_DIR, "traffic.csv"))
