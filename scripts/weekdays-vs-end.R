# read in accidents dataset
library(data.table)

accidents <-
  data.frame(fread("data/RoadTrafficAccidentLocations.csv"))

accidents <- accidents %>% 
  mutate(Weekend = AccidentWeekDay %in% c("aw406", "aw407"))

ggplot(data = accidents) +
  geom_bar(mapping = aes(x = Weekend, fill = Weekend), stat = ) +
  facet_wrap( ~ RoadType_de)
  