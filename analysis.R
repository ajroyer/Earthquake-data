library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

dataraw <- read_csv("all_month.csv")


world <- rnaturalearth::ne_countries(returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(aes(x = longitude,
                 y = latitude,
                 alpha = time), data = dataraw)
