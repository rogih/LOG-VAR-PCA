library(ggplot2)
library(ggmap)
library(readr)
library(geosphere)

fn_circle <- function(id1, lon1, lat1, radius){ 
  data.frame(ID = id1, degree = 1:360) %>%
    rowwise() %>%
    mutate(lon = destPoint(c(lon1, lat1), degree, radius)[1]) %>%
    mutate(lat = destPoint(c(lon1, lat1), degree, radius)[2]) 
}


stations <- read_csv("coordinates.csv")
head(stations)
stations.new <-  bind_cols(ID=1:8,stations)
stations.new <- select(stations.new,-"Station")
stations.new <- as.data.frame(stations.new)


circle <- apply(stations.new, 1, function(x) fn_circle(x[1], x[2], x[3], 1500))
circle <- do.call(rbind, circle)

vix <- get_map(location = c(lon =-40.31, lat= -20.28), zoom = 12, maptype = "terrain", source="stamen")
pdf("ramqarstations.pdf")
ggmap(vix) +  geom_point(data = stations,  aes(x = as.numeric(stations$Longitude), y = as.numeric(stations$Latitude)), colour = "black", size=1, alpha=1) +
  geom_polygon(data = as.data.frame(circle), aes(lon, lat, group = ID), color = "purple", alpha = 0) +
  geom_point(data = stations,  aes(x = as.numeric(stations$Longitude), y = as.numeric(stations$Latitude)), colour = "purple", size=27, alpha=0.5) +
  geom_text(aes(x=as.numeric(stations$Longitude), y=as.numeric(stations$Latitude), label=stations$Station), data=stations, hjust=-0.2) 
dev.off()  
