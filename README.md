# corona
#libraries
#install.packages(c("gganimate", "readr", "dplyr", "ggplot2", "maps",
#                   "ggthemes", "tibble", "lubridate", "DT", "viridis", "purrr", "magick", "ggmap",
#                   "readr", "RColorBrewer", "animation"))
library(viridis)
library(ggplot2) 
library(ggmap) 
library(dplyr) 
library(readr) 
library(RColorBrewer) 
library(purrr) 
library(magick) 
library(animation)
library(ggthemes)
library(gganimate)
library(tmap)
library(rmarkdown)
library(ggrepel)
library(gapminder)
library(personalized)
#loading files
setwd("")
dati=read.csv2("COVID-19-geographic-disbtribution-worldwide-2020-03-20.csv")
coordinate=read.csv2("paesi2.csv")
str(dati)

#wright coordinates format
coordinate$lon=as.numeric(gsub(".", "", coordinate$lon, fixed = TRUE))
coordinate$lon=coordinate$lon*1e-8
coordinate$lat=as.numeric(gsub(".", "", coordinate$lat, fixed = TRUE))
coordinate$lat=coordinate$lat*1e-8


#changing names and merging datasets
colnames(coordinate)=c("Countries.and.territories", "lon", "lat")
data=merge(dati, coordinate, by="Countries.and.territories", all=TRUE)
str(data)
data$DateRep=as.Date(data$DateRep,  "%d/%m/%Y")
head(data)
attach(data)
data=data[,-8]
data=data[order(DateRep),]

data=data %>%
  group_by(Countries.and.territories) %>%
  mutate("Cases" = cumsum(Cases))

data=data %>%
  group_by(Countries.and.territories) %>%
  mutate("Deaths" = cumsum(Deaths))

data=na.omit(data)

#creating dynamic map
((world <- ggplot() +
    borders("world", colour = "grey", fill = "gray90") +
    theme_map() ))
  
created_at = c(as.Date("2019-12-31"),
               as.Date("2020-03-20"))  #2020-03-20

map = world +
  geom_point(aes(x = lon, y = lat, size=ifelse(Cases==0, NA, Cases), group=DateRep),
             data =data,
             colour = 'red', alpha = .3, stroke=4) + #.5
  scale_fill_continuous(breaks=c(5,50,5555))+
  labs(size = "SARS-CoV-2")+
  theme( panel.background = element_rect(fill = "cornflowerblue"),
         legend.background = element_rect(fill = "lightblue", linetype = "solid",
                                          color="blue"))+
  transition_time(DateRep, created_at)+
  labs(title = "Day: {frame_time}")+
  scale_size_continuous(breaks = c(5, 1000, 5000, 10000))
animate(map, fps=10, width = 1000, height = 800) #first check rendering
setwd("")
animate(map, fps=30, width = 1920, height = 1080, duration=30, gifski_renderer())
anim_save("Covid19.mp4")
