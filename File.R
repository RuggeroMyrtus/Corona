#libraries
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
dati=read.csv2("COVID-19-geographic-disbtribution-worldwide-2020-03-22.csv")
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
               as.Date("2020-03-22"))  #2020-03-20

# x11()
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
animate(map, fps=30, width = 1920, height = 1080, duration=60, gifski_renderer(),
        end_pause = 50)-> for_mp4
anim_save("Covid19-Cartina.mpeg")
#write.csv2(data, file = "data_finali.csv")

data.barplot <- data %>%
  group_by(DateRep) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Cases),
         Cases_rel = Cases/Cases[rank==1],
         Cases_lbl = paste0(" ",Cases)) %>%
  group_by(Countries.and.territories) %>% 
  filter(rank <=10) %>%
  ungroup()

staticplot = ggplot(data.barplot, aes(rank, group = Countries.and.territories, 
                                      fill = as.factor(Countries.and.territories), 
                                      color = as.factor(Countries.and.territories))) +
  geom_tile(aes(y = Cases/2,
                height = Cases,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Countries.and.territories, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Cases,label = Cases_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

(anim = staticplot + transition_states(DateRep, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = TRUE)  +
    labs(title = 'Day:{closest_state}',  
         subtitle  =  "Top 10 Countries",
         caption  = "Number of Cases of Covid19 | 
       Data Source: European Centre for Disease Prevention and Control"))
setwd("")
animate(anim, fps=30, width = 1920, height = 1080, duration=60, gifski_renderer(),
        end_pause = 50)-> for_mp4
anim_save("Covid19-barplot.mpeg")
