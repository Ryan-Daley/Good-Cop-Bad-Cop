#data manipulation and graphs
library(tidyverse) 
library(stringr)
library(knitr)
library(cowplot)
library(kableExtra)
#For maps
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(sf)
#For Coloring and Breaks
library(classInt)
library(RColorBrewer)
library(tmap)
library(na.tools)

library(gridExtra)
library(miscset)
library(DT)
library(data.table)
library(rgdal)
library(tmap)
library(tidytext) 
library(tm)
library(wordcloud)
library(plotly)
library(htmlwidgets)

prepped3749 <- read.csv("C://Users/Ryan/Documents/University of Essex/MA304 - Exploratory Data Analysis and Data Visualisation/Assessments/Final Project/prepped3749.csv")

#OFFICER RACE VS SUBJECT RACE
empcat49 <- names(which(sapply(prepped3749,is.character)))

ggplotGrid(ncol = 2,lapply(empcat49[c(5,6,14,13)],function(col) {
  ggplot(prepped3749, aes_string(col)) + geom_bar(fill = "red") + 
    theme(axis.text.x = element_text(size  = 8,
                                     angle = 90,
                                     hjust = 1,
                                     vjust = 1))
}))

#OFFICER RACE VS SUBJECT RACE 2
ggplot(prepped3749,aes(x = OFFICER_RACE,fill = SUBJECT_RACE)) + 
  geom_bar(position = "fill") + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))
#ALTERNATIVE
ggplot(prepped3749,aes(x = OFFICER_RACE,fill = SUBJECT_RACE)) + 
  geom_bar(position = "dodge") + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))
#OFFICER YEARS ON FORCE
ggplot(prepped3749,aes(as.numeric(OFFICER_YEARS_ON_FORCE))) + geom_histogram()+ xlab("Officer years on force") + ylab("Count")
#OFFICER RACE VS SUBJECT INJURY
ggplotGrid(ncol = 2,lapply(empcat49[c(9,15)],function(col) {
  ggplot(prepped3749, aes_string(col)) + geom_bar(fill = "red") + 
    theme(axis.text.x = element_text(size  = 8,
                                     angle = 90,
                                     hjust = 1,
                                     vjust = 1)) 
}))
#OFFICER RACE VS SUBJECT INJURY
p1 =  ggplot(prepped3749,aes(x = OFFICER_RACE,fill = SUBJECT_INJURY)) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject injury"))
p2 = ggplot(prepped3749,aes(x = OFFICER_RACE,fill = SUBJECT_INJURY)) + 
  geom_bar(position = "dodge") + 
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject injury"))
grid.arrange(p1,p2,ncol = 2)
#REASON FOR ARREST AND SUBJECT RACE
p3 = ggplot(prepped3749,aes(x = SUBJECT_DESCRIPTION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject description") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

p4 = ggplot(prepped3749,aes(x = SUBJECT_DESCRIPTION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject description") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

grid.arrange(p3,p4,ncol = 2)

#In which DIVISION more BLACKS are arrested?
p5 = ggplot(prepped3749,aes(x = DIVISION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Division") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

p6 = ggplot(prepped3749,aes(x = DIVISION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Division") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))
grid.arrange(p5,p6,ncol = 2)

#REASON FOR Incident VS OFFICER RACE
p7 = ggplot(prepped3749,aes(x = OFFICER_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))
p8 = ggplot(prepped3749,aes(x = OFFICER_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1))  + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))
grid.arrange(p7,p8,ncol = 2)  
#REASON FOR INCIDENT VS SUBJECT RACE

p9 = ggplot(prepped3749,aes(x = SUBJECT_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))
p10 = ggplot(prepped3749,aes(x = SUBJECT_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))
grid.arrange(p9,p10,ncol = 2)

dep37shp <- readOGR(dsn = "C:/Users/Ryan/Documents/University of Essex/MA304 - Exploratory Data Analysis and Data Visualisation/Assessments/Final Project/Dept_37-00049/37-00049_Shapefiles/EPIC.shp")
dep37shp <- spTransform(dep37shp, CRS("+proj=longlat +datum=WGS84"))
dep37shp_df <- fortify(dep37shp)
racemap <- ggplot(data = dep37shp_df, aes(long,lat)) +
  geom_polygon(aes(group = group), fill="darkblue") +
  coord_equal() 

racemap

race_crime = prepped3749 %>%
  group_by(`LOCATION_LONGITUDE`,`LOCATION_LATITUDE`, SUBJECT_RACE,`LOCATION_DISTRICT`) %>% 
  count() %>% arrange(desc(n)) %>% filter(n > 1 )

race_crime$LOCATION_LONGITUDE <- as.numeric(race_crime$LOCATION_LONGITUDE)
race_crime$LOCATION_LATITUDE <- as.numeric(race_crime$LOCATION_LATITUDE)

crime_race_map <- racemap + geom_point(aes(x = `LOCATION_LONGITUDE`, y = `LOCATION_LATITUDE`, size = n, alpha = 0.8, 
                         color = SUBJECT_RACE), data = race_crime) + xlab("Longitude (Degrees)") + ylab("Latitude (Degrees)")
ggplotly(crime_race_map)

#RACE PERCENTAGE OF CRIMES
prepped3749%>%
  group_by(SUBJECT_RACE) %>%
  filter(!is.na(SUBJECT_RACE)) %>%
  summarise(Count = n()) %>%
  mutate(TotalCount = nrow(prepped3749)) %>%
  mutate(Percentage = (Count/TotalCount) * 100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(SUBJECT_RACE = reorder(SUBJECT_RACE,Count)) %>%
  
  ggplot(aes(x = SUBJECT_RACE,y = Percentage)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = SUBJECT_RACE, y = 1, label = paste0("(",round(Percentage,2)," % )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'red',
            fontface = 'bold') +
  labs(x = 'Race', 
       y = 'Percentage', 
       title = 'Race Percentage in Crimes') +
  coord_flip() + 
  theme_bw()
attach(prepped3749)
min(INCIDENT_DATE)
max(INCIDENT_DATE)

