#Crime Presentation EDA

library(data.table)
library(tidyverse)
library(dplyr)
library(plotly)
library(knitr)
library(widgetframe)
library(gapminder)
library(ggplot2)
library(hrbrthemes)
library(leaflet)
library(webshot)

opts_chunk$set(
  warning = FALSE,
  message = FALSE, 
  eval=TRUE, 
  echo = TRUE,
  cache=FALSE, 
  include=TRUE,
  fig.width = 7,  
  fig.align = 'center',
  fig.asp = 0.618,
  out.width = "700px")
  
crimedat <- read_rds("crimedatfinal.rds")
crimedat <- as.data.frame(crimedat)


#### EDA: Months with the Highest Crime in the Top 5 Areas
#Subset dataset
ncrimesdate <-
  crimedat %>%
  group_by(Area, Month, Year) %>%
  summarise(
    n = n()
  ) %>%
  arrange(Area, desc(n))

#Subset to top 5 areas and only 2020 and 2021 data
#Central, 77th Street, Pacific, Southwest, Hollywood
ncrimesdate <- subset(ncrimesdate,
                      Area %in% c("Central", "77th Street", "Pacific", "Southwest", "Hollywood") &
                      Year %in% c("2020", "2021"))

#Order by month
ncrimesdate <- 
  ncrimesdate %>%
  mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May",  "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

#Order by rank (doesn't work)
ncrimesdate <- 
  ncrimesdate %>%
  mutate(Area = factor(Area, levels = c("Central", "77th Street", "Pacific", "Southwest", "Hollywood")))
         
#Months with the Highest Crime in the Top 5 Areas
month_crime = plot_ly(ncrimesdate, x = ~Month, y = ~n, color = ~Area, type = "bar")
month_crime <- month_crime %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
month_crime <- month_crime %>% layout(title = "Months with the Highest Crime in the Top 5 Areas")
month_crime_plotly <- ggplotly(month_crime)
month_crime_plotly 

#############################
#############################
#############################
###Histogram of Victims
#Create df
victim_demographics <-
  crimedat %>%
  select(`Record No`, `Crime Code`,`Victim Age`, `Victim Sex`, `Victim Ethnicity` , Area, Crime, `Year`)

#Sex, Ethnicity Factor
victim_demographics <- subset(victim_demographics, 
                              `Victim Sex` %in% c("F", "M") &
                                `Victim Ethnicity`  %in% c("B", "H", "W", "A", "O"))
#Factoring Ethnicity and Age
victim_demographics <-
  victim_demographics %>%
  mutate(victim_ethnicity_f = case_when(victim_demographics$`Victim Ethnicity` == "W" ~ "White",
                                        victim_demographics$`Victim Ethnicity` == "B" ~ "Black",
                                        victim_demographics$`Victim Ethnicity` == "A" ~ "Asian",
                                        victim_demographics$`Victim Ethnicity` == "H" ~ "Hispanic",
                                        victim_demographics$`Victim Ethnicity` == "O" ~ "Other"))

victim_demographics <-
  victim_demographics %>%
  mutate(victim_age_f = 
           case_when(victim_demographics$`Victim Age` >= 0 & victim_demographics$`Victim Age`<= 10 ~ "0-10",
                     victim_demographics$`Victim Age` >= 11 & victim_demographics$`Victim Age`<= 20 ~ "11-20",
                     victim_demographics$`Victim Age` >= 21 & victim_demographics$`Victim Age`<= 30 ~ "21-30",
                     victim_demographics$`Victim Age` >= 31 & victim_demographics$`Victim Age`<= 40 ~ "31-40",
                     victim_demographics$`Victim Age` >= 41 & victim_demographics$`Victim Age`<= 50 ~ "41-50",
                     victim_demographics$`Victim Age` >= 51 & victim_demographics$`Victim Age`<= 60 ~ "51-60",
                     victim_demographics$`Victim Age` >= 61 & victim_demographics$`Victim Age`<= 70 ~ "61-70",
                     victim_demographics$`Victim Age` >= 71 & victim_demographics$`Victim Age`<= 80 ~ "71-80",
                     victim_demographics$`Victim Age` >= 81 & victim_demographics$`Victim Age`<= 90 ~ "81-90",
                     victim_demographics$`Victim Age` >= 91 & victim_demographics$`Victim Age`<= 100 ~ "91-100"))

#EDA 2: Victim Age and Sex
#Interactive Histogram
vict_demo <-plot_ly(victim_demographics, x = ~victim_age_f, color = ~`Victim Sex`) %>%
  add_histogram()
#vict_demo <- vict_demo %>% layout(barmode = "overlay")
vict_demo <- vict_demo %>% layout(title = "Histogram of Victim's Age and Gender",
                                  yaxis = list(
                                    title = "Count"
                                  ),
                                  xaxis = list(
                                    title = "Age"
                                  ))
vict_demo_plotly <- ggplotly(vict_demo)
vict_demo_plotly


#############################
#############################
#############################
#Central
detach(package:dplyr)
library(dplyr)
#Creating a new df to subset for Central
central_top_crimes <-
  crimedat %>%
  select(`Record No`, `Crime Code`, Area, Crime, `Year`, `Lat`, `Lon`)

#Subset Area = Central and Year = 2021
central_top_crimes <- subset(central_top_crimes, 
                             `Area` %in% c("Central") &
                               `Year` %in% c("2021"))

#View top 5 crimes in Central
central_top_crimes %>% 
  group_by(Area, `Crime Code`, Crime) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(,10) %>%
  knitr::kable()

#Now subset Crime Code to top 5 in that area
central_top_crimes <- subset(central_top_crimes, 
                             `Crime Code` %in% c("330", "624", "740", "230", "440"))

#Map color palette
central.pal <- colorFactor(c('red', 'yellow', 'blue', 'green', 'purple'), domain = central_top_crimes$`Crime`)

central_map <- 
  leaflet(central_top_crimes) %>%  
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(
    lat = ~Lat, 
    lng = ~Lon,
    label = ~paste0(central_top_crimes$`Crime`),
    color = ~ central.pal(central_top_crimes$`Crime`),
    opacity = 0.5,
    fillOpacity = 1,
    radius = 5
  ) %>%
  addLegend('bottomleft', 
            pal = central.pal,  
            values = central_top_crimes$`Crime`,
            title = 'Central Crime Map', 
            opacity = 1)
central_map



#####################
#Plot 1
##TESTING

ncrimes <-
  crimedat %>%
  group_by(Area, `Crime Code`, Crime, Year) %>%
  summarise(
    n = n()
  ) %>%
  arrange(Area, desc(n)) 


ncrimes <- subset(ncrimes,
                      Area %in% c("Central", "77th Street", "Pacific", "Southwest", "Hollywood") &
                        Year %in% c("2020", "2021") &
                    `Crime Code` %in% c("330", "510", "624", "230", "210", "626", "740", "440"))

#Crimes per Top Areas 
top_crime <- ncrimes %>% 
  plot_ly(x = ~Crime, y = ~n,
          type = 'scatter',
          mode = 'markers',
          color = ~Area,
          sizes = c(5, 70),
          marker = list(sizemode='diameter', opacity=0.5))
top_crime <- top_crime %>% layout(title = "Most Common Crime in Top Areas")
top_crime_plotly <- ggplotly(top_crime)
top_crime_plotly

p2 <- ggplot(ncrimes, aes(x = Crime, y = Area, color = Area)) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)
#FIX: pick top 5 crimes



#Subset of overall crime number per area
crime_frequency <- count(crimedat, Area)


#Other EDA
#Testing: area vs crime
p3 <- crime_frequency %>%
  ggplot( aes(Area, n, size = n, color=Area)) +
  geom_point() +
  theme_bw()
#FIX: pick top 5 areas, too scrunched

#Testing: date vs crime
value <- crime_frequency$n
q <- crimedat %>%
  ggplot( aes(x=Month, y=Crime)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("count") 
#doesn't work



#### MAP EDA



#####################################
##PROF CODE
ncrimes <-
  crimedat %>%
  group_by(Area, Crime) %>%
  summarise(
    n = n()
  ) %>%
  arrange(Area, desc(n))
#EDA 1: Victim Demographics
#Create df
victim_demographics <-
  crimedat %>%
  select(`Record No`, `Crime Code`,`Victim Age`, `Victim Sex`, `Victim Ethnicity` , Area, Crime, `Year`)

#Sex, Ethnicity
victim_demographics <- subset(victim_demographics, 
                              `Victim Sex` %in% c("F", "M") &
                                `Victim Ethnicity`  %in% c("B", "H", "W", "A", "O"))
#Factoring Ethnicity and Age
victim_demographics <-
  victim_demographics %>%
  mutate(victim_ethnicity_f = case_when(victim_demographics$`Victim Ethnicity` == "W" ~ "White",
                                        victim_demographics$`Victim Ethnicity` == "B" ~ "Black",
                                        victim_demographics$`Victim Ethnicity` == "A" ~ "Asian",
                                        victim_demographics$`Victim Ethnicity` == "H" ~ "Hispanic",
                                        victim_demographics$`Victim Ethnicity` == "O" ~ "Other"))

victim_demographics <-
  victim_demographics %>%
  mutate(victim_age_f = 
           case_when(victim_demographics$`Victim Age` >= 0 & victim_demographics$`Victim Age`<= 10 ~ "0-10",
                     victim_demographics$`Victim Age` >= 11 & victim_demographics$`Victim Age`<= 20 ~ "11-20",
                     victim_demographics$`Victim Age` >= 21 & victim_demographics$`Victim Age`<= 30 ~ "21-30",
                     victim_demographics$`Victim Age` >= 31 & victim_demographics$`Victim Age`<= 40 ~ "31-40",
                     victim_demographics$`Victim Age` >= 41 & victim_demographics$`Victim Age`<= 50 ~ "41-50",
                     victim_demographics$`Victim Age` >= 51 & victim_demographics$`Victim Age`<= 60 ~ "51-60",
                     victim_demographics$`Victim Age` >= 61 & victim_demographics$`Victim Age`<= 70 ~ "61-70",
                     victim_demographics$`Victim Age` >= 71 & victim_demographics$`Victim Age`<= 80 ~ "71-80",
                     victim_demographics$`Victim Age` >= 81 & victim_demographics$`Victim Age`<= 90 ~ "81-90",
                     victim_demographics$`Victim Age` >= 91 & victim_demographics$`Victim Age`<= 100 ~ "91-100"))








