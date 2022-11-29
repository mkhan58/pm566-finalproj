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

###Loop for Crime Count###
# Add variables for cases

#Create list of Areas (following lab)
crime_list <- unique(crimedat$Area)

for (i in 1:length(crime_list)) {
  crime_subset = subset(crimedat, Area == crime_list[i])
  crime_subset = crime_subset[order(crime_subset$`Date Occurred`),]
  # add starting level for new cases and deaths
  crime_subset$Cases = crime_subset$Crime[1]
  for (j in 2:nrow(crime_subset)) {
    crime_subset$Crime[j] = crime_subset$Crime[j] - crime_subset$Crime[j-1]
  }
  # include in main dataset
  crimedat$Cases[crimedat$Area==crime_list[i]] = crime_subset$Cases
}


#Plot 1
#crime_frequency <-count(crimedat, Area)
p1 <- ggplot(crimedat, aes(x = `Date Occurred`, y = Cases, color = Area)) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p1)
#Output doesnt work



#Subset of overall crime number per area
crime_frequency <- count(crimedat, Area)


#Other EDA
#Testing: area vs crime
p <- crime_frequency %>%
  ggplot( aes(Area, n, size = n, color=Area)) +
  geom_point() +
  theme_bw()
#too scrunched

#Testing: date vs crime
value <- crime_frequency$n
q <- crimedat %>%
  ggplot( aes(x=Month, y=Crime)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("count") 
#doesn't work

#####################################
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








