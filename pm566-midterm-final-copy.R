#PM566 Midterm

library(stringr)
library(dplyr)
library(tidyverse)
library(rvest)
library(xml2)
library(dbplyr)
library(dplyr)
library(skimr) 
library(ggplot2)
library(data.table)
library(lubridate)
library(leaflet)
library(webshot)
library(rapportools)
library(lubridate)
library(dtplyr)

crimeraw <- read.csv("Crime_Data_from_2020_to_Present.csv")

#Rename the columns to be more descriptive
colnames(crimeraw) <- c('Record No',
                        'Date Reported',
                        'Date Occurred',
                        'Time Occurred',
                        'Area No',
                        'Area',
                        'Rpt Dist No', 
                        'Part', 
                        'Crime Code',
                        'Crime',
                        'MO', 
                        'Victim Age',
                        'Victim Sex',
                        'Victim Ethnicity',
                        'Premise Code',
                        'Premise', 
                        'Weapon Code',
                        'Weapon',
                        'Case Code',
                        'Case Status',
                        'Crime Code 1',
                        'Crime Code 2',
                        'Crime Code 3',
                        'Crime Code 4',
                        'Location',
                        'Cross Street',
                        'Lat',
                        'Lon'
)

#Drop unwanted columns
crimeraw <- select(crimeraw, -c('Rpt Dist No','Part', 'MO','Premise', 'Crime Code 1', 'Crime Code 2', 'Crime Code 3', 'Crime Code 4', 'Cross Street'))

#Reformat date variables
crimeraw$`Date Reported` <- as.Date(crimeraw$`Date Reported`, format = "%m/%d/%Y")
crimeraw$`Date Occurred` <- as.Date(crimeraw$`Date Occurred`, format = "%m/%d/%Y")

#Change miltary to standard time
crimeraw$`Time Occurred` <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", crimeraw$`Time Occurred`), 
                                                              format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')

#Create Month & Year column for later analysis
crimeraw$Month = format(crimeraw$`Date Occurred`, "%b")
crimeraw$Year = format(crimeraw$`Date Occurred`, "%Y")

#Drop (0,0) coordinates
sum(crimeraw$Lat == 0)
sum(crimeraw$Lon == 0)
crimeraw <- subset(crimeraw, Lat != 0, Lon != 0)

#Examine Victim Age
summary(crimeraw$`Victim Age`)
crimeraw <-
  crimeraw %>%
  filter(`Victim Age` >= 0 & `Victim Age` <= 100)

#Replace empty cells with NA values
crimeraw$Weapon[is.empty(crimeraw$Weapon)] <- "NONE"
crimeraw$`Victim Sex`[is.empty(crimeraw$`Victim Sex`)] <- NA
crimeraw$`Victim Ethnicity`[is.empty(crimeraw$`Victim Ethnicity`)] <- NA
crimeraw$`Victim Age`[crimeraw$`Victim Age` == 0] <- NA

#Replace missing cells in Weapon Code with 999 to signify no weapon
crimeraw$`Weapon Code` <- crimeraw$`Weapon Code` %>% replace_na(999)

#Save as changes as a new file
#write_rds(crimeraw, "crimedatfinal.rds")
crimedat <- read_rds("crimedatfinal.rds")
crimecopy <- crimedat #make a copy for reference
crimedat <- as.data.frame(crimedat)

#EDA 1
#Calculate proportion of crime per area
crime_frequency <-count(crimedat, Area)
nc <- sum(crime_frequency$n)

crime_frequency %>% 
  arrange(desc(n)) %>%
  mutate(Percent = round( n / nc * 100, 2))  %>%
  knitr::kable()

v1 <- table(crimedat$`Area`)
barplot(sort(v1, T)[1:10], las = 2, col = rainbow(12), cex.names= .7, main = "Top 10 Areas with Crime", xlab = "Area", ylab = "Count")

#EDA 2
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

#EDA 2 77th Street

#Creating a new df to subset for 77th Street
st77_top_crimes  <-
  crimedat %>%
  select(`Record No`, `Crime Code`, Area, Crime, `Year`, `Lat`, `Lon`)

#Subset Area = 77th Street and Year = 2021
st77_top_crimes <- subset(st77_top_crimes, 
                          `Area` %in% c("77th Street") &
                            `Year` %in% c("2021"))

#View top 5 crimes in Central
st77_top_crimes %>% 
  group_by(Area, `Crime Code`, Crime) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(,10) %>%
  knitr::kable()

#Now subset Crime Code to top 5 in that area
st77_top_crimes <- subset(st77_top_crimes, 
                          `Crime Code` %in% c("510", "230", "624", "626", "210"))

st77.pal <- colorFactor(c('red', 'yellow', 'blue', 'green', 'purple'), domain = st77_top_crimes$`Crime`)

st77_map <- 
  leaflet(st77_top_crimes) %>%  
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(
    lat = ~Lat, 
    lng = ~Lon,
    label = ~paste0(st77_top_crimes$`Crime`),
    color = ~ st77.pal(st77_top_crimes$`Crime`),
    opacity = 0.5,
    fillOpacity = 1,
    radius = 5
  ) %>%
  addLegend('bottomleft', 
            pal = st77.pal,  
            values = st77_top_crimes$`Crime`,
            title = '77th Street Map', 
            opacity = 1)
st77_map

#EDA 3
#Month highest crime
#detach(package:dplyr)
#library(dplyr)
crime_count_month <-  
  crimedat %>%
  group_by(Month, Year, Area) %>% 
  summarize(CrimeCount = n()) 

crime_count_month <- subset(crime_count_month, 
                            Year %in% c("2020", "2021")) #2022 is still in progress

#Order months in order for graph
month.ordered <- 
  crime_count_month %>%
  mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May",  "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

month.ordered  %>%
  ggplot(aes(x = Month, y = CrimeCount, fill = Area)) +
  geom_bar(stat = "identity")

#EDA 4
#Age, ethnicity, sex
#Create df
victim_demographics <-
  crimedat %>%
  select(`Record No`, `Crime Code`,`Victim Age`, `Victim Sex`, `Victim Ethnicity` , Area, Crime, `Year`)

#For simplicity
victim_demographics <- subset(victim_demographics, 
                              `Victim Sex` %in% c("F", "M") &
                                `Victim Ethnicity`  %in% c("B", "H", "W", "A", "O"))

#Age
victim_demographics %>%
  ggplot(mapping = aes(x = `Victim Age`)) + 
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 1, color = "black", fill = "red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ggtitle("Victim Age Distribution") +
  xlab("Age") +
  ylab("Count")

#Sex
victim_demographics %>%
  ggplot(aes(x=`Victim Age`, color= `Victim Sex`)) +
  geom_histogram(fill="white", alpha=0.2, position="identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) 

#Ethnicity
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

victim_demographics %>%
  ggplot(mapping = aes(x = victim_age_f, fill = factor(victim_ethnicity_f))) + 
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Victim Ethnicity by Age") +
  xlab("Count") +
  ylab("Age") +
  labs(fill = "Victim Ethnicity")