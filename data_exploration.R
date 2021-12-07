#Code for Atlas of Living Australia
#Created by Francisca Maron
#Date 07/12/2021


#Dependencies
library(tidyverse)
library(lubridate)
library(janitor)
library(httr)
library(jsonlite)
library(sf)
library(leaflet)
library(inspectdf)

# Download data ##########################################################

#Search reptiles with the API https://api.ala.org.au/
search_reptiles <- GET("https://biocache-ws.ala.org.au/ws/occurrences/search", 
             query = list(q = 'class:Reptilia',
                          fq = 'cl22:Australian Capital Territory',
                          facets = paste("decimalLatitude",
                          "decimalLongitude",
                          "scientificName",
                          "dataResourceName",
                          "basisOfRecord",
                          "cl22",
                          "cl10902", 
                          "class", sep = ",")))
search_reptiles #json 

#Transform to readable data
search_content <- fromJSON(rawToChar(search_reptiles$content))
names(search_content)


#Download occurrences (zip file according to documentation)
download_reptiles <- GET("https://biocache-ws.ala.org.au/ws/occurrences/index/download", 
             query = list(q = 'class:Reptilia',
                          fq = 'cl22:Australian Capital Territory',
                          fields = paste("decimalLatitude",
                                         "decimalLongitude",
                                         "scientificName",
                                         "dataResourceName",
                                         "basisOfRecord",
                                         "class",
                                         "eventDate",
                                         "cl22",
                                         "cl10902", sep = ","),
                          email= "[YOUR_EMAIL]",
                          reasonTypeId = "6"), write_disk("ala_reptiles.zip", overwrite = TRUE))

download_reptiles #zip

#Unzip the data
unzip("ala_reptiles.zip")




# Data cleaning ##########################################################
#Read the data
reptiles <- read.csv("data.csv", header = TRUE)
head(reptiles)
names(reptiles) #messy column names!

# Clean names
reptiles <- reptiles %>%
  clean_names(case = "small_camel") #match the task

#Rename for the columns that don't match and to facilitate manipulation
reptiles <- reptiles %>%
  rename("decimalLatitude" = decimalLatitudeWgs84,
         "decimalLongitude" = decimalLongitudeWgs84,
         "scientificName" = scientificNameIntepreted,
         "state" = australianStatesAndTerritories,
         "forest2013" = forestsOfAustralia2013V2_0)

unique(reptiles$state)#filter query didn't work because the forest layer had greater extent
unique(reptiles$class)#Reptilia


#Filter the data for ACT and select relevant columns
reptiles <- reptiles %>%
  filter(state == "Australian Capital Territory") %>%
  select(decimalLatitude, decimalLongitude, eventDate, 
         scientificName, class, dataResourceName,
         basisOfRecord, state, forest2013)

#Check if filter and select worked  
unique(reptiles$state) #ACT
head(reptiles) #selected columns

#Inspect structure
str(reptiles)

#Inspect eventDate column
reptiles$eventDate 

#Transform eventDate as datetime instead of character
reptiles <- reptiles %>%
  mutate(eventDate = ymd_hms(eventDate))

str(reptiles) #date is POSIXct now


# Exploratory analysis ##########################################################

#Inspect missing values
inspect_na(reptiles) #just eventDate missing data

#Inspect categorical variables
inspect_cat(reptiles)

# Univariate analyses

#Check species 
unique(reptiles$scientificName)
length(unique(reptiles$scientificName))#102 unique species (not good for bar plot)
table(reptiles$scientificName) #some species don't have the complete scientific name



#Check forest types
unique(reptiles$forest2013) #there is a blank string as a value
length(unique(reptiles$forest2013)) #11 different
table(reptiles$forest2013) #17 occurrences on the blank string, what is this?

ggplot(reptiles, aes(x = forest2013)) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

#Most species occurrences on Eucalypt Medium Woodland

#Check resource
unique(reptiles$dataResourceName)
length(unique(reptiles$dataResourceName)) #23 resources
table(reptiles$dataResourceName) # ACT Wildlife Atlas with most ocurrence

#Check basis of record
unique(reptiles$basisOfRecord)
ggplot(reptiles, aes(x = basisOfRecord))+
  geom_bar()+
  theme_bw()
#Human observation has the most records followed by unknown

#Check date range
range(reptiles$eventDate, na.rm = TRUE)

#Records from 1954!

#Save the data and perform bivariate analysis as part the visualisation task
save(reptiles, file = "reptiles.RData")

