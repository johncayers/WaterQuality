# Package ID: edi.705.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Global data set of long-term summertime vertical temperature profiles in 153 lakes.
# Data set creator:  Rachel Pilla - Miami University 
# Data set creator:  Elizabeth Mette - Miami University 
# Data set creator:  Craig Williamson - Miami University 
# Data set creator:  Boris Adamovich - Belarusian State University 
# Data set creator:  Rita Adrian - IGB-Berlin 
# Data set creator:  Orlane Anneville - INRAE, University of Savoie Mont-Blanc 
# Data set creator:  Esteban Balseiro - University of Comahue: INIBIOMA, CONICET 
# Data set creator:  Syuhei Ban - University of Shiga Prefecture 
# Data set creator:  Sudeep Chandra - University of Nevada, Reno 
# Data set creator:  William Colom-Montero - Uppsala University 
# Data set creator:  Shawn Devlin - University of Montana 
# Data set creator:  Margaret Dix - Universidad del Valle de Guatemala Centro de Estudios Atitlan 
# Data set creator:  Martin Dokulil - University of Insbruck 
# Data set creator:  Natalie Feldsine - Mohonk Preserve 
# Data set creator:  Heidrun Feuchtmayr - UK Centre for Ecology & Hydrology 
# Data set creator:  Natalie Fogarty - Seqwater 
# Data set creator:  Evelyn Gaiser - Florida International University 
# Data set creator:  Scott Girdner - U.S. National Park Service 
# Data set creator:  MarÃ­a GonzÃ¡lez - Miami University 
# Data set creator:  K Hambright - University of Oklahoma 
# Data set creator:  David Hamilton - Griffith University 
# Data set creator:  Karl Havens - University of Florida 
# Data set creator:  Dag Hessen - University of Oslo 
# Data set creator:  Harald Hetzenauer - Institut fÃ¼r Seenforschung 
# Data set creator:  Scott Higgins - IISD Experimnetal Lake Area Inc. 
# Data set creator:  Timo Huttula - FAO; BELSPO 
# Data set creator:  Hannu Huuskonen - University of Eastern Finland 
# Data set creator:  Peter Isles - Swiss Federeal Institute of Aquatic Science and Technology 
# Data set creator:  Klaus Joehnk - CSIRO 
# Data set creator:  Wendel Keller - Laurentian University 
# Data set creator:  Jen Klug - Fairfield University 
# Data set creator:  Lesley Knoll - University of Minnesota 
# Data set creator:  Johanna Korhonen - Finnish Environment Institute SYKE 
# Data set creator:  Nikolai Korovchinsky - A.N. Severtsov Institute of Ecology and Evolution of The Russian Academy of Sciences 
# Data set creator:  Oliver KÃ¶ster - Zurich Water Supply 
# Data set creator:  Benjamin Kraemer - Leibniz-Institute of Freshwater Ecology and Inland Fisheries 
# Data set creator:  Peter Leavitt - University of Regina 
# Data set creator:  Barbara Leoni - Milano-Bicocca University 
# Data set creator:  Fabio Lepori - University of Applied Sciences and Arts of Southern Switzerland 
# Data set creator:  Ekaterina Lepskaya - Kamchatka Research Institute of Fisheries & Oceanography 
# Data set creator:  Noah Lottig - University of Wisconsin 
# Data set creator:  Martin Luger - Federal Agency for Water Management 
# Data set creator:  Stephen Maberly - UK Centre for Ecology & Hydrology 
# Data set creator:  Sally MacIntyre - University of California Santa Barbara 
# Data set creator:  Chris McBride - University of Waikato 
# Data set creator:  Peter McIntyre - University of Wisconsin 
# Data set creator:  Stephanie Melles - Ryerson University 
# Data set creator:  Beatriz Modenutti - University of Comahue: INIBIOMA, CONICET 
# Data set creator:  DÃ¶rthe MÃ¼ller-Navarra - University of Hamburg 
# Data set creator:  Laura Pacholski - Dominion Diamond Mines 
# Data set creator:  Andrew Paterson - Ontario Ministry of the Environment, Conservation and Parks 
# Data set creator:  Don Pierson - Uppsala University 
# Data set creator:  Helen Pislegina - Irkutsk State University 
# Data set creator:  Pierre-Denis Plisnier - University of LiÃ¨ge 
# Data set creator:  David Richardson - SUNY New Paltz 
# Data set creator:  Alon Rimmer - Israel Oceanographic and Limnological Research 
# Data set creator:  Michela Rogora - CNR Water Research institute 
# Data set creator:  Denis Rogozin - Krasnoyarsk Scientific Center SB RAS 
# Data set creator:  Jim Rusak - Ontario Ministry of the Environment, Conservation and Parks 
# Data set creator:  Olga Rusanovskaya - Irkutsk State University 
# Data set creator:  Steve Sadro - University of California Davis 
# Data set creator:  Nico Salmaso - Fondazione Edmund Mach 
# Data set creator:  Jasmine Saros - University of Maine 
# Data set creator:  Jouko Sarvala - University of Turku 
# Data set creator:  Ãmilie Saulnier-Talbot - UniversitÃ© Laval Departments of biology and geography 
# Data set creator:  Daniel Schindler - University of Washington 
# Data set creator:  Svetlana Shimaraeva - Irkutsk State University 
# Data set creator:  Eugene Silow - Irkutsk State University 
# Data set creator:  Lewis Sitoki - The Technical University of Kenya 
# Data set creator:  Ruben Sommaruga - University of Innsbruck 
# Data set creator:  Dietmar Straile - University of Konstanz 
# Data set creator:  Kristin Strock - Dickinson College 
# Data set creator:  Hilary Swain - Archbold Biological Station 
# Data set creator:  Jason Tallant - University of Michigan 
# Data set creator:  Wim Thiery - 1 Vrije Universiteit Brussel; 2 ETH Zurich 
# Data set creator:  Maxim Timofeyev - Irkutsk State University 
# Data set creator:  Alexander Tolomeev - Krasnoyarsk Scientific Center SB RAS 
# Data set creator:  Koji Tominaga - University of Oslo 
# Data set creator:  Michael Vanni - Miami University 
# Data set creator:  Piet Verburg - National Institute of Water & Atmospheric Research 
# Data set creator:  Rolf Vinebrooke - University of Alberta 
# Data set creator:  Josef WanzenbÃ¶ck - University of Insbruck 
# Data set creator:  Kathleen Weathers - SUNY New Paltz; Cary Institute 
# Data set creator:  Gesa Weyhenmeyer - Uppsala University 
# Data set creator:  Egor Zadereev - Krasnoyarsk Scientific Center SB RAS 
# Data set creator:  Tatyana Zhukova - Belarusian State University 
# Contact:  Rachel Pilla -  Miami University  - pillarm@miamioh.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/705/5/1c3181f05aab7420182a14851f916ef3") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
dt1 <-read_delim(infile1  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "SiteID",   
                   "LakeID",   
                   "LakeName",   
                   "Date",   
                   "Depth_m",   
                   "Temperature_degCelsius"   ), 
                 col_types=list(
                   col_number() , 
                   col_number() ,  
                   col_character(),  
                   col_date("%Y-%m-%d"),  
                   col_number() , 
                   col_number() ), 
                 na=c(" ",".","NA","")  )


# Convert Missing Values to NA for individual vectors 
dt1$Temperature_degCelsius <- ifelse((trimws(as.character(dt1$Temperature_degCelsius))==trimws("NA")),NA,dt1$Temperature_degCelsius)               
suppressWarnings(dt1$Temperature_degCelsius <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temperature_degCelsius))==as.character(as.numeric("NA"))),NA,dt1$Temperature_degCelsius))


# Observed issues when reading the data. An empty list is good!
problems(dt1) 
# Here is the structure of the input data tibble: 
glimpse(dt1) 
# And some statistical summaries of the data 
summary(dt1) 
# Get more details on character variables

summary(as.factor(dt1$LakeName)) 
infile2 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/705/5/1e5e05e46c3094d2d368a798eae22cbf") 
infile2 <-sub("^https","http",infile2)
# This creates a tibble named: dt2 
dt2 <-read_delim(infile2  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "SiteID",   
                   "LakeID",   
                   "LakeName",   
                   "Date",   
                   "Depth_m",   
                   "Temperature_degCelsius"   ), 
                 col_types=list(
                   col_number() , 
                   col_number() ,  
                   col_character(),  
                   col_date("%Y-%m-%d"),  
                   col_number() , 
                   col_number() ), 
                 na=c(" ",".","NA","")  )


# Convert Missing Values to NA for individual vectors 
dt2$Temperature_degCelsius <- ifelse((trimws(as.character(dt2$Temperature_degCelsius))==trimws("NA")),NA,dt2$Temperature_degCelsius)               
suppressWarnings(dt2$Temperature_degCelsius <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Temperature_degCelsius))==as.character(as.numeric("NA"))),NA,dt2$Temperature_degCelsius))


# Observed issues when reading the data. An empty list is good!
problems(dt2) 
# Here is the structure of the input data tibble: 
glimpse(dt2) 
# And some statistical summaries of the data 
summary(dt2) 
# Get more details on character variables

summary(as.factor(dt2$LakeName)) 
infile3 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/705/5/974aa15895f4d691edd7a9dcb0ab4457") 
infile3 <-sub("^https","http",infile3)
# This creates a tibble named: dt3 
dt3 <-read_delim(infile3  
                 ,delim=","   
                 ,skip=1 
                 ,quote='"'  
                 , col_names=c( 
                   "SiteID",   
                   "LakeID",   
                   "LakeName",   
                   "AlternateLakeName",   
                   "LakeOrReservoir",   
                   "CountryOfLake",   
                   "Region",   
                   "Latitude",   
                   "Longitude",   
                   "Elevation_m",   
                   "SurfaceArea_km2",   
                   "Volume_km3",   
                   "MaxDepth_m",   
                   "MeanDepth_m",   
                   "Secchi_m",   
                   "Chlorophyll_ug_L",   
                   "TotalPhosphorus_ug_L",   
                   "DissolvedOrganicCarbon_mg_L",   
                   "Contributor",   
                   "ContributorContact",   
                   "ContributorInstitution"   ), 
                 col_types=list(
                   col_number() , 
                   col_number() ,  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(),  
                   col_character(), 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() , 
                   col_number() ,  
                   col_character(),  
                   col_character(),  
                   col_character()), 
                 na=c(" ",".","NA","")  )


# Convert Missing Values to NA for individual vectors 
dt3$Elevation_m <- ifelse((trimws(as.character(dt3$Elevation_m))==trimws("NA")),NA,dt3$Elevation_m)               
suppressWarnings(dt3$Elevation_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Elevation_m))==as.character(as.numeric("NA"))),NA,dt3$Elevation_m))
dt3$SurfaceArea_km2 <- ifelse((trimws(as.character(dt3$SurfaceArea_km2))==trimws("NA")),NA,dt3$SurfaceArea_km2)               
suppressWarnings(dt3$SurfaceArea_km2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$SurfaceArea_km2))==as.character(as.numeric("NA"))),NA,dt3$SurfaceArea_km2))
dt3$Volume_km3 <- ifelse((trimws(as.character(dt3$Volume_km3))==trimws("NA")),NA,dt3$Volume_km3)               
suppressWarnings(dt3$Volume_km3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Volume_km3))==as.character(as.numeric("NA"))),NA,dt3$Volume_km3))
dt3$MaxDepth_m <- ifelse((trimws(as.character(dt3$MaxDepth_m))==trimws("NA")),NA,dt3$MaxDepth_m)               
suppressWarnings(dt3$MaxDepth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$MaxDepth_m))==as.character(as.numeric("NA"))),NA,dt3$MaxDepth_m))
dt3$MeanDepth_m <- ifelse((trimws(as.character(dt3$MeanDepth_m))==trimws("NA")),NA,dt3$MeanDepth_m)               
suppressWarnings(dt3$MeanDepth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$MeanDepth_m))==as.character(as.numeric("NA"))),NA,dt3$MeanDepth_m))
dt3$Secchi_m <- ifelse((trimws(as.character(dt3$Secchi_m))==trimws("NA")),NA,dt3$Secchi_m)               
suppressWarnings(dt3$Secchi_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Secchi_m))==as.character(as.numeric("NA"))),NA,dt3$Secchi_m))
dt3$Chlorophyll_ug_L <- ifelse((trimws(as.character(dt3$Chlorophyll_ug_L))==trimws("NA")),NA,dt3$Chlorophyll_ug_L)               
suppressWarnings(dt3$Chlorophyll_ug_L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Chlorophyll_ug_L))==as.character(as.numeric("NA"))),NA,dt3$Chlorophyll_ug_L))
dt3$TotalPhosphorus_ug_L <- ifelse((trimws(as.character(dt3$TotalPhosphorus_ug_L))==trimws("NA")),NA,dt3$TotalPhosphorus_ug_L)               
suppressWarnings(dt3$TotalPhosphorus_ug_L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$TotalPhosphorus_ug_L))==as.character(as.numeric("NA"))),NA,dt3$TotalPhosphorus_ug_L))
dt3$DissolvedOrganicCarbon_mg_L <- ifelse((trimws(as.character(dt3$DissolvedOrganicCarbon_mg_L))==trimws("NA")),NA,dt3$DissolvedOrganicCarbon_mg_L)               
suppressWarnings(dt3$DissolvedOrganicCarbon_mg_L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$DissolvedOrganicCarbon_mg_L))==as.character(as.numeric("NA"))),NA,dt3$DissolvedOrganicCarbon_mg_L))


# Observed issues when reading the data. An empty list is good!
problems(dt3) 
# Here is the structure of the input data tibble: 
glimpse(dt3) 
# And some statistical summaries of the data 
summary(dt3) 
# Get more details on character variables

summary(as.factor(dt3$LakeName)) 
summary(as.factor(dt3$AlternateLakeName)) 
summary(as.factor(dt3$LakeOrReservoir)) 
summary(as.factor(dt3$CountryOfLake)) 
summary(as.factor(dt3$Region)) 
summary(as.factor(dt3$Contributor)) 
summary(as.factor(dt3$ContributorContact)) 
summary(as.factor(dt3$ContributorInstitution))
