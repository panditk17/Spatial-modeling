# code to create fishnet and aggregate points within 
# each grid cells useful for spatial analysis

setwd("C:/Karuns_documents/pine_disease/survival_fishnet")

rm(list=ls())

library(dplyr)
library(reshape)
library(ggplot2)

library(dplyr)
library(reshape)
library(ggplot2)
library(USAboundaries)
library(sf)
library(gridExtra)
library(rgdal)
library(sp)
library(raster)

# Create a list of states for the analysis
# st_list = c("Alabama", "Arkansas","Florida",
#             "Georgia", "Kentucky", "Louisiana",
#              "Mississippi", "North Carolina", "Oklahoma", 
#             "South Carolina", "Tennessee",
#             "Texas", "Virginia")

st_list = c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida",
            "Georgia", "Illinois", "Indiana",
            "Iowa", "Kansas", "Kentucky", "Louisiana",
            "Maine", "Maryland","Massachusetts",  "Michigan",
            "Minnesota", "Mississippi", "Missouri","Nebraska",
            "New Hampshire","New Jersey",
            "New York", "North Carolina", "North Dakota",
            "Ohio", "Oklahoma", "Pennsylvania",
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
            "Texas", "Vermont","Virginia",
            "West Virginia", "Wisconsin")
# 
# 
# devtools::install_github("ropensci/USAboundaries")
# devtools::install_github("ropensci/USAboundariesData")


states_contemporary <- us_states(states=st_list)
plot(st_geometry(states_contemporary))
title("Southeastern US")

ext1<-extent(states_contemporary)
proj1<-crs(states_contemporary)

proj_latlon="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

 proj3<-"+proj=laea +lat_0=0 +lon_0=-80"
# 
 map <- st_transform(states_contemporary, proj3)
# plot(st_geometry(map))

proj4<-crs(map)

point_data=read.csv("data/fia_coord1.csv")  

pixelsize=100000


source("codes/aggregate_fishnet_fn.R")

aggregate_fishnet_fn(point_data,map,pixelsize)

