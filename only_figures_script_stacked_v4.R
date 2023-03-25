# code to identify repeated measurements (trees and plots) using FIA data
# with fusiform rust from 2013 to 2019 in Southern US
# K. Pandit

# read the climate files from the directory

setwd("C:/Karuns_documents/pine_disease/fusiform_hotspots")

rm(list=ls())

do.call(file.remove, 
        list(list.files("C:/Karuns_documents/pine_disease/fusiform_hotspots/temp_climate_data", 
                        full.names = TRUE)))
# activate libraries required for the analysis
library(dplyr)
library(reshape)
library(ggplot2)
library(USAboundaries)
library(sf)
library(gridExtra)
library(rgdal)
library(sp)
library(raster)



# 
# # # read RCS file with tree data 
# raw_data<-readRDS("../data/FIA_all_fusiform_form_2013_2020.RDS")
# 
# source("codes/fusiform_data_to_plot_summary_fn.R")
# 
# fusiform_data_to_plot_summary_fn(raw_data)

# install.packages("USAboundariesData", repos = "https://ropensci.r-universe.dev", type = "source")

# install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')


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
title("Eastern US")

ext1<-extent(states_contemporary)
proj1<-crs(states_contemporary)


proj3<-"+proj=laea +lat_0=0 +lon_0=-80"

map <- st_transform(states_contemporary, proj3)
plot(st_geometry(map))

proj4<-crs(map)

# type_list<-c("longleaf")


type_list<-c("loblolly")
orig_list<-c(0,1)
pixelsize=30000
scenario_list<-c(4.5)

# vars<-c("pr")
vars<-c("pr","vpd","tasmax","tasmin","PotEvap","huss")


date1<-c(2030,2050,2070,2090)
date2<-c(2039,2059,2079,2099)

date3<-data.frame(date1,date2)

for(scenario in scenario_list){

source("codes_v1/future_prediction_stacked_v3_fn.R")

for (origincd in orig_list){
  if (origincd==0) {orgcd="natural"}
  
  if (origincd==1) {orgcd="plant"} 
  
  for (type in type_list){

future_prediction_stacked_v3_fn(date3,map,range_map,type,orgcd,start_yr,end_yr,pixelsize,scenario)
# 
# do.call(file.remove,
# list(list.files("C:/Karuns_documents/pine_disease/fusiform_hotspots/temp_climate_data",
#                 full.names = TRUE)))
}
  }

}
