# function to create fishnet and aggregate by each grid
## updated 04-17-2023


aggregate_fishnet_fn<-function(point_data,map,pixelsize) {


# read RDS file with tree data 
data<-point_data
  
  # read plot number, longitude and latitude from the data

lob_nat<-data
lob_nat2<-lob_nat[c("plot","long","lat")]
lob_nat2$presence<-1
coordinates(lob_nat2)<-~long+lat

projlatlon<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
proj4string(lob_nat2)=CRS(projlatlon) # set it to lat-long


lob_nat3 = spTransform(lob_nat2,proj4)


## identify the projection and change it
projection(lob_nat3)
extent(lob_nat3)

ccc<-projection(map)

fishnet.r <- raster(extent(map))
res(fishnet.r) <- pixelsize

proj4string(fishnet.r) <- CRS("+proj=laea +lat_0=0 +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

fishnet <- rasterToPolygons(fishnet.r)

plot(fishnet)


fishnet@data$id<-as.numeric(rownames(fishnet@data))

fishnet_latlon<-spTransform(fishnet, proj_latlon)

centroid_latlon <- as.data.frame(coordinates(fishnet_latlon))
centroid_proj <- as.data.frame(coordinates(fishnet))

centroid_both<-merge(centroid_latlon,centroid_proj, by="row.names")

# get centroid of fishnet grids
# 
# centroid_id <- as.data.frame(fishnet_latlon@data$id)
# 
# centroid2<-merge(centroid_id,centroid_latlon, by="row.names")
# 
# fishnet_latlon@data<-st_join(fishnet_latlon,centroid_latlon)

fishnet@data <- merge(x=fishnet@data,y=centroid_both,by.x="id",
                          by.y="Row.names", all.x=TRUE)


# fishnet2<-st_as_sf(fishnet)
# plot(fishnet2)
# 
# 
# 
# points_new<-data.frame(identify(lob_nat3,fishnet2))


# pnts <- lob_nat3 %>% mutate(
#   intersection = as.integer(st_intersects(geometry, fishnet2))
#   , area = if_else(is.na(intersection), '', fishnet2$id[intersection])
# ) 


point_dd<-data.frame(lob_nat3)
points_nn<-over(lob_nat3,fishnet)
points_id<-data.frame(lob_nat3$plot)
points2<-merge(points_id,points_nn,by="row.names")

points3<-points2[!is.na(points2$id),]

points4<-cbind(points3[,1:3],points3[,5:8])

colnames(points4)<-c("rowid","plot","fishnetid","fishlong1","fishlat1",
                     "fishlong2","fishlat2")


write.csv(points4,"data_point_fishnet.csv")


}
