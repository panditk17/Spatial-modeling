#******************************************************************
#Codes to read raster grid with height information, select the pixels 
#with vegetation, list the heights for selected pixels, produce csv
#file with vegetation height in one column and type of PFT in another
#Written by K. Pandit (2017)
#*******************************************************************

#Install required r packages
# install.packages("raster")
# install.packages("rgdal")
#install.packages("sp")
#install.packages(rhdf5)
#install.packages(ncdf4)


# remove previous files
rm(list = ls())

#load required libraries for the code
library(rhdf5)
#library(ncdf4)
#library(h5)

library(raster)
library(rgdal)
library(sp)
library(s4vd)

#set the working directory
setwd ("H:/Lidar")

#read the raster with heights
r <- raster("trial")


#specify total number of pixels in height raster map
wnr = 16 # total number of rows
wnc = 16   #total number of columns
totcell = wnr*wnc #total number of cells

#specify number of polygons for each row/column
div = 2 # number of divisions within x or y

#number of total polygons
totpol = div*div

#initialize count of polygons with 1
k = 1

#calculate number of rows and columns for individual polygon
nr = wnr/div # number of rows of a polygon
nc = wnc/div # number of columsn of a polygon

#Run for loop to read raster in each polygon, reclassify and
#overlay to produce height raster for selected pixels 

for (i in seq(1,wnr, by =nr)) {

  for (j in seq(1,wnc, by =nc)) {

    
      #name for each polygon to be created
      f <- paste0("poly",k) 
    
      # name for each reclassified polygon  
      g <- paste0("rcpoly",k)
      
      #name for polygons with only selected heights
      h <- paste0("htpoly",k)

      
#select cells within a polygon defined by rows and columns      
b <- getValuesBlock(r, row = i, nrows = nr, col = j, ncols= nc)

#create matrix of selected cells 
c<- matrix(b,nrow=nr,ncol=nc,byrow = TRUE)

#forcing matrix data into a raster
c<- raster(c)

#write height polgyons
writeRaster(c,filename = f, overwrite = TRUE)

#reclassify raster to get ones for selected cells
rc<- reclassify(c, c(-Inf,0.3,NA,0.3,1.5,1,1.5,Inf,NA))

#write reclassified raster
writeRaster(rc,filename = g, overwrite = TRUE)

# overlay reclassified raster with height raster
# to get heights for selected pixels only

ht<- c*rc

#write raster file for selected heights only
writeRaster(ht,filename = h, overwrite = TRUE)

# increase count for next polygon
k = k+1

  }

  #End of for loop
} 


#Change coordinate from utm to latlong

#import a grid with latlong projection
grid_hol<-shapefile("latlong_grid_holister")

#aggregate raster to number of polygons
agg1 <- aggregate(r, nc)

#change projection of aggregated raster to latlong
agg_latlon<-projectRaster(agg1,crs=projection(grid_hol))

#extract number of cells to get latlong
forcord<-seq(from=1,to=ncell(agg_latlon),by=1)

#get latitutde and longitude for the polygons
lon1<-xFromCell(agg_latlon, c(forcord))
lat1<-yFromCell(agg_latlon, c(forcord))

#round off lat and long within 4 decimals
lon<-round(lon1,4)
lat<-round(lat1,4)


#Create csv files with height and PFT

#count for filename 
u = 1

# for loop to read height raster and produce csv files
for (u in 1:totpol) {
 
# define input and output filenames
  
s <- paste0("htpoly",u,".gri") # name of input raster file  

#write csv file name
q <- paste0("holi.lat",lat[u],"lon",lon[u],".css")  # name of output csv file

 
#write csv file to represent patches in the polygons
p <- paste0("holi.lat",lat[u],"lon",lon[u],".csv")  # name of output csv file


#t <- paste0("poly",u,".csv")  # name of output csv file
#t <- paste0("poly-g0",u,".h5")  # name of output csv file

#read height raster
mm <- raster(s)

#convert raster file to vector format
zz <-as.vector(mm)

#remove "NA" codes from the data
xx <- na.omit(zz)

# ............... height equation can be included here
# to get shrub height based on field measurements

#Define length of xx file
v= length(xx)

#create new vector with the dimensions from previous file
#give PFT number for the new vector

PFT<-rep(17,v)

dbh<-rep(0,v)
time<-rep("2014",v)
patch<-rep("A",v)
cohort<-seq(from=1,to=v,by=1)
n<-rep(0.027,v)
bdead<-rep(0,v)
balive<-rep(0,v)
avgRg<-rep(0,v)

#merge vectors xx (with height invormation) and vec1 (with PFT numbers)
both = cbind(time, patch, cohort, dbh, xx, PFT, n, bdead, balive, avgRg)

#provide column names for two columns 
colnames(both)<- c("time","patch","cohort","dbh","hite","pft","n","bdead","balive","avgRg")

#write csv file 
write.table(both, file = q, col.names=TRUE, row.names=FALSE,sep = ",")

time<-rep("2014",1)
patch<-rep("A",1)
trk<-rep(2,1)
age<-rep(20,1)
area<-rep(1,1)
water<-rep(0,1)
fsc<-rep(0,1)
stsc<-rep(0,1)
stsl<-rep(0,1)
ssc<-rep(0,1)
psc<-rep(0,1)
msn<-rep(0,1)
fsn<-rep(0,1)

p_all = cbind(time, patch, trk,age,area,water,fsc,stsc,stsl,ssc,psc,msn,fsn)


#provide column names for two columns 
colnames(p_all)<- c("time","patch","trk","age","area","water","fsc","stsc","stsl","ssc","psc","msn","fsn")

#write csv file 
write.table(p_all, file = p, col.names=TRUE, row.names=FALSE,sep = ",")


#h5createFile(t)

#h5createDataset(t, "dbh", c(v,1,1))
#h5createDataset(t, "hite", c(v,1,1))
#h5createDataset(t, "PFT", c(v,1,1))
#h5createDataset(t, "n", c(v,1,1))

#h5write(dbh, file = t, name = "/dbh")
#h5write(xx, file = t, name = "/hite")
#h5write(PFT, file = t, name = "/PFT")
#h5write(n, file = t, name = "/n")


#increase the count of polygons
u = u+1

#end of for loop
}

               