future_prediction_stacked_v3_fn<-function(date3,map,range_map,type,orgcd,start_yr,end_yr,pixelsize,scenario) {

setwd("C:/Karuns_documents/pine_disease/fusiform_hotspots")

 ############################
  ## read range map
   
  ncol_date<-ncol(date3)
  

  range_n<- paste0("range_data/range_",type,"_litt.shp")
  range_n2<- st_read(range_n)
  
  range_map <- st_transform(range_n2, proj3)
  
 
  #################################
  ############ figure 1

  setwd("./output")
  
  # for (type)
  type="loblolly"
  orgcd="natural"
  
  filein<-paste0(type,orgcd)
  lll<-paste0("pred",filein,"_",scenario)
  filenames <- list.files(pattern=paste0(lll,"+.*csv"))
  
  ##Create list of data frame names without the ".csv" part 
  names <-substr(filenames,1,36)
  
  if (exists("dataset")){rm(dataset)}
  
  for (data in filenames){
    
    # Create the first data if no data exist yet
    if (!exists("dataset")){
      dataset <- read.csv(data, header=TRUE)
    }
    
    # if data already exist, then append it together
    if (exists("dataset")){
      tempory <-read.csv(data, header=TRUE)
      dataset <-unique(cbind(dataset, tempory))
      rm(tempory)
    }
  }
  
  
  
  
  col_len8<- ncol(dataset)
  df1<-dataset[c(2,seq(3,col_len8,3))]
  df2<-subset(df1, select = -c(3) )
  
  current_hot<-read.csv(paste0("whole_fishnet",filein,"_climate.csv"))
  ncol_hot<-dim(current_hot)[2]
  current_hot1<-current_hot[c(2,ncol_hot)]
  current_hot2<-na.omit(current_hot1)
  
  # ppp<-colnames(df)[2]
  
  predicted<-merge(current_hot1,df2,by.x="id",by.y="id",all=TRUE)
  names(predicted)[2]<-"current"
  
  
  setwd("../")
  
  
  ncol_6<-ncol(predicted)  
  
  library(animation)
  library(gganimate)
  # toSave<- list(
  
  
  plot_list1 = list()
  
  
  for(jkl in seq(2,ncol_6,by=1)) {
    rm(fishnetread)
    # jkl=3   
    fishnetread<-readOGR("figure/fishnet_with_data.shp")
    # jkl=3
    pred_hot_rf1<-predicted[c(1,jkl)]
    
    # colnames(pred_hot_rf1)<-c("id","predicted")
    
    fishnetread@data <- merge(x=fishnetread@data,y=pred_hot_rf1,by.x="id",
                              by.y="id", all.x=TRUE)
    
    
    fishnet_pred<-st_as_sf(fishnetread)
    
    mmm<-st_union(map)
    # proj5<-crs(map)
    # 
    # map <- st_transform(fishnet_pred, proj5)
    
    oo<-st_intersection(fishnet_pred,mmm)
    
    breaks2 = c(-20, -2.58,-1.96, -1.65,1.65, 1.96, 2.58, 20)
    palette2=c("vvl", "vl", "l", "n", "h",
               "vh","vvh")
    
    # if(jkl==2) 
    qqq<-ncol(oo)-1
    jjj<-names(oo[qqq])[1]
    
    oo$class = palette2[cut(oo[,jjj,drop=TRUE], breaks2)]
    
    oo=oo[!is.na(oo$class),]
    
    grid<-pixelsize/1000
    
    
    
    if (orgcd=="natural") {orgcd1="natural"}
    
    if (orgcd=="plant") {orgcd1="plantation"} 
    
    if(jjj=="current"){kkk="Current hotspot"}
    if(jjj=="pred_2030.2039"){kkk="predicted:2030-2039"}
    if(jjj=="pred_2050.2059"){kkk="predicted:2050-2059"}
    if(jjj=="pred_2070.2079"){kkk="predicted:2070-2079"}
    if(jjj=="pred_2090.2099"){kkk="predicted:2090-2099"}
    
    
    figure1<-paste0("figure/panel_hotspots_",type,orgcd1,"_",scenario,"_",jjj,"_",grid,".png")
    figure2<-paste0(type,"-",orgcd1)
    figure3<-paste0(kkk)
    
    
    p<-ggplot() +
      geom_sf(data = oo, aes(fill = class ),color=NA,na.rm=TRUE)+ 
      scale_fill_manual(values=c("vvl"="navy","vl"="royalblue","l"="skyblue2",
                                 "n"="khaki1", "h"="salmon1","vh"="red2",
                                 "vvh"="red4"),
                        labels=c("vvl"="Coldspot - 99% significant",
                                 "vl"="Coldspot - 95% significant",
                                 "l"="Coldspot - 90% significant",
                                 "n"="Non significant",
                                 "h"="Hotspot - 90% significant",
                                 "vh"="Hotspot - 95% significant",
                                 "vvh"="Hotspot - 99% significant",
                                 "NA"="NA"))+
      geom_sf(data=map,alpha=0.1,color="gray55") +
      theme(panel.grid = element_line(size = 2*.pt)) +
      geom_sf(data=range_map,color="gray3",alpha=0.15,lwd=0.8)+
      
      labs(title=figure2,subtitle=figure3)+
      # 
      #    
      #   ggtitle(figure2)+
      theme_bw()+
      theme(axis.text.x = element_text(size=34),
            axis.text.y = element_text(size=34),
            plot.title = element_text(size = 40, hjust = 0.5),
            plot.subtitle = element_text(size = 36, hjust = 0.5),
            axis.title.y = element_text(size = 34),
            axis.title.x=element_text(size=34),
            legend.title=element_text(size=34),
            legend.text = element_text(size=34),
            legend.position="")+
      theme( panel.grid.major = element_line(size = 0.1, linetype = 'twodash',
                                             colour = "gray55"))
    
    plot_list1[[jkl]] = p
    
  }
  
  # gg_animate(toSave)
  ani.options(ani.height=1200,ani.width=1500,interval=1)
  fileout<-paste0("animation_1",filein,"_",scenario,"_",grid)
  
  setwd("./figure/")
  saveGIF(
    {lapply(plot_list1, print)}
    , paste0(fileout,".gif"))
  

  
  library(magick)

  
  
  plot_list1a<-image_read(paste0(fileout,".gif"))
  
setwd("../")  
    
################################

setwd("./output")

# for (type)
type="loblolly"
orgcd="plant"

filein<-paste0(type,orgcd)
lll<-paste0("pred",filein,"_",scenario)
filenames <- list.files(pattern=paste0(lll,"+.*csv"))

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,36)

if (exists("dataset")){rm(dataset)}

for (data in filenames){
  
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read.csv(data, header=TRUE)
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read.csv(data, header=TRUE)
    dataset <-unique(cbind(dataset, tempory))
    rm(tempory)
  }
}




col_len8<- ncol(dataset)
df1<-dataset[c(2,seq(3,col_len8,3))]
df2<-subset(df1, select = -c(3) )

current_hot<-read.csv(paste0("whole_fishnet",filein,"_climate.csv"))
ncol_hot<-dim(current_hot)[2]
current_hot1<-current_hot[c(2,ncol_hot)]
current_hot2<-na.omit(current_hot1)

# ppp<-colnames(df)[2]

predicted<-merge(current_hot1,df2,by.x="id",by.y="id",all=TRUE)
names(predicted)[2]<-"current"


setwd("../")


ncol_6<-ncol(predicted)  

library(animation)
library(gganimate)
# toSave<- list(


plot_list2 = list()


for(jkl in seq(2,ncol_6,by=1)) {
  rm(fishnetread)
  # jkl=3   
  fishnetread<-readOGR("figure/fishnet_with_data.shp")
  # jkl=3
  pred_hot_rf1<-predicted[c(1,jkl)]
  
  # colnames(pred_hot_rf1)<-c("id","predicted")
  
  fishnetread@data <- merge(x=fishnetread@data,y=pred_hot_rf1,by.x="id",
                            by.y="id", all.x=TRUE)
  
  
  fishnet_pred<-st_as_sf(fishnetread)
  
  mmm<-st_union(map)
  # proj5<-crs(map)
  # 
  # map <- st_transform(fishnet_pred, proj5)
  
  oo<-st_intersection(fishnet_pred,mmm)
  
  breaks2 = c(-20, -2.58,-1.96, -1.65,1.65, 1.96, 2.58, 20)
  palette2=c("vvl", "vl", "l", "n", "h",
             "vh","vvh")
  
  # if(jkl==2) 
  qqq<-ncol(oo)-1
  jjj<-names(oo[qqq])[1]
  
  oo$class = palette2[cut(oo[,jjj,drop=TRUE], breaks2)]
  
  oo=oo[!is.na(oo$class),]
  
  grid<-pixelsize/1000
  

  
  if (orgcd=="natural") {orgcd1="natural"}
  
  if (orgcd=="plant") {orgcd1="plantation"} 
  
  if(jjj=="current"){kkk="Current hotspot"}
  if(jjj=="pred_2030.2039"){kkk="predicted:2030-2039"}
  if(jjj=="pred_2050.2059"){kkk="predicted:2050-2059"}
  if(jjj=="pred_2070.2079"){kkk="predicted:2070-2079"}
  if(jjj=="pred_2090.2099"){kkk="predicted:2090-2099"}
  
  
  figure1<-paste0("figure/panel_hotspots_",type,orgcd1,"_",scenario,"_",jjj,"_",grid,".png")
  figure2<-paste0(type,"-",orgcd1)
  figure3<-paste0(kkk)
  
  
  q<-ggplot() +
    geom_sf(data = oo, aes(fill = class ),color=NA,na.rm=TRUE)+ 
    scale_fill_manual(values=c("vvl"="navy","vl"="royalblue","l"="skyblue2",
                               "n"="khaki1", "h"="salmon1","vh"="red2",
                               "vvh"="red4"),
                      labels=c("vvl"="Coldspot - 99% significant",
                               "vl"="Coldspot - 95% significant",
                               "l"="Coldspot - 90% significant",
                               "n"="Non significant",
                               "h"="Hotspot - 90% significant",
                               "vh"="Hotspot - 95% significant",
                               "vvh"="Hotspot - 99% significant",
                               "NA"="NA"))+
    geom_sf(data=map,alpha=0.1,color="gray55") +
    theme(panel.grid = element_line(size = 2*.pt)) +
    geom_sf(data=range_map,color="gray3",alpha=0.15,lwd=0.8)+
    
    labs(title=figure2,subtitle=figure3)+
    # 
    #    
    #   ggtitle(figure2)+
    theme_bw()+
    theme(axis.text.x = element_text(size=34),
          axis.text.y = element_text(size=34),
          plot.title = element_text(size = 40, hjust = 0.5),
          plot.subtitle = element_text(size = 36, hjust = 0.5),
          axis.title.y = element_text(size = 34),
          axis.title.x=element_text(size=34),
          legend.title=element_text(size=34),
          legend.text = element_text(size=34),
          legend.position="")+
    theme( panel.grid.major = element_line(size = 0.1, linetype = 'twodash',
                                           colour = "gray55"))
  
  plot_list2[[jkl]] = q
  
}

# gg_animate(toSave)
ani.options(ani.height=1200,ani.width=1500,interval=1)
fileout<-paste0("animation_1",filein,"_",scenario,"_",grid)

setwd("./figure/")
saveGIF(
  {lapply(plot_list2, print)}
  , paste0(fileout,".gif"))

# setwd("../")


# new_gif<-image_append(c(plot_list[1], plot_list2[1]))
# 
# library(magick)
# setwd("./figure/")



plot_list2a<-image_read(paste0(fileout,".gif"))

# 
# 
# new_gif<-image_append(c(plot_lista[1], plot_list2a[1]))
# for(i in 2:4){
#   combined <- image_append(c(plot_lista[i], plot_list2a[i]))
#   new_gif<-c(new_gif,combined)
# }
# 
# new_gif
# 
# library(gifski)
# image_write_gif(new_gif,delay=0.4,"new_gif.gif")


setwd("../")
################################

setwd("./output")

# for (type)
type="slash"
orgcd="natural"

filein<-paste0(type,orgcd)
lll<-paste0("pred",filein,"_",scenario)
filenames <- list.files(pattern=paste0(lll,"+.*csv"))

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,36)

if (exists("dataset")){rm(dataset)}

for (data in filenames){
  
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read.csv(data, header=TRUE)
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read.csv(data, header=TRUE)
    dataset <-unique(cbind(dataset, tempory))
    rm(tempory)
  }
}




col_len8<- ncol(dataset)
df1<-dataset[c(2,seq(3,col_len8,3))]
df2<-subset(df1, select = -c(3) )

current_hot<-read.csv(paste0("whole_fishnet",filein,"_climate.csv"))
ncol_hot<-dim(current_hot)[2]
current_hot1<-current_hot[c(2,ncol_hot)]
current_hot2<-na.omit(current_hot1)

# ppp<-colnames(df)[2]

predicted<-merge(current_hot1,df2,by.x="id",by.y="id",all=TRUE)
names(predicted)[2]<-"current"


setwd("../")


ncol_6<-ncol(predicted)  

library(animation)
library(gganimate)
# toSave<- list(


plot_list3 = list()


for(jkl in seq(2,ncol_6,by=1)) {
  rm(fishnetread)
  # jkl=3   
  fishnetread<-readOGR("figure/fishnet_with_data.shp")
  # jkl=3
  pred_hot_rf1<-predicted[c(1,jkl)]
  
  # colnames(pred_hot_rf1)<-c("id","predicted")
  
  fishnetread@data <- merge(x=fishnetread@data,y=pred_hot_rf1,by.x="id",
                            by.y="id", all.x=TRUE)
  
  
  fishnet_pred<-st_as_sf(fishnetread)
  
  mmm<-st_union(map)
  # proj5<-crs(map)
  # 
  # map <- st_transform(fishnet_pred, proj5)
  
  oo<-st_intersection(fishnet_pred,mmm)
  
  breaks2 = c(-20, -2.58,-1.96, -1.65,1.65, 1.96, 2.58, 20)
  palette2=c("vvl", "vl", "l", "n", "h",
             "vh","vvh")
  
  # if(jkl==2) 
  qqq<-ncol(oo)-1
  jjj<-names(oo[qqq])[1]
  
  oo$class = palette2[cut(oo[,jjj,drop=TRUE], breaks2)]
  
  oo=oo[!is.na(oo$class),]
  
  grid<-pixelsize/1000
  
  
  
  if (orgcd=="natural") {orgcd1="natural"}
  
  if (orgcd=="plant") {orgcd1="plantation"} 
  
  if(jjj=="current"){kkk="Current hotspot"}
  if(jjj=="pred_2030.2039"){kkk="predicted:2030-2039"}
  if(jjj=="pred_2050.2059"){kkk="predicted:2050-2059"}
  if(jjj=="pred_2070.2079"){kkk="predicted:2070-2079"}
  if(jjj=="pred_2090.2099"){kkk="predicted:2090-2099"}
  
  
  figure1<-paste0("figure/panel_hotspots_",type,orgcd1,"_",scenario,"_",jjj,"_",grid,".png")
  figure2<-paste0(type,"-",orgcd1)
  figure3<-paste0(kkk)
  
  
  r<-ggplot() +
    geom_sf(data = oo, aes(fill = class ),color=NA,na.rm=TRUE)+ 
    scale_fill_manual(values=c("vvl"="navy","vl"="royalblue","l"="skyblue2",
                               "n"="khaki1", "h"="salmon1","vh"="red2",
                               "vvh"="red4"),
                      labels=c("vvl"="Coldspot - 99% significant",
                               "vl"="Coldspot - 95% significant",
                               "l"="Coldspot - 90% significant",
                               "n"="Non significant",
                               "h"="Hotspot - 90% significant",
                               "vh"="Hotspot - 95% significant",
                               "vvh"="Hotspot - 99% significant",
                               "NA"="NA"))+
    geom_sf(data=map,alpha=0.1,color="gray55") +
    theme(panel.grid = element_line(size = 2*.pt)) +
    geom_sf(data=range_map,color="gray3",alpha=0.15,lwd=0.8)+
    
    labs(title=figure2,subtitle=figure3)+
    # 
    #    
    #   ggtitle(figure2)+
    theme_bw()+
    theme(axis.text.x = element_text(size=34),
          axis.text.y = element_text(size=34),
          plot.title = element_text(size = 40, hjust = 0.5),
          plot.subtitle = element_text(size = 36, hjust = 0.5),
          axis.title.y = element_text(size = 34),
          axis.title.x=element_text(size=34),
          legend.title=element_text(size=34),
          legend.text = element_text(size=34),
          legend.position="")+
    theme( panel.grid.major = element_line(size = 0.1, linetype = 'twodash',
                                           colour = "gray55"))
  
  plot_list3[[jkl]] = r
  
}

# gg_animate(toSave)
ani.options(ani.height=1200,ani.width=1500,interval=1)
fileout<-paste0("animation_1",filein,"_",scenario,"_",grid)

setwd("./figure/")
saveGIF(
  {lapply(plot_list3, print)}
  , paste0(fileout,".gif"))

# setwd("../")


# new_gif<-image_append(c(plot_list[1], plot_list2[1]))

library(magick)
# setwd("./figure/")

plot_list3a<-image_read(paste0(fileout,".gif"))


setwd("../")
############################## 


setwd("./output")

# for (type)
type="slash"
orgcd="plant"

filein<-paste0(type,orgcd)
lll<-paste0("pred",filein,"_",scenario)
filenames <- list.files(pattern=paste0(lll,"+.*csv"))

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,36)

if (exists("dataset")){rm(dataset)}

for (data in filenames){
  
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read.csv(data, header=TRUE)
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read.csv(data, header=TRUE)
    dataset <-unique(cbind(dataset, tempory))
    rm(tempory)
  }
}




col_len8<- ncol(dataset)
df1<-dataset[c(2,seq(3,col_len8,3))]
df2<-subset(df1, select = -c(3) )

current_hot<-read.csv(paste0("whole_fishnet",filein,"_climate.csv"))
ncol_hot<-dim(current_hot)[2]
current_hot1<-current_hot[c(2,ncol_hot)]
current_hot2<-na.omit(current_hot1)

# ppp<-colnames(df)[2]

predicted<-merge(current_hot1,df2,by.x="id",by.y="id",all=TRUE)
names(predicted)[2]<-"current"


setwd("../")


ncol_6<-ncol(predicted)  

library(animation)
library(gganimate)
# toSave<- list(


plot_list4 = list()


for(jkl in seq(2,ncol_6,by=1)) {
  rm(fishnetread)
  # jkl=3   
  fishnetread<-readOGR("figure/fishnet_with_data.shp")
  # jkl=3
  pred_hot_rf1<-predicted[c(1,jkl)]
  
  # colnames(pred_hot_rf1)<-c("id","predicted")
  
  fishnetread@data <- merge(x=fishnetread@data,y=pred_hot_rf1,by.x="id",
                            by.y="id", all.x=TRUE)
  
  
  fishnet_pred<-st_as_sf(fishnetread)
  
  mmm<-st_union(map)
  # proj5<-crs(map)
  # 
  # map <- st_transform(fishnet_pred, proj5)
  
  oo<-st_intersection(fishnet_pred,mmm)
  
  breaks2 = c(-20, -2.58,-1.96, -1.65,1.65, 1.96, 2.58, 20)
  palette2=c("vvl", "vl", "l", "n", "h",
             "vh","vvh")
  
  # if(jkl==2) 
  qqq<-ncol(oo)-1
  jjj<-names(oo[qqq])[1]
  
  oo$class = palette2[cut(oo[,jjj,drop=TRUE], breaks2)]
  
  oo=oo[!is.na(oo$class),]
  
  grid<-pixelsize/1000
  
  
  
  if (orgcd=="natural") {orgcd1="natural"}
  
  if (orgcd=="plant") {orgcd1="plantation"} 
  
  if(jjj=="current"){kkk="Current hotspot"}
  if(jjj=="pred_2030.2039"){kkk="predicted:2030-2039"}
  if(jjj=="pred_2050.2059"){kkk="predicted:2050-2059"}
  if(jjj=="pred_2070.2079"){kkk="predicted:2070-2079"}
  if(jjj=="pred_2090.2099"){kkk="predicted:2090-2099"}
  
  
  figure1<-paste0("figure/panel_hotspots_",type,orgcd1,"_",scenario,"_",jjj,"_",grid,".png")
  figure2<-paste0(type,"-",orgcd1)
  figure3<-paste0(kkk)
  
  
  s<-ggplot() +
    geom_sf(data = oo, aes(fill = class ),color=NA,na.rm=TRUE)+ 
    scale_fill_manual(values=c("vvl"="navy","vl"="royalblue","l"="skyblue2",
                               "n"="khaki1", "h"="salmon1","vh"="red2",
                               "vvh"="red4"),
                      labels=c("vvl"="Coldspot - 99% significant",
                               "vl"="Coldspot - 95% significant",
                               "l"="Coldspot - 90% significant",
                               "n"="Non significant",
                               "h"="Hotspot - 90% significant",
                               "vh"="Hotspot - 95% significant",
                               "vvh"="Hotspot - 99% significant",
                               "NA"="NA"))+
    geom_sf(data=map,alpha=0.1,color="gray55") +
    theme(panel.grid = element_line(size = 2*.pt)) +
    geom_sf(data=range_map,color="gray3",alpha=0.15,lwd=0.8)+
    
    labs(title=figure2,subtitle=figure3)+
    # 
    #    
    #   ggtitle(figure2)+
    theme_bw()+
    theme(axis.text.x = element_text(size=34),
          axis.text.y = element_text(size=34),
          plot.title = element_text(size = 40, hjust = 0.5),
          plot.subtitle = element_text(size = 36, hjust = 0.5),
          axis.title.y = element_text(size = 34),
          axis.title.x=element_text(size=34),
          legend.title=element_text(size=34),
          legend.text = element_text(size=34),
          legend.position="")+
    theme( panel.grid.major = element_line(size = 0.1, linetype = 'twodash',
                                           colour = "gray55"))
  
  plot_list4[[jkl]] = s
  
}

# gg_animate(toSave)
ani.options(ani.height=1200,ani.width=1500,interval=1)
fileout<-paste0("animation_1",filein,"_",scenario,"_",grid)

setwd("./figure/")
saveGIF(
  {lapply(plot_list4, print)}
  , paste0(fileout,".gif"))

# setwd("../")


# new_gif<-image_append(c(plot_list[1], plot_list2[1]))

library(magick)
# setwd("./figure/")

plot_list4a<-image_read(paste0(fileout,".gif"))



############################## 


## create a png figure for legend


png("legend_plot.png",width=3000,height=1000)
legend_plot<-ggplot() +
  geom_sf(data = oo, aes(fill = class ),color=NA,na.rm=TRUE)+ 
  scale_fill_manual(name="",values=c("vvl"="navy","vl"="royalblue","l"="skyblue2",
                             "n"="khaki1", "h"="salmon1","vh"="red2",
                             "vvh"="red4"),
                    labels=c("vvl"="Coldspot - 99% significant",
                             "vl"="Coldspot - 95% significant",
                             "l"="Coldspot - 90% significant",
                             "n"="Non significant",
                             "h"="Hotspot - 90% significant",
                             "vh"="Hotspot - 95% significant",
                             "vvh"="Hotspot - 99% significant",
                             "NA"="NA"))+
  geom_sf(data=map,alpha=0.1,color="gray55") +
  theme(panel.grid = element_line(size = 2*.pt)) +
  geom_sf(data=range_map,color="gray3",alpha=0.15,lwd=0.8)+
  
  labs(title=figure2,subtitle=figure3)+
  # 
  #    
  #   ggtitle(figure2)+
  theme_bw()+
  theme(axis.text.x = element_text(size=34),
        axis.text.y = element_text(size=34),
        plot.title = element_text(size = 34, hjust = 0.5),
        plot.subtitle = element_text(size = 28, hjust = 0.5),
        axis.title.y = element_text(size = 34),
        axis.title.x=element_text(size=34),
        legend.title=element_text(size=38),
        legend.text = element_text(size=42),
        legend.position="bottom",legend.box="vertical")+
  theme( panel.grid.major = element_line(size = 0.1, linetype = 'twodash',
                                         colour = "gray55"))+
  guides(fill = guide_legend(nrow = 2))



dev.off()


library(gtable)
legend11 = gtable_filter(ggplot_gtable(ggplot_build(legend_plot)), "guide-box")

plot(legend11)

library(ggpubr)
dev.new()
png("legend.png",width=2700,height=200)
as_ggplot(legend11)
dev.off()

legend_a<-image_read("legend.png")


new_gif1<-image_append(c(plot_list1a[1], plot_list2a[1]))
i=2
for(i in 2:5){
  combined <- image_append(c(plot_list1a[i], plot_list2a[i]))
  new_gif1<-c(new_gif1,combined)
}

new_gif1

library(gifski)
image_write_gif(new_gif1,delay=0.8,"new_gif1.gif")




new_gif2<-image_append(c(plot_list3a[1], plot_list4a[1]))
i=2
for(i in 2:5){
  combined2 <- image_append(c(plot_list3a[i], plot_list4a[i]))
  new_gif2<-c(new_gif2,combined2)
}

new_gif2

library(gifski)
image_write_gif(new_gif2,delay=0.8,"new_gif2.gif")



one_gif<-image_read("new_gif1.gif")
two_gif<-image_read("new_gif2.gif")


new_gif3<-image_append(c(one_gif[1],two_gif[1]),stack=TRUE)
i=2
for(i in 2:5){
  combined3 <- image_append(c(one_gif[i],two_gif[i]),stack=TRUE)
  new_gif3<-c(new_gif3,combined3)
}



new_gif3

image_write_gif(new_gif3,delay=0.8,"new_gif3.gif")



four_gif<-image_read("new_gif3.gif")
five_gif<-image_read("legend.png")


new_gif4<-image_append(c(four_gif[1],five_gif),stack=TRUE)
i=2
for(i in 2:5){
  combined4 <- image_append(c(four_gif[i],five_gif),stack=TRUE)
  new_gif4<-c(new_gif4,combined4)
}



new_gif4

image_write_gif(new_gif4,delay=0.8,"new_gif4.gif")





}
  # }}

