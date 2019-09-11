# Codes to read cohort level outputs, patch level outputs and polygon level outputs
# and calculate polygon level outputs from cohort outputs over time
# Karun Pandit

#setwd("c:/users/karunpandit.BOISESTATE.003/Documents/rms_fire")


setwd('/home/kpandit/scratch/out_point_fire/out_rms_his7') 


#setwd('/home/karunpandit/sla_2') 

# remove previous files
rm(list=ls()) 

# install necessary packages
library(rhdf5)
#library(ncdf4)
#library(dplyr)

styr <- 2060
endyr <- 2065
stmn <- 1
endmn <- 12

var = "DMEAN_GPP_CO"
var_name = " GPP "
NPLANT = "NPLANT"
PFT = "PFT"

abc_1<-matrix(nrow=0,ncol=7)

for (year in seq (styr, endyr,1)) {
  if (year==styr) {stmn=10} else {stmn=1} 
    if (year==endyr) {endmn=9} else {endmn=12}
      for (cmm in seq(stmn, endmn, 1)) {
    
        if (cmm == 1) {dd = 31}
        if (cmm == 2) {dd = 28}
        if (cmm == 3) {dd = 31}
        if (cmm == 4) {dd = 30}
        if (cmm== 5) {dd = 31}
        if (cmm== 6) {dd = 30}
        if (cmm== 7) {dd = 31}
        if (cmm== 8) {dd = 31}
        if (cmm== 9) {dd = 30}
        if (cmm== 10) {dd = 31}
        if (cmm== 11) {dd = 30}
        if (cmm== 12) {dd = 31}
    
        for (cdd in seq(1,dd,1)) {
         

        cmmm <- sprintf("%02d",cmm)
        cddd <- sprintf("%02d",cdd)
      
        filexx <- paste0("hhh-D-",year,"-",cmmm,"-",cddd,"-000000-g01.h5")
        fileyyyy    <- h5read(filexx,var)
        filepft     <- h5read(filexx,PFT)
        filenplant  <- h5read(filexx,NPLANT)
        filegpppy<-h5read(filexx,"DMEAN_GPP_PY")
        file_pa <- h5read(filexx,"PACO_ID")
        file_npa <- h5read(filexx,"PACO_N")
        file_area <- h5read(filexx,"AREA")
      
      filemult <- fileyyyy*filenplant
      filetwo <- cbind(filemult,filepft)
      
      dim_pa<-dim(file_pa)
 
      
      if (dim_pa>1) {   
            pat_end<-dim_pa
            ln_co<-file_pa[dim_pa]
            ln_pa<-length(file_pa)
            patch_s<-matrix(0,nrow=0,ncol=3)
            for (kkk in seq (1, pat_end,1)) {  
              cell<-file_npa[kkk]
                    if (cell==0) {
                    sum_18<-0
                    sum_5<-0
                    }
              
                    else {
                        if (kkk< ln_pa) {
                        
                            lll<- file_pa[kkk]
                            mmm<-file_pa[kkk+1]
                            patch1 <- data.frame(filetwo[lll:mmm-1,])
                            ccdd<-dim(patch1)
                            if (ccdd[2]==1) {
                              patch1<-t(patch1)
                              patch1_18<-(subset(patch1,patch1[,2]==18))
                              patch1_5 <- subset(patch1,patch1[,2]==5)
                              sum_18<-sum(patch1_18[,1])
                              sum_5<-sum(patch1_5[,1])
                            }
                            else {
                            
                            patch1_18<-subset(patch1,patch1[,2]==18)
                            patch1_5 <- subset(patch1,patch1[,2]==5)
                            sum_18<-sum(patch1_18[,1])
                            sum_5<-sum(patch1_5[,1])
                            }
                        }
                        else { 
                            lll<-file_pa[kkk]  
                            mmm<-dim(filepft)
                            patch1 <- data.frame(filetwo[lll:mmm,])
                            ccdd<-dim(patch1)
                            if (ccdd[2]==1) {
                            patch1<-t(patch1)
                            
                            patch1_18<-(subset(patch1,patch1[,2]==18))
                            patch1_5 <- subset(patch1,patch1[,2]==5)
                            sum_18<-sum(patch1_18[,1])
                            sum_5<-sum(patch1_5[,1])
                            
                            }
                            else{    
                            patch1_18<-(subset(patch1,patch1[,2]==18))
                            patch1_5 <- subset(patch1,patch1[,2]==5)
                            sum_18<-sum(patch1_18[,1])
                            sum_5<-sum(patch1_5[,1])
                            }
                        }
                    }
           patch_sum <- cbind(kkk,sum_18,sum_5)
           patch_s <- rbind(patch_s,patch_sum)
           wt_18<- sum(patch_s[,2]*file_area)
           wt_5 <-sum(patch_s[,3] *file_area)
            }
        }
        else {
        patch1_18<-filetwo[which(filepft==18)]
        patch1_5 <- filetwo[which(filepft==5)]
        wt_18<-sum(patch1_18)
        wt_5<-sum(patch1_5)
        }
      
        tot_gpp<-wt_18+wt_5    
        abab<- cbind(wt_18,wt_5,tot_gpp,filegpppy,year,cmmm,cddd)
        abc_1 <- rbind(abc_1,abab)    
      }
    }
}
   
write.csv(abc_1,"pft_gpp_co.csv")



aaa<- as.numeric(as.character(abc_1[,1]))
nnn<- length(aaa)
bbb<- gl(nnn,365,nnn)
abc_2<-aggregate(aaa~bbb, FUN=mean)


aaaa<- as.numeric(as.character(abc_1[,2]))
nnnn<- length(aaaa)
bbbb<- gl(nnnn,365,nnnn)
bcd_2<-aggregate(aaaa~bbbb, FUN=mean)

aa<- as.numeric(as.character(abc_1[,3]))
nn<- length(aa)
bb<- gl(nn,365,nn)
cde_2<-aggregate(aa~bb, FUN=mean)


def_2<-cbind(abc_2,bcd_2[,2],cde_2[,2])

write.csv(def_2,"annual_pft_gpp.csv")
