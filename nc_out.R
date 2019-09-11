nc_out <- function(i,j,var) {
  
  library(ncdf4)
  jj <- sprintf("%02d",j)
  
  infile <- paste0("d02_",i,"_",jj,".nc")
  ncin<-nc_open(infile)
  out<-ncvar_get(ncin,var)
  return(out)
}
