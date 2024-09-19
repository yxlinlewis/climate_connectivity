library(raster)

setwd('')
timestart<-Sys.time()

for (dp in c("surface","mesopelagic","abyssopelagic","bathypelagic")){
  # import seascape boundary
  mask_r <- raster(paste0("topo/",dp,'.tif'))
  
  # import canvas (180*360 raster, value=0)
  canvas <- raster('canvas.tif')
  
  for (phy in list.files(paste0("weight/",dp))){
    # list of all species
    files <- list.files(paste0("weight/",dp,"/",phy), 
                        pattern = '.tif$', full.names = T)
    
    for (f in files){
      f <- raster(f)
      # expand extend by NA (only rasters with the same extent can be summed)
      f <- extend(f,extent(-180, 180, -90, 90),value=NA)
      # sum all
      canvas <- sum(canvas,f, na.rm = T)
    }
    print(paste(exp,year,dp,phy,sep = "-"))
  }
  
  # extract canvas by mask (boundary)
  canvas_r <- mask(canvas, mask_r)
  
  # export
  writeRaster(canvas_r, paste0("cum_CI/",dp,".tif"),overwrite = TRUE)
  print(paste(exp,"-",year,"-",dp,"Done!",sep = ""))
}


timeend<-Sys.time()
runningtime <- round(timeend-timestart,1)
print(runningtime)

