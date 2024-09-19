library(raster)

setwd('')
dir_in <- ''
dir_out <- ''

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  files <- list.files(paste0(dir_in,'/',h), full.names = T, pattern = '*.tif$', recursive = T)
  for (f in files){
    rs <- raster(f)
    
    # sensitivity for different resistance scales (we adopted 100)
    for (s in c(50,100,200)){
      fname <- paste0('resist_',h,'_',strsplit(names(rs),'_')[[1]][2],'_scale_',s,'.tif')
      toPath <- paste0(dir_out,'/scale_',s,'/',h,'/',fname)
      newrs <- rs / maxValue(rs) * (s - 1) + 1 
      writeRaster(newrs, toPath, overwrite=T)
    }
    print(paste0('complete: ',f))
  }
}


