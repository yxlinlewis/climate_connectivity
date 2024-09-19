library(raster)
library(magrittr)

setwd('')

#####
# clip by hemisphere in preparation for equidistant projection
## set resistance as example
for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  for (f in list.files(paste0('resistance/',h), full.names = T, recursive = T)){
    rs <- raster(f)
    fname <- names(rs)
    # northern
    crop(rs, extent(-180, 180, -10, 90)) %>% 
      writeRaster(., paste0('Batch/N_hemi/Resistance/',h,'/',fname,'.tif'))
    # southern
    crop(rs, extent(-180, 180, -90, 10)) %>% 
      writeRaster(., paste0('Batch/S_hemi/Resistance/',h,'/',fname,'.tif'))
  }
}

#####
# project raster to polar coordination

for (hemi in c('N_hemi', 'S_hemi')){
  files <- list.files(hemi, recursive = T, full.names = T)
  for (f in files){
    para <- strsplit(f,'/')[[1]][2]
    depth <- strsplit(f,'/')[[1]][3]
    fname <- strsplit(f,'/')[[1]][length(strsplit(f,'/')[[1]])]
    
    # set output path
    if (length(strsplit(fname,'_')[[1]])==4){
      ## future
      scen <- strsplit(fname,'_')[[1]][3]
      to_path <- paste0(hemi,'sphere/',para,'/',depth,'/',scen,'/',
                        strsplit(hemi, '_')[[1]][1],'_',fname)
    }else{
      ## present
      to_path <- paste0(hemi,'sphere/',para,'/',depth,'/',
                        strsplit(hemi, '_')[[1]][1],'_',fname)
    }
    
    # output projected raster
    if (strsplit(hemi, '_')[[1]][1] == 'N'){
      ## Northern hemisphere
      projectRaster(raster(f), 
                    crs = "+proj=aeqd +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                    method = "ngb", over = FALSE, filename =  to_path, overwrite = T)
    }else{
      ## Southern hemisphere
      projectRaster(raster(f), 
                    crs = "+proj=aeqd +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                    method = "ngb", over = FALSE, filename =  to_path, overwrite = T)
    }
  }
}


