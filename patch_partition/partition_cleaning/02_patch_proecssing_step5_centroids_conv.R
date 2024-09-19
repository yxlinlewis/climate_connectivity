library(sf)
library(raster)
library(magrittr)

#######
# replace initial centroids of patches with nearest raster cell centers

setwd('')

for (hemi in c('N', 'S')){
  files <- list.files(paste0('step4_patches_hemi/',hemi,'_centro'), 
                      pattern = '.shp$', recursive = T, full.names = T)
  for (f in files){
    ## centroid point
    cp <- st_read(f) %>% as.data.frame()
    cp$cpx_nbh <- NA
    cp$cpy_nbh <- NA
    
    ## topographic data
    depth <- strsplit(f,'/')[[1]][3]
    topo <- raster(paste0('topo/',hemi,'_topo/',hemi,'_',depth,'.tif')) %>% 
      as.data.frame(., xy=T) %>% .[complete.cases(.),]
    row.names(topo) <- seq(1,nrow(topo))
    
    for (r in 1:nrow(cp)){
      ## initial centroid point
      cpx_ini <- cp$cp_x[r]
      cpy_ini <- cp$cp_y[r]
      
      ## calculate Euclidean distance between centroid point and each raster cell center
      compare <- topo
      compare$cpx_cpmpare <- abs(compare$x - cpx_ini)
      compare$cpy_cpmpare <- abs(compare$y - cpy_ini)
      compare$distance <- ((compare$cpx_cpmpare)^2 + (compare$cpy_cpmpare)^2)^0.5
      
      ## replace initial centroid point with nearest cell center
      nbh <- which(compare$distance == min(compare$distance))
      cp$cpx_nbh[r] <- compare$x[nbh]
      cp$cpy_nbh[r] <- compare$y[nbh]
    }
    
    ## create point from dataframe
    if (hemi == 'N'){
      cp_nbh <- st_as_sf(cp, coords = c('cpx_nbh','cpy_nbh'), 
                         crs='+proj=aeqd +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
    }else{
      cp_nbh <- st_as_sf(cp, coords = c('cpx_nbh','cpy_nbh'), 
                         crs='+proj=aeqd +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
    }
    
    ## export centroids to <step5_centro_hemi>
    if (length(strsplit(f,'/')[[1]])==4){
      to_path <- paste0('step5_centro_hemi/',hemi,'_centro/',
                        depth,'/',hemi,'_centro_',depth,'_present.shp')
    }else{
      scen <- strsplit(f,'/')[[1]][4]
      yr <- strsplit(strsplit(f,'/')[[1]][5],'_')[[1]][5]
      to_path <- paste0('step5_centro_hemi/',hemi,'_centro/',
                        depth,'/',scen,'/',hemi,'_centro_',depth,'_',scen,'_',yr,'.shp')
    }
    st_write(cp_nbh, to_path)
  }
}


