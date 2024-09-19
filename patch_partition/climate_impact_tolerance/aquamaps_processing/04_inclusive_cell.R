library(raster)
library(magrittr)
library(sf)

# inclusive criterion (at least one cell out of four)

pixelmix <- function(rs, output_path){
  dt <- as.data.frame(rs, xy = T) %>% 
    .[complete.cases(.),]
  colnames(dt)[3] <- "value"
  
  # rasterize
  newdt <- subset(dt, value >= 1)
  if (nrow(newdt) != 0){
    newdt <- cbind(newdt, val=1)
    rs.sf <- st_as_sf(newdt, coords = c('x','y'), crs=4326)
    rs.sp <- as(rs.sf, "Spatial") 
    rs <- raster(crs = crs(rs.sp), vals = 0, resolution = c(1, 1), 
                 ext = extent(c(-180, 180, -90, 90))) %>%
      rasterize(rs.sp, ., field='val', fun='first')  
    writeRaster(rs, output_path, overwrite=T) 
  }else{}
}


from_dir <- ''
to_dir <- ''
for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  for (p in list.files(paste0(from_dir,'/',h))){
    setwd(paste0(from_dir,'/',h,'/',p))
    files <- list.files(full.names = F, pattern='*.tif$')
    for (f in files){
      to_path <- paste0(to_dir,'/',h,'/',p,'/',f)
      raster(f) %>% pixelmix(., to_path)
    }
    print(paste0(h,'-',p,'- complete'))
  }
}




