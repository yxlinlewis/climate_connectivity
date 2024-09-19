library(sf)
library(raster)
library(magrittr)
library(dplyr)

# dissolve the spatially-connected multi-part patches ####

setwd('')

## patch raster (f) values indicate patch ID
files <- list.files('step1_patches_id2rs', pattern = '.tif$', recursive = T, full.names = T)
for (f in files){
  ## class raster (f1) values indicate patch classes
  f1 = paste0('patches_raw/', strsplit(f,'step1_patches_id2rs/')[[1]][2])
  
  ## combine class and patch id
  rs <- stack(f1, f)
  dt_all <- as.data.frame(rs, xy=T)
  colnames(dt_all)[3] <- 'class'
  colnames(dt_all)[4] <- 'patch'
  
  ## extract boundaries
  dt_left <- subset(dt_all, x == -179.5)
  row.names(dt_left) <- seq(1, nrow(dt_left))
  dt_right <- subset(dt_all, x == 179.5)
  row.names(dt_right) <- seq(1, nrow(dt_right))
  
  ## scan all rows
  id = -1
  for (r in 1:nrow(dt_left)){
    left_class <- dt_left$class[r]
    right_class <- dt_right$class[r]
    ## judge NA
    if (is.na(left_class)==F & is.na(right_class)==F){
      ## judge whether adjacent patches (at both sides of boundaries) have the same class
      if (left_class==right_class){
        # cover initial patch id with new one
        dt_all[which(dt_all$patch==dt_left$patch[r]), 'patch'] <- id
        dt_all[which(dt_all$patch==dt_right$patch[r]), 'patch'] <- id
        id = id - 1
      }
    }
  }
  
  ## rasterize
  dt_all <- dt_all[complete.cases(dt_all),]
  rs.sf <- st_as_sf(dt_all, coords = c('x','y'), crs=4326)
  rs.sp <- as(rs.sf, "Spatial") 
  newrs <- raster(crs = crs(rs.sp), vals = 0, resolution = c(1, 1), 
               ext = extent(c(-180, 180, -90, 90))) %>%
    rasterize(rs.sp, ., field='patch', fun='first')  
  
  ## convert to shp (dissolve the same patch id)
  sf.poly <- rasterToPolygons(newrs, dissolve=T) %>% st_as_sf(.)
  
  ## calculate shape area (in decimal degree)
  dt_area <- c()
  for (i in unique(dt_all$patch)){
    dt_area <- rbind(dt_area, 
                     cbind(layer=i, shape_area=length(which(dt_all$patch==i)))) %>% 
      as.data.frame()
  }
  
  ## join attribute table by patch id
  sf.poly <- left_join(sf.poly, dt_area, by='layer')
  
  ## export shp to <step2_patches_newid>
  st_write(sf.poly, paste0('step2_patches_newid/','<filename>'))
}


