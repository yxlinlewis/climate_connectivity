library(magrittr)
library(raster)
library(sf)

setwd('')

# create folders
dir.create('step1_cmip6_dpmean')
dir.create('step2_cmip6_expand')
dir.create('step3_cmip6_idw')
dir.create('step4_cmip6_mdmean')
dir.create('step5_cmip6_smooth')

for (h in c('surface','mesopelagic','bathypelagic','abyssopelagic')){
  dir.create(paste0('step1_cmip6_dpmean/',h))
  dir.create(paste0('step2_cmip6_expand/',h))
  dir.create(paste0('step3_cmip6_idw/',h))
  dir.create(paste0('step4_cmip6_mdmean/',h))
  dir.create(paste0('step5_cmip6_smooth/',h))
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    dir.create(paste0('step1_cmip6_dpmean/',h,'/',s))
    dir.create(paste0('step2_cmip6_expand/',h,'/',s))
    dir.create(paste0('step3_cmip6_idw/',h,'/',s))
    dir.create(paste0('step4_cmip6_mdmean/',h,'/',s))
    dir.create(paste0('step5_cmip6_smooth/',h,'/',s))
    for (yr in c(2020:2100)){
      dir.create(paste0('step1_cmip6_dpmean/',h,'/',s,'/',yr))
      dir.create(paste0('step2_cmip6_expand/',h,'/',s,'/',yr))
      dir.create(paste0('step3_cmip6_idw/',h,'/',s,'/',yr))
    }
  }
}



#######
# step1: average all among depth layers #


levmean <- function(x){mean(x,na.rm = T)}

setwd('D:/cmip6')

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  
  from_dir <- paste0("cmip6_dp/", h)
  to_dir <- paste0("step1_cmip6_dpmean/", h)
  
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    for (yr in c(2020:2100)){
      models <- list.files(paste0(from_dir,'/',s,'/',yr))
      for (m in models){
        to_path <- paste0(to_dir,'/',s,'/',yr)
        name <- paste0('thetao_',m,'_',s,'_',h,'_',yr,'.tif')
        
        # stack tif files
        if (length(list.files(paste0(from_dir,'/',s,'/',yr,'/',m))) > 0){
          tifs <- list.files(paste0(from_dir,'/',s,'/',yr,'/',m), 
                             full.names = T, pattern = '*.tif$', recursive = T) %>% 
            stack()
          # define mean calculation
          mean <- calc(tifs, levmean)
          flip(mean, direction = 'y') %>%
            writeRaster(., paste0(to_path,'/',name), overwrite=TRUE)
          
        }
      }
      print(paste0(yr, '-', s,'-',h,'-complete'))
    }
  }
}



#######
# step2: expand data by 5 degree & convert to shapefile point #

rs_exp <- function(rs, output_path){
  # convert to dataframe
  rs <- as.data.frame(rs, xy=T) %>%
    .[complete.cases(.),]
  colnames(rs)[3] <- "value"
  
  # snap
  rs$x <- rs$x + 0.5
  rs$y <- rs$y - 1
  
  # expand data
  rs1 <- subset(rs, x < -175)
  rs2 <- subset(rs, x > 175)
  newrs <- rbind(rs1, rs2, rs)
  
  # convert to point
  rs.sf <- st_as_sf(newrs, coords = c('x','y'), crs=4326)
  st_write(rs.sf, output_path)
}

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  from_dir <- paste0("step1_cmip6_dpmean/", h)
  to_dir <- paste0("step2_cmip6_expand/", h)
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    for (yr in c(2020:2100)){
      files <- list.files(paste0(from_dir,'/',s,'/',yr), pattern = '*.tif$')
      for (f in files){
        fname <- strsplit(f,'.tif')[[1]]
        output_path <- paste0(to_dir,'/',s,'/',yr,'/',fname,'.shp')
        input_path <- paste0(from_dir,'/',s,'/',yr,'/',f)
        raster(input_path) %>%
          rs_exp(., output_path)
      }
      print(paste0(s, ': ', yr, ' complete'))
    }
  }
  print(paste0(h, ': complete ----'))
}



#######
# step3: IDW interpolation (Python environment) #
# see this script on Github: 



#######
# step4: average all among GCMs #

modelmean <- function(dt, output_path){
  newdt <- c()
  interval <- c()
  
  for (i in 1:nrow(dt)){
    cell <- dt[i,3:ncol(dt)] %>% as.numeric(.)
    
    if (length(which(is.na(cell) == F)) == 0){next}
    else if (length(which(is.na(cell) == F)) >= 5){
      cell <- cell[which(is.na(cell) == F)]
      newmean <- cell[which(cell>=mean(cell)-2*sd(cell) & cell<=mean(cell)+2*sd(cell))] %>%
        mean(.)
      interval <- rbind(interval, cbind(dt[i,1:2], value=newmean))
      
    }
    else{
      cell <- cell[which(is.na(cell) == F)]
      newmean <- mean(cell)
      interval <- rbind(interval, cbind(dt[i,1:2], value=newmean))}
    
    if (i%%5000 == 0){
      newdt <- rbind(newdt, interval)
      interval <- c()
    }
  }
  newdt <- rbind(newdt, interval)
  
  # rasterize
  rs.sf <- st_as_sf(newdt, coords = c('x','y'), crs=4326)
  rs.sp <- as(rs.sf, "Spatial") 
  rs <- raster(crs = crs(rs.sp), vals = 0, resolution = c(1, 1), 
               ext = extent(c(-180, 180, -90, 90))) %>%
    rasterize(rs.sp, ., field='value', fun='first')  
  
  # expand by 1 degree
  rs1 <- crop(rs, extent(-180, -179, -90, 90))
  rs2 <- crop(rs, extent(179, 180, -90, 90))
  extent(rs1) <- c(180, 181, -90, 90)
  extent(rs2) <- c(-181, -180, -90, 90)
  newrs <- merge(rs2, rs, rs1)
  
  writeRaster(newrs, output_path, overwrite=T) 
  
}

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  from_dir <- paste0("step3_cmip6_idw/", h)
  to_dir <- paste0("step4_cmip6_mdmean/", h)
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    for (yr in c(2020:2100)){
      to_path <- paste0(to_dir,'/',s)
      name <- paste0('thetao_',h,'_',s,'_',yr,'.tif')
      
      # load all rasters to create raster stacks
      dt <- list.files(paste0(from_dir,'/',s,'/',yr), pattern = '.tif$', full.names = T) %>% 
        stack() %>%
        as.data.frame(., xy=T)
      modelmean(dt, paste0(to_path,'/',name))
      print(paste0(s, ': ', yr, ' complete'))
    }
  }
  print(paste0(h, ': complete ----'))
}



#######
# step5: smooth data by moving-window averaging (Python environment) #
# see this script on Github: 


