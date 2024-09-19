library(sf)
library(raster)
library(magrittr)
library(robustbase)

setwd('')
dir_in <- ''
dir_out <- ''

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  dt <- list.files(paste0(dir_in,'/',h), full.names = T) %>% 
    stack() %>% as.data.frame(., xy=T) %>% .[complete.cases(.),]
  row.names(dt) <- seq(1,nrow(dt))

  # 5 sig figures (2 digits after rescaled to 1-100)
  dt <- cbind(dt[,1:2], round(dt[,3:13], 5))
  
  result <- c()
  years <- seq(2003,2013)
  for (r in 1:nrow(dt)){
    hi_hist <- dt[r,3:13] %>% as.numeric()
    
    ## robust linear regression
    if (sum(hi_hist)==0){
      # exclude zero result
      interc <- 0
      sl <- 0
      R2 <- NA
    }else{
      # judge errors
      tryCatch({
        hi_rlm <- lmrob(hi_hist ~ years)
        interc <- hi_rlm$coefficients[[1]]
        sl <- hi_rlm$coefficients[[2]]
        R2 <- summary(hi_rlm)$r.squared
      }, warning = function(w){
        # replace by linear regression
        hi_rlm <- lm(hi_hist ~ years)
        interc <- hi_rlm$coefficients[[1]]
        sl <- hi_rlm$coefficients[[2]]
        R2 <- summary(hi_rlm)$r.squared
      }, error = function(e){
        # replace by linear regression
        hi_rlm <- lm(hi_hist ~ years)
        interc <- hi_rlm$coefficients[[1]]
        sl <- hi_rlm$coefficients[[2]]
        R2 <- summary(hi_rlm)$r.squared
      })
    }
    result <- rbind(result, cbind(dt[r,1:2], intercept=interc, slope=sl, rsquare=R2))
  }
  
  # estimation
  result$hi_2020 <- result$slope * 2020 + result$intercept
  
  # set zero for data less than zero (negative regression)
  result$hi_2020[which(result$hi_2020<0)] <- 0
  
  # convert to tif
  rs.sf <- st_as_sf(result, coords = c('x','y'), crs=4326)
  rs.sp <- as(rs.sf, "Spatial")
  for (f in colnames(result[,6:12])){
    rs <- raster(crs = crs(rs.sp), vals = 0, resolution = c(1, 1),
                 ext = extent(c(-180, 180, -90, 90))) %>%
      rasterize(rs.sp, ., field=f, fun='first')
    writeRaster(rs, paste0(dir_out,'/',h,'/',f,'.tif'), overwrite=T)
    print(f)
  }
  print(paste0(h, ': complete ----'))
}



