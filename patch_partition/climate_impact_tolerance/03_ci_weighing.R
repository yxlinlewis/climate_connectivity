library(sf)
library(raster)
library(magrittr)

setwd('')
years <- ''

# set weight of climate impacts (maximum: 10)
weight_main <- 8/5
weight_sub <- 2/12


for (dp in c("surface","mesopelagic","abyssopelagic","bathypelagic")){
  # list of phyla
  phys <- list.files(paste0("raw/",year,"/",dp))
  
  for (phy in phys){
    # list of species
    spes <- list.files(paste0("raw/",dp,"/",phy), pattern='.tif$')
    
    if (length(spes)>0){  # skip empty folders
      for (spe in spes){
        # climate impacts of each parameters (17 cols)
        spe_ci <- read.csv(paste0("raw/",dp,"/",phy,"/",spe))
        
        # calculate weighed sum of climate impacts of each species
        ## major weight (80%, equal intervals): temperature, salinity, dissolved oxygen, pH, primary production (5 in total)
        ## minor weight (20%, equal intervals): the rest parameters (12 in total)
        spe_ci$CI_total <- (spe_ci[,10]*weight_main+
                            spe_ci[,11]*weight_main+
                            spe_ci[,12]*weight_main+
                            spe_ci[,16]*weight_main+
                            spe_ci[,18]*weight_main+

                            spe_ci[,3]*weight_sub+
                            spe_ci[,4]*weight_sub+
                            spe_ci[,5]*weight_sub+
                            spe_ci[,6]*weight_sub+
                            spe_ci[,7]*weight_sub+
                            spe_ci[,8]*weight_sub+
                            spe_ci[,9]*weight_sub+
                            spe_ci[,13]*weight_sub+
                            spe_ci[,14]*weight_sub+
                            spe_ci[,15]*weight_sub+
                            spe_ci[,17]*weight_sub+
                            spe_ci[,19]*weight_sub)

      }
      
      # convert to raster
      ## convert dataframe to sf, then sp
      rs.sf <- st_as_sf(dt, coords = c('x','y'), crs=4326)
      rs.sp <- as(rs.sf, "Spatial") 
      
      ## create constant raster and rasterize from sp; export
      raster(crs = crs(rs.sp), vals = 1, resolution = c(1, 1), ext = extent(c(-180, 180, -90, 90))) %>%
        rasterize(rs.sp, ., field='CI_total', fun='first', mask = T) %>% 
        writeRaster(., paste0("weight/",dp,"/",phy,"/",strsplit(spe,'.csv')[[1]][1],'.tif'))
      
    }
    print(paste0(dp,"done"))
  }
}

