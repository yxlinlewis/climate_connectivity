library(magrittr)
library(dplyr)
library(raster)
library(sf)

setwd('')
edge <- read.csv('cc/Tedge.csv')

# create files
dir.create('patch_species')
for (h in c('surface','mesopelagic','bathypelagic','abyssopelagic')){
  dir.create(paste0('patch_species/',h))
  for (phy in unique(edge$phylum)){
    dir.create(paste0('patch_species/',h,'/',phy))
  }
}

for (h in c('surface','mesopelagic','bathypelagic','abyssopelagic')){
  # species distribution (generated from AquaMaps)
  spelist <- list.files(paste0('SpeciesDistribution/',h), 
                        full.names = T, recursive = T, pattern = '.tif$')
  
  ## load patches
  patch <- raster(paste0('patch_id/',h,'/patch_',h,'_present.tif'))
  
  # create progress bar
  print(paste0('--- ',h,' started...'))
  pb <- txtProgressBar(style = 3)
  
  for (spe in spelist){
    ## extract species information
    phy <- strsplit(spe, '/')[[1]][4]
    spename <- strsplit(strsplit(spe, '/')[[1]][5],'.tif$')[[1]][1]
    
    ## species data
    spe_location <- raster(spe)
    spename <- strsplit(strsplit(spe,'/')[[1]][5],'.tif$')[[1]][1]
    
    ## extract patches by mask
    spe_bypatch <- mask(patch, spe_location) %>% 
      as.data.frame(., xy=T) %>% .[complete.cases(.),]
    colnames(spe_bypatch)[3] <- 'pid'
    
    if (nrow(spe_bypatch)>0){
      
      write.csv(spe_bypatch, 
                paste0('patch_species/',h,'/',phy,'/',spename,'.csv'), 
                row.names = F)
      
    }
    
    # refresh progress bar
    setTxtProgressBar(pb, which(spelist==spe)/length(spelist))
  }
  
  # end progress bar
  close(pb)
  
}
