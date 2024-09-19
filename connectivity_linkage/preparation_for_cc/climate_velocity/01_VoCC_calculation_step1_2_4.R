library(sf)
library(raster)
library(magrittr)
library(VoCC)
library(dplyr)

setwd('D:/')

# create folders
# dir.create('vocc')
dir.create('vocc/step1_velocity')
dir.create('vocc/step2_trajlns')
dir.create('vocc/step3_trajlns_byp')
dir.create('vocc/step4_trajpchs')
for (h in c('surface','mesopelagic','bathypelagic','abyssopelagic')){
  dir.create(paste0('vocc/step1_velocity/',h))
  dir.create(paste0('vocc/step2_trajlns/',h))
  dir.create(paste0('vocc/step3_trajlns_byp/',h))
  dir.create(paste0('vocc/step4_trajpchs/',h))
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    dir.create(paste0('vocc/step3_trajlns_byp/',h,'/',s))
  }
}





#######
# Step1: calculate climate velocity #

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    
    # import raster stack
    list <- list.files(paste0('cmip6/step6_cmip6_final/',h,'/',s), full.names = T)
    rs <- stack(list)
    
    # calculate climate velocity (no extension)
    vt <- tempTrend(rs, th = 20)
    vg <- spatGrad(rs, th = 0.0001, projected = FALSE)
    gv <- gVoCC(vt, vg)
    
    # output
    writeRaster(gv, paste0('vocc/step1_velocity/',h,'/vocc_',h,'_',s,'_2020_2100.tif'), overwrite=T)
    
    print(paste0(h,'-',s,'-complete'))
  }
}



#######
# Step2: climate trajectory with velocity #

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  # import patch and patch ID
  patch <- raster(paste0('cc/patch_id/',h,'/patch_',h,'_present.tif'))
  patchid <- as.data.frame(patch, xy=T) %>% .[complete.cases(.),]
  colnames(patchid)[3] <- 'pid'
  
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    
    # import temperature
    list <- list.files(paste0('cmip6/step6_cmip6_final/',h,'/',s), full.names = T)
    rs <- stack(list)
    
    # import velocity
    gv <- brick(paste0('vocc/step1_velocity/',h,'/vocc_',h,'_',s,'_2020_2100.tif'))
    
    # extract velocity and angle
    vel <- gv[[1]]
    ang <- gv[[2]]
    
    # calculate mean temperature
    mn <- mean(rs, na.rm = T)
    
    # extract x-y points of all cells
    lonlat <- patchid
    row.names(lonlat) <- seq(1,nrow(lonlat))
    lonlat <- lonlat[,-3]
    lonlat$vel <- raster::extract(vel, lonlat)
    lonlat$ang <- raster::extract(ang, lonlat[,1:2])
    lonlat$mn <- raster::extract(mn, lonlat[,1:2])
    lonlat <- lonlat[complete.cases(lonlat),]
    
    # velocity trajectory
    # time span: 2020~2100, decadal intervals
    traj <- voccTraj(lonlat, vel, ang, mn, tyr = 81, 
                     trajID = as.numeric(rownames(lonlat)), correct=T)
    traj_lns <- trajLine(x = traj) %>% st_as_sf(.)
    
    # extract start points and retrieve patch IDs
    xystart <- traj[1:nrow(traj_lns),]
    xystart$pid <- raster::extract(patch, xystart[,1:2])
    
    # left-join to polyline attributes
    traj_lns <- left_join(traj_lns, xystart, by='trajIDs')
    
    for (p in unique(traj_lns$pid)){
      # trajectory lines of each patch
      Pch_traj <- subset(traj_lns, pid==p)
      
      # export trajectories by patches 
      st_write(st_as_sf(Pch_traj), 
               paste0('vocc/step3_trajlns_byp/',h,'/',s,'/traj_',h,'_',s,'_2020_2100_',p,'.shp'))
    }
    
    # export overall trajectory line
    st_write(st_as_sf(traj_lns), 
             paste0('vocc/step2_trajlns/',h,'/traj_',h,'_',s,'_2020_2100.shp'))
  }
}



#######
# Step3: extract patches traversed by trajectory lines (in Python) #
# see script on Github: 



#######
# Step4: integrate pairwise trajectory patches #

for (h in c('surface', 'mesopelagic', 'bathypelagic', 'abyssopelagic')){
  # import total patch and patch ID
  patch <- raster(paste0('cc/patch_id/',h,'/patch_',h,'_present.tif'))
  patchid <- as.data.frame(patch, xy=T) %>% .[complete.cases(.),]
  colnames(patchid)[3] <- 'pid'
  pch_all <- unique(patchid$pid)    ## overall patches
  
  for (s in c('ssp126', 'ssp245', 'ssp585')){
    
    # load available patch ID (have velocity trajectory)
    traj_all <- st_read(paste0('vocc/step2_trajlns/',h,'/traj_',h,'_',s,'_2020_2100.shp'))
    pch_traj <- unique(traj_all$pid)    ## available patches
    
    # initialize
    result <- c()
    
    for (p in pch_all){
      if (p %in% pch_traj){
        # trajectory lines of each patch
        traj_path <- raster(paste0('vocc/step3_trajlns_byp/',h,'/',s,'/traj_',h,'_',s,'_2020_2100_',p,'.tif')) %>% 
          as.data.frame(.) %>% .[complete.cases(.),] %>% unique(.)
        result <- rbind(result, cbind(from_patch=p, to_patch=traj_path)) %>% 
          as.data.frame(.)
      }else{
        # for non-available trajectories (discrete patches)
        result <- rbind(result, cbind(from_patch=p, to_patch=p))
      }
    }
    
    # export overall trajectory results
    write.csv(result, paste0('vocc/step4_trajpchs/',h,'/traj_',h,'_',s,'_2020_2100.csv'),row.names = F)
    print(paste0(h,'-',s,'- complete'))
  }
}

