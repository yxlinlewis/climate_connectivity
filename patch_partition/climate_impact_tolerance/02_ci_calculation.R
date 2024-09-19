# calculate climate impacts

library(raster)
library(magrittr)

setwd('')
from_dir <- ''
to_dir <- ''

func <- function(x,lim_lower,lim_upper){
  x$cc <- 0
  
  # calculate
  for (r in which(x[,3] < lim_lower)){x$cc[r] <- (lim_lower-x[r,3])/(lim_upper-lim_lower)}
  for (r in which(x[,3] > lim_upper)){x$cc[r] <- (x[r,3]-lim_upper)/(lim_upper-lim_lower)}
  
  return(x)
}

# load files
lower_sd <- read.csv('tolerance/lower_sd.csv')
upper_sd <- read.csv('tolerance/upper_sd.csv')
spelist <- read.csv('species.csv')

# create parameter list
paras <- colnames(lower_sd)[-c(1,2)]

# initialization
scen <- 'ssp585'
years <- 2020

for (r in 1:nrow(spelist)){
  # extract information
  exist <- spelist[r,] %>% as.character()
  spe <- exist[1]
  phy <- exist[2]
  
  # identify the distribution
  distri <- colnames(spelist)[which(exist != 'NA')][-c(1,2)]
  for (d in distri){
    # input species raster
    spe_rs <- raster(paste0(from_dir,'/',d,'/',phy,'/',spe,'.tif'))
    
    # create environment list
    envlist <- c()
    for (p in paras){
      envname <- paste0(p,'_',d,'_',scen,'_',yr,'.tif')
      envlist <- append(envlist, paste0('cmip6_future/',d,'/',scen,'/',p,'/',envname))
    }
    # create realized niches each year
    env_rs <- stack(envlist)
    dt <- mask(env_rs, spe_rs) %>% as.data.frame(., xy=T) %>% .[complete.cases(.),]
    colnames(dt)[3:19] <- paras
    
    if (nrow(dt) > 0){
      row.names(dt) <- seq.int(1:nrow(dt))
      
      # calculate climate exposure
      lonlat <- dt[,1:2]
      for (p in paras){
        # extract species tolerance
        upperlim <- upper_sd[r,p]
        lowerlim <- lower_sd[r,p]
        # calculation
        x <- dt[,c('x','y',p)]
        cal <- func(x,lowerlim,upperlim)
        lonlat <- cbind(lonlat,cal[,4])
      }
      colnames(lonlat)[3:19] <- paras
      
      # export 
      write.csv(lonlat, paste0(to_dir,'/',scen,'/',yr,'/',d,'/',phy,'/',spe,'.csv'), row.names = F)
      print(paste0(yr,'-',d,'-',phy,'-',spe,'- complete'))
    }
  }
}

