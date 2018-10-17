# DB Hydro WTD as Raster Heatmap
# Pete Gill
# 21/02/2018

#clear environment
rm(list=ls())

library(ggplot2)

setwd("setup")
load("hydro_setup_workspace.RData")
setwd("..")


params <- as.vector(read.table(file="hydro_parameters.txt",
                               header=FALSE))

#Model run time (days)
sim_time <- params[1,] / params[2,]


# ncolsb <- params[4,]
# ncols <- ncolsb - 2
# nrowsb <- params[5,]
# nrows <- nrowsb - 2
# no_layers <- params[6,] -1

no_layers <- length(hydro_t[1,1,])
ncolsb <- length(hydro_t[1,,1])
ncols <- ncolsb - 2
nrowsb <- length(hydro_t[,1,1])
nrows <- nrowsb - 2

thickness <- hydro_t[,,1]
thickness[,] <- 0
for (row in 1:nrowsb){
  for(col in 1:ncolsb){
    thickness[row,col] <- sum(hydro_t[row,col,], na.rm=TRUE)
  }
}



array_thickness <- array(thickness, dim=c(nrowsb,ncolsb,sim_time))

# trim boundary cells
array_thickness <- array_thickness[2:(nrowsb-1),2:(ncolsb-1),]



#Read in hydro output file
hydro_wt_height <- as.vector(read.table(file = "hydro_wt_output.txt",
                                        header = FALSE, dec = ".",
                                        fill = TRUE))

hydro_wt_height <- hydro_wt_height[ ,1]
# trim boundary data
# hydro_wt_height_act <- hydro_wt_height[hydro_wt_height!=-999]

# create 3d array (x,y columns through time)
wt_array <- array(hydro_wt_height, dim=c(nrowsb,ncolsb,sim_time))

wt_array <- wt_array[2:(nrowsb-1),2:(ncolsb-1),]
wt_array[wt_array==-999] <- NA
wtd_array <- array_thickness - wt_array


# calculate some statistics to keep colour breaks the same across all images
mx <- floor(max(wtd_array, na.rm=TRUE)) +1
mn <- params[9,] * -1 # ponding depth
tdf <- mx + abs(mn)
zpct <- mx / tdf
zat <- 1 - zpct # where does 0 fall as percentile (0-1), between min and max
# zat <- 0.5

lm <- max(abs(mn),mx)

#readline(prompt="Press [enter] to produce gganimate pngs...")

###########
# gganimate
###########é
d <- ncolsb * nrowsb
maxt <- length(wtd_array[1,1,])
for (t in 1:1){
# for (t in 1:maxt){
  wtd_gganimate <- data.frame(matrix(NA,nrow=d,ncol=4))
  colnames(wtd_gganimate) <- c("day","x","y","wtd")
  counter <- 1
  
  for (row in 1:nrows){
    for (col in 1:ncols){
      wtd_gganimate[counter,1] <- t
      #flip the coordinates to orient to north
      #[1,1] on array (top left) is not the same as (1,1) coordinate (bottom left) on graph
      wtd_gganimate[counter,2] <- col
      wtd_gganimate[counter,3] <- nrows - row + 1
      wtd_gganimate[counter,4] <- wtd_array[row,col,t]
      counter <- counter + 1
    }
  }
  
  # 
  #ditch green #17d817
  df_ani <- ggplot(wtd_gganimate, aes(x=x, y=y, fill = wtd)) +
    geom_raster(aes(fill=wtd),interpolate=FALSE) +
    scale_fill_gradientn(colours=c("blue","white","red"),
                         breaks=c(mn,0,mx),
                         #limits=c(-0.25,60),
                         limits=c(mn,mx),
                         values=c(0,zat,1),
                         #na.value="red",
                         guide=guide_colourbar(reverse = TRUE)) +
    ylab("Horizontal Distance Up Slope (m)") +
    xlab("Horizontal Distance Across Slope (m)") +
    labs(fill = "Water Table\nDepth (cm)") +
    ggtitle(paste("Day:",t))
  

  # ggsave(file = paste("Day",t,".png"),dpi=300)
  # print(paste("Complete:",t))
}

df_ani
