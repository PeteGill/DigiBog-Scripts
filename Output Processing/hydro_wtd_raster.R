# DB Hydro WTD as Raster Heatmap
# Pete Gill
# 21/02/2018

# This script requires the following files:
#   1. hydro_setup_workspace.RData, produced during setup with hydro_DTM_setup.R.
#   2. hydro_parameters.txt to read x, y and z extents and simulation time.
#   3. hydro_wt_output.txt, to plot the water table for each output timestep in the simulation.
# 
# Outputs:
#   A heatmap plot of water table depth across the landscape for each timestep.
# 
# To Do:
#   1. Add arguments to give a range of timesteps for outputs



#clear environment
rm(list=ls())

library(ggplot2)

load("hydro_setup_workspace.RData")

#args <- commandArgs(trailingOnly = TRUE)

params <- as.vector(read.table(file="hydro_parameters.txt",
                               header=FALSE))

#Model run time (days)
sim_time <- params[1,] / params[2,]


x_extent <- params[4,]
x_cells <- x_extent - 2
y_extent <- params[5,]
y_cells <- y_extent - 2
no_layers <- params[6,] -1

# x_extent <- params[2,]
# x_cells <- x_extent - 2
# y_extent <- params[3,]
# y_cells <- y_extent - 2
# no_layers <- params[4,] -1

# hydro_thickness <- as.vector(read.table(file="hydro_thickness.txt",
#                               header=FALSE))
# hydro_thickness <- hydro_thickness[,1]
# 
# thickness <- as.vector(0)
# n <- 1
# count <- 0
# for (i in 1:length(hydro_thickness)){
#   if (hydro_thickness[i] == 0){
#     thickness[n] <- hydro_thickness[i]
#     n <- n + 1
#   } else if (hydro_thickness[i] == 1){
#     next
#   } else if (hydro_thickness[i] == 12){
#     next
#   } else{
#     thickness[n] <- hydro_thickness[i] + 17
#     n <- n + 1
#   }
# }

thickness <- as.vector(0)
n <- 1
for (x in 1:x_extent){
  for (y in 1:y_extent){
    thickness[n] <- sum(fthickness[x,y,], na.rm=TRUE)
    n <- n+1
  }
}



array_thickness <- array(thickness, dim=c(y_extent,x_extent,sim_time))

# trim boundary cells
array_thickness <- array_thickness[2:(y_extent-1),2:(x_extent-1),]



#Read in hydro output file
hydro_wt_height <- as.vector(read.table(file = "hydro_wt_output.txt",
                                        header = FALSE, dec = ".",
                                        fill = TRUE))

hydro_wt_height <- hydro_wt_height[ ,1]
# trim boundary data
# hydro_wt_height_act <- hydro_wt_height[hydro_wt_height!=-999]

# create 3d array (x,y columns through time)
wt_array <- array(hydro_wt_height, dim=c(y_extent,x_extent,sim_time))

wt_array <- wt_array[2:(y_extent-1),2:(x_extent-1),]
wt_array[wt_array==-999] <- NA
wtd_array <- array_thickness - wt_array



#readline(prompt="Press [enter] to produce gganimate pngs...")

out_from <- 1
out_to <- sim_time

d <- x_extent * y_extent
 for (t in out_from:out_to){
  wtd_gganimate <- data.frame(matrix(NA,nrow=d,ncol=4))
  colnames(wtd_gganimate) <- c("day","x","y","wtd")
  counter <- 1
  
  for (x in 1:x_cells){
    for (y in 1:y_cells){
      wtd_gganimate[counter,1] <- t
      wtd_gganimate[counter,2] <- x
      wtd_gganimate[counter,3] <- y
      wtd_gganimate[counter,4] <- wtd_array[y,x,t]
      counter <- counter + 1
    }
  }
  
  # 
  #ditch green #17d817
  df_ani <- ggplot(wtd_gganimate, aes(x=x, y=y, fill = wtd, frame = day)) +
    geom_raster(aes(fill=wtd),interpolate=FALSE) +
    scale_fill_gradientn(colours=c("blue","palegreen","yellow"),
                         #breaks=c(0,15,30,45,116),
                         #limits=c(-0.25,60),
                         limits=c(-0.25,116),
                         #values=c(0,0.5,1),
                         #na.value="red",
                         guide=guide_colourbar(reverse = TRUE)) +
    ylab("Horizontal Distance Up Slope (m)") +
    xlab("Horizontal Distance Across Slope (m)") +
    labs(fill = "Water Table\nDepth (cm)") +
    ggtitle(paste("Day:",t))
  
  df_ani
  
  # filename <- paste("Day: ",t,".png")
  ggsave(file = paste("Day",t,".png"),dpi=300)
  print(paste("Complete:",t))
}

