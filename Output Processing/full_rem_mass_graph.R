#Processing DigiBog 2D Model Outputs
#Produces long-profile of Fractional Remaining Mass
#Peter Gill 2017-08-10

library("ggplot2")
library("lattice")
library("dplyr")
library("tidyr")


#Read in info file
info <- read.table(file = "010_DigiBog_BB_IN_information.txt", header = FALSE, dec = ".", fill = TRUE)

#Define the cols and spatial step
cols <- as.integer(info[14,1] - 2)
spatial_step <- as.integer(info[15,1])

# Name of simulation for plot titles. Useful to define the ox decay value and 
#Q10 values used in the simulation.
#simulation <- toString(readline(prompt = "Enter the name of the simulation "))
simulation <- "sim"

#---------------------------------------------------------

rem_mass <- read.csv(file = "190_DigiBog_BB_OUT_rem_mass_file.txt", header = FALSE, dec = ".", fill = TRUE)
# Read in column outputs
baltitude <- read.table(file = "050_DigiBog_BB_IN_baltitude.txt", header = FALSE, dec = ".", fill = TRUE)
col_height <- data.frame(read.table(file = "070_DigiBog_BB_OUT_column_height.txt", header = FALSE, dec = ".", fill = TRUE))
wt_height <- data.frame(read.table(file = "080_DigiBog_BB_OUT_wt_height.txt", header = FALSE, dec = ".", fill = TRUE))
wt_depth <- data.frame(read.table(file = "110_DigiBog_BB_OUT_wt_depth.txt", header = FALSE, dec = ".", fill = TRUE))
summer_wt_depth <- data.frame(read.table(file = "160_DigiBog_BB_OUT_wt_depth_summer.txt", header = FALSE, dec = ".", fill = TRUE))

#Tidy up data
# Remove -999.000 values if they exist
wt_height <- subset(wt_height, wt_height[ ,1] != -999.000)
wt_depth <- subset(wt_depth, wt_depth[ ,1] != -999.000)
summer_wt_depth <- subset(summer_wt_depth, summer_wt_depth[ ,1] != -999.000)
col_height <- subset(col_height, col_height[ ,1] != -999.000)

# Calculate the number of years in the simulation
years <- as.integer(dim(wt_height)[1]/cols)

#Create groups for each column and number of years to be used to subset data
column_groups <- data.frame(rep(1:cols, years))
years_group <- data.frame(rep(1:years, each = cols))

#Calculate the spatial extent for plotting
extent <- column_groups * spatial_step

#Remove 0.00 and diri values from hydro_baltitude file
baltitude <- subset(baltitude, baltitude[ ,1] > 0)
#cols+1 is to ensure that the dir value is removed and ALL base altitude heights
#are included.
baltitude <- baltitude[2:(cols + 1), ]

baltitude <- as.data.frame(rep(baltitude, years))

# Name the columns of the vectors created above
colnames(wt_height)[1] <- 'Wt_height'
colnames(wt_depth)[1] <- 'Mean_yr_wtd'
colnames(col_height)[1] <- 'Col_height'
colnames(column_groups)[1] <- 'Col_no'
colnames(years_group)[1] <- 'Years'
colnames(baltitude)[1] <- 'Alt'
colnames(summer_wt_depth)[1] <- 'Mean_summer_wtd'

# Create a data frame of the column data and grouping vectors
col.wt.heights <- data.frame(years_group, column_groups, baltitude, wt_height, col_height, baltitude + wt_height, baltitude + col_height, col_height - wt_height, extent, wt_depth, baltitude + col_height - wt_depth, baltitude + col_height - summer_wt_depth)

#Name the additional data frame columns for altitude and water-table depth plots
colnames(col.wt.heights)[6] <- 'Wt_alt'
colnames(col.wt.heights)[7] <- 'Col_alt'
colnames(col.wt.heights)[8] <- 'Wt_depth'
colnames(col.wt.heights)[9] <- 'Spatial_step'
colnames(col.wt.heights)[11] <- 'Mean_wt_alt'
colnames(col.wt.heights)[12] <- 'Summer_wt_alt'

#Save dataframe as csv file)
write.csv(file = "DB_BB_dataframe.csv", col.wt.heights)


#Rename columns
colnames(rem_mass)[1] <- 'col_num'
colnames(rem_mass)[2] <- 'z_layer'
colnames(rem_mass)[3] <- 'mass'
colnames(rem_mass)[4] <- 'thickness'
colnames(rem_mass)[5] <- 'elevation'

#Count total layers
total_layers <- nrow(rem_mass)

#Vector of column base altitudes
col_base_alt <- slice(baltitude,1:cols)
#Vector of column y extent distances
col_base_hor <- col.wt.heights$Spatial_step[1:cols]

#For each layer, calculate 'height' as height + elevation
for (i in 1:total_layers){
  rem_mass$height[i] <- col_base_alt[rem_mass$col_num[i],1] + rem_mass$elevation[i]
}

rem_mass_sort <- arrange(rem_mass,desc(z_layer))


#---------------------------------------------------------

rem_mass_old <- rem_mass
#Create ID for each layer
rem_mass <- unite(rem_mass,col_num,z_layer,col="id",sep=".")

values <- rem_mass
values$thickness <- NULL
values$elevation <- NULL
values$height <- NULL


ids <- rem_mass$id

#Create new data frame for polygon drawing; 3 rows (four corners) for each layer
positions <- data.frame( id = rep(ids, each=4))
positions$x <- 0
positions$y <- 0

#Change heights from 'list' to 'double'
rem_mass$height <- unlist(rem_mass$height)



row_count = 1

#For each layer in the data frame
for (i in 1:total_layers){
  
  #Top Left
  positions$x[row_count] <- col_base_hor[round(as.integer(rem_mass$id[i]))] - 100
  positions$y[row_count] <- rem_mass$height[i]
  
  row_count = row_count + 1
  
  #Top Right
  positions$x[row_count] <- col_base_hor[round(as.integer(rem_mass$id[i]))] + 100
  positions$y[row_count] <- rem_mass$height[i]
  
  
  row_count = row_count + 1
  
  #Bottom Right
  positions$x[row_count] <- col_base_hor[round(as.integer(rem_mass$id[i]))] + 100
  positions$y[row_count] <- rem_mass$height[i] - rem_mass$thickness[i]
  
  
  
  row_count = row_count + 1
  
  #Bottom Left
  positions$x[row_count] <- col_base_hor[round(as.integer(rem_mass$id[i]))] - 100
  positions$y[row_count] <- rem_mass$height[i] - rem_mass$thickness[i]
  
  
  row_count = row_count + 1
  
}



datapoly <- merge(values, positions, by = c("id"))
for (i in 1:nrow(datapoly)){
  datapoly$mass[i] <- datapoly$mass[i] * 100
}

p <- ggplot(datapoly, aes(x = x, y = y)) +
  geom_polygon(aes(fill = mass, group = id ))
p <- p + scale_x_reverse()
p + scale_fill_gradientn(name="Remaining\nMass (%)",breaks=c(25,50,75),labels=c("25","50","75"),colours = rainbow(4)) +
  labs(title = paste("Remaining Mass Profile: ",years," years"),y="Height (cm)", x= "y extent (cm)")


dev.copy2pdf(file = "Remaining_Mass.pdf")




