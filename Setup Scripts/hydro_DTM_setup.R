# classifying peatland landscape using local neighbourhood elevation

# projecting the shapes of peatlands, from a DEM, onto a plane surface

#clear environment
rm(list = ls())

args = commandArgs()

setwd(args[6])


# using an options file for reusability; arguments saved here for a run, not passed on command line
options <- read.table(file="setup_options.txt",header = TRUE,sep=",")

print("Read setup options") #test print to console.out on c# wpf

# if(FALSE){
#   # check that files exist etc
#   stop("some error condition")
# }


setSides <- function(dataFrame, sidesTo){
  numcol <- ncol(dataFrame)
  numrow <- nrow(dataFrame)
  dataFrame[1,] <- sidesTo
  dataFrame[,1] <- sidesTo
  dataFrame[numrow,] <- sidesTo
  dataFrame[,numcol] <- sidesTo
  return(dataFrame)
}

setCorners <- function(dataFrame, cornersTo){
  numcol <- ncol(dataFrame)
  numrow <- nrow(dataFrame)
  dataFrame[1,1] <- cornersTo
  dataFrame[1,numcol] <- cornersTo
  dataFrame[numrow,1] <- cornersTo
  dataFrame[numrow,numcol] <- cornersTo
  return(dataFrame)
}

statusrowcol <- function(row,col){
  print(paste('Row:',row,'/',numrows,'  Col:',col,'/',numcols))
}
statusrow <- function(row){
  print(paste('Row:',row,'/',numrows))
}


#read .asc file from GIS output (categorised where number = peat profile & properties)
# dem_surf <- read.table(file="ascDTM.asc", sep = " ", skip = 6)
# dem_surf <- read.table(file="surfDEM.txt", sep = " ", skip = 7)


# dem[1,1] (top, left) is NW corner
# dem is aligned to North as seen in View(dem)


# dem_surf <- dem_surf[,-1]
# # dem_surf <- dem_surf[1:100,1:200] # smaller subset for faster testing
# 
# dem_surf_min <- min(dem_surf, na.rm = TRUE)
# dem_surf_max <- max(dem_surf, na.rm = TRUE)
# 
# dem_surf[is.na(dem_surf)] <- -10000
# 
# write.table(dem_surf,file="dem_surface.txt",sep=" ", col.names = FALSE,row.names = FALSE)

# make a dummy classification of features
###key
#0 off
#101 diri
#102 neu
#200 first peat type (hag)
#201 second peat type (inter-hag)
#202
#203
#...
#300 first bare mineral type
#301
#302
#...



dem_surf <- read.table(file=toString(options$dem_surface_file), sep = " ")

if (options$classification){
  print(paste("Loading Classification from file: ",toString(options$classification_file)))
  classification <- read.table(file=toString(options$classification_file), sep = " ")
} else {
  # set all layers to a single peat type
  classification <- dem_surf
  classification[,] <- 201
  classification <- setSides(classification,102)
  classification <- setCorners(classification,0)
}


if(toString(options$dem_base_type) == "FILE"){
  print(paste("Loading Basal DEM from file: ",toString(options$dem_base_file)))
  dem_base <- read.table(file=toString(options$dem_base_file), sep = " ")
  
} else if(toString(options$dem_base_type) == "MINIMUM"){
  
  print("Creating Basal DEM from MINIMUM DEM values")
  # make a dummy basal dem
  fake_base_height <- floor(min(dem_surf))
  dem_base <- dem_surf
  dem_base[,] <- fake_base_height
  
} else if(toString(options$dem_base_type) == "FIXED"){
  
  print("Creating Basal DEM from FIXED DEM values")
  # make a dummy basal dem
  fixed_height <- options$dem_base_fixed_height
  dem_base <- dem_surf - fixed_height
}


# stop("force stop")

dem_thickness <- dem_surf - dem_base
# dem_thickness <- dem_thickness * 100
dem_thickness <- round(dem_thickness, 2)



numcols <- ncol(dem_surf)
numrows <- nrow(dem_surf)


layer_props <- read.table(file="layer_props.csv",header = TRUE,sep=",")
list_props <- unique(layer_props$Property)

layer_t <- layer_props[which(layer_props$Property == list_props[1]),]
layer_k <- layer_props[which(layer_props$Property == list_props[2]),]
layer_s <- layer_props[which(layer_props$Property == list_props[3]),]

# Placeholder for working with peat class layers
lastLayerPos <- length(layer_t[1,])

layer_t$numLayers <- NA
layer_t$sum <- NA
layer_k$numLayers <- NA
layer_s$numLayers <- NA



# Calculate number of active layers for each peat class
for (row in 1:nrow(layer_t)){
  layer_t[row,]$numLayers <-  length(which(!is.na(layer_t[1,-(1:2)])))
}
for (row in 1:nrow(layer_k)){
  layer_k[row,]$numLayers <-  length(which(!is.na(layer_k[1,-(1:2)])))
}
for (row in 1:nrow(layer_s)){
  layer_s[row,]$numLayers <-  length(which(!is.na(layer_s[1,-(1:2)])))
}
# Calculate total peat thickness for each peat class
for (row in 1:nrow(layer_t)){
  layer_t[row,]$sum <-  sum(layer_t[row,3:lastLayerPos], na.rm=TRUE)
}


if(all(layer_t$numLayers == layer_k$numLayers) && all(layer_t$numLayers == layer_s$numLayers)){
  print("Info: Number of layer properties matches. Continuing.")
} else {
  stop("Error: Number of layer properties does not match. Stopping.")
}


# validate total ncol from all files here
maxlayers <- max(layer_k$numLayers)


#number of layers array
# create array of int(layers), then fill array edges with 1
no_layers <- array(1, dim=c(numrows,numcols))
col_status <- array('on', dim=c(numrows,numcols))
hydro_k <- array(NA, dim=c(numrows,numcols,maxlayers))
hydro_s <- array(NA, dim=c(numrows,numcols,maxlayers))
hydro_t <- array(NA, dim=c(numrows,numcols,maxlayers))

# boundary and off conditions
for (row in 1:numrows){
  for (col in 1:numcols){
    if (classification[row,col] >= 200){ # everything above a 200 classification is 'on'.
      col_status[row,col] <- 'on'
    } else if (classification[row,col] == 101){
      col_status[row,col] <- 'diri'
    } else if (classification[row,col] == 102){
      col_status[row,col] <- 'neu'
    } else if (classification[row,col] == 0){
      col_status[row,col] <- 'off'
    } else {
      stop("Error: Incorrect classification number detected. Stopping.")
    }
  }
}


# setting numberof layers
for (row in 1:numrows){
  statusrow(row)
  for (col in 1:numcols){
    if ((classification[row,col] > 200)&(classification[row,col] < 300)) {
      select <- classification[row,col] - 200 # allows for multiple peat types: 201,202,203...
      
      
      if (dem_thickness[row,col] >= layer_t[select,ncol(layer_t)]){
        # EXPAND LOWER
        difference <- dem_thickness[row,col] - layer_t[select,ncol(layer_t)]
        
        # apply first properties with an expanded lower peat layer
        hydro_k[row,col,1] <- layer_k[select,1]
        hydro_s[row,col,1] <- layer_s[select,1]
        hydro_t[row,col,1] <- layer_t[select,1] + difference
        
        # apply the remaining layer properties as normal
        for (layer in 2:(layer_k[select,ncol(layer_k)])){
          hydro_k[row,col,layer] <- layer_k[select,layer]
          hydro_s[row,col,layer] <- layer_s[select,layer]
          hydro_t[row,col,layer] <- layer_t[select,layer]
        }
        
        no_layers[row,col] <- layer_t[select,]$numLayers
        
       } else if (dem_thickness[row,col] > (layer_t[select,ncol(layer_t)] - layer_t[select,1])){
        # if the profile fits the thickness with a reduced lower layer
        # reduce the lowest layer by difference
        difference <- dem_thickness[row,col] - layer_t[select,ncol(layer_t)]

        # apply first properties with an expanded lower peat layer
        hydro_k[row,col,1] <- layer_k[select,1]
        hydro_s[row,col,1] <- layer_s[select,1]
        hydro_t[row,col,1] <- layer_t[select,1] + difference
        
        # apply the remaining layer properties as normal
        for (layer in 2:(layer_k[select,ncol(layer_k)])){
          hydro_k[row,col,layer] <- layer_k[select,layer]
          hydro_s[row,col,layer] <- layer_s[select,layer]
          hydro_t[row,col,layer] <- layer_t[select,layer]
        }

        no_layers[row,col] <- layer_t[select,]$numLayers

      } else if (dem_thickness[row,col] <= 0){
        # if the layer has no thickness (catch exception)
        hydro_k[row,col,1] <- 0
        hydro_s[row,col,1] <- 0
        hydro_t[row,col,1] <- 0
        no_layers[row,col] <- 1


      } else {
        # reduce the thicknesses by proportions, to fit
        fit_thickness <- dem_thickness[row,col] # get thickness to fill
        prop_vector <- layer_t[select,] # get properties vector
        thickness_per <- fit_thickness / (prop_vector[1,maxlayers+2] - prop_vector[1,1]) # calculate proportional thickness divisor
        prop_vector[1,2:maxlayers] <- prop_vector[1,2:maxlayers] * thickness_per
        prop_vector[1,1] <- 0
        prop_vector$sum <- sum(prop_vector[1:maxlayers])

        for (layer in 1:(layer_k[select,ncol(layer_k)])){
          hydro_k[row,col,layer] <- layer_k[select,layer]
          hydro_s[row,col,layer] <- layer_s[select,layer]
          hydro_t[row,col,layer] <- layer_t[select,layer]
        }
        no_layers[row,col] <- layer_t[select,]$numLayers

      }
    
      
    } else if ((classification[row,col] == 101)|(classification[row,col] == 102)|(classification[row,col] == 0)){
      # boundary condition cells
      
      hydro_k[row,col,1] <- 0.0
      hydro_s[row,col,1] <- 0.0
      hydro_t[row,col,1] <- 0.0
      no_layers[row,col] <- 1
      
    } # add else minerals and restoration here
      
      
    }
  }


# =====
# wt bc
# =====
hydro_watertable <- dem_thickness


# ==========================
# old params
# ==========================

# 288 timesteps in a day, output daily (every 288 steps..)
tdaily <- 288
textent <- 30
ttotal <- textent * tdaily
pLayers <- maxlayers + 1
ponding_depth <- 25
cell_size <- 3.5
steady <- numcols * numrows
params <- c(ttotal,tdaily, tdaily, numcols,numrows,pLayers,
           steady,cell_size,ponding_depth,0.000)


write.table(params,file="hydro_parameters.txt",
           quote=FALSE,row.names=FALSE,col.names=FALSE)


# ==========================
# new params
# ==========================

# # 288 timesteps in a day, output daily (every 288 steps..)
# textent <- 30
# sim_layers <- maxlayers + 1
# ponding_depth <- 25
# cell_size <- 3.5
# params <- c(textent, numcols,numrows,sim_layers,
#             cell_size,ponding_depth)
# 
# 
# write.table(params,file="hydro_parameters.txt",
#             quote=FALSE,row.names=FALSE,col.names=FALSE)





# ==========================

vecNoLayers <- vector(mode="double",length=(length(which(!is.na(no_layers)))))
i <- 1
for (col in 1:numcols){
  for (row in 1:numrows){
    vecNoLayers[i] <- no_layers[row,col]
    i <- i+1
  }
}
write.table(vecNoLayers,file="hydro_no_layers.txt",
            row.names=FALSE,col.names=FALSE)



# ==========================

vecColStatus <- vector(mode="double",length=(length(which(!is.na(col_status)))))
i <- 1
for (col in 1:numcols){
  for (row in 1:numrows){
    vecColStatus[i] <- col_status[row,col]
    i <- i+1
  }
}
write.table(vecColStatus,file="hydro_column_status.txt",
            row.names=FALSE,col.names=FALSE,quote=FALSE)


# ==========================


vecBaltitude <- vector(mode="double",length=(length(which(!is.na(dem_base)))))
i <- 1
for (col in 1:numcols){
  for (row in 1:numrows){
    vecBaltitude[i] <- dem_base[row,col]
    i <- i+1
 }
}
write.table(vecBaltitude,file="hydro_baltitude.txt",
           row.names=FALSE,col.names=FALSE,quote=FALSE)

# ==========================


vecThickness <- vector(mode="double",length=(length(which(!is.na(hydro_t)))))
i <- 1
for (row in 1:numrows){
  for (col in 1:numcols){
   for (layer in 1:maxlayers){
     if (!is.na(hydro_t[row,col,layer])){
       vecThickness[i] <- hydro_t[row,col,layer]
       i <- i+1
     }
   }
 }
}
write.table(vecThickness,file="hydro_thickness.txt",
           row.names=FALSE,col.names=FALSE)

# ==========================

vecPorosity <- vector(mode="double",length=(length(which(!is.na(hydro_s)))))
i <- 1
for (row in 1:numrows){
  for (col in 1:numcols){
    for (layer in 1:maxlayers){
      if (!is.na(hydro_s[row,col,layer])){
        vecPorosity[i] <- hydro_s[row,col,layer]
        i <- i+1
      }
    }
  }
}
write.table(vecPorosity,file="hydro_s.txt",
           row.names=FALSE,col.names=FALSE)
# ==========================

vecConductivity <- vector(mode="double",length=(length(which(!is.na(hydro_k)))))
i <- 1
for (row in 1:numrows){
  for (col in 1:numcols){
    for (layer in 1:maxlayers){
      if (!is.na(hydro_k[row,col,layer])){
        vecConductivity[i] <- hydro_k[row,col,layer]
        i <- i+1
      }
    }
  }
}
vecConductivity <- format(vecConductivity, scientific = FALSE)
write.table(vecConductivity,file="hydro_k.txt",
           row.names=FALSE,col.names=FALSE,quote=FALSE)

# ==========================

vecwaterTable <- vector(mode="double",length=(length(which(!is.na(hydro_watertable)))))
i <- 1
for (col in 1:numcols){
  for (row in 1:numrows){
    vecwaterTable[i] <- hydro_watertable[row,col]
    i <- i+1
  }
}
write.table(vecwaterTable,file="hydro_wt_bc_input.txt",
           row.names=FALSE,col.names=FALSE,quote=FALSE)
# ==========================

save.image(file = "hydro_setup_workspace.RData")
