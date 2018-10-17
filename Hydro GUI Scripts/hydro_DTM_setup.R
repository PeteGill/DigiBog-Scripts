# classifying peatland landscape using local neighbourhood elevation

# projecting the shapes of peatlands, from a DEM, onto a plane surface

#clear environment
rm(list = ls())

# args = commandArgs()
# print(args)
# setwd(args[6])


# using an options file for reusability; arguments saved here for a run, not passed on command line

options <- read.table(file="setup\\setup_options.ini",header = TRUE,sep=",")

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



setwd("setup")
dem_surf <- read.table(file=toString(options$dem_surface_file), sep = " ")
setwd("..")

if (options$classification){
  print(paste("Loading Classification from file: ",toString(options$classification_file)))
  setwd("setup")
  classification <- read.table(file=toString(options$classification_file), sep = " ")
  setwd("..")
  classification <- setSides(classification,102)
  classification <- setCorners(classification,0)
} else {
  # set all layers to a single peat type
  classification <- dem_surf
  classification[,] <- 201
  classification <- setSides(classification,102)
  classification <- setCorners(classification,0)
}


if(toString(options$dem_base_type) == "FILE"){
  print(paste("Loading Basal DEM from file: ",toString(options$dem_base_file)))
  setwd("setup")
  dem_base <- read.table(file=toString(options$dem_base_file), sep = " ")
  setwd("..")
  
} else if(toString(options$dem_base_type) == "MINIMUM"){
  
  print("Creating Basal DEM from MINIMUM DEM values")
  fake_base_height <- floor(min(dem_surf))
  dem_base <- dem_surf + options$dem_base_fixed_height
  dem_base[,] <- fake_base_height
  
} else if(toString(options$dem_base_type) == "FIXED"){
  
  print("Creating Basal DEM from FIXED DEM values")
  # make a dummy basal dem
  fixed_height <- options$dem_base_fixed_height
  dem_base <- dem_surf - fixed_height
}


# SCALE ALL DEMS FROM M(ASL) TO CM(ABOVE BASE) HERE -----------------------

AslToAboveBase <- function(data){
  b <- min(data)
  data <- data - b
  return(data)
}

dem_base <- AslToAboveBase(dem_base)
dem_surf <- AslToAboveBase(dem_surf)

dem_base <- dem_base * 100
dem_surf <- dem_surf * 100

# stop("force stop")

dem_thickness <- dem_surf - dem_base

# dem_thickness <- dem_thickness * 100
# dem_base <- dem_base * 100
# dem_surf <- dem_surf * 100
# dem_thickness <- round(dem_thickness, 2)



numcols <- ncol(dem_surf)
numrows <- nrow(dem_surf)

setwd("setup")
layer_props <- read.table(file="layer_props.ini",header = TRUE,sep=",")
setwd("..")
list_props <- unique(layer_props$Property)

layer_t <- layer_props[which(layer_props$Property == list_props[1]),]
layer_k <- layer_props[which(layer_props$Property == list_props[2]),]
layer_s <- layer_props[which(layer_props$Property == list_props[3]),]

# Placeholder for working with peat class layers
lastLayerPos <- length(layer_t[1,])

# Just create NA placeholders for now.
# Prevents columns being counted in subsequent: length(...is.na(...))
layer_t$numLayers <- NA
layer_t$sum <- NA
layer_k$numLayers <- NA
layer_s$numLayers <- NA



# Calculate number of active layers for each peat class
for (row in 1:nrow(layer_t)){
  layer_t[row,]$numLayers <-  length(which(!is.na(layer_t[row,-(1:2)])))
}
for (row in 1:nrow(layer_k)){
  layer_k[row,]$numLayers <-  length(which(!is.na(layer_k[row,-(1:2)])))
}
for (row in 1:nrow(layer_s)){
  layer_s[row,]$numLayers <-  length(which(!is.na(layer_s[row,-(1:2)])))
}
# Calculate total peat thickness for each peat class
for (row in 1:nrow(layer_t)){
  layer_t[row,]$sum <-  sum(layer_t[row,3:lastLayerPos], na.rm=TRUE)
}

# Check that there are the same number of layers (not NA) for each peat class,
# against each other peat property.
if(all(layer_t$numLayers == layer_k$numLayers) && all(layer_t$numLayers == layer_s$numLayers)){
  print("Info: Number of layer properties matches. Continuing.")
} else {
  stop("Error: Number of layer properties does not match. Stopping.")
}


# Max number of peat layers across all peat classes
maxlayers <- max(layer_k$numLayers)


# number of layers array
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


# Assign peat properties and set number of layers per peat column
for (row in 1:numrows){
  # statusrow(row)
  for (col in 1:numcols){
    if ((classification[row,col] >= 200)&(classification[row,col] < 300)) {
      select <- classification[row,col]
      
      # PROPERTIES FIT IN THICKNESS -> EXPAND LOWER LAYER
      if (dem_thickness[row,col] >= layer_t[(which(layer_t[,]$Class_ID == select)),]$sum){
        # EXPAND LOWER
        difference <- dem_thickness[row,col] - layer_t[(which(layer_t[,]$Class_ID == select)),]$sum
        
        # apply first properties with an expanded lower peat layer
        hydro_k[row,col,1] <- layer_k[(which(layer_t[,]$Class_ID == select)),3]
        hydro_s[row,col,1] <- layer_s[(which(layer_t[,]$Class_ID == select)),3]
        hydro_t[row,col,1] <- layer_t[(which(layer_t[,]$Class_ID == select)),3] + difference
        
        # apply the remaining layer properties as normal
        for (layer in 2:(layer_k[(which(layer_t[,]$Class_ID == select)),]$numLayers)){
          hydro_k[row,col,layer] <- layer_k[(which(layer_k[,]$Class_ID == select)),layer+2]
          hydro_s[row,col,layer] <- layer_s[(which(layer_s[,]$Class_ID == select)),layer+2]
          hydro_t[row,col,layer] <- layer_t[(which(layer_t[,]$Class_ID == select)),layer+2]
        }
        
        no_layers[row,col] <- layer_t[(which(layer_t[,]$Class_ID == select)),]$numLayers
        
        # PROPERTIES FIT IF YOU REDUCE THE LOWER LAYER
       } else if (dem_thickness[row,col] > (layer_t[(which(layer_t[,]$Class_ID == select)),]$sum - layer_t[(which(layer_t[,]$Class_ID == select)),3])){
        # if the profile fits the thickness with a reduced lower layer
        # reduce the lowest layer by difference
        difference <- dem_thickness[row,col] - layer_t[(which(layer_t[,]$Class_ID == select)),]$sum

        # apply first properties with an expanded lower peat layer
        hydro_k[row,col,1] <- layer_k[(which(layer_k[,]$Class_ID == select)),3]
        hydro_s[row,col,1] <- layer_s[(which(layer_s[,]$Class_ID == select)),3]
        hydro_t[row,col,1] <- layer_t[(which(layer_t[,]$Class_ID == select)),3] + difference
        
        # apply the remaining layer properties as normal
        for (layer in 2:(layer_k[(which(layer_k[,]$Class_ID == select)),]$numLayers)){
          hydro_k[row,col,layer] <- layer_k[(which(layer_k[,]$Class_ID == select)),layer+2]
          hydro_s[row,col,layer] <- layer_s[(which(layer_s[,]$Class_ID == select)),layer+2]
          hydro_t[row,col,layer] <- layer_t[(which(layer_t[,]$Class_ID == select)),layer+2]
        }

        no_layers[row,col] <- layer_t[(which(layer_t[,]$Class_ID == select)),]$numLayers

      } else if (dem_thickness[row,col] <= 0){
        # if the layer has no thickness (catch exception ?)
        hydro_k[row,col,1] <- 0
        hydro_s[row,col,1] <- 0
        hydro_t[row,col,1] <- 0
        no_layers[row,col] <- 1

      # PROPERTIES DO NOT FIT -> REDUCE EVERYTHING TO FIT
      } else {
        # reduce the thicknesses by proportions, to fit
        fit_thickness <- dem_thickness[row,col] # get thickness to fill
        prop_vector <- layer_t[(which(layer_t[,]$Class_ID == select)),] # get properties vector
        last_layer <- layer_t[(which(layer_t[,]$Class_ID == select)),]$numLayers +2
        thickness_per <- fit_thickness / (prop_vector[1,]$sum - prop_vector[1,3]) # calculate proportional thickness divisor
        prop_vector[1,3:last_layer] <- prop_vector[1,3:last_layer] * thickness_per
        prop_vector[1,3] <- 0
        prop_vector$sum <- sum(prop_vector[3:last_layer])

        for (layer in 1:(layer_k[(which(layer_t[,]$Class_ID == select)),]$numLayers)){
          hydro_k[row,col,layer] <- layer_k[(which(layer_t[,]$Class_ID == select)),layer+2]
          hydro_s[row,col,layer] <- layer_s[(which(layer_t[,]$Class_ID == select)),layer+2]
          hydro_t[row,col,layer] <- layer_t[(which(layer_t[,]$Class_ID == select)),layer+2]
        }
        no_layers[row,col] <- layer_t[(which(layer_t[,]$Class_ID == select)),]$numLayers

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
# hydro_watertable[1,] <- 0
# hydro_watertable[,1] <- 0
# hydro_watertable[numrows,] <- 0
# hydro_watertable[,numcols] <- 0
# hydro_watertable[,] <- 0


# ==========================
# old params
# ==========================

# 288 timesteps in a day, output daily (every 288 steps..)

totalSteps <- options$simulation_time
outInterval <-  options$output_frequency
dailySteps <- options$hydro_timestep
pLayers <- maxlayers + 1
ponding_depth <- options$pond_depth
cell_size <- 100
steady <- numcols * numrows
params <- c(totalSteps,outInterval, dailySteps, numcols,numrows,pLayers,
           steady,cell_size,ponding_depth,0.000)


write.table(params,file="hydro_parameters.txt",
           quote=FALSE,row.names=FALSE,col.names=FALSE)


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
for (col in 1:numcols){
  for (row in 1:numrows){
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
for (col in 1:numcols){
  for (row in 1:numrows){
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
for (col in 1:numcols){
  for (row in 1:numrows){
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
setwd("setup")
save.image(file = "hydro_setup_workspace.RData")
setwd("..")
print("Setup Complete.")