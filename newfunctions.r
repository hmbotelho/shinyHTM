library(ggplot2)
library(plotly)

image_concat <- data.frame(Metadata_wellNum = rep(1:384, each = 4),
                           Metadata_posNum = rep(1:4, 384),
                           measurement = rnorm(384*4, 5.2, 1),
                           batch = rep("plate_01", 384*4))


# image_concat <- data.frame(Metadata_wellNum = rep(1:20, each = 9),
#                            Metadata_posNum = rep(1:9, 20),
#                            measurement = rnorm(20*9, 5.2, 1),
#                            batch = rep("plate_01", 20*9))

# df        <- image_concat
# colWell   <- "Metadata_wellNum"
# colPos    <- "Metadata_posNum"
# nWellX    <- 5
# nWellY    <- 4
# nPosX     <- 3
# nPosY     <- 3
# posJitter <- 1


# This function adds columns with coordinates for plotting
addHeatMapCoordinates <- function(df, colWell, colPos, nWellX, nWellY, nPosX, nPosY, posJitter = 0.2){

    
    ############################################
    ########## Initialization ##################
    ############################################
    
    
    # Graphical settings for plotting
    offset_wellborder     <- 1 * posJitter
    offset_wellseparation <- 3 * posJitter
    offset_plateborder    <- 7 * posJitter
    square_size           <- 5 * posJitter
    
    
    # Initialize variables
    nWells <- nWellX * nWellY
    nPos   <- nPosX * nPosY
    
    interwell_distanceX <- nPosX*square_size+(nPosX-1)*posJitter+offset_wellborder*2+offset_wellseparation
    interwell_distanceY <- nPosY*square_size+(nPosY-1)*posJitter+offset_wellborder*2+offset_wellseparation
    
    
    # Matrices describing all plate wells and positions
    mat_wells    <- matrix(1:nWells, nWellY, nWellX, byrow = TRUE)
    mat_allWells <- matrix(unlist(tapply(1:nWells, rep(1:nWellY, each = nWellX), function(x) rep(rep(x, each = nPosX), nPosY))), nWellY * nPosY, nWellX * nPosX, byrow = TRUE)
    mat_allPos   <- matrix(rep(unlist(tapply(1:nPos, rep(1:nPosY, each = nPosX), function(x) rep(x, nWellX))), nWellY), nWellY*nPosY, nWellX*nPosX, byrow = TRUE)
    
    

    #################################################
    ########## Compute Coordinates ##################
    #################################################
    
    mat_xMin_pos <- matrix(
        sapply(1:nWellX, function(x) (x-1) * interwell_distanceX + (offset_plateborder + offset_wellborder+0:(nPosX-1)*(posJitter+square_size))),
        nWellX * nPosX,
        nWellX * nPosX,
        byrow = TRUE)
    
    mat_xMax_pos <- mat_xMin_pos + square_size
    
    mat_yMin_pos <- matrix(
        rev(
            sapply(1:nWellY, function(x) (x-1) * interwell_distanceY + (offset_plateborder + offset_wellborder+0:(nPosY-1)*(posJitter+square_size)))
        ),
        nWellY * nPosY,
        nWellX * nPosX,
        byrow = FALSE)
    
    mat_yMax_pos <- mat_yMin_pos + square_size
    
    mat_xMin_well <- matrix(
        offset_plateborder + (0:(nWellX-1))*(offset_wellborder*2+nPosX*square_size+(nPosX-1)*posJitter+offset_wellseparation),
        nWellY,
        nWellX,
        byrow = TRUE)
    
    mat_xMax_well <- mat_xMin_well + 2*offset_wellborder + nPosX*square_size + (nPosX-1)*posJitter
    
    mat_yMin_well <- matrix(
        rev(
            offset_plateborder + (0:(nWellY-1))*(offset_wellborder*2+nPosY*square_size+(nPosY-1)*posJitter+offset_wellseparation)
        ),
        nWellY,
        nWellX,
        byrow = FALSE)
    
    mat_yMax_well <- mat_yMin_well + 2*offset_wellborder + nPosY*square_size + (nPosY-1)*posJitter
    
    
    
    ###########################################################
    ########## Add coordinates to data frame ##################
    ###########################################################
    
    # Initialize data frame columns
    df$HTM_xMin_well <- NA
    df$HTM_xMax_well <- NA
    df$HTM_yMin_well <- NA
    df$HTM_yMax_well <- NA
    
    df$HTM_xMin_pos  <- NA
    df$HTM_xMax_pos  <- NA
    df$HTM_yMin_pos  <- NA
    df$HTM_yMax_pos  <- NA
    
    for(i in 1:nrow(df)){
        
        # Well and position numbers
        w <- df[i, colWell]
        p <- df[i, colPos]
        
        # Coordinates
        coord_w <- which(mat_wells == w, arr.ind = TRUE)
        coord_p <- which(mat_allWells == w & mat_allPos == p, arr.ind = TRUE)
        
        # Annotating data frame with coordinates
        if(nrow(coord_w) != 0){
            df[i, "HTM_xMin_well"] <- mat_xMin_well[coord_w]
            df[i, "HTM_xMax_well"] <- mat_xMax_well[coord_w]
            df[i, "HTM_yMin_well"] <- mat_yMin_well[coord_w]
            df[i, "HTM_yMax_well"] <- mat_yMax_well[coord_w]
        }
        if(nrow(coord_p) != 0){
            df[i, "HTM_xMin_pos"]  <- mat_xMin_pos[coord_p]
            df[i, "HTM_xMax_pos"]  <- mat_xMax_pos[coord_p]
            df[i, "HTM_yMin_pos"]  <- mat_yMin_pos[coord_p]
            df[i, "HTM_yMax_pos"]  <- mat_yMax_pos[coord_p]
        }
        
    }
    
    
    # Return annotated data frame
    df
}


df <- addHeatMapCoordinates(df      = image_concat,
                      colWell = "Metadata_wellNum",
                      colPos  = "Metadata_posNum",
                      nWellX  = 24,
                      nWellY  = 16,
                      nPosX   = 2,
                      nPosY   = 2,
                      posJitter = 0.2)

# plot data

ggplot() + geom_rect(data=df, mapping=aes(xmin=HTM_xMin_well, xmax=HTM_xMax_well, ymin=HTM_yMin_well, ymax=HTM_yMax_well), color=NA, fill = "white", alpha=0.2) + 
    geom_rect(data=df, mapping=aes(xmin=HTM_xMin_pos, xmax=HTM_xMax_pos, ymin=HTM_yMin_pos, ymax=HTM_yMax_pos, fill = measurement), color = NA)
	
# + geom_rect(data=df[df$Metadata_wellNum == 50,], mapping=aes(xmin=HTM_xMin_well, xmax=HTM_xMax_well, ymin=HTM_yMin_well, ymax=HTM_yMax_well), color="black", fill = "white", alpha = 0.5)	
