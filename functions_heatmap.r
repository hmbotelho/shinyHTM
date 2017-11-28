library(ggplot2)


##########################################################
##   Define Functions
##########################################################


# Adds columns with coordinates for plotting
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
    
    mat_xAvg_well <- matrix(
        offset_plateborder + (0:(nWellX-1)) * interwell_distanceX + (0.5*(offset_wellborder*2+square_size*nPosX+posJitter*(nPosX-1))),
        nWellY,
        nWellX,
        byrow = TRUE)
    
    mat_yAvg_well <- matrix(
        rev(
            offset_plateborder + (0:(nWellY-1)) * interwell_distanceY + (0.5*(offset_wellborder*2+square_size*nPosY+posJitter*(nPosY-1)))
        ),
        nWellY,
        nWellX,
        byrow = FALSE)
    
    
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
    
    df$HTM_xAvg_well <- NA
    df$HTM_yAvg_well <- NA
    
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
            df[i, "HTM_xAvg_well"] <- mat_xAvg_well[coord_w]
            df[i, "HTM_yAvg_well"] <- mat_yAvg_well[coord_w]
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


# Heatmap plot of measurements in multiwell plates
plotHeatmap <- function(df, xMin_well = "HTM_xMin_well", xMax_well = "HTM_xMax_well", yMin_well = "HTM_yMin_well", yMax_well = "HTM_yMax_well", xMin_pos = "HTM_xMin_pos", xMax_pos = "HTM_xMax_pos", yMin_pos = "HTM_yMin_pos", yMax_pos = "HTM_yMax_pos", xAvg_well = "HTM_xAvg_well", yAvg_well = "HTM_yAvg_well", col_treatment = "Metadata_wellNum", treatmentREGEX = ".*", col_measurement = "measurement", posJitter = 0.2){
    library(ggplot2)
    
    all_xAvg_well <- sort(unique(df[[xAvg_well]]))
    all_yAvg_well <- sort(unique(df[[yAvg_well]]))
    
    nWellX <- length(all_xAvg_well)
    nWellY <- length(all_yAvg_well)
    
    ggplot(df) + 
        geom_rect(data=df[grepl(treatmentREGEX, df[[col_treatment]]),], mapping=aes_string(xmin=xMin_well, xmax=xMax_well, ymin=yMin_well, ymax=yMax_well), color="black", fill = NA, alpha = 0.5) +
        geom_rect(data=df, mapping=aes_string(xmin=xMin_pos, xmax=xMax_pos, ymin=yMin_pos, ymax=yMax_pos, fill = col_measurement), color = NA) + 
        theme(panel.grid = element_blank()) +
        scale_x_continuous(breaks = all_xAvg_well, labels = 1:nWellX) +
        scale_y_continuous(breaks = all_yAvg_well, labels = LETTERS[nWellY:1]) + 
        theme(axis.text.x = element_text(size=6, face="bold"), axis.text.y = element_text(size=6, face="bold")) + 
        theme(strip.text.x = element_text(size = 6)) + 
        scale_fill_distiller(palette = "Spectral", direction = 1)
}




##########################################################
##   Example with simulated data
##########################################################


set.seed(100)

# Simulate data
HTMdf <- data.frame(treatment = rep(sapply(1:96, function(x) paste(rep(sample(LETTERS[1:10],1), 4), collapse = "")), each = 4),
                    well      = rep(1:96, each = 4),
                    pos       = rep(1:4, 96),
                    value     = as.numeric(sapply(rnorm(96, 100, 20), function(x) rep(x, 4)*runif(4, .9, 1.1))),
                    stringsAsFactors = FALSE)


# Add coordinates for drawing polygons and outlines
HTMdf <- addHeatMapCoordinates(df      = HTMdf,
                               colWell = "well",
                               colPos  = "pos",
                               nWellX  = 12,
                               nWellY  = 8,
                               nPosX   = 2,
                               nPosY   = 2)

# Create heatmap as ggplot object
g <- plotHeatmap(df              = HTMdf, 
                 col_treatment   = "treatment", 
                 treatmentREGEX  = "AA.*", 
                 col_measurement = "value", 
                 posJitter = 0.2)

# Print heatmap
g