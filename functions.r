# To do:
#
# - Update htm_qc values (remove statement in addHeatmapCoordinates())
#
# - Color by batch, treatment, replicate, ....
#
# - Plotly lasso to select images


# Initialize variables
    hmsymbols <- c(ok=15, rejected=4)   # Symbols for heatmaps [squares and crosses]
    plsymbols <- c(ok=16, rejected=4)   # Symbols for heatmaps [circles and crosses]

    col_QC <- "htm_qc"                  # Name of the column with QC [TRUE/FALSE i.e. OK/Rejected]

    
    
# Generate Plotly scatter plot
scatterPlot <- function(df, batch_col, batch, x, y){
    
    # Subset batches
    if(batch != "All batches"){
        df <- df[df[[batch_col]] == batch,]
    }
    
    # Make plot
    g <- ggplot(df, aes_string(x, y))
    g <- g + geom_point() +
        ggtitle(batch)
    ggplotly(g)
}

# Generate Plotly jitter plot
jitterPlot <- function(df, batch_col, batch, x, y){
    
    # Subset batches
    if(batch != "All batches"){
        df <- df[df[[batch_col]] == batch,]
    }
    
    # Make plot
    g <- ggplot(df, aes_string(x, y))
    g <- g + geom_jitter() +
        ggtitle(batch)
    ggplotly(g)
}

# Generate Plotly boxplot
boxPlot <- function(df, batch_col, batch, x, y){
    
    # Subset batches
    if(batch != "All batches"){
        df <- df[df[[batch_col]] == batch,]
    }
    
    # Make plot
    g <- ggplot(df, aes_string(x, y))
    g <- g + geom_boxplot() +
        ggtitle(batch)
    ggplotly(g)
}

# Generate Plotly heatmap
heatmapPlot <- function(df, measurement, batch, nrows, ncolumns, symbolsize=1){
    g <- ggplot(df, aes_string("heatX", "heatY", color = measurement))
    g <- g + geom_point(size=symbolsize, shape=df$hmsymbols) + 
        scale_color_gradient2(midpoint=mean(df[[measurement]]), low="blue", mid="white", high="red") +
        ggtitle(batch) + 
        theme(panel.grid = element_blank()) +
        scale_x_continuous(breaks=1:ncolumns) + 
        scale_y_continuous(breaks = 1:nrows, labels = LETTERS[nrows:1]) + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank())
    ggplotly(g)
}




# Can open multiple files at once
# filePath is an array of character strings
OpenInFiji <- function(filePath, FijiPath = "C:\\Fiji.app\\ImageJ-win64.exe"){
    filePath <- gsub("\\\\", "/", filePath)
    
    # Generate the expression opening each image
    fileexpression <- ""
    for (path in filePath){
        fileexpression <- paste0(fileexpression, " -eval \"open('/", path, "')\"")
    }
    
    # Evoke Fiji with the expression compiled above
    cmd <- paste0("\"", FijiPath, "\" -debug", fileexpression)
    system(cmd)
}

# Generate coordinates for heatmap
heatmapCoord <- function(WellX, WellY, PosX, PosY, subposjitter = 0.2){
    
    numWells <- WellX * WellY
    numSubpos <- PosX * PosY
    
    # Map well numbers to a plate layout
    wellLayout <- matrix(1:numWells, WellY, WellX, byrow = TRUE, dimnames = list(LETTERS[1:WellY], 1:WellX))
    
    # (x,y) coordinates for all well centers
    WellCenters <- data.frame(wellnum = integer(), X = integer(), Y = integer())
    for (i in 1:WellY){
        for (j in 1:WellX){
            temp <- which(wellLayout == wellLayout[i,j], arr.ind=TRUE)
            WellCenters <- rbind(WellCenters, 
                                 data.frame(wellnum = wellLayout[i,j], X = temp[1,"col"], Y = -temp[1,"row"]+WellY+1)
            )
        }
    }
    
    
    # Map subposition array
    posLayout <- matrix(1:numSubpos, PosY, PosX, byrow = TRUE, dimnames = list(LETTERS[1:PosY], 1:PosX))
    
    # (x,y) coordinates for each subposition cluster.
    PosCenters <- data.frame(posnum = integer(), X = numeric(), Y = numeric())
    for (u in 1:PosY){
        for (v in 1:PosX){
            temp <- which(posLayout == posLayout[u,v], arr.ind=TRUE)
            PosCenters <- rbind(PosCenters, 
                                data.frame(posnum = posLayout[u,v], X = temp[1,"col"]-0.5-PosX/2, Y = -temp[1,"row"]+PosY/2+0.5)
            )
        }
    }
    PosCenters[, c("X", "Y")] <- PosCenters[, c("X", "Y")] * subposjitter
    
    
    # (x,y) coordinates for all images in a plate
    temp <- data.frame(wellnum = integer(), posnum = integer(), X = numeric(), Y = numeric())
    for(w in 1:numWells){
        
        localcenter <- do.call("rbind", replicate(numSubpos, WellCenters[w,], simplify = FALSE))
        localcenter$posnum <- PosCenters$posnum
        localcenter$X <- localcenter$X + PosCenters$X
        localcenter$Y <- localcenter$Y + PosCenters$Y
        
        temp <- rbind(temp, localcenter)
    }
    
    row.names(temp) <- NULL
    temp
}

# Annotate a data frame with coordinates for heatmap plotting
addHeatmapCoordinates <- function(df, dfCoords, batch_col, batch, col_Well, col_Pos){

    if(batch == "All batches") return(NULL)
    
    df <- df[df[[batch_col]] == batch,]
    
    df$heatX <- NA
    df$heatY <- NA
#remove this asap#####################################################
    df[col_QC] <- TRUE
    df$hmsymbols <- hmsymbols["ok"]
    
    for(i in 1:nrow(df)){
        w  <- df[i,col_Well]
        p  <- df[i,col_Pos]
        xy <- dfCoords[dfCoords$wellnum == w & dfCoords$posnum == p, c("X", "Y")]
        
        df[i, "heatX"] <- xy[1,"X"]
        df[i, "heatY"] <- xy[1,"Y"]
        df[i, "hmsymbols"] <- ifelse(df[i,col_QC], hmsymbols["ok"], hmsymbols["rejected"])
    }
    
    df
}

