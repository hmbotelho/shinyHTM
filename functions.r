# To do:
#
# - Color by batch, treatment, replicate, ....
# - Plotly lasso to select images



    

# Generate Plotly scatter/jitter plot
pointPlot <- function(df, batch_col, batch, x, y){
        
    # Subset batches
    if(batch != "All batches"){
        df <- df[df[[batch_col]] == batch,]
    }


    # Define the data to be plotted
    g <- ggplot(df, aes_string(x, y)) + ggtitle(batch)

    
    # Define the plot type (scatter vs jitter) depending on the data types
    g <- if(class(df[[x]]) == "numeric" & class(df[[y]]) == "numeric"){
        g + geom_point()
    } else{
        g + geom_jitter()
    }


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
generateHeatmapCoordinates <- function(WellX, WellY, PosX, PosY, subposjitter = 0.2){
    
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
    
    temp <- temp[, c("wellnum", "posnum", "X", "Y")]
    row.names(temp) <- NULL
    temp
}

# Creates a 1-batch data.frame with all additional data required for a
makeHeatmapDataFrame <- function(df, WellX, WellY, PosX, PosY, subposjitter = 0.2, batch_col, batch, col_Well, col_Pos, col_QC = "HTM_QC"){

    if(batch == "All batches") return(NULL)

    # Subset data frame
    df <- df[df[[batch_col]] == batch,]

    # Calculate coordinates for heatmap
    dfCoords <- generateHeatmapCoordinates(WellX, WellY, PosX, PosY, subposjitter)
    
    # Initialize variables
    hmsymbols <- c(ok=15, rejected=4)   # Symbols for heatmaps [squares and crosses]
    plsymbols <- c(ok=16, rejected=4)   # Symbols for heatmaps [circles and crosses]
    
    # Add heatmap symbols
    df$hmsymbols <- sapply(df[[col_QC]], function(x) ifelse(x, hmsymbols["ok"],hmsymbols["rejected"]))

    # Add heatmap coordinates
    for(i in 1:nrow(df)){
        w  <- df[i,col_Well]
        p  <- df[i,col_Pos]
        xy <- dfCoords[dfCoords$wellnum == w & dfCoords$posnum == p, c("X", "Y")]
        
        df[i, "heatX"] <- xy[1,"X"]
        df[i, "heatY"] <- xy[1,"Y"]
    }
    
    df
}





# Apply QC
applyQC <- function(df, dfQC){

# This an example of how the 'dfQC' data.frame needs to look like
# 'measurement' is the name of one of the columns in 'df'
# All values in 'df' are text
# dfQC <- data.frame(type = c("Numeric QC", "Numeric QC", "Text QC", "Failed experiment"), measurement = c("Count_cell_all", "Count_cell_final", "FileName_PM", "Metadata_platePath"), minimum = c("50", "20", "image1.tif", "myplate_01"), maximum = c("500", "100", "image1.tif", "myplate_01"), stringsAsFactors = FALSE)

    # Make sure the data frame only contains character variables (no factors wanted!)
    dfQC[] <- lapply(dfQC, as.character)
    
    apply(df, 1, function(x){

        QCoverall <- NULL
        for(i in 1:nrow(dfQC)){
            
            testvalue <- x[dfQC[i, "measurement"]]
            
            temp <- switch (as.character(dfQC[i,"type"]),
                "Numeric QC"        = as.numeric(testvalue) >= as.numeric(dfQC[i, "minimum"]) & as.numeric(testvalue) <= as.numeric(dfQC[i, "maximum"]),
                "Text QC"           = testvalue != dfQC[i, "minimum"],
                "Failed experiment" = testvalue != dfQC[i, "minimum"]
            )
            
            QCoverall <- c(QCoverall, temp)
        }
        
        all(QCoverall)
    })
    
}