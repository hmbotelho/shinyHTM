# Generate Plotly scatter plot
scatterPlot <- function(df, x, y){
    g <- ggplot(df, aes_string(x, y))
    g <- g + geom_point()
    ggplotly(g)
}

# Generate Plotly jitter plot
jitterPlot <- function(df, x, y){
    g <- ggplot(df, aes_string(x, y))
    g <- g + geom_jitter()
    ggplotly(g)
}

# Generate Plotly heatmap
heatmapPlot <- function(){
    
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
                                 data.frame(wellnum = wellLayout[i,j], X = temp[1,"col"], Y = -temp[1,"row"]+r+1)
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