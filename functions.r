# To do:
#
# - Color by batch, treatment, replicate, ....
# - Plotly lasso to select images
# - Summarize multiple measurements simultaneously


#
# Generic functions
#
loadpackage <- function(pckg){
    if(!(pckg %in% installed.packages())){
        install.packages(pckg)
    }
    library(pckg, character.only=TRUE)
}

# Print a string to the R console and to the webpage
echo <- function(...){
    temp <- do.call("paste0", list(...))
    print(temp)
    message(temp)
}


# Reads a data.frame from csv, tsv, xls and xlsx files. The file format is auto-detected.
read.HTMtable <- function(filepath){
    # filepath : full path to file. includes the filename. In Windows this is a temporary folder where the file is named '0'
    # filename : original filename
    # Requires package xlsx
    
    tryCatch(read.xlsx(filepath, sheetIndex = 1, stringsAsFactors=FALSE), 
             error = function(e){
                 
                 # File not Excel
                 if(grepl("\t", readLines(filepath, n = 1))){
                     
                     # Tab-separated file
                     return(read.csv(filepath, sep = "\t", stringsAsFactors = FALSE))
                 }
                 
                 if(grepl(",", readLines(filepath, n = 1))){
                     
                     # Comma-separated file
                     return(read.csv(filepath, sep = ",", stringsAsFactors = FALSE))
                 }
                 
                 # Unknown file type
                 warning("Unknown file format. Loading an empty data table.")
                 return(data.frame())
                 
             },
             finally = function(e){
                 
                 # Excel file
                 return(read.xlsx(filepath, sheetIndex = 1, stringsAsFactors=FALSE))
             }
    )
}




# Generate Plotly scatter/jitter plot
pointPlot <- function(df, batch_col, batch, x, y, col_QC, highlightQCfailed, splitBy = "None", filterByColumn = "None", whichValues = "All"){

    # Initialize variables
    plotSymbols <- c(approved = 20, rejected = 4)
    
    
    # Subset batches
    if(batch != "All batches"){
        df <- df[df[[batch_col]] == batch,]
    }


    # Subset by columns
    if(filterByColumn != "None"){
        if(!("All" %in% whichValues) & !is.null(whichValues)){
            OKrows <- sapply(df[[filterByColumn]], function(x){
                x %in% whichValues
            })
            df <- df[OKrows,]
        }
    }    


    # Define the data to be plotted
    g <- ggplot(df, aes_string(x, y)) + ggtitle(batch)

    
    # Calculate the symbol to be used at each data point
    symbols <- if(highlightQCfailed){
        sapply(df[[col_QC]], function(x) ifelse(x, plotSymbols["approved"], plotSymbols["rejected"]))
    } else{
        plotSymbols["approved"]
    }

    
    # Define the plot type (scatter vs jitter) depending on the data types
    g <- if(class(df[[x]]) == "numeric" & class(df[[y]]) == "numeric"){
        g + geom_point(shape = symbols)
    } else{
        g + geom_jitter(shape = symbols)
    }

    # Customize plot
    if(splitBy != "None"){
        g <- g + 
            facet_grid(as.formula(paste("~", splitBy)), scales = "free_x") + 
            theme(strip.text.x = element_text(angle = 90))
    }

    # Output finished plot
    ggplotly(g)
}

# Generate Plotly boxplot
boxPlot <- function(df, batch_col, batch, x, y, col_QC, highlightQCfailed, highlightCenter = "No", splitBy = "None"){

    # Subset batches
    if(batch != "All batches"){
        df <- df[df[[batch_col]] == batch,]
    }
    
    
    # Hide rejected data points if requested
    if(highlightQCfailed){
        df <- df[df[[col_QC]],]
    }


    # Make plot
    g <- ggplot(df, aes_string(x, y))
    g <- g + geom_boxplot() +
        ggtitle(batch)
    
    # Customize plot
    if(highlightCenter != "No"){
        g <- switch(highlightCenter,
                    "Mean"   = g + stat_summary(fun.y = "mean",   colour = "red", size = 4, geom = "point", alpha = 0.5),
                    "Median" = g + stat_summary(fun.y = "median", colour = "red", size = 4, geom = "point", alpha = 0.5)
        )
    }
    if(splitBy != "None"){
        g <- g + 
        facet_grid(as.formula(paste("~", splitBy)), scales = "free_x") + 
        theme(strip.text.x = element_text(angle = 90))
    }
        
    # Output finished plot
    ggplotly(g)
}

# Generate Plotly heatmap
heatmapPlot <- function(df, measurement, batch, nrows, ncolumns, symbolsize=1, col_QC, highlightQCfailed){
    
    # Initialize variables
    plotSymbols <- c(approved = 15, rejected = 4)
    
    
    # Define the data to be plotted
    g <- ggplot(df, aes_string("heatX", "heatY", color = measurement))
    
    
    # Calculate the symbol to be used at each data point
    symbols <- if(highlightQCfailed){
        sapply(df[[col_QC]], function(x) ifelse(x, plotSymbols["approved"], plotSymbols["rejected"]))
    } else{
        plotSymbols["approved"]
    }
    
    
    # Other plot settings
    g <- g + geom_point(size=symbolsize, shape=symbols) + 
        scale_color_gradient2(midpoint=mean(df[[measurement]]), low="blue", mid="white", high="red") +
        ggtitle(batch) + 
        theme(panel.grid = element_blank()) +
        scale_x_continuous(breaks=1:ncolumns) + 
        scale_y_continuous(breaks = 1:nrows, labels = LETTERS[nrows:1]) + 
        theme(axis.title.x=element_blank(), axis.title.y=element_blank())
    
    
    # Output finished plot
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
    
    cmd <- paste0("\"", FijiPath, "\" -debug", fileexpression)
    
    # print command for user info and debugging
    print(cmd)
    
    # Evoke Fiji with the expression compiled above
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
            if(is.na(testvalue)){                   # Make sure "NA" is correctly interpreted
                testvalue <- "NA"}
            if(is.nan(testvalue)){                  # Make sure "NaN" is correctly interpreted
                testvalue <- "NaN"}

            temp <- switch (as.character(dfQC[i,"type"]),
                "Numeric QC"        = if(is.na(testvalue)){
                                            FALSE
                                      } else{
                                            as.numeric(testvalue) >= as.numeric(dfQC[i, "minimum"]) & as.numeric(testvalue) <= as.numeric(dfQC[i, "maximum"])
                                      },
                "Text QC"           = testvalue != dfQC[i, "minimum"],
                "Failed experiment" = testvalue != dfQC[i, "minimum"]
            )
            
            QCoverall <- c(QCoverall, temp)
        }
        
        all(QCoverall)
    })
    
}




#####################################################
################ From Tischi (below) ################
#####################################################

#
# Normalization
#

htmNormalization <- function(data, measurements, col_Experiment, transformation, gradient_correction, normalisation, negcontrol, col_QC, col_Well, col_Treatment, num_WellX, num_WellY) {

    echo("*")
    echo("* Data normalization")
    echo("*")
    echo("")

    
    # remove previously computed columns
    drops = names(data)[which(grepl("HTM_norm", names(data)))]
    data <- data[ ,!(names(data) %in% drops)]
    
    
    # get all necessary information
    measurements        <- sort(measurements)
    experiments         <- sort(unique(data[[col_Experiment]]))
#    transformation      <- transformation
#    gradient_correction <- gradient_correction
#    normalisation       <- normalisation
#    negcontrol          <- negcontrol
    
    
    # compute
    for (measurement in measurements) {
        
        echo("Measurement:")
        echo(measurement)
        echo("Negative Control:")
        echo("negcontrol")
        
        #
        # Check           
        #
        if( ! (measurement %in% names(data)) ) {
            cat(names(data))
            cat("\nError: selected measurement does exist in data columns!\n")
            return(data)
        }
        
        #
        # Analyze
        #
        
        manipulation <- "__"
        input <- measurement
        
        # Log2
        if(transformation == "log2") {
            
            echo("")
            echo("Log2:")
            echo("  Input: ", input)
            
            # compute log transformation
            # create new column name
            manipulation <- paste0(manipulation,"log2__")
            
            output = paste0("HTM_norm",manipulation,measurement)
            
            idsGtZero <- which(data[[input]]>0)
            idsSmEqZero <- which(data[[input]]<=0)
            data[idsGtZero,output] <- log2(data[idsGtZero,input]) 
            data[idsSmEqZero,output] <- NaN
            echo("  Output: ", output)
            echo("  Number of data points: ", length(data[[input]]))
            echo("  NaN's due to <=0: ", length(idsSmEqZero))
            
            # todo: this should be at a more prominent position
            #print("Replacing -Inf in log scores ******************************")
            #logScores = data[[output]]
            #finiteLogScores = subset(logScores,is.finite(logScores))
            #minimum = min(finiteLogScores)
            #idsInf = which(is.infinite(logScores))
            #logScores[idsInf] <- minimum
            #data[[output]] <- logScores
            
            #htmAddLog("Replacing Infinities in Log2 Score by")
            #htmAddLog(minimum)
            #htmAddLog("Affected Wells:")
            #for(id in idsInf) {
            #  htmAddLog(htm@wellSummary$treatment[id])
            #  htmAddLog(htm@wellSummary$wellQC[id])
            #  htmAddLog(htm@wellSummary[id,logScoreName])
            #  htmAddLog("")
            #}
            
            input <- output
            
        } # if log transformation
        
        
        if(gradient_correction == "median polish") {
            
            echo("  median polish of ", input)
            
            # also store the background
            gradient = paste0("HTM_norm",paste0(manipulation,"__medpolish_gradient__"),measurement)
            
            manipulation <- paste0(manipulation,"__medpolish_residuals__")
            output = paste0("HTM_norm",manipulation,measurement)
            
            data[[output]] = rep(NA,nrow(data))
            
            for(experiment in experiments) {
                
                echo("")
                echo("  Experiment: ",experiment)
                
                indices_all <- which((data[[col_Experiment]] == experiment))
                #indices_ok <- which((data[[col_Experiment]] == experiment) & (data[[col_QC]]) & !is.na(data[[input]]))
                
                # extract values
                xy = htm_convert_wellNum_to_xy(data[indices_all, col_Well], num_WellX, num_WellY) 
                mp = htmMedpolish(xx=xy$x, yy=xy$y, val=data[indices_all, input], num_WellX, num_WellY)
                
                data[indices_all, output] = mp$residuals
                data[indices_all, gradient] = mp$gradient
                
            } # experiment loop
            
            input <- output
            
        } #medpolish
        
        
        
        if( gradient_correction %in% c("median 7x7","median 5x5","median 3x3")) {
            
            echo("  median filter of ", input)
            
            gradient = paste0("HTM_norm",paste0(manipulation,"__",gradient_correction,"__gradient__"),measurement)
            manipulation <- paste0(manipulation,"__",gradient_correction,"__residuals__")
            output = paste0("HTM_norm",manipulation,measurement)
            
            data[[output]] = rep(NA,nrow(data))
            
            for(experiment in experiments) {
                
                echo("  Experiment: ", experiment)
    
                indices_all <- which((data[[col_Experiment]] == experiment))
                indices_ok  <- which((data[[col_Experiment]] == experiment) & (data[[col_QC]]) & !is.na(data[[input]]))
                
                xy = htm_convert_wellNum_to_xy(data[indices_ok, col_Well], num_WellX, num_WellY) 
                
                if(gradient_correction == "median 7x7") {
                    mp = htmLocalMedian(xx=xy$x, yy=xy$y, val=data[indices_ok, input], size=7, num_WellX, num_WellY)
                }
                if(gradient_correction == "median 5x5") {
                    mp = htmLocalMedian(xx=xy$x, yy=xy$y, val=data[indices_ok, input], size=5, num_WellX, num_WellY)
                }
                if(gradient_correction == "median 3x3") {
                    mp = htmLocalMedian(xx=xy$x, yy=xy$y, val=data[indices_ok, input], size=3, num_WellX, num_WellY)
                }
                
                data[indices_ok, output] = mp$residuals
                data[indices_ok, gradient] = mp$gradient
                
            } # experiment loop
            
            input <- output
            
        } #median filter
        
        
        if( gradient_correction %in% c("z-score 5x5")) {
            # Mean = E(X)
            # Variance = E(X^2)-E(X)^2
            # Z-Score = (Xi - E(X)) / Sqrt(E(X^2)-E(X)^2)
            
            echo("  5x5 z-score filter of ", input)
            
            # also store the background
            standard_deviation = paste0("HTM_norm",paste0(manipulation,"__5x5_standard_deviation__"),measurement)
            mean_value = paste0("HTM_norm",paste0(manipulation,"__5x5_mean__"),measurement)
            manipulation <- paste0(manipulation,"__5x5_z_score__")
            output = paste0("HTM_norm",manipulation,measurement)
            
            data[[output]] = rep(NA,nrow(data))
            data[[standard_deviation]] = rep(NA,nrow(data))
            data[[mean_value]] = rep(NA,nrow(data))
            
            for(experiment in experiments) {
                
                echo("  Experiment: ", experiment)
                
                indices_all <- which((data[[col_Experiment]] == experiment))
                xy = htm_convert_wellNum_to_xy(data[indices_all, col_Well], num_WellX, num_WellY) 
                
                mp = htmLocalZScore(xx=xy$x, yy=xy$y, val=data[indices_all, input], size=5, num_WellX, num_WellY)
                
                data[indices_all, mean_value] = mp$avg
                data[indices_all, standard_deviation] = mp$sd
                data[indices_all, output] = mp$z
                
                
            } # experiment loop
            
            input <- output
            
        } #median filter
        
        
        
        if(normalisation != "None selected") {
            
            echo("")
            echo("Per batch normalisation:")
            echo("  Method: ", normalisation)
            echo("  Input: ", input)

            # init columns
            manipulation <- paste0(manipulation,normalisation,"__")
            output = paste0("HTM_norm",manipulation,measurement)
            data[[output]] = NA
            echo("  Output: ",output)
            
            # computation
            #cat("\nComputing normalisations...\n")
            
            for(experiment in experiments) {
                
                #print("")
                #print(paste("  Experiment:",experiment))

                indices_all <- which((data[[col_Experiment]] == experiment))
                indices_ok  <- which((data[[col_Experiment]] == experiment) & (data[[col_QC]]) & !is.na(data[[input]]))
                
                if("All treatments" %in% negcontrol) {
                    indices_controls_ok <- indices_ok
                } else {
                    indices_controls_ok <- which((data[[col_Experiment]] == experiment) 
                                                 & !is.na(data[[input]]) 
                                                 & (data[[col_QC]]) 
                                                 & (data[[col_Treatment]] %in% negcontrol))
                }
                
                #print(paste("   Total", length(indices_all)))
                #print(paste("   Valid", length(indices_ok)))      
                #print(paste("   Valid Control", length(indices_controls_ok)))
                
                # extract control values 
                valuescontrol <- data[indices_controls_ok, input]
                #print(valuescontrol)
                
                nr_of_controls <- length(valuescontrol)
                meancontrol    <- mean(valuescontrol)    
                sigmacontrol   <- sd(valuescontrol) 
                mediancontrol  <- median(valuescontrol)
                madcontrol     <- mad(valuescontrol)  
                semcontrol     <- sigmacontrol/sqrt(nr_of_controls)     
                #print(paste("    Control Mean:", meancontrol))
                #print(paste("    Control SD:", sigmacontrol))
                #print(paste("    Control Median:", mediancontrol))
                #print(paste("    Control MAD:", madcontrol))
                
                if(normalisation == "z-score") {
                    data[indices_all, output] <- ( data[indices_all, input] - meancontrol ) / sigmacontrol
                } 
                else if(normalisation == "robust z-score") {
                    data[indices_all, output] <- ( data[indices_all, input] - mediancontrol ) / madcontrol
                } 
                else if(normalisation == "subtract mean ctrl") {
                    data[indices_all, output] <- data[indices_all, input] - meancontrol 
                }
                else if(normalisation == "divide by mean ctrl") {
                    data[indices_all, output] <- data[indices_all, input] / meancontrol 
                }
                else if(normalisation == "subtract median ctrl") {
                    data[indices_all, output] <- data[indices_all, input] - mediancontrol 
                }
                else if(normalisation == "divide by median ctrl") {
                    data[indices_all, output] <- data[indices_all, input] / mediancontrol 
                }
                
            } # experiment loop
            
            input <- output
            
        } # if normalisation
        
    } # measurement loop
    
    return(data)
    
}


#
# Spatial position related
#

htm_convert_wellNum_to_xy <- function(wellNum, plate.ncol, plate.nrow) {
    
    ### GET PLATE INFO
    plate.nwells = plate.nrow * plate.ncol
    
    ### intialise
    xx = vector(length=length(wellNum))
    yy = xx
    
    ### WELLS  
    plate.wellNumToRow = vector(length=plate.nwells);
    plate.wellNumToCol = vector(length=plate.nwells);
    iw = 1;
    for(ir in 1:plate.nrow) {
        for(ic in 1:plate.ncol) {
            plate.wellNumToRow[iw] = ir;
            plate.wellNumToCol[iw] = ic;
            iw=iw+1;
        }  
    }
    
    
    ## return
    
    list(y = plate.wellNumToRow[wellNum],
         x = plate.wellNumToCol[wellNum])
    
    
}


#
# Local data normalisation
#

htmMedpolish <- function(xx, yy, val, num_PosX, num_PosY) {
    
    
    # averaging for multi-sub-positions?
    m = matrix(nrow=num_PosX,ncol=num_PosY)        # from Hugo to Tischi: aren't the xy coordinates flipped?
    mi = m 
    for(i in seq(1:length(val))) {
        #print(paste(xx[i],yy[i],val[i]))
        m[xx[i],yy[i]] <- val[i]
        mi[xx[i],yy[i]] <- i # remember where the data belongs in the original format
    }
    #print("raw"); dev.new(); image(m, col=gray((0:32)/32))
    
    med = medpolish(m, maxiter = 100, na.rm = T)
    m_gradient <-  med$overall + outer(med$row,med$col, "+")
    m_residuals <- m - m_gradient
    
    #print("gradient"); dev.new(); image(m_gradient, col=gray((0:32)/32))
    #print("residuals"); dev.new(); image(m_residuals, col=gray((0:32)/32))
    
    # covert back
    val_gradient = vector(length=length(val))
    val_residual = vector(length=length(val))
    for(i in seq(1:length(val))) {
        val_gradient[mi[xx[i],yy[i]]] = m_gradient[xx[i],yy[i]]
        val_residual[mi[xx[i],yy[i]]] = m[xx[i],yy[i]] - m_gradient[xx[i],yy[i]] 
    }
    
    list(gradient = val_gradient,
         residuals = val_residual)
}

htmLocalMedian <- function(xx, yy, val, size, num_PosX, num_PosY) {
    
    echo("  median filter with size ", size)
    
    
    x <- htmXYVtoMatrix(xx, yy, val, num_PosX, num_PosY) # averaging for multi-sub-positions?
    m <- x$m
    
    m_gradient  <- as.matrix(focal(raster(m), matrix(1, size, size), function(z) median(z, na.rm=T), pad = T, padValue = NA))
    m_residuals <- m - m_gradient
    
    list(gradient  = htmMatrixToXYV(xx, yy, m_gradient, x$mi),
         residuals = htmMatrixToXYV(xx, yy, m_residuals, x$mi))
}

htmLocalZScore <- function(xx, yy, val, size, num_PosX, num_PosY) {
    
    echo("  local z_score filter with size ", size)
    
    
    x <- htmXYVtoMatrix(xx, yy, val, num_PosX, num_PosY) # averaging for multi-sub-positions?
    m <- x$m
    
    m_mean <- as.matrix(focal(raster(m), matrix(1, size, size), function(z) mean(z, na.rm=T), pad = T, padValue = NA))
    m_meansqr <- as.matrix(focal(raster(m^2), matrix(1, size, size), function(z) mean(z, na.rm=T), pad = T, padValue = NA))
    m_sd = sqrt( m_meansqr - m_mean^2 )
    m_z = (m - m_mean) / m_sd
    
    #print("before")
    #print(m[1:10,1:6])
    #print("avg")
    #print(m_mean[1:10,1:6])
    #print("sd")
    #print(m_sd[1:10,1:6])
    #print("z")
    #print(m_z[1:10,1:6])
    #ddd
    
    list(avg = htmMatrixToXYV(xx, yy, m_mean, x$mi),
         sd  = htmMatrixToXYV(xx, yy, m_sd, x$mi),
         z   = htmMatrixToXYV(xx, yy, m_z, x$mi))
}

htmXYVtoMatrix <- function(xx, yy, val, num_PosX, num_PosY) {
    m = matrix(nrow=num_PosY, ncol=num_PosX)
    mi = m 
    for(i in seq(1:length(val))) {
        m[yy[i],xx[i]] <- val[i]
        mi[yy[i],xx[i]] <- i # remember where the data belongs in the original format
    }
    return(list(m = m, mi = mi))
}

htmMatrixToXYV <- function(xx, yy, m, mi) {
    val = vector(length=length(xx))
    for(i in seq(1:length(xx))) {
        val[mi[yy[i],xx[i]]] = m[yy[i],xx[i]]
    }
    return(val)
}


#
# Treatment Summary
#

htmTreatmentSummary <- function(data, measurements, col_Experiment, col_Treatment, col_ObjectCount, col_QC, negative_ctrl, positive_ctrl, excluded_Experiments) {
    
    echo("")
    echo("Treatment Summary:")
    echo("******************")
    echo("")
    
    # get all necessary information
    measurement <- measurements
    experiments <- sort(unique(data[[col_Experiment]]))
    #experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    # negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    #transformation <- htmGetListSetting(htm,"statistics","transformation")
    treatments <- sort(unique(data[[col_Treatment]]))
    #colObjectCount <- colObjectCount
    
    # output
    echo(""); echo("Experiments:")
              echo(experiments)
    echo(""); echo("Number of treatments: ", length(treatments))
    echo(""); echo("Negative control: ", negative_ctrl)
    echo(""); echo("Positive control: ", positive_ctrl)
    echo(""); echo("Measurement: ", measurement)
    echo(""); echo("")
    
    if(!(measurement %in% names(data))) {
        echo("ERROR: measurement ", measurement, " does not exist in data")
        return(0)
    }
    if(!(col_ObjectCount %in% names(data))) {
        echo("ERROR: object count ", measurement, " does not exist in data")
        return(0)
    }
    
    
    numEntries = length(treatments)
    
    results <- data.frame(measurement      = rep(measurement,numEntries),
                          controls         = rep(negative_ctrl,numEntries),  
                          treatment        = rep(NA,numEntries),
                          batches          = rep(NA,numEntries),
                          means            = rep(NA,numEntries),
                          median__means    = rep(NA,numEntries),
                          #z_scores = rep(NA,numEntries),
                          #median__z_scores = rep(NA,numEntries),
                          
                          t_test__estimate = rep(NA,numEntries),
                          t_test__p_value  = rep(NA,numEntries),
                          t_test__signCode = rep(NA,numEntries),
                          
                          
                          #z_score__allBatches=rep(NA,numEntries),
                          #robust_z_score__allBatches=rep(NA,numEntries),
                          
                          mean_number_of_objects_per_image=rep(NA,numEntries),
                          
                          numObjectsOK     = rep(NA,numEntries),
                          numImagesOK      = rep(NA,numEntries), 
                          numReplicatesOK  = rep(NA,numEntries),
                          #numPositionsOK=rep(NA,numEntries),
                          #numPositions=rep(NA,numEntries),
                          stringsAsFactors = FALSE
    )
    
    
    
    ###################################
    # Compute stats
    ###################################
    
    echo("Computing statistics...")
    ids_treatments = split(1:nrow(data), data[[col_Treatment]])
    
    i=0
    
    for(ids in ids_treatments) {
        
        # ********************************
        # T-test  
        # ********************************
        
        # treatment name
        treat <- data[ids[1],col_Treatment]
        
        # init
        t_test__p_value  = NA
        t_test__signCode = NA
        t_test__estimate = NA
        z_scores         = NA
        median__z_scores = NA
        median__means    = NA
        means            = NA
        batches          = NA
        d = data.frame(value=NA, treatment=NA, experiment=NA)
        
        # compute
        if(1) { #treat %in% c("SETDB1_s19112")) {
            
            # only keep valid treatment values to find the corresponding experiments
            ids  <- ids[which((data[ids,col_QC]==1) & !(is.na(data[ids,measurement])))]
            exps <- unique(data[ids,col_Experiment])
            
            # extract treatment and control values of the respective experiments
            d <- subset(data, 
                        (data[[col_Treatment]] %in% c(treat,negative_ctrl))
                        & (data[[col_Experiment]] %in% exps) 
                        # & !(data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")) 
                        & (data[[col_QC]]==1)
                        & !(is.na(data[[measurement]])), 
                        select = c(col_Treatment,measurement,col_Experiment,col_ObjectCount))
            
            names(d)[names(d)==measurement]     <- "value"
            names(d)[names(d)==col_Treatment]   <- "treatment"
            names(d)[names(d)==col_Experiment]  <- "experiment"
            names(d)[names(d)==col_ObjectCount] <- "count"
            
            d$treatment = ifelse(d$treatment %in% negative_ctrl, "control", d$treatment)
            
            #print(d)
            
            if ( (sum(d$treatment=="control")>1) & (sum(d$treatment==treat)>1) ) {
                
                #d$experiment <- as.factor(substr(d$experiment, nchar(d$experiment)-7+1, nchar(d$experiment)))
                d$treatment <- as.factor(d$treatment)
                d$treatment <- relevel( d$treatment, "control" ) # control must be the 1st level for the linear model
                t <- t.test(d$value ~ d$treatment)  # as there typically is enough data no equal variance is assumed
                
                nBlocks = length(unique(d$experiment)) 
                n = nrow(d)
                #print(2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 ))
                
                t_test__p_value  <- 2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 )
                t_test__estimate <- t$estimate[2]
                t_test__signCode <- ifelse(t_test__p_value<0.001,"***",
                                           ifelse(t_test__p_value<0.01,"**",
                                                  ifelse(t_test__p_value<0.05,"*",
                                                         ifelse(t_test__p_value<0.1,"."," "
                                                         ))))
            }
            
            if ( (sum(d$treatment=="control")>=1) & (sum(d$treatment==treat)>=1) ) {
                
                d_ctrl = subset(d, d$treatment=="control")
                means_ctrl <- tapply(d_ctrl$value, d_ctrl$experiment, mean)
                sds_ctrl <- tapply(d_ctrl$value, d_ctrl$experiment, sd)
                
                d_treat = subset(d, d$treatment==treat)
                means_treat <- tapply(d_treat$value, d_treat$experiment, mean)
                
                tryCatch(z_scores <- (means_treat - means_ctrl) / sds_ctrl
                         , error = function(e) {
                             echo(d)
                             echo(treat)
                             echo(means_treat)
                             echo(negative_ctrl)
                             echo(means_ctrl)
                             #print(sds_ctrl)
                             echo(z_scores)
                             echo(ids)
                             echo(idsOld)
                             echo(data[ids,])
                             echo(data[idsOld,])
                             echo(e)
                             
                             
                             ddd
                         })
                
                median__z_scores = median(z_scores)
                
                z_scores = paste(round(z_scores,2),collapse=";")
                
            }
            
            #print(d)
            #print(treat)
            #print(means_treat)
            #print(negative_ctrl)
            #print(means_ctrl)
            #print(sds_ctrl)
            #print(z_scores)
            #ddd        
            
            
        } # select treatment for debugging
        
        
        if(!(treat %in% negative_ctrl)) {
            d_treated = subset(d, d$treatment==treat )
        } else {
            d_treated = subset(d, d$treatment=="control")
        }
        
        
        # these  values need no negative control, that's why they are outside of above if-statement
        means_treated <- tapply(d_treated$value, d_treated$experiment, mean)
        batches       <- paste(names(means_treated),collapse=";")
        means         <- paste(round(means_treated,3),collapse=";")
        median__means <- median(means_treated)
        
        
        i = i + 1
        results$treatment[i] <- treat
        results$batches[i] = batches
        results$means[i] = means
        results$median__means[i] = median__means
        results$t_test__p_value[i] = t_test__p_value
        results$t_test__signCode[i] = t_test__signCode
        results$t_test__estimate[i] = t_test__estimate
        #results$z_scores[i] = z_scores
        #results$median__z_scores[i] = median__z_scores
        results$numObjectsOK[i] = sum(d_treated$count)
        results$numImagesOK[i] = nrow(d_treated)
        results$numReplicatesOK[i]= length(unique(d_treated$experiment))
        results$mean_number_of_objects_per_image[i] = results$numObjectsOK[i]/results$numImagesOK[i]
        
        
    }  # treatment loop   
    
    
    echo("")
    echo("done. Created Treatment Summary Table.")
    
    
    # Diagnostics 
    
    #hist(res)
    #dev.new()
    #qqnorm(res)
    #qqline(res)
    #print(wellScoreForANOVA)
    
    # display controls plot: raw well scores
    #print("Plot control scores...")
    #htmJitterplot(htm, cx="experiment", cy=well_raw_score, .ylab=well_raw_score, datatype="wells", 
    #              treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
    #              showMedian = F, showMean = T)
    
    # display controls plot: batch corrected well scores
    #print("Plot batch corrected control scores...")
    #htmJitterplot(htm, cx="experiment", cy=wellMinusMeanCtrlScores, .ylab=wellMinusMeanCtrlScores, datatype="wells", 
    #              treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
    #              showMedian = F, showMean = T)
    
    
    # save controls plot
    #  htmJitterplot(htm, cx="experiment", cy=wellScoreForANOVA, .ylab=wellScoreForANOVA, datatype="wells", 
    #                treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
    #                showMedian = F, showMean = T, save2file = T, newdev = F)
    
    # save histogram as plot
    #  print(paste("histo:",wellMinusMeanCtrlScores))
    #  htmHisto(wellMinusMeanCtrlScores, datatype="wells", treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"), save2file=T)
    
    #htm@wellSummary$exp_treat = paste(htm@wellSummary$experiment,htm@wellSummary$treatment,sep="_")
    #edit(htm@wellSummary$exp_treat)
    #print(wellMinusMeanCtrlScores)
    # todo: plot only if makes sense
    #htmJitterplot(htm, cx="exp_treat", cy=wellMinusMeanCtrlScores, .ylab=wellMinusMeanCtrlScores, datatype="wells", 
    #                treatmentSubset = c(htmGetListSetting(htm,"statistics","negativeControl"),htmGetListSetting(htm,"statistics","positiveControl")),
    #                showMedian = F, showMean = T, save2file = F, newdev = F)
    
    
    #####
    
    if(positive_ctrl != "None selected") {
        
        echo("")
        echo("")
        echo("Checking positive and negative control separation in each batch:")
        echo("")
        echo("Measurement: ", measurement)
        echo("Positive control: ", positive_ctrl)
        echo("Negative control: ", negative_ctrl)
        echo("")
        
        z_scores = c()
        
        for(exp in experiments) {
            
            d <- subset(data, (data[[col_Experiment]]==exp) & (data[[col_QC]]==1) & !(is.na(data[[measurement]])), select = c(col_Treatment, measurement))
            names(d)[names(d) == measurement] <- "value"
            names(d)[names(d) == col_Treatment] <- "treatment"
            
            d_neg    <- subset(d,d$treatment == negative_ctrl)
            mean_neg <- mean(d_neg$value)
            sd_neg   <- sd(d_neg$value)
            n_neg    <- length(d_neg$value)
            
            d_pos    <- subset(d,d$treatment == positive_ctrl)
            mean_pos <- mean(d_pos$value)
            sd_pos   <- sd(d_pos$value)
            n_pos    <- length(d_pos$value)
            
            #print(paste(min_neg,max_neg,mean_pos,sd_pos))
            #probability_of_pos_outside_neg = 1 - integrate( function(x) {dnorm(x,mean=mean_pos,sd=sd_pos)}, min_neg, max_neg)$value
            z_score = (mean_pos-mean_neg) / sd_neg
            z_scores = c(z_scores, z_score)
            #t_value = (mean_pos-mean_neg) / sqrt(sd_pos^2+sd_neg^2)
            
            
            #print(paste0(exp,"  N_neg: ",n_neg,"  N_pos: ",n_pos,"  Probability: ",round(probability_of_pos_outside_neg,3))) #,"  t-value: ",t_value))
            #quality = ifelse(abs(z_score)<1,"XX",
            #                 ifelse(abs(z_score)<2,"X",""
            #                               ))
            quality = ""
            
            #if ( exp %in% excluded_Experiments ) {
            #    comment = "(Excluded)  " 
            #} else {
                comment = ""
            #}
            
            echo(comment, exp, "; N_neg: ", n_neg, "; N_pos: ", n_pos, "; mean z-score of positive controls: ", round(z_score,3), " ", quality) #,"  t-value: ",t_value))
            
        }
        
        echo("")
        echo("z-scores (N, mean, sd): ", length(z_scores), " ", mean(z_scores, na.rm=T), " ", sd(z_scores, na.rm=T))
        
    }
    
    
    # sorted hit-list with significance level * ** ***
    # plot colored according to significance level
    #with hits marked as sign
    
    results_ordered <- results[order(results$t_test__p_value),]
    return(results_ordered)
    
}
