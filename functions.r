# Generate Plotly scatter plot
scatterPlot <- function(df, x, y){
    g <- ggplot(hugo, aes_string(x, y))
    g <- g + geom_point()
    ggplotly(g)
}

# Generate Plotly jitter plot
jitterPlot <- function(df, x, y){
    g <- ggplot(hugo, aes_string(x, y))
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
    cmd <- paste0("\"", FijiPath, "\"", fileexpression)
    system(cmd)
}