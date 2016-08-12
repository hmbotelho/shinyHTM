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