library(parallel)


# Concatenates all csv files matching the pattern. Supports multi-core processing
# big disadvantage: I believe it is not possible to print the progress to the console.
concatenate.csv <- function(folder = getwd(), pattern = "objects\\.csv", recursive = TRUE, parallelize = TRUE){
    
    # Get file list
    file_list <- list.files(folder, pattern, full.names = TRUE, recursive = recursive)
    
    if(detectCores() <= 2 | !parallelize){
        # Read all files using single core processing
        
        output <- lapply(file_list, function(file){
            message(paste0("    ", file))
            read.csv(file, stringsAsFactors = FALSE)
        })
        
    } else{
        # Read all files using multi-core processing
        
        cluster <- makePSOCKcluster(detectCores() - 1)
        output <- parLapply(cluster, file_list, function(file){
            read.csv(file, stringsAsFactors = FALSE)
        })
        stopCluster(cluster)
    }

    # Return concatenated data.frame
    do.call("rbind", output)
}