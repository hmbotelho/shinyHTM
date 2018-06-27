#
# INPUT ( CHANGE HERE! )
#

input.Directory <- '/Volumes/almfscreen/Gbekor/ATAT1--analysed/tables'

####################################
# CODE, NORMALLY NO NEED TO CHANGE #
####################################

#
# Init
#

rm(list=ls()) #clear all variables
if ( ! require(plyr) ) install.packages('plyr')
library(plyr)
#if (!require(stringr)) install.packages('stringr')

#
# FUNCTIONS
#

concatenateTables <- function( input.Directory, input.FileNamesContain ) {
  
  input.Pattern = paste0( "*", input.FileNamesContain, "*" )
  
  cat ('Looking for tables in directory: ', input.Directory, '\n')
  input.Files <- dir( path = input.Directory, pattern = input.Pattern, full.names = FALSE, recursive = TRUE, include.dirs = TRUE)
  cat ('Number of tables: ', length( input.Files ), '\n')
  
  cat ('Start merging...','\n')
  
  
  # Combine files into one big table
  Merged.Data <- data.frame()
  
  #library(stringr)
  
  for ( input.File in input.Files ) {
    
    if ( ! ("merged" %in% input.File ) )
    {
      cat ( 'Processing file: ' )
      cat ( input.File, '\n' )
      Rep.DataAndHeader <- read.csv( file.path( input.Directory, input.File ), header = T, as.is = T )
      Merged.Data <- rbind.fill( Merged.Data, Rep.DataAndHeader )
    }
    
  }
  
  # Save output table
  output.FileName = paste0( "merged_", input.FileNamesContain, ".csv" )
  write.csv( Merged.Data, file = file.path( input.Directory, output.FileName ), row.names = FALSE, quote = FALSE )
  
  cat ('Done!','\n')
  
  Merged.Data
  
}


mergeImageAndObjectTables <- function( input.Directory, image.table, object.table )
{
  
  merged.table <-  merge( image.table, object.table )
  
  # Save output table
  output.FileName = paste0( "merged_images_and_objects.csv" )
  write.csv( merged.table, file = file.path( input.Directory, output.FileName ), row.names = FALSE, quote = FALSE )
  
  cat ('Done!','\n')
  
  merged.table
  
}

#
# MAIN
#

input.FileNamesContain <- 'objects' 
object.table <- concatenateTables( input.Directory = input.Directory, input.FileNamesContain = input.FileNamesContain )

input.FileNamesContain <- 'image' 
image.table <- concatenateTables( input.Directory = input.Directory, input.FileNamesContain = input.FileNamesContain )

merged.table <- mergeImageAndObjectTables( input.Directory, image.table, object.table )

