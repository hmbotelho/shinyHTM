#
# Init
#

rm(list=ls()) #clear all variables
if ( ! require(plyr) ) install.packages('plyr')
library(plyr)
#if (!require(stringr)) install.packages('stringr')


#
# INPUT ( CHANGE HERE! )
#

input.Directory <- '/Volumes/almfscreen/Gbekor/ATAT1--analysed/tables'
input.FileNamesContain <- 'objects' 
mergeTables( input.Directory = input.Directory, input.FileNamesContain = input.FileNamesContain )


#
# Code (normally, do not touch)
#

mergeTables <- function( input.Directory, input.FileNamesContain ) {

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

}
