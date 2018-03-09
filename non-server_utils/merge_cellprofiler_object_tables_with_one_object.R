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
input.NamePattern<-'*objects.csv'
outputFileName<-'merged_object_table.csv'


#
# Code (normally, do not touch)
#

cat ('Looking for tables in directory: ', input.Directory, '\n')
input.Files <- dir( path = input.Directory, pattern = input.NamePattern, full.names = FALSE, recursive = TRUE, include.dirs = TRUE)
cat ('Number of tables: ', length( input.Files ), '\n')

cat ('Start merging...','\n')


# Combine files into one big table
Merged.Data <- data.frame()

#library(stringr)

for ( input.File in input.Files ) {
  
  cat ( 'Processing file: ' )
  cat ( input.File, '\n' )
  
  Rep.DataAndHeader <- read.csv( file.path( input.Directory, input.File ), header = T, as.is = T )
  
  Merged.Data <- rbind.fill( Merged.Data, Rep.DataAndHeader )
  
}
# Save output table
write.csv( Merged.Data, file = file.path( input.Directory, outputFileName ), row.names = FALSE, quote = FALSE )
  
cat ('Done!','\n')

