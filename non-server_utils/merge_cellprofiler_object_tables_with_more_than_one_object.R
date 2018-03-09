
if (!require(plyr)) install.packages('plyr')
#if (!require(stringr)) install.packages('stringr')

rm(list=ls())#clear all variables

#input arduments
inputPath<-'C:/tempDat/Hugo/merge-test-20180225/EXTDRG_valid2_plate2_batch1_20--cellprofiler/'
outputFileName<-'merged_table.csv'
inputNamePattern<-'*Objects.csv' # can be pattern rather than name
subfolderNamePattern<-'W(?<WellNum>[0-9]{4})--(?<GeneName>[^-]*)--(?<SiRNATag>[^-]*)/P(?<Position>[0-9]{3})--[^-]*--[^-]*'

columnSeparator<-','
decimalSeparator<-'.'

cat ('Start CSV merging','\n')

# Identify input data files
input.Files <- dir( path = inputPath, pattern = inputNamePattern, full.names = FALSE, recursive = TRUE, include.dirs = TRUE)

# Combine files into one big table
Merged.Data<-data.frame()
library(plyr)
#library(stringr)
for (input.File in input.Files){
    cat ('Processing file: ')
    cat (input.File,'\n')
    
    Rep.Data.Headers <- read.csv( file.path( inputPath, input.File ), header=F, as.is = T, nrows = 2)
    Rep.Data.Headers[1,] <- gsub( 'Image', '', Rep.Data.Headers[1,], perl = TRUE)
    Rep.Data.Headers[3,] <- paste( Rep.Data.Headers[1,], Rep.Data.Headers[2,], sep='.')
    
    Rep.Data <- read.csv( file.path( inputPath, input.File ), header = F, as.is = T, skip = 2)
    names( Rep.Data ) <- Rep.Data.Headers[ 3, ]
    
    Rep.Data$FileName_DatasetTable<-basename(input.File)
    dataset.Subfolder <- dirname(input.File)
    Rep.Data$PathName_DatasetTable <- file.path('root',dataset.Subfolder)
    Rep.Match <- regexpr( subfolderNamePattern, dataset.Subfolder, perl = T )
    for( groupName in attr( Rep.Match, 'capture.name' ) ){
        Rep.Data[groupName]<-substr(dataset.Subfolder, 
                                    attr( Rep.Match, 'capture.start' )[,groupName],
                                    attr( Rep.Match, 'capture.start' )[,groupName]
                                    + attr( Rep.Match, 'capture.length' )[,groupName]
                                    - 1)
    }
    Merged.Data <- rbind.fill( Merged.Data, Rep.Data )
}

# Save output table
write.csv(Merged.Data, file=file.path( inputPath,outputFileName ), row.names = FALSE,quote = FALSE)

cat ('End CSV merging','\n')
