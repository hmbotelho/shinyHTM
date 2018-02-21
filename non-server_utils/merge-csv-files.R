
if (!require(plyr)) install.packages('plyr')
#if (!require(stringr)) install.packages('stringr')

rm(list=ls())#clear all variables


inputPath<-'/Volumes/cba/tischer/projects/transmission-3D-stitching-organoid-size-measurement--data/small-test-image-sequences--analysis/'
outputFileName<-'merged_table.csv'
inputNamePattern<-'*Objects.csv' # can be pattern rather than name
subfolderNamePattern<-'DataSet--(?<Well>\\w[0-9]{1,2})--W(?<WellNum>[0-9]{5})--P(?<Position>[0-9]{5})--Z--T--'

columnSeparator<-','
decimalSeparator<-'.'

cat ('Start CSV merging','\n')


# Identify input data files
input.Files<-dir( path = inputPath, pattern = inputNamePattern, full.names = FALSE,recursive = TRUE,include.dirs = TRUE)

# Combine files into one big table
Merged.Data<-data.frame()
library(plyr)
#library(stringr)
for (input.File in input.Files){
    cat ('Processing file: ')
    cat (input.File,'\n')
    Rep.Data <- read.csv(file.path(inputPath,input.File),as.is = T)
    Rep.Data$FileName_DatasetTable<-basename(input.File)
    dataset.Subfolder <- dirname(input.File)
    Rep.Data$PathName_DatasetTable <- file.path('root',dataset.Subfolder)
    Rep.Match <- regexpr( subfolderNamePattern, 'DataSet--B4--W00016--P00004--Z--T--', perl = T )
    for( groupName in attr( Rep.Match, 'capture.name' ) ){
        Rep.Data[groupName]<-substr(dataset.Subfolder, 
                                    attr( Rep.Match, 'capture.start' )[,groupName],
                                    attr( Rep.Match, 'capture.start' )[,groupName]
                                    + attr( Rep.Match, 'capture.length' )[,groupName]
                                    - 1)
    }
    Merged.Data<-rbind.fill(Merged.Data,Rep.Data)
}

# Save output table
write.csv(Merged.Data, file=file.path( inputPath,outputFileName ), row.names = FALSE,quote = FALSE)

cat ('End CSV merging','\n')
