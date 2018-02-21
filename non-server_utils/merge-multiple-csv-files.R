# Merging CSV files csv files produced by cluster jobs implementing different jobs on different datasets
# Current assumptions
# 1. File name consists of the dataset-specific part (prefix) and analysis job specific part (suffix)
# 2. Subfolder names do not need to be considered. All required information for matching datasets is stored in filenames.
# Output:
# - merged table
# - by default stored under inputPath
# - Information about well and position is extracted from fileName

if (!require(plyr)) install.packages('plyr')
#if (!require(stringr)) install.packages('stringr')

rm(list=ls())#clear all variables

suffixes.Dataset.Tables<-c('ApplyClassifier')
suffixes.Object.Tables<-c('foreground.tif--Volume--AnalyzeObjects','foreground.tif--BoundingBox--AnalyzeObjects','foreground.tif--Ellipsoid--AnalyzeObjects')

columnLabel.Dataset<-'DataSetID_FACT'
columnLabel.Object<-'Label'


inputPath<-'C:/tempDat/Tischi/small-test-image-sequences--analysis--20180221'
outputFileName<-'merged_table.csv'
#inputNameSuperPattern<-'(?<Prefix>.*)%s.csv' #can be pattern rather than name
#PrefixNamePattern<-'(?<Well>\\w[0-9]{1,2})--W(?<WellNum>[0-9]{5})--P(?<Position>[0-9]{5})--Z--T--'
dataset.Pattern<-'(?<Well>\\w[0-9]{1,2})--W(?<WellNum>[0-9]{5})--P(?<Position>[0-9]{5})--Z--T'
fileNamePattern<-'\\w[0-9]{1,2}--W[0-9]{5}--P[0-9]{5}--Z--T--(?<Suffix>.*)\\.csv'#


columnSeparator<-','
decimalSeparator<-'.'

cat ('Start CSV merging','\n')
setwd(inputPath)

# Identify input data files
input.Files<-data.frame(SubPath=dir(path='.', pattern = '*.csv', full.names = FALSE,recursive = TRUE,include.dirs = TRUE),stringsAsFactors = FALSE)
nFiles<-nrow(input.Files)
for (fileIndex in 1:nFiles){
    input.Files$FileName[fileIndex]<-basename(input.Files$SubPath[fileIndex])
}
input.Files<-input.Files[grep(pattern = fileNamePattern,x = input.Files$FileName,value = FALSE,perl = TRUE),]



for (fileIndex in 1:nFiles){
    match<-regexpr(fileNamePattern,input.Files$FileName[fileIndex],perl = TRUE)
    matched.groups<-attr(match,'capture.name')
    #cat(matched.groups)
    for(groupName in matched.groups){
        if (!(groupName %in% names(input.Files))){
            input.Files[,groupName]<-NA
        }
        input.Files[fileIndex,groupName]<-substr(input.Files$FileName[fileIndex], 
                                                 attr(match, 'capture.start')[,groupName],
                                                 attr(match, 'capture.start')[,groupName]
                                                 + attr(match, 'capture.length')[,groupName]
                                                 - 1)
    }
    
}

#combine separately tables of different types

bindTableLines<-function(fileSubPathes){
    result<-data.frame(stringsAsFactors = FALSE)
    for (subPath in fileSubPathes){
        
        rbind.fill(result,read.csv(subPath,as.is = T))
    }
    return(result);
}

table.List<-list()
for (suffix in c(suffixes.Dataset.Tables,suffixes.Object.Tables)){
    bb<-input.Files$SubPath[input.Files$Suffix==suffix]
    cat(suffix)
    aa<-bindTableLines(input.Files$SubPath[input.Files$Suffix==suffix])
    table.List[[suffix]]<-bindTableLines(input.Files$SubPath[input.Files$Suffix==suffix])
}


initial.Files<-input.Files[input.Files$Suffix==suffixes.Dataset[1],]
nInitialFiles<-nrow(initial.Files)

Merged.Data<-data.frame(stringsAsFactors = FALSE)
for (datasetIndex in 1:nInitialFiles){
    
    
    
}

# Combine files into one big table
Merged.Data<-data.frame()
library(plyr)
#library(stringr)
for (input.File in input.Files){
    cat ('Procssing file: ')
    cat (input.File,'\n')
    Rep.Data<-read.csv(input.File,as.is = T)
    Rep.Data$FileName_DatasetTable<-basename(input.File)
    dataset.Subfolder<-dirname(input.File)
    Rep.Data$PathName_DatasetTable<-file.path('root',dataset.Subfolder)
    Rep.Match<-regexpr(subfolderNamePattern,'DataSet--B4--W00016--P00004--Z--T--',perl = T)
    for(groupName in attr(Rep.Match,'capture.name')){
        Rep.Data[groupName]<-substr(dataset.Subfolder, 
                                    attr(Rep.Match, 'capture.start')[,groupName],
                                    attr(Rep.Match, 'capture.start')[,groupName]
                                    + attr(Rep.Match, 'capture.length')[,groupName]
                                    - 1)
    }
    Merged.Data<-rbind.fill(Merged.Data,Rep.Data)
}

# Save output table
write.csv(Merged.Data, file=outputFileName,row.names = FALSE,quote = FALSE)

cat ('End CSV merging','\n')
