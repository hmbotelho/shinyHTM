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

inputPath<-'/Volumes/cba/tischer/projects/transmission-3D-stitching-organoid-size-measurement--data//small-test-image-sequences--analysis'
outputFileName<-'merged_table.csv'
dataset.Pattern<-'(?<Well>\\w[0-9]{1,2})--W(?<WellNum>[0-9]{5})--P(?<Position>[0-9]{5})--Z--T'
fileNamePattern<-'\\w[0-9]{1,2}--W[0-9]{5}--P[0-9]{5}--Z--T--(?<Suffix>.*)\\.csv'#

suffixes.Dataset.Tables<-c('ApplyClassifier')
suffixes.Object.Tables<-c('foreground.tif--Volume--AnalyzeObjects','foreground.tif--BoundingBox--AnalyzeObjects','foreground.tif--Ellipsoid--AnalyzeObjects')

columnLabel.Dataset<-'DataSetID_FACT'
columnLabel.Object<-'Label'

columnSeparator<-','
decimalSeparator<-'.'
skip.Input.First.Column<-TRUE

cat ('Start CSV merging','\n')
setwd(inputPath)

# Identify input data files
input.Files<-data.frame(SubPath=dir(path='.', pattern = '*.csv', full.names = FALSE,recursive = TRUE,include.dirs = TRUE),stringsAsFactors = FALSE)
nFiles<-nrow(input.Files)
for (fileIndex in 1:nFiles){
    input.Files$FileName[fileIndex]<-basename(input.Files$SubPath[fileIndex])
}
input.Files<-input.Files[grep(pattern = fileNamePattern,x = input.Files$FileName,value = FALSE,perl = TRUE),]

extractPatternGroups<-function(inputTable,pattern,compColumnName){
    ndata<-nrow(inputTable)
    for (dataIndex in 1:ndata){
        match<-regexpr(pattern,inputTable[dataIndex,compColumnName],perl = TRUE)
        matched.groups<-attr(match,'capture.name')
        #cat(matched.groups)
        for(groupName in matched.groups){
            if (!(groupName %in% names(inputTable))){
                inputTable[,groupName]<-NA
            }
            inputTable[dataIndex,groupName]<-substr(inputTable[dataIndex,compColumnName], 
                                                     attr(match, 'capture.start')[,groupName],
                                                     attr(match, 'capture.start')[,groupName]
                                                     + attr(match, 'capture.length')[,groupName]
                                                     - 1)
        }
        
    }
    return (inputTable)
}

input.Files<-extractPatternGroups(inputTable = input.Files,pattern = fileNamePattern,compColumnName = 'FileName')

#combine separately tables of different types

bindTableLines<-function(fileSubPathes){
    result<-data.frame(stringsAsFactors = FALSE)
    for (subPath in fileSubPathes){
        imported.Table<-read.csv(subPath,as.is = T,dec = decimalSeparator,sep = columnSeparator)
        if (skip.Input.First.Column)
            imported.Table<-imported.Table[-1]
        result<-rbind.fill(result,imported.Table)
    }
    return(result);
}

table.List<-list()
for (suffix in c(suffixes.Dataset.Tables,suffixes.Object.Tables)){
    table.List[[suffix]]<-bindTableLines(input.Files$SubPath[input.Files$Suffix==suffix])
}

table.List[[suffixes.Dataset.Tables[1]]]<-extractPatternGroups(inputTable = table.List[[suffixes.Dataset.Tables[1]]],
                                                               pattern = dataset.Pattern,
                                                               compColumnName = columnLabel.Dataset)

Merged.Data<-table.List[[suffixes.Object.Tables[1]]]
n.Suffixes.Object.Tables<-length(suffixes.Object.Tables)
if(n.Suffixes.Object.Tables>1){
    for (suffix.Index in 2:n.Suffixes.Object.Tables){
        Merged.Data<-merge(Merged.Data,table.List[[suffixes.Object.Tables[suffix.Index]]],
                           by=c(columnLabel.Dataset,columnLabel.Object),all=TRUE)
    }
}

for (suffix in suffixes.Dataset.Tables){
    Merged.Data<-merge(Merged.Data,table.List[[suffix]],
                       by=c(columnLabel.Dataset),all=TRUE)
    
}

# 
# Save output table
write.csv(Merged.Data, file=outputFileName,row.names = FALSE,quote = FALSE)

cat ('End CSV merging','\n')
