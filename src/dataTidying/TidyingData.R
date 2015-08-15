library('logging') 
library('stringr')


#load config file
source('src/Configuration.R')
source('src/setup/Setup.R')


#Tyding data
#This is irst step in pipeline.  Input is raw data organized in csv file.  Method TidyingData.loadRawData will load data, parse it,
#all categorical features will be trimmed and set to lower case. Missing values , defined in config value by param will
#be replaced with NA

#this method will create codebook of features and tydi data set ion csv file.  Check config params under results in section ##Tyding data (Step1)
#in confif file


TidyingData.run <- function(codeBookRaw = step1.result.code.book.raw ,resultsFolder = step1.result.folder){
  
  loginfo(paste0("START STEP-1  , TIDYING THE DATA *****"))
  
  #config logging
  basicConfig()
  
  #create necessary folders if needed
  Setup.createResultsFolderStructure()
  
  #load raw data - all strings as factors
  rawData <- read.csv(file=fileLocation,sep = separator,stringsAsFactors = T)
  loginfo(paste0("file ", fileLocation ," loaded"))
  
  #remove . from features name
  colnames(rawData) <- gsub("\\.","_",colnames(rawData))
  
  
  #trim , and set to lower replace missing rawData marked as ? with NA
  for(column in colnames(rawData)){
    
    if(class(rawData[[column]])=="factor"){
      loginfo(paste0("tyding column ", column))
      rawData[[column]] <- sapply(rawData[[column]],str_trim)
      rawData[[column]] <- as.factor(sapply(rawData[[column]],tolower))
    }
    
    if(nrow(rawData[rawData[column]==missingSign,][column])>0 ){
      
      rawData[rawData[column]==missingSign,][column] <- NA
    }
  }
  
  rawData <- Setup.adjustDF(rawData)
  
  #create code book of RAW data
  TidyingData.createCodeBook(codeBookRaw,rawData)
  
  tidyData <- rawData
  
  #save(tidyData,file="tmp/tidyData.rda")
  loginfo(paste0("save result of step1 (tidying data) in tmp/tidyData.rda"))
  
  #also set the class feature to be factor
  tidyData[[global.labelFeature]] <- as.factor(tidyData[[global.labelFeature]])
  
  write.csv(tidyData,file=step1.result.tidy.file,row.names = F)
  loginfo(paste0("save tidy data in ",step1.result.tidy.file))
  
  save(tidyData,file=paste0(step1.result.tidy.file,".rda"))
  
  return(tidyData)
}


TidyingData.createCodeBook <- function(outputfileLocation,df){
  
  total <- nrow(df)
  
  percentOfMissing <- sapply(colnames(df),function(colname){round(nrow(df[is.na(df[[colname]]),]) * 100/total,3)})
  
  
  d=data.frame(features=colnames(df),type=sapply(colnames(df),function(colname){class(df[[colname]])}),levels=sapply(colnames(df),function(colname){length(levels(df[[colname]]))}),missing=sapply(colnames(df),function(colname){nrow(df[is.na(df[[colname]]),])}),percentOfMissing)
  d$type <- as.character(d$type)
  
  if(length(d[d$type=="integer",]$type)>0){
    d[d$type=="integer",]$type = "Integer"
  }
  
  if(length(d[d$type=="factor",]$type)>0){
    d[d$type=="factor",]$type = "Categorical"
  }
  
  if(length(d[d$type=="numeric",]$type)>0){
    d[d$type=="numeric",]$type = "Double"
  }
  
  d <- cbind(feature_id=1:nrow(d),d)
  
  write.csv(d,file=outputfileLocation,row.names = F)
  
  loginfo(paste0("created code book ", outputfileLocation))
  
  
}

