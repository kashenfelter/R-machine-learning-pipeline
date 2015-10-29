library('logging') 
library('FSelector')
library('sqldf')
library('randomForest')
library('caret')
library('xlsx')


#source('src/Configuration.R')

DimensionalityReduction.run <- function(step2=NA){
  
  basicConfig()
  
  if(!file.exists(step2.output.file)){
    logerror(paste0("missing file ",step2.output.file))
    #stop("missing step2 file!")
  }
  
  if(is.na(step2)){
    load(file=paste0(step2.output.file,".rda"))
    step2<-tidyData
    #step2 <- read.csv(file=step2.output.file,stringsAsFactors = T)
    loginfo(paste0("loaded ",step2.output.file))
    step2<-Setup.adjustDF(step2)
  }
  
  #chi squared 
  DimensionalityReduction.chiSquared(step2)
  
  #CFS filter
  DimensionalityReduction.variousFromFSelection(step2)
  
  DimensionalityReduction.fixXlsxColumnSize()
  
  write.csv(step2,file=step3.output.file,row.names = F)
  save(step2,file=paste0(step3.output.file,".rda"))
  loginfo(paste0("write file:" ,step3.output.file))
  
  #do the feature voter
  DimensionalityReduction.voteForBestFeatures()
  
  print("end")
  return(step2)
  
}



DimensionalityReduction.voteForBestFeatures <- function(){
  
  data1 <- read.xlsx(file=step3.featurSelection.overview,sheetName = "chiSquared")
  data2 <- read.xlsx(file=step3.featurSelection.overview,sheetName = "cfs")
  data3 <- read.xlsx(file=step3.featurSelection.overview,sheetName = "informationGain")
  data4 <- read.xlsx(file=step3.featurSelection.overview,sheetName = "gainRatio")
  data5 <- read.xlsx(file=step3.featurSelection.overview,sheetName = "RF_accuracy_decrease")
  data6 <- read.xlsx(file=step3.featurSelection.overview,sheetName = "RF_nodeImpurity_decrease")
  
  data1$points <- 1/1:nrow(data1)
  data2$points <- 1/1:nrow(data2)
  data3$points <- 1/1:nrow(data3)
  data4$points <- 1/1:nrow(data4)
  data5$points <- 1/1:nrow(data5)
  data6$points <- 1/1:nrow(data6)
  
  data2$features <- data2$selectedFeatures
  
  allC <- rbind(data1[,c("features","points")],data2[,c("features","points")],data3[,c("features","points")],data4[,c("features","points")],data5[,c("features","points")],data6[,c("features","points")])
  
  res <- sqldf("select features ,SUM(points) as totalPoints from allC group by features order by totalPoints DESC")
  
  write.xlsx2(res,file=step3.featurSelection.overview, sheetName = "FeatureVote",row.names = F,append = T)  
}


#Chi squared
DimensionalityReduction.chiSquared <- function(step2){
  options(java.parameters = "-Xmx24g")
  loginfo(paste0("start chiSquared feature selection"))
  
  formula = paste0(global.labelFeature,"~.")
  
  #do the chisquared
  weights <- chi.squared(formula, step2)
  
  weights$features <- rownames(weights) 
  
  weightsSorted <- sqldf("select features,attr_importance from weights order by attr_importance desc")
  
  #write.csv(weightsSorted,file=step3.chiaquared.file,row.names = F)
  
  write.xlsx2(weightsSorted,file=step3.featurSelection.overview, sheetName = "chiSquared",row.names = F)  
  
  
}


#CFS filter
DimensionalityReduction.variousFromFSelection <- function(step2){
  
  formula = paste0(global.labelFeature,"~.")
  
  loginfo(paste0("cfs feature selection"))
  #cfs
  
  options(java.parameters = "-Xmx24g")
  subset <- cfs(formula, step2)
  #write.csv(subset,file=step3.cfs.file,row.names = F)
  write.xlsx2(data.frame("selectedFeatures"=subset),file=step3.featurSelection.overview, sheetName = "cfs",row.names = F,append = T)  
  
  loginfo(paste0("information.gain feature selection"))
  #informationGain
  res <- information.gain(formula, step2)
  res$features <- rownames(res) 
  sorted <- sqldf("select features,attr_importance from res order by attr_importance desc")
  #write.csv(sorted,file=step3.informationGain.file,row.names = F)
  write.xlsx2(sorted,file=step3.featurSelection.overview, sheetName = "informationGain",row.names = F,append = T)  
  
  
  loginfo(paste0("gain.ratio feature selection"))
  #gain.ratio
  res <- gain.ratio(formula, step2)
  res$features <- rownames(res) 
  sorted <- sqldf("select features,attr_importance from res order by attr_importance desc")
  #write.csv(sorted,file=step3.gainRatio.file,row.names = F)
  
  write.xlsx2(sorted,file=step3.featurSelection.overview, sheetName = "gainRatio",row.names = F,append = T)  
  
  
  #RF - type1 (mean decrease in accuracy)
  
  #downSample to approx 1000
  #total <- nrow(step2)
  #perc <- 10000 /total
  #inTraining <- createDataPartition(step2[[global.labelFeature]], p = perc, list = FALSE)
  #forest = randomForest(step2[inTraining,!(names(step2) %in% global.labelFeature)],step2[inTraining,(names(step2) %in% global.labelFeature)], ntree = 1000, keep.forest = FALSE, importance = TRUE)
#   
#   loginfo(paste0("starting randomForest feature selection. This could take a while..."))
#   forest = randomForest(step2[,!(names(step2) %in% global.labelFeature)],step2[,(names(step2) %in% global.labelFeature)], ntree = 1000, keep.forest = FALSE, importance = TRUE)
#   
#   res = as.data.frame(importance(forest, type = 1))
#   res$features <- rownames(res) 
#   colnames(res)[1] = "attr_importance"
#   sorted <- sqldf("select features,attr_importance from res order by attr_importance desc")
#   
#   write.xlsx2(sorted,file=step3.featurSelection.overview, sheetName = "RF_accuracy_decrease",row.names = F,append = T)  
#   
#   
#   res2 = as.data.frame(importance(forest, type = 2))
#   res2$features <- rownames(res2) 
#   colnames(res2)[1] = "attr_importance"
#   sorted2 <- sqldf("select features,attr_importance from res2 order by attr_importance desc")
#   write.xlsx2(sorted2,file=step3.featurSelection.overview, sheetName = "RF_nodeImpurity_decrease",row.names = F,append = T)  
#   
}


DimensionalityReduction.fixXlsxColumnSize <- function(){
  
  wb <- loadWorkbook(step3.featurSelection.overview)
  sheets <- getSheets(wb)
  
  # set widths to 20
  setColumnWidth(sheets[[1]], colIndex=1:2, colWidth=20)
  autoSizeColumn(sheets[[1]], colIndex=1:2)
  
  setColumnWidth(sheets[[2]], colIndex=1, colWidth=20)
  autoSizeColumn(sheets[[2]], colIndex=1)
  
  setColumnWidth(sheets[[3]], colIndex=1:2, colWidth=20)
  autoSizeColumn(sheets[[3]], colIndex=1:2)
  
  setColumnWidth(sheets[[4]], colIndex=1:2, colWidth=20)
  autoSizeColumn(sheets[[4]], colIndex=1:2)
  
  
  setColumnWidth(sheets[[5]], colIndex=1:2, colWidth=20)
  autoSizeColumn(sheets[[5]], colIndex=1:2)
  
  setColumnWidth(sheets[[6]], colIndex=1:2, colWidth=20)
  autoSizeColumn(sheets[[6]], colIndex=1:2)
  
  
  saveWorkbook(wb,step3.featurSelection.overview)
  
  
}