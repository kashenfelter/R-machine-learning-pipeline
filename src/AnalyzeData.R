library(data.table)
library(bit64)
library(caret)

global.labelFeature <- "target"

AnalyzeData.check <- function(){
  
  
  options(java.parameters = "-Xmx24g")
  
  ds <- fread(input = "/home/sensei/projects/kagglescripts/data/springleaf/train.csv")
  ds$target <- as.factor(ds$target)
  
  total <- nrow(ds)
  perc <- 50000/total
  inTraining <- createDataPartition(ds[["target"]], p =perc, list = FALSE)
  
  ds <- as.data.frame(ds)
  
  for(k in seq(400,2000,by = 150)){
    
    
  step3.featurSelection.overview <- paste0("data_reduction_to",k,"_50k.xlsx")
  randomPart <- ds[ inTraining,(k-199):k]
  
  randomPart$target <- ds[ inTraining,c("target")]
  
  
  
  #################analyze
  
  
  DimensionalityReduction.chiSquared(randomPart)  #fails
  
  DimensionalityReduction.variousFromFSelection(randomPart) #fails
  
  }
  #res <- information.gain(target~., randomPart)
  
  #forest = randomForest(randomPart[,!(names(randomPart) %in% global.labelFeature)],randomPart[,(names(randomPart) %in% global.labelFeature)], ntree = 500, keep.forest = FALSE, importance = TRUE)
  
  
}