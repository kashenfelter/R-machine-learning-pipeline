library(data.table)
library(bit64)
library(caret)

AnalyzeData.check <- function(){
  
  ds <- fread(input = "/home/sensei/projects/kagglescripts/data/springleaf/train.csv")
  ds$target <- as.factor(ds$target)
  
  total <- nrow(ds)
  perc <- 1000/total
  inTraining <- createDataPartition(ds[["target"]], p =perc, list = FALSE)
  
  ds <- as.data.frame(ds)
  randomPart <- ds[ inTraining,]
  
  summary(randomPart$target)
  
  
  #################analyze
  
  step3.featurSelection.overview <- "data_reduction.xlsx"
  global.labelFeature <- "target"
  
  DimensionalityReduction.chiSquared(randomPart)
  
  
}