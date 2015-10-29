
source('src/dataTidying/TidyingData.R')
source('src/exploratoryAnalysis/ExploratoryAnalysis.R')
source('src/dimensionalityReduction/DimensionalityReduction.R')
source('src/predictionModeling/PredictionModeling.R')
source('src/finalOutput/FinalOutput.R')

Pipeline.start <- function(){
  
  #setup
  Setup.createResultsFolderStructure()
  
  
  #load data
  
  #STEP 1
  step1Res <- TidyingData.run()
  
  #STEP 2
  step2Res <- ExploratoryAnalysis.run(step1Res)
  
  #STEP 3 
  step3Res <- DimensionalityReduction.run(step2Res)
  
  
  #STEP4 
  PredictionModeling.run(step3Res)
  
  FinalOutput.create()
  
  
  
}