
library('logging') 
library('sqldf')
library('randomForest')
library('caret')
library('xlsx')

library('parallel')
library('doMC')
library('data.table')

source('Configuration.R')



PredictionModeling.run <- function(step3=NA){
  
  
  basicConfig()
  
  loginfo(paste0("start PredictionModeling phase (Step4)  "))
  
  if(!file.exists(step3.output.file)){
    logerror(paste0("missing file ",step3.output.file))
    stop("missing step3 file!")
  }
  
  if(is.na(step3)){
    #LOAD FILE
    load(file=paste0(step3.output.file,".rda"))
    step3<-step2
    step3 <- read.csv(file=step3.output.file,stringsAsFactors = T)
    step3<-Setup.adjustDF(step3)
    loginfo(paste0("loaded ",step3.output.file))
  }
  
  
#   if(length(step4.features.selected)>0){
#     step3 <- step3[,c(step4.features.selected,global.labelFeature)]
#   }
  
  #CREATE TRAIN AND VALIDATION SET
  if(!(file.exists(step4.train.file) & file.exists(step4.validate.file) )){
    #splitting the data into training/testing and validation sets
    #stratified sampling
    loginfo(paste0("create train and validate set, use split coefficient   ",step4.split.coefficient))
    
    
    total <- nrow(step3)
    
    perc_1000 <- 1000/total
    
    
    inTraining <- createDataPartition(step3[[global.labelFeature]], p =perc_1000, list = FALSE)
    
    random1000 <- step3[ inTraining,]
    
    
    step3  <- step3[-inTraining,]
    
    write.csv(random1000,file=step4.random1000.file,row.names = F)
    save(random1000,file=paste0(step4.random1000.file,".rda"),row.names = F)
    
    
    loginfo(paste0("created file  ",step4.random1000.file))
    
    
    inTraining <- createDataPartition(step3[[global.labelFeature]], p =step4.split.coefficient, list = FALSE)
    
    train <- step3[ inTraining,]
    validate  <- step3[-inTraining,]
    
    #save 
    write.csv(train,file=step4.train.file,row.names = F)  #better save as rda
    loginfo(paste0("created file  ",step4.train.file))
    
    write.csv(validate,file=step4.validate.file,row.names = F)
    loginfo(paste0("created file  ",step4.validate.file))
  }else{ #not happens a often!
    loginfo(paste0("good, train/validate files already present...  will use them  "))
    train <- read.csv(file=step4.train.file)
    validate  <- read.csv(file=step4.validate.file)
  }
  
  
  #create various models
  models <- step4.classification.models.list
  
  res <- lapply(models,function(model){
    PredictionModeling.createModel(train,validate,model)
  })
  
  
  #final result
  resultingDF <- rbindlist(res)
  
  sortBy_precisionPositive <- resultingDF[with(resultingDF, order(-precisionPositive)), ]
  sortBy_recallPositive <- resultingDF[with(resultingDF, order(-recallPositive)), ]
  sortBy_FmeasurePositive <- resultingDF[with(resultingDF, order(-FmeasurePositive)), ]
  
  write.xlsx2(sortBy_precisionPositive,file=step4.modelResults.overview, sheetName = "sortBy_precisionPositive",row.names = F)
  write.xlsx2(sortBy_recallPositive,file=step4.modelResults.overview, sheetName = "sortBy_recallPositive",row.names = F,append = T)  
  write.xlsx2(sortBy_FmeasurePositive,file=step4.modelResults.overview, sheetName = "sortBy_FmeasurePositive",row.names = F,append = T)  
  
  #fix formatting
  PredictionModeling.fixXlsxColumnSize(step4.modelResults.overview)
  
  
  #NOW DO VOTING
  loginfo(paste0("start voting.... "))
  #PredictionModeling.voting(train,validate)
  
}




#load saved model for voting
PredictionModeling.getSavedModel <- function(model){
  
  saveFolder <- step4.models.folder
  
  if(!file.exists(paste0(saveFolder,"/",model,".rda"))){
    
    stop("error, file ",paste0(saveFolder,"/",model,".rda")," doesn't exist")
  }else{
    
    load(file=paste0(saveFolder,"/",model,".rda"))
    message("loading.. ", paste0(saveFolder,"/",model,".rda"))
    
  }
  return(fit)
  
}


PredictionModeling.voting <- function(trainData,validateData){
  
  
  models <- step4.classification.models.list
  
  loadedModels <- lapply(models,PredictionModeling.getSavedModel)
  
  classIndex <- grep(global.labelFeature, colnames(trainData))
  
  predicts <- lapply(loadedModels,function(md){
    predict(md,newdata=validateData[, -classIndex])
  })
  
  finalDF <- data.frame()
  
  numberOfVoters <- length(step4.classification.models.list)
  
  
  
  
  #voters
  for(voters in 2:numberOfVoters){
    
    vec <- combn(models, voters)
    
    for(combination in 1: ncol(vec)){
      
      #go through all combinations
      
      votes <- lapply(match(vec[,combination],models), function(ind){
        
        predicts[[ind]]
        #as.data.frame(as.numeric(as.character((predict(fit,newdata=validatingSet)))))
        
      })
      
      
      #create preds
      
      preds <- do.call("cbind", votes)
      
      res <- rowMeans(preds)
      
      final <- ifelse(res>1,2,1)
      
      validateData[[global.labelFeature]] <- as.character(validateData[[global.labelFeature]])
      
      metrics <- confusionMatrix(final,validateData[[global.labelFeature]],global.labelFeature)
      
      
      index = 1:2 # 2 class
      positiveClassNumeric <- index[levels(validateData[[global.labelFeature]])==step4.positive.class.value]
      
      
      metrics <- confusionMatrix(final,as.numeric(validateData[[global.labelFeature]]),as.character(positiveClassNumeric))
      
      accuracy <- metrics$overall[1]
      recallPositive <- metrics$byClass[1] 
      recallNegative <- metrics$byClass[2] 
      precisionPositive <- metrics$byClass[3]
      precisionNegative <- metrics$byClass[4]
      
      FmeasurePositive <- 2*precisionPositive*recallPositive/(precisionPositive+recallPositive)
      FmeasureNegative <- 2*precisionNegative*recallNegative/(precisionNegative+recallNegative)
      
      name <- paste(vec[,combination],collapse = "_")
      
      #step4.positive.class.value
      customMetrics <- data.frame(name,accuracy,precisionPositive,precisionNegative,recallPositive,recallNegative,FmeasurePositive,FmeasureNegative)
      
      
      if(nrow(finalDF)==0){
        finalDF <- customMetrics
        
      }else{
        
        finalDF <- rbind(finalDF,customMetrics)
      }
      
    }
    
  }
  
    sortBy_precisionPositive <- finalDF[with(finalDF, order(-precisionPositive)), ]
    sortBy_recallPositive <- finalDF[with(finalDF, order(-recallPositive)), ]
    sortBy_FmeasurePositive <- finalDF[with(finalDF, order(-FmeasurePositive)), ]
    
    write.xlsx2(sortBy_precisionPositive,file=step4.modelResults.voting.overview, sheetName = "sortBy_precisionPositive",row.names = F)
    write.xlsx2(sortBy_recallPositive,file=step4.modelResults.voting.overview, sheetName = "sortBy_recallPositive",row.names = F,append = T)  
    write.xlsx2(sortBy_FmeasurePositive,file=step4.modelResults.voting.overview, sheetName = "sortBy_FmeasurePositive",row.names = F,append = T)  
    
    #fix formatting
    PredictionModeling.fixXlsxColumnSize(step4.modelResults.voting.overview)
  
  
}


PredictionModeling.fixXlsxColumnSize <- function(file){
  
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  
  # set widths to 20
  setColumnWidth(sheets[[1]], colIndex=1:8, colWidth=20)
  autoSizeColumn(sheets[[1]], colIndex=1:8)
  
  setColumnWidth(sheets[[2]], colIndex=1:8, colWidth=20)
  autoSizeColumn(sheets[[2]], colIndex=1:8)
  
  setColumnWidth(sheets[[3]], colIndex=1:8, colWidth=20)
  autoSizeColumn(sheets[[3]], colIndex=1:8)
  
  
  
  saveWorkbook(wb,file)
  
  
}



#create or load already created model
PredictionModeling.createModel <- function(trainData, validateData,model,loadExisting=T){
  
  #lets use all cores
  registerDoMC(cores = detectCores())
  
  loginfo(paste0("MODEL: ",model))
  
  saveFolder <- step4.models.folder
  
  classIndex <- grep(global.labelFeature, colnames(trainData))
  
  if(file.exists(paste0(saveFolder,"/",model,".rda")) & loadExisting){
    
    load(file=paste0(saveFolder,"/",model,".rda"))
    loginfo(paste0("loading already saved model: ",paste0(saveFolder,"/",model,".rda")))
    
  }else{
    
    train_control <- trainControl(method="cv", number=10)
    
    #create model
    fit <- train(trainData[, -classIndex], trainData[, classIndex], method = model,trControl=train_control)
    
    #save R model itself
    save(fit,file=paste0(saveFolder,"/",model,".rda"))
    
    #save best tunning parameters
    write.csv(fit$bestTune,file=paste0(step4.models.folder,"/bestTune_",model,".csv"),row.names = F)
    
  }
  
  
  #predictions
  preds <- predict(fit,newdata=validateData[, -classIndex])
  
  metrics <- confusionMatrix(preds,validateData[[global.labelFeature]],step4.positive.class.value)
  
  write.csv(metrics$table,file=paste0(step4.models.folder,"/cm_",model,".csv"))
  
  
  accuracy <- metrics$overall[1]
  recallPositive <- metrics$byClass[1] 
  recallNegative <- metrics$byClass[2] 
  precisionPositive <- metrics$byClass[3]
  precisionNegative <- metrics$byClass[4]
  
  FmeasurePositive <- 2*precisionPositive*recallPositive/(precisionPositive+recallPositive)
  FmeasureNegative <- 2*precisionNegative*recallNegative/(precisionNegative+recallNegative)
  
  #step4.positive.class.value
  customMetrics <- data.frame(model,accuracy,precisionPositive,precisionNegative,recallPositive,recallNegative,FmeasurePositive,FmeasureNegative)
  
  return(customMetrics)
}


