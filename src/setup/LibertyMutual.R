
source('src/Configuration.R')
source('src/metrics/regression/RegressionMetrics.R')

library('caret')
library('parallel')
library('doMC')
library('sqldf')
library('logging')

LibertyMutual.pipeline <- function(){
  
  basicConfig()
  #load files
  train <- read.csv(file=input.train.file)
  test <- read.csv(file=input.test.file)
  
  total <- nrow(train)
  
  perc_4000 <- 4000/total
  set.seed(1)
  inTraining <- createDataPartition(train$Hazard, p =perc_4000, list = FALSE)
  
  
  
  #save for later use
  train_Id <- train$Id
  test_Id <- test$Id
  train_label <- train[[global.labelFeature]]
  
  train$Id <- NULL
  test$Id <- NULL
  train[[global.labelFeature]] <- NULL
  
  dataset <- rbind(train,test)
  
  #for(perc in 0:15){
  
  perc=6
    
    if(perc>0){
      
      #percent rule
      dataset <- LibertyMutual.percentRuleForCategorical(dataset,perc)
      
    }
    
    nums <- sapply(dataset, is.numeric)
    numericColumns <- dataset[ , nums]
    
    fac <- sapply(dataset, is.factor)
    factorColumns <- dataset[,fac]
    
    #one hot encode
    p <- as.data.frame(model.matrix(~., factorColumns))
    
    #remove intercept
    p <- p[,-1]
    
    
    #bind back together
    dataset_numeric <- cbind(numericColumns,p)
    
    
    #cool now SPLIT back them
    
    trainFixed <-  cbind(dataset_numeric[1:nrow(train),],Hazard=train_label)
    
    testFixed <-  dataset_numeric[(nrow(train)+1):nrow(dataset_numeric),]
    
    
    #create models 
    
    registerDoMC(cores = detectCores())
    
    classIndex <- grep(global.labelFeature, colnames(trainFixed))
    
    train_control <- trainControl(method="cv", number=10)#change this
    
    
    
    #get validation set
    
    
    random5000 <- trainFixed[ inTraining,]
    
    train_modeling  <- trainFixed[-inTraining,]
    
    
    model <- "glm"
    
    fit <- train(train_modeling[, -classIndex], train_modeling[, classIndex], method = model,trControl=train_control)
    save(fit,file=paste0("results/model_",model,"_perc_",perc,".rda"))
    
    
    #calculate NG on validation set
    predictions <- predict(fit,random5000)
    ng <- RegressionMetrics.normalizedGini(random5000$Hazard,predictions)
    
    
    message(paste0(model," ",perc," normalized gini: ",ng))
    
    #real stuff
    fit <- train(trainFixed[, -classIndex], trainFixed[, classIndex], method = model,trControl=train_control)
    
    predictions <- predict(fit,trainFixed)
    ng <- RegressionMetrics.normalizedGini(trainFixed$Hazard,predictions)
    
    message(paste0(model," ",perc," normalized gini: ",ng))
    
    
    output <- cbind(Id=test_Id,Hazard=predictions)
    
    write.csv(output,file=paste0("res",model,"_perc_",perc,".csv"),row.names = F)
    
  #}
  
  
}

LibertyMutual.percentRuleForCategorical <- function(df,percent=step2.percent.rule.value){
  
  columns <- colnames(df)
  
  total <- nrow(df)
  
  percentNumber <- round(percent*total/100,0)
  
  
  #actualy its just one
  for(column in columns){
    
    if(class(df[[column]]) == "factor"){
      
      
      #get values that are below 5%
      problematicValues <- sqldf(paste0("select ",column," ,count(*) from df group by ",column," having count(*)<=",percentNumber))
      
      if(nrow(problematicValues) > 0){
        
        #loginfo(paste0(percent,"% percent  rule for column ",column))
        
        goodLevels = levels(df[[column]])[levels(df[[column]]) %in% sqldf(paste0("select ",column," ,count(*) from df group by ",column," having count(*)>",percentNumber))[,1]]
        
        #just temporary add this level so we can replace
        levels(df[[column]]) <- c(levels(df[[column]]), paste0(column,"_percentRule")) 
        
        #now replace
        for(index in 1:length(problematicValues[,1])){
          df[as.character(df[[column]])==as.character(problematicValues[index,1]),][,c(column)]  <- paste0(column,"_percentRule")
        }
        
        df[[column]] <- as.character(df[[column]])
        df[[column]] <- as.factor(df[[column]])
        
      }
      
    }
    
  }
  
  return(df)
  
}

