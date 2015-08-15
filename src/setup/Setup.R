
source('src/Configuration.R')


Setup.createResultsFolderStructure <- function(){
  
  loginfo(paste0("Set the results folder structure"))
  dir.create(result.root.folder)
  dir.create(step1.result.folder)
  dir.create(step2.result.folder)
  dir.create(step2.result.image.folder)
  
  dir.create(step2.outliers.univariate.folder)
  
  dir.create(step3.result.folder)
  
  
  dir.create(step4.result.folder)
  dir.create(step4.models.folder)
  
  dir.create(paste0(result.root.folder,"/","finalOutput"))
  
}

Setup.adjustDF <- function(df){
  
  
  if(global.ml.type=="classification"){
    
    df[[global.labelFeature]] <- as.factor(df[[global.labelFeature]])
  }
  
  
  if(global.ml.type=="regression"){
    
    df[[global.labelFeature]] <- as.numeric(df[[global.labelFeature]])
  }
  
  
  if(length(step1.features.factors)>0){
    
    for(colName  in step1.features.factors ){
      
      df[[colName]] <- as.factor(df[[colName]])
    }
    
  }
  
  return(df)
  
}
