library('logging') 
library('caret')
library('sqldf')
library('GGally')  # for nice plots

library('DMwR')

source('src/Configuration.R')

#takes "tidy data " from step1, removes those with missing values if configured that way (step2.remove.incomplete.rows) ,performs "percent_rule"
#defined with config value (step2.percent.rule.value) and finally creates plots with all predictors vs label feature (global.labelFeature)

ExploratoryAnalysis.run <- function(){
  
  
  basicConfig()
  
  if(!file.exists(step1.result.tidy.file)){
    logerror(paste0("missing file ",result.tidy.file))
    stop("missing tidy file!")
  }
  
  tidyData <- read.csv(file=step1.result.tidy.file,stringsAsFactors = T)
  loginfo(paste0("loaded ",step1.result.tidy.file))
  
  
  #domain expert suggested feature removal
  if(length(step2.remove.features) >0){
    
    loginfo(paste0("removing feature: ",step2.remove.features))
    
    tidyData <- tidyData[,!(names(tidyData) %in% step2.remove.features)]
    
  }
  
  #complete cases
  if(step2.remove.incomplete.rows){
    total <- nrow(tidyData)
    #keep just complete cases
    tidyData <- tidyData[complete.cases(tidyData),]
    
    afterCompleteCases <- nrow(tidyData)
    
    if(afterCompleteCases<total){
      loginfo(paste0("removed  ",total-afterCompleteCases ," rows without complete cases. That's ",round((total-afterCompleteCases)*100/total,2)," percent"))
    }
  }
  
  #percent rule for categorical
  tidyData <- ExploratoryAnalysis.percentRuleForCategorical(tidyData)
  
  #deal with univariate numeric outliers - for now just plot
  tidyData <- ExploratoryAnalysis.univariate_outliers(tidyData)
  
  #LOF outliers  -TODO seems we need to find better package for this
  #tidyData <- ExploratoryAnalysis.LOF_outliers(tidyData)
  
  #create codeBook for step2 
  ExploratoryAnalysis.createCodeBook(step2.codebook.file,tidyData)
  
  write.csv(tidyData,file=step2.output.file,row.names = F)
  
  #plot relations 
  for(colname in colnames(tidyData)){
    
    if(colname != global.labelFeature){  # no need to compare it to itself
      
      loginfo(paste0("plotting  ",colname))
      image_name=paste0(step2.result.image.folder,"/",colname,"_Vs_",global.labelFeature,".svg")
      svg(image_name, height = 14, width = 14)
      g <- ggpairs(tidyData[,c(colname,global.labelFeature)], diag=list(continuous="density", discrete="bar"), axisLabels="show")
      
      print(g)
      dev.off()
      
    }
    
  }
  
  
}


#percent rule
ExploratoryAnalysis.percentRuleForCategorical <- function(df,percent=step2.percent.rule.value){
  
  columns <- colnames(df)
  
  total <- nrow(df)
  
  percentNumber <- round(percent*total/100,0)
  
  
  #actualy its just one
  for(column in columns){
    
    if(class(df[[column]]) == "factor"){
      
      
      #get values that are below 5%
      problematicValues <- sqldf(paste0("select ",column," ,count(*) from df group by ",column," having count(*)<=",percentNumber))
      
      if(nrow(problematicValues) > 0){
        
        loginfo(paste0(percent,"% percent  rule for column ",column))
        
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



ExploratoryAnalysis.createCodeBook <- function(outputfileLocation,df){
  
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

#deal with numeric outliers 
#check http://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf
#http://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm
ExploratoryAnalysis.univariate_outliers <- function(df){
  
  loginfo(paste0("start univariate outlier detection"))
  
  for(column in colnames(df)){
    
    if(class(df[[column]])!="factor"){
      
      a <- which(df[[column]] %in% boxplot.stats(df[[column]])$out)
      
      if(length(a)==0)next
      
      image_name=paste0(step2.outliers.univariate.folder,"/","outliers_",column,".png")
      png(filename=image_name,width = 1000, height = 400)
      plot(df[a,column],ylab = paste0(column),xlab = "number of outliers")
      title(paste0("outliers for  ",column))
      dev.off()
      
    }
    
  }
  
  return(df)
  
}


#https://en.wikipedia.org/wiki/Local_outlier_factor
ExploratoryAnalysis.LOF_outliers <- function(df){
  
  loginfo(paste0("start LOF outlier detection, this will take some time..."))
  
  #keep just numeric
  numericDF <- df[,sapply(df,class) != "factor"]
  
  outlier.scores <- lofactor(numericDF, k=step2.outliers.lof.K)
   
  plot(density(outlier.scores))
  outliers <- order(outlier.scores, decreasing=T)[1:5]
  
  return(df)
  
}