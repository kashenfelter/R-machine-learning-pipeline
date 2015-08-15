
######################THIS IS SOMETHING YOU SHOULD ADJUST FOR NEW DATA SET===============================
global.ml.type <- "regression"
global.labelFeature <- "Hazard"

input.train.file <- "data/train.csv"   #we are expecting csv file 
input.test.file <- "data/test.csv"

step1.input.file.separator <- "," #default separator is ","




result.root.folder = "results"

#####################



##Tidying data (STEP1)
step1.result.folder <- paste0(result.root.folder,"/","tidyingData")
step1.result.tidy.file <- paste0(step1.result.folder,"/","tidyData.csv")
step1.result.code.book.raw = paste0(step1.result.folder,"/","rawCodeBook.csv")


## Exploratory Analysis (STEP2)
step2.result.folder <- paste0(result.root.folder,"/","exploratoryAnalysis")
step2.result.image.folder <- paste0(step2.result.folder,"/","images")

#expert suggestions for removing
step2.remove.features <- c()

step2.percent.rule.value <- 5
step2.remove.incomplete.rows <- TRUE
step2.outliers.lof.K <- 5  #check http://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf

step2.outliers.univariate.folder <- paste0(step2.result.folder,"/","univariate_numerical_outliers")

step2.codebook.file <- paste0(step2.result.folder,"/","codeBook_step2.csv")
step2.output.file <- paste0(step2.result.folder,"/","outputFile_step2.csv")

step2.correlation.output.file <- paste0(step2.result.folder,"/","correlation_step2.csv")

#STEP3 - Dimensionality reduction
step3.result.folder <- paste0(result.root.folder,"/","dimensionalityReduction")

step3.featurSelection.overview <- paste0(step3.result.folder,"/","featureSelection_overview.xlsx")


step3.output.file <- paste0(step3.result.folder,"/","outputFile_step3.csv")


#STEP4 -

step4.result.folder <- paste0(result.root.folder,"/","predictionModeling")
step4.split.coefficient <- 0.70   #train vs validate stratified sampling
step4.train.file <-  paste0(step4.result.folder,"/","train.csv")
step4.validate.file <-  paste0(step4.result.folder,"/","validate.csv")
step4.random1000.file <-  paste0(step4.result.folder,"/","random_1000.csv")

step4.models.folder <-  paste0(step4.result.folder,"/","models")

step4.classification.models.list <- c("nb","C5.0","avNNet","nnet","rf","gbm","glm")


step4.modelResults.overview <-  paste0(step4.result.folder,"/","models_overview.xlsx")
step4.modelResults.voting.overview <- paste0(step4.result.folder,"/","voting_overview.xlsx")

step4.features.selected <- c("MARITAL_STATUS","AGE","FLAG_RESIDENCIAL_PHONE","MONTHS_IN_THE_JOB")

output.file.location <- "output.xlsx"

