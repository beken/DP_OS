rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
setwd(mainProjectDir)

experimentType <- "MixTime"
devsDataSizeOption.Mix <- "Neq" #Neq, Eq
dataBalancingOption <- "Under" # Under, SMOTE
developerSelectionVersion <- "SelectedV2"

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

inputDataDir <- "/Volumes/Extreme 500/MixTime_Neq_Under/"
outputDataDir <- "/Users/beken/Projects/DP_OS/data/processing/MixTime_Neq_Under/"

for(p in 5:5){
  projectName <- projectList[p]
  loadProjectData(projectName)
  loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
  
  for(d in c(41)){
    authorName <- selectedAuthorNames[d]
    authorAlias <- selectedAuthorAliases[d]
  
    for(r in 1:numberOfRuns){
      for(f in 1:numberOfFolds){
        print(paste("Under sampling data making smaller -- ", projectName, " - ", authorName, " - ", authorAlias, " - Run: ", r, " - Fold: ", f, sep = ""))
        
        dir.create(paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, sep = ""), showWarnings = FALSE, recursive = TRUE)
        
        # read hash ids of train-test sets 
        testSet <- read.csv(paste(inputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
        trainSetPM <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM",f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
        trainSetSM <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
        trainSetGM <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
 
        write.csv(x = testSet[, c(processMetrics, "CLASS")], file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = trainSetPM[, c(processMetrics, "CLASS")], file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM", f, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = trainSetSM[, c(processMetrics, "CLASS")], file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = trainSetGM[, c(processMetrics, "CLASS")], file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), row.names = FALSE)
      }
    }
  }
}
