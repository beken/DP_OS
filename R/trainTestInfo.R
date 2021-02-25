# prepare an information file for train-test pairs to analyze if there is a mistake in train-test files or experimentations

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "SeqTime"
baseExperimentSignature <- "SeqTime" 
samplingOption <- ""
testSetMonths <- "6"

setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

outExperimentSignature <- paste(baseExperimentSignature, "_IntervalInfo", sep = "")

loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)



for(p in 1:length(projectList)){
  projectName <- projectList[p]
  # loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
  
  inputDir <- paste(mainProjectDir, "/data/final/SeqTime_Intervals/6Months/", projectName, "/", sep = "")
  
  infoTable <- as.data.frame(matrix(ncol = 4, nrow = 0))
  colnames(infoTable) <- c("nrow", "uniqueCommit", "buggyCommit", "uniqueAuthor")
  
  fileNames <- list.files(path = inputDir, pattern = "*PM*.csv", recursive = T)
  # fileNames <- list.files(path = inputDir, pattern = "*.xlsx", recursive = T)
  
  for(f in 1:length(fileNames)){
    data <- read.csv(file = paste(inputDir, fileNames[f], sep = ""))
    
    nrowCount <- nrow(data)
    uniqueCommitCount <- length(unique(data$COMMIT_ID))
    buggyCommitCount <- length(which(data$CLASS == "BUG"))
    uniqueAuthorCount <- length(unique(data$AUTHOR))
    
    infoTable[nrow(infoTable) + 1, "nrow"] <- nrowCount
    infoTable[nrow(infoTable), "uniqueCommit"] <- uniqueCommitCount
    infoTable[nrow(infoTable), "buggyCommit"] <- buggyCommitCount
    infoTable[nrow(infoTable), "uniqueAuthor"] <- uniqueAuthorCount
    
    rownames(infoTable)[nrow(infoTable)] <- paste(basename(fileNames[f]))
  }
  
  infoTable$differenceNrowUniqueCommit <- infoTable$nrow - infoTable$uniqueCommit
  
  if(p == 1)
    write.xlsx(infoTable, paste(plotOutputDir, baseExperimentSignature, "_PMinfoTableProjectBased.xlsx", sep = ""), sheetName = projectName, row.names = T, col.names = T)
  else
    write.xlsx(infoTable, paste(plotOutputDir, baseExperimentSignature, "_PMinfoTableProjectBased.xlsx", sep = ""), sheetName = projectName, row.names = T, col.names = T, append = T)
}



# for(p in 1:length(projectList)){
#   projectName <- projectList[p]
#   loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
#   
#   for(a in 1:length(authorNames)){
#     if(experimentType == "MixTime")
#       authorName <- authorNames[a]
#     else
#       authorName <- authorAliases[a]
#     
#     fileNames <- list.files(path = paste(trainTestDataDir, testSetMonths, "Months/", projectName, "/", authorName, sep = ""), pattern = "*.csv")
#     
#   
#   }
# }
# 




