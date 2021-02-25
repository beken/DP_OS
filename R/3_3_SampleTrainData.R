# apply data sampling techniques (SMOTE/under/over) and save the balanced data to train later

library(caret)
library(e1071)
library(ROSE)
library(DMwR)

rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
setwd(mainProjectDir)

experimentType <- "MixTime"
devsDataSizeOption.Mix <- "Neq" #Neq, Eq
dataBalancingOption <- "SMOTE" # Under, SMOTE
developerSelectionVersion <- "SelectedV2"
# preparedTrainTestOrInterval <- "P" #P,I
analyzeTheDataProportionAfterSampling <- FALSE

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

baseExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix, sep = "")
outExperimentSignature <- paste(baseExperimentSignature, dataBalancingOption, sep = "_")

loadDataProcessingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature)

if(experimentType == "MixTime"){
  for(p in c(3)){ 
    # Rails p:5, d:33 de kaldi
    # Gimp p:1, d:28 de kaldi
    # Perl komple kaldi
    
    # Rails	45
    # Gimp	51
    # Perl	87
    
    # Maven2	5
    # PostgreSql	27
    # Rhino	7
    
  # for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectData(projectName)
    loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
    
    projectAllCommits <- get(paste(projectName, "Changes", sep = ""))
    
    for(d in c(13,30,20,14)){
    # for(d in 1:numberOfSelectedAuthors){
      authorName <- selectedAuthorNames[d]
      authorAlias <- selectedAuthorAliases[d]
      
      # infoTable <- data.frame(matrix(nrow = 0, ncol = 9))
      # colnames(infoTable) <- c("Project", "Author", "Alias", "Run", "Fold", "Model", "Buggy", "NonBuggy", "Total")
      
      for(r in 1:numberOfRuns){
        for(f in 1:numberOfFolds){
          print(paste(dataBalancingOption, " sampling -- ", projectName, " - ", authorName, " - ", authorAlias, " - Run: ", r, " - Fold: ", f, sep = ""))
         
          # read hash ids of train-test sets 
          testSet.HashIds <- read.csv(paste(inputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          trainSetPM.HashIds <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM",f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          trainSetSM.HashIds <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          trainSetGM.HashIds <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          # trainSetWPM <- read.csv(paste(inputDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorName, "_TrainWPM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          
          switch(dataBalancingOption,
                 Over = {
                   trainDataPM <- upSample(x = trainDataPM[, -ncol(trainDataPM)], y = trainDataPM$CLASS)     
                   colnames(trainDataPM)[ncol(trainDataPM)] = "CLASS"
                   trainDataPM <- trainDataPM[sample(nrow(trainDataPM)), ]
                   
                   trainDataSM <- upSample(x = trainDataSM[, -ncol(trainDataSM)], y = trainDataSM$CLASS)      
                   colnames(trainDataSM)[ncol(trainDataSM)] = "CLASS"
                   trainDataSM <- trainDataSM[sample(nrow(trainDataSM)), ]
                   
                   trainDataGM <- upSample(x = trainDataGM[, -ncol(trainDataGM)], y = trainDataGM$CLASS)                         
                   colnames(trainDataGM)[ncol(trainDataGM)] = "CLASS"
                   trainDataGM <- trainDataGM[sample(nrow(trainDataGM)), ]
                   
                   trainDataWPM <- upSample(x = trainDataWPM[, -ncol(trainDataWPM)], y = trainDataWPM$CLASS)                         
                   colnames(trainDataWPM)[ncol(trainDataWPM)] = "CLASS"
                   trainDataWPM <- trainDataWPM[sample(nrow(trainDataWPM)), ]
                 },
                 Under = {
                   # gather other columns of data (features) matched with the hash ids
                   testSet <- projectAllCommits[projectAllCommits$HASHID %in% testSet.HashIds[, 1], ]
                   trainSetPM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetPM.HashIds[, 1], ]
                   trainSetSM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetSM.HashIds[, 1], ]
                   trainSetGM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetGM.HashIds[, 1], ]
                   
                   buggyCount <- length(which(trainSetPM$CLASS == "BUG"))
                   noBuggyCount <- length(which(trainSetPM$CLASS == "NO_BUG"))
                   if(round(buggyCount / noBuggyCount, 1) != 1){
                     trainSetPM <- ovun.sample(CLASS~ ., trainSetPM, method = "under")$data
                     trainSetPM <- trainSetPM[sample(nrow(trainSetPM)), ]
                   }
                   else{
                     print("sampling is not applied for PM")
                   }
                   
                   buggyCount <- length(which(trainSetSM$CLASS == "BUG"))
                   noBuggyCount <- length(which(trainSetSM$CLASS == "NO_BUG"))
                   if(round(buggyCount / noBuggyCount, 1) != 1){
                     trainSetSM <- ovun.sample(CLASS~ ., trainSetSM, method = "under")$data
                     trainSetSM <- trainSetSM[sample(nrow(trainSetSM)), ]
                   }
                   else{
                     print("sampling is not applied for SM")
                   }
                   
                   buggyCount <- length(which(trainSetGM$CLASS == "BUG"))
                   noBuggyCount <- length(which(trainSetGM$CLASS == "NO_BUG"))
                   if(round(buggyCount / noBuggyCount, 1) != 1){
                     trainSetGM <- ovun.sample(CLASS~ ., trainSetGM, method = "under")$data
                     trainSetGM <- trainSetGM[sample(nrow(trainSetGM)), ]
                   }
                   else{
                     print("sampling is not applied for GM")
                   }
                   
                   # trainDataWPM <- ovun.sample(CLASS~ ., trainDataWPM, method = "under")$data
                   # trainDataWPM <- trainDataWPM[sample(nrow(trainDataWPM)), ]
                 },
                 SMOTE = {
                   # gather other columns of data (features) matched with the hash ids
                   testSet <- projectAllCommits[projectAllCommits$HASHID %in% testSet.HashIds[, 1], c(processMetrics, "CLASS")]
                   trainSetPM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetPM.HashIds[, 1], c(processMetrics, "CLASS")]
                   trainSetSM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetSM.HashIds[, 1], c(processMetrics, "CLASS")]
                   trainSetGM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetGM.HashIds[, 1], c(processMetrics, "CLASS")]
                   
                   # convert columns to factor since the SMOTE function works with factors
                   testSet <- data.frame(lapply(testSet, as.factor))
                   trainSetPM <- data.frame(lapply(trainSetPM, as.factor))
                   trainSetSM <- data.frame(lapply(trainSetSM, as.factor))
                   trainSetGM <- data.frame(lapply(trainSetGM, as.factor))
                   
                   
                   buggyCount <- length(which(trainSetPM$CLASS == "BUG"))
                   notBuggyCount <- length(which(trainSetPM$CLASS == "NO_BUG"))
                   halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                   if(halfDifferenceBetweenNonBuggyAndBuggy > 0){
                     overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                     if(overPerc > 0){
                       underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                       trainSetPM <- SMOTE(CLASS~ ., trainSetPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                       trainSetPM <- trainSetPM[sample(nrow(trainSetPM)), ]
                     }
                   }
                   else{
                     halfDifferenceBetweenNonBuggyAndBuggy <- -halfDifferenceBetweenNonBuggyAndBuggy
                     overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / notBuggyCount * 100)
                     if(overPerc > 0){
                       underPerc <- round((buggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                       trainSetPM <- SMOTE(CLASS~ ., trainSetPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                       trainSetPM <- trainSetPM[sample(nrow(trainSetPM)), ] 
                     }
                   }
                   # table(trainSetPM$CLASS)
                   
                   buggyCount <- length(which(trainSetSM$CLASS == "BUG"))
                   notBuggyCount <- length(which(trainSetSM$CLASS == "NO_BUG"))
                   halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                   underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                   trainSetSM <- SMOTE(CLASS~ ., trainSetSM, k = 1, perc.over = overPerc, perc.under = underPerc)
                   trainSetSM <- trainSetSM[sample(nrow(trainSetSM)), ]
                   # table(trainSetSM$CLASS)
                   
                   buggyCount <- length(which(trainSetGM$CLASS == "BUG"))
                   notBuggyCount <- length(which(trainSetGM$CLASS == "NO_BUG"))
                   halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                   underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                   trainSetGM <- SMOTE(CLASS~ ., trainSetGM, k = 1, perc.over = overPerc, perc.under = underPerc)
                   trainSetGM <- trainSetGM[sample(nrow(trainSetGM)), ]
                   # table(trainSetGM$CLASS)
                   
                   # buggyCount <- length(which(trainSetWPM$CLASS == "BUG"))
                   # notBuggyCount <- length(which(trainSetWPM$CLASS == "NO_BUG"))
                   # halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                   # if(halfDifferenceBetweenNonBuggyAndBuggy > 0){
                   #   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                   #   underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                   #   trainSetWPM <- SMOTE(CLASS~ ., trainDataWPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                   #   trainSetWPM <- trainSetWPM[sample(nrow(trainSetWPM)), ]
                   # }
                   # else{
                   #   halfDifferenceBetweenNonBuggyAndBuggy <- -halfDifferenceBetweenNonBuggyAndBuggy
                   #   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / notBuggyCount * 100)
                   #   underPerc <- round((buggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                   #   trainSetWPM <- SMOTE(CLASS~ ., trainSetWPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                   #   trainSetWPM <- trainSetWPM[sample(nrow(trainSetWPM)), ]
                   # }
                 }
          )
          
          dir.create(paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, sep = ""), showWarnings = FALSE, recursive = TRUE)

          # infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "PM", length(which(testSet$CLASS == "BUG")), length(which(testSet$CLASS == "NO_BUG")), nrow(testSet))
          # infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "PM", length(which(trainSetPM$CLASS == "BUG")), length(which(trainSetPM$CLASS == "NO_BUG")), nrow(trainSetPM))
          # infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "SM", length(which(trainSetSM$CLASS == "BUG")), length(which(trainSetSM$CLASS == "NO_BUG")), nrow(trainSetSM))
          # infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "GM", length(which(trainSetGM$CLASS == "BUG")), length(which(trainSetGM$CLASS == "NO_BUG")), nrow(trainSetGM))
          
          write.csv(x = testSet, file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), row.names = FALSE)
          write.csv(x = trainSetPM, file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM", f, ".csv", sep = ""), row.names = FALSE)
          write.csv(x = trainSetSM, file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), row.names = FALSE)
          write.csv(x = trainSetGM, file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), row.names = FALSE)
          # write.csv(x = trainDataWPM, file = paste(outputDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainWPM", f, ".csv", sep = ""), row.names = FALSE)
        }
      }
      # write.csv(x = infoTable, file = paste(outputDataDir, projectName, "/", authorAlias, ".csv", sep = ""), row.names = FALSE)
    }
  }
}

if(experimentType == "SeqTime"){
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
    
    for(d in 1:numberOfSelectedDev){
      authorName <- authorAliases[d]
      
      testIntervals <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))[d, paste("TestIntervals_", testSetMonths, "Months", sep = "")])
      testIntervals <- unlist(strsplit(testIntervals, "-", fixed = TRUE))
      
      for(interval in testIntervals){
        print(paste(underOverSamplingOption, " sampling -- ", projectName, " - ", authorName, " - test intervals: ", interval, sep = ""))
        
        testSet <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "PM", preparedTrainTestOrInterval, "csv")[[2]]
        trainSetPM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "PM", preparedTrainTestOrInterval, "csv")[[1]]
        trainSetSM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "SM", preparedTrainTestOrInterval, "csv")[[1]]
        trainSetGM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "GM", preparedTrainTestOrInterval, "csv")[[1]]
        
        testSetData <- testSet
        trainDataPM <- trainSetPM
        trainDataSM <- trainSetSM
        trainDataGM <- trainSetGM
        # trainDataWPM <- trainSetWPM
        
        # testSetData <- testSet[, -columnsToBeRemoved]
        # trainDataPM <- trainSetPM[, -columnsToBeRemoved]
        # trainDataSM <- trainSetSM[, -columnsToBeRemoved]
        # trainDataGM <- trainSetGM[, -columnsToBeRemoved]
        # trainDataWPM <- trainSetWPM[, -columnsToBeRemoved]
          
        switch(underOverSamplingOption,
               Over = {
                 trainDataPM <- upSample(x = trainDataPM[, -ncol(trainDataPM)], y = trainDataPM$CLASS)     
                 colnames(trainDataPM)[ncol(trainDataPM)] = "CLASS"
                 trainDataPM <- trainDataPM[sample(nrow(trainDataPM)), ]
                 
                 trainDataSM <- upSample(x = trainDataSM[, -ncol(trainDataSM)], y = trainDataSM$CLASS)      
                 colnames(trainDataSM)[ncol(trainDataSM)] = "CLASS"
                 trainDataSM <- trainDataSM[sample(nrow(trainDataSM)), ]
                 
                 trainDataGM <- upSample(x = trainDataGM[, -ncol(trainDataGM)], y = trainDataGM$CLASS)                         
                 colnames(trainDataGM)[ncol(trainDataGM)] = "CLASS"
                 trainDataGM <- trainDataGM[sample(nrow(trainDataGM)), ]
                 
                 # trainDataWPM <- upSample(x = trainDataWPM[, -ncol(trainDataWPM)], y = trainDataWPM$CLASS)                         
                 # colnames(trainDataWPM)[ncol(trainDataWPM)] = "CLASS"
                 # trainDataWPM <- trainDataWPM[sample(nrow(trainDataWPM)), ]
               },
               Under = {
                 trainDataPM <- ovun.sample(CLASS~ ., trainDataPM, method = "under")$data
                 trainDataPM <- trainDataPM[sample(nrow(trainDataPM)), ]
                 
                 trainDataSM <- ovun.sample(CLASS~ ., trainDataSM, method = "under")$data
                 trainDataSM <- trainDataSM[sample(nrow(trainDataSM)), ]
                 
                 trainDataGM <- ovun.sample(CLASS~ ., trainDataGM, method = "under")$data
                 trainDataGM <- trainDataGM[sample(nrow(trainDataGM)), ]

                 # trainDataWPM <- ovun.sample(CLASS~ ., trainDataWPM, method = "under")$data
                 # trainDataWPM <- trainDataWPM[sample(nrow(trainDataWPM)), ]
               },
               Both = { 
                 # trainPM <- ovun.sample(CLASS~ ., trainPM, method = "both")$data
                 
                 buggyCount <- length(which(trainDataPM$CLASS == "BUG"))
                 notBuggyCount <- length(which(trainDataPM$CLASS == "NO_BUG"))
                 halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                 if(halfDifferenceBetweenNonBuggyAndBuggy > 0){
                   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                   if(overPerc > 0){
                     underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                     trainDataPM <- SMOTE(CLASS~ ., trainDataPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                     trainDataPM <- trainDataPM[sample(nrow(trainDataPM)), ]
                   }
                 }
                 else{
                   halfDifferenceBetweenNonBuggyAndBuggy <- -halfDifferenceBetweenNonBuggyAndBuggy
                   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / notBuggyCount * 100)
                   if(overPerc > 0){
                     underPerc <- round((buggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                     trainDataPM <- SMOTE(CLASS~ ., trainDataPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                     trainDataPM <- trainDataPM[sample(nrow(trainDataPM)), ] 
                   }
                 }
                 
                 buggyCount <- length(which(trainDataSM$CLASS == "BUG"))
                 notBuggyCount <- length(which(trainDataSM$CLASS == "NO_BUG"))
                 halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                 overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                 underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                 trainDataSM <- SMOTE(CLASS~ ., trainDataSM, k = 1, perc.over = overPerc, perc.under = underPerc)
                 trainDataSM <- trainDataSM[sample(nrow(trainDataSM)), ]
                 
                 buggyCount <- length(which(trainDataGM$CLASS == "BUG"))
                 notBuggyCount <- length(which(trainDataGM$CLASS == "NO_BUG"))
                 halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                 overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                 underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                 trainDataGM <- SMOTE(CLASS~ ., trainDataGM, k = 1, perc.over = overPerc, perc.under = underPerc)
                 trainDataGM <- trainDataGM[sample(nrow(trainDataGM)), ]
                 
                 # buggyCount <- length(which(trainDataWPM$CLASS == "BUG"))
                 # notBuggyCount <- length(which(trainDataWPM$CLASS == "NO_BUG"))
                 # halfDifferenceBetweenNonBuggyAndBuggy <- (notBuggyCount - buggyCount) / 2
                 # if(halfDifferenceBetweenNonBuggyAndBuggy > 0){
                 #   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / buggyCount * 100)
                 #   underPerc <- round((notBuggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                 #   trainDataWPM <- SMOTE(CLASS~ ., trainDataWPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                 #   trainDataWPM <- trainDataWPM[sample(nrow(trainDataWPM)), ]
                 # }
                 # else{
                 #   halfDifferenceBetweenNonBuggyAndBuggy <- -halfDifferenceBetweenNonBuggyAndBuggy
                 #   overPerc <- round(halfDifferenceBetweenNonBuggyAndBuggy / notBuggyCount * 100)
                 #   underPerc <- round((buggyCount - halfDifferenceBetweenNonBuggyAndBuggy) / halfDifferenceBetweenNonBuggyAndBuggy * 100)
                 #   trainDataWPM <- SMOTE(CLASS~ ., trainDataWPM, k = 1, perc.over = overPerc, perc.under = underPerc)
                 #   trainDataWPM <- trainDataWPM[sample(nrow(trainDataWPM)), ]
                 # }
               }
        )
          
        dir.create(paste(outputDataDir, projectName, "/", authorName, sep = ""), showWarnings = FALSE, recursive = TRUE)
          
        write.csv(x = testSetData, file = paste(outputDataDir, projectName, "/", authorName, "/", authorName, "TestPM", interval, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = testSetData, file = paste(outputDataDir, projectName, "/", authorName, "/", authorName, "TestSM", interval, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = testSetData, file = paste(outputDataDir, projectName, "/", authorName, "/", authorName, "TestGM", interval, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = trainDataPM, file = paste(outputDataDir, projectName, "/", authorName, "/", authorName, "TrainPM", interval, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = trainDataSM, file = paste(outputDataDir, projectName, "/", authorName, "/", authorName, "TrainSM", interval, ".csv", sep = ""), row.names = FALSE)
        write.csv(x = trainDataGM, file = paste(outputDataDir, projectName, "/", authorName, "/", authorName, "TrainGM", interval, ".csv", sep = ""), row.names = FALSE)
      }
    }
  }
}

if(analyzeTheDataProportionAfterSampling){
  projectList <- c("Gimp")
  
  finalTable <- as.data.frame(matrix(nrow = 0, ncol = 6))
  colnames(finalTable) <- c("Project", "Author", "Iteration", "Fold", "SelfProportion", "OthersProportion")
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
    
    for(d in 1:numberOfSelectedDev){
      authorName <- authorNames[d]
      
      for(r in 1:numberOfRuns){
        for(f in 1:numberOfFolds){
          sampledDataDir <- outputDataDir
          
          testSet <- read.csv(paste(sampledDataDir, projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          # trainSetPM <- read.csv(paste(sampledDataDir , projectName, "/", authorName, "/Run", r, "/", authorName, "TrainPM",f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          # trainSetSM <- read.csv(paste(sampledDataDir , projectName, "/", authorName, "/Run", r, "/", authorName, "TrainSM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          # trainSetGM <- read.csv(paste(sampledDataDir , projectName, "/", authorName, "/Run", r, "/", authorName, "TrainGM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          trainSetWPM <- read.csv(paste(sampledDataDir , projectName, "/", authorName, "/Run", r, "/", authorName, "TrainWPM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
          
          trainTestCombined <- rbind(trainSetWPM, testSet)
          numberOfSelfCommits <- length(which(trainTestCombined$AUTHOR_NAME == paste("'", authorName, "'", sep = "")))
          numberOfOthersCommits <- length(which(trainTestCombined$AUTHOR_NAME != paste("'", authorName, "'", sep = "")))
          
          proportionOfSelfCommits <- numberOfSelfCommits / nrow(trainTestCombined)
          proportionOfOthersCommits <- numberOfOthersCommits / nrow(trainTestCombined)
          
          finalTable[nrow(finalTable) + 1, ] <- c(projectName, authorName, r, f, round(proportionOfSelfCommits, 2), round(proportionOfOthersCommits, 2))
        }
      }
    }
  }
  
  write.xlsx(x = finalTable, file = paste(outputDataDir, "ProportionInfo.xlsx", sep = ""), sheetName = "proportions", row.names = FALSE)
}
