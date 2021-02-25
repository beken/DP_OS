library(xlsx)

### functions for loading the data for appropriate experimentation  ##############
loadProjectData <- function(projectName){
  changesDfName <- paste(projectName, "Changes", sep = "")
  
  #read all changes
  assign(changesDfName, read.csv(file = paste(changeDataDir, projectName, ".csv", sep = ""), header = TRUE), envir = .GlobalEnv)
}

loadProjectAuthorsData <- function(experimentType, projectName, developerSelectionVersion){
  selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
  
  if(experimentType == "MixTime"){
    selectedAuthorsTable <- xlsx::read.xlsx(file = paste(finalDataDir, selectedAuthorsFileName.Mix, sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, selectedAuthorsTable[which(selectedAuthorsTable[, developerSelectionVersion] == "T"), ], envir = .GlobalEnv)
  }
  else{
    selectedAuthorsTable <- xlsx::read.xlsx(file = paste(finalDataDir, selectedAuthorsSeqTimeName, sep = ""), sheetName = paste(projectName), header = TRUE)
    
    if(testSetLengthInMonth.Seq == 3)
      assign(selectedAuthorsDfName, selectedAuthorsTable[which(selectedAuthorsTable$Selected_3Months == "T"), ], envir = .GlobalEnv)
    else if(testSetLengthInMonth.Seq == 6)
      assign(selectedAuthorsDfName, selectedAuthorsTable[which(selectedAuthorsTable$Selected_6Months == "T"), ], envir = .GlobalEnv)
  }
  
  assign("selectedAuthorNames", as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author), envir = .GlobalEnv)
  assign("selectedAuthorAliases", as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Alias), envir = .GlobalEnv)
  assign("numberOfSelectedAuthors", length(selectedAuthorNames), envir = .GlobalEnv) 
  assign("minChangeCountAmongSelectedAuthors", min(get(paste(projectName, "SelectedAuthors", sep = ""))$CommitCount), envir = .GlobalEnv) 
}

loadDataProcessingEnvironment <- function(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature){
  assign("inputDataDir", paste(finalDataDir, baseExperimentSignature, "_TrainTest/", sep = ""), envir = .GlobalEnv)
  assign("outputDataDir", paste(dataDir, "processing/", outExperimentSignature,"/", sep = ""), envir = .GlobalEnv)

  dir.create(outputDataDir, recursive = T, showWarnings = F)
}

loadModelBuildingEnvironment <- function(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature){
  
  
  
  # ATTENTION: change the lines later (after train SMOTE)
  # assign("trainTestDataDir", paste(finalDataDir, baseExperimentSignature, "_TrainTest/", sep = ""), envir = .GlobalEnv)
  assign("trainTestDataDir", paste(processingDataDir, baseExperimentSignature, "/", sep = ""), envir = .GlobalEnv)
  
  
  
  assign("modelOutputDir", paste(mainProjectDir, "output/", outExperimentSignature, "/", sep = ""), envir = .GlobalEnv)
  dir.create(modelOutputDir, recursive = T, showWarnings = F)
}

loadPlotEnvironment <- function(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature){
  assign("trainTestDataDir", paste(finalDataDir, baseExperimentSignature, "_TrainTest/", sep = ""), envir = .GlobalEnv)
  assign("plotOutputDir", paste(mainProjectDir, "plots/", outExperimentSignature, "/", sep = ""), envir = .GlobalEnv)
  
  assign("NB_Dir", paste(mainProjectDir, "output/", baseExperimentSignature, "_NB/", sep = ""), envir = .GlobalEnv)
  assign("RF_Dir", paste(mainProjectDir, "output/", baseExperimentSignature, "_RF/", sep = ""), envir = .GlobalEnv)
  assign("HyperSMURF_Dir", paste(mainProjectDir, "output/", baseExperimentSignature, "_HyperSMURF/", sep = ""), envir = .GlobalEnv)
  assign("XGBoost_Dir", paste(mainProjectDir, "output/", baseExperimentSignature, "_XGBoost/", sep = ""), envir = .GlobalEnv)
  
  dir.create(plotOutputDir, recursive = T, showWarnings = F)

  assign("pVal", 0.05, envir = .GlobalEnv)
}

loadAllAuthorsInfo <- function(experimentType, developerSelectionVersion){
  selectedAuthorsDf <- data.frame()
  for(projectName in projectList){
    projectAuthors <- xlsx::read.xlsx(file = paste(finalDataDir, selectedAuthorsFileName.Mix, sep = ""), sheetName = paste(projectName), header = TRUE)

    selectedAuthorsDf <- rbind(selectedAuthorsDf, 
                               cbind(projectName, 
                                     projectAuthors[which(projectAuthors[, developerSelectionVersion] == "T"), ]))
  }
  assign("allAuthorsInfo", selectedAuthorsDf, envir = .GlobalEnv)
}
#######################################################################

### functions to use during the experimentation ##############
createAuthorInfoDf <- function(inputCommitDf, authorColumnName, classColumnName, buggyClassLabel, nonBuggyClassLabel){
  commits <- inputCommitDf
  
  authorInfo <- table(commits[, authorColumnName])
  authorInfo <- cbind(authorInfo, as.data.frame(table(commits[which(commits$CLASS == buggyClassLabel), authorColumnName]))[, 2])
  authorInfo <- cbind(authorInfo, as.data.frame(table(commits[which(commits$CLASS == nonBuggyClassLabel), authorColumnName]))[, 2])
  authorInfo <- cbind(authorInfo, as.numeric(authorInfo[,2] / authorInfo[,1]))
  authorInfo <- cbind(authorInfo, as.numeric(authorInfo[,1] / sum(authorInfo[,1])))
  authorInfo <- authorInfo[order(-authorInfo[, 1]), ]
  authorInfo <- cbind(authorInfo, cumsum(authorInfo[,5]))
  authorInfo <- cbind(rownames(authorInfo), authorInfo)
  
  authorInfo <- as.data.frame(authorInfo)
  colnames(authorInfo) <- c("Author", "CommitCount", "BuggyCommitCount", "NonBuggyCommitCount", "BuggyCommitRatio", "CommitRatio", "CumulativeCommitRatio")
  
  authorInfo$BuggyCommitRatio <- as.numeric(levels(authorInfo$BuggyCommitRatio))[authorInfo$BuggyCommitRatio]
  authorInfo$CommitRatio <- as.numeric(levels(authorInfo$CommitRatio))[authorInfo$CommitRatio]
  authorInfo$CumulativeCommitRatio <- as.numeric(levels(authorInfo$CumulativeCommitRatio))[authorInfo$CumulativeCommitRatio]
  authorInfo$CommitCount <- as.numeric(levels(authorInfo$CommitCount))[authorInfo$CommitCount]
  authorInfo$BuggyCommitCount <- as.numeric(levels(authorInfo$BuggyCommitCount))[authorInfo$BuggyCommitCount]
  authorInfo$NonBuggyCommitCount <- as.numeric(levels(authorInfo$NonBuggyCommitCount))[authorInfo$NonBuggyCommitCount]
  
 
  return(authorInfo)
}
getAuthorMaskedName <- function(nameOfAuthor, authorInfoDf){
  name <- authorInfoDf[which(authorInfoDf[, "Author"] == nameOfAuthor), "Alias"]
  return(name)
}
getAuthorCommitInfoTimeBased <- function(aliasOfAuthor, commitDateInfoDf){
  commitDateInfoDf[commitDateInfoDf[, 1] %in% paste("PM_", aliasOfAuthor, sep = ""), ]
}
getTestSetIntervalsOfAuthor <- function(aliasOfAuthor, selectedAuthorsTable){
  
}
getIntervalDataset <- function(inputDataDir, projectName, authorName, interval, testSetLengthInMonth.Seq, modelType, preparedTrainTestOrInterval, extension){
  firstIndisOfTestSet <- as.numeric(unlist(strsplit(interval, "&", fixed = TRUE))[1])
  
  if(preparedTrainTestOrInterval == "I"){
    if(authorName == "TM"){
      if(extension == "xlsx")
        testSet <- read.xlsx(paste(inputDataDir , projectName, "/", "Int", firstIndisOfTestSet, "_TM.", extension, sep = ""), sheetIndex = 1, header = TRUE)
      else
        testSet <- read.csv.with.err(paste(inputDataDir , projectName, "/", "Int", firstIndisOfTestSet, "_TM.", extension, sep = ""))
      if(testSetLengthInMonth.Seq == 6){
        secondPartOFTestSetFile <- paste(inputDataDir , projectName, "/", "_Int", (firstIndisOfTestSet + 1), "_PM.", extension, sep = "")
        if(file.exists(secondPartOFTestSetFile)){
          if(extension == "xlsx")
            testSet <- rbind(testSet, read.xlsx(secondPartOFTestSetFile, sheetIndex = 1, header = TRUE))
          else
            testSet <- rbind(testSet, read.csv.with.err(secondPartOFTestSetFile))
        }
      }
      
      trainSet <- data.frame()
      for(j in (firstIndisOfTestSet - 1):1){
        fileTrain <- paste(inputDataDir , projectName, "/", "Int", j, "_TM.", extension, sep = "")
        if(file.exists(fileTrain)){
          if(nrow(trainSet) < maxCommitCountInTrainSet){
            if(extension == "xlsx")
              trainSet <- rbind(trainSet, read.xlsx(fileTrain, sheetIndex = 1, header = T))
            else
              trainSet <- rbind(trainSet, read.csv.with.err(fileTrain))
          }
        }
      }
    } 
    else{
      if(extension == "xlsx")
        testSet <- read.xlsx(paste(inputDataDir , projectName, "/", authorName, "/", authorName, "_Int", firstIndisOfTestSet, "_PM.", extension, sep = ""), sheetIndex = 1, header = TRUE)
      else
        testSet <- read.csv.with.err(paste(inputDataDir , projectName, "/", authorName, "/", authorName, "_Int", firstIndisOfTestSet, "_PM.", extension, sep = ""))
      
      if(testSetLengthInMonth.Seq == 6){
        secondPartOFTestSetFile <- paste(inputDataDir , projectName, "/", authorName, "/", authorName, "_Int", (firstIndisOfTestSet + 1), "_PM.", extension, sep = "")
        if(file.exists(secondPartOFTestSetFile)){
          if(extension == "xlsx")
            testSet <- rbind(testSet, read.xlsx(secondPartOFTestSetFile, sheetIndex = 1, header = TRUE))
          else
            testSet <- rbind(testSet, read.csv.with.err(secondPartOFTestSetFile))
        } 
      }
      
      trainSet <- data.frame()
      for(j in (firstIndisOfTestSet - 1):1){
        fileTrain <- paste(inputDataDir , projectName, "/", authorName, "/", authorName, "_Int", j, "_", modelType, ".", extension, sep = "")
        if(file.exists(fileTrain)){
          if(nrow(trainSet) < maxCommitCountInTrainSet){
            if(extension == "xlsx")
              trainSet <- rbind(trainSet, read.xlsx(fileTrain, sheetIndex = 1, header = T))
            else
              trainSet <- rbind(trainSet, read.csv.with.err(fileTrain))
          }
        }
      }
    }
  }
  else if(preparedTrainTestOrInterval == "P"){
    if(authorName == "TM"){
      if(extension == "xlsx"){
        trainSet <- read.xlsx(paste(inputDataDir , projectName, "/", "TrainTM", interval, ".", extension, sep = ""), header = TRUE, sheetIndex = 1)
        testSet <- read.xlsx(paste(inputDataDir , projectName, "/", "TestTM", interval, ".", extension, sep = ""), header = TRUE, sheetIndex = 1)
      }
      else{
        trainSet <- read.csv(paste(inputDataDir , projectName, "/", "TrainTM", interval, ".", extension, sep = ""))
        testSet <- read.csv.with.err(paste(inputDataDir , projectName, "/", "TestTM", interval, ".", extension, sep = ""))
      }
    } 
    else{
      if(extension == "xlsx"){
        trainSet <- read.xlsx(paste(inputDataDir , projectName, "/", authorName, "/", authorName, "Train", modelType, interval, ".", extension, sep = ""), header = TRUE, sheetIndex = 1)
        testSet <- read.xlsx(paste(inputDataDir , projectName, "/", authorName, "/", authorName, "Test", modelType, interval, ".", extension, sep = ""), header = TRUE, sheetIndex = 1)
      } 
      else{
        trainSet <- read.csv.with.err(paste(inputDataDir , projectName, "/", authorName, "/", authorName, "Train", modelType, interval, ".", extension, sep = ""))
        testSet <- read.csv.with.err(paste(inputDataDir , projectName, "/", authorName, "/", authorName, "Test", modelType, interval, ".", extension, sep = ""))
      }
    }
  }
  
  datasetList <- list()
  datasetList[[1]] <- trainSet
  datasetList[[2]] <- testSet
  
  return(datasetList)
}

getAuthorResults <- function(inputDir, projectName, authorAlias, modelName) {
  results <- read.csv(file = paste(inputDir, projectName, "/results/", authorAlias, "_Results", modelName, ".csv", sep = ""), header = TRUE)
  return(results)
}
getNemenyiComparisonResultOfModels <- function(authorName, projectName, numberOfModels, resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta){
  resultsNemenyiPVal <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) + 1 )) 
  colnames(resultsNemenyiPVal) <- c("models", metricList)
  
  if(numberOfModels == 5){
    for(m in 1:length(metricList)){
      modelCompMatrix <- as.matrix(cbind(resultsPM[, metricList[m]], resultsSM[, metricList[m]], resultsGM[, metricList[m]], resultsWSM[, metricList[m]], resultsMeta[, metricList[m]]))
      colnames(modelCompMatrix) <- c("PM", "SM", "GM", "WPM", "Meta")
      modelCompResults <- posthoc.friedman.nemenyi.test(modelCompMatrix)
      
      #modelCompResults matrix pVals:
      ##    PM    SM    GM    WSM   Meta
      #SM   [1,1] NA    NA    NA    NA
      #GM   [2,1] [2,2] NA    NA    NA
      #WSM  [3,1] [3,2] [3,3] NA    NA
      #PM+  [4,1] [4,2] [4,3] [4,4] [4,5]
      
      resultsNemenyiPVal[1, "models"] <- paste("PM / SM", sep = "")
      resultsNemenyiPVal[2, "models"] <- paste("PM / GM", sep = "")
      resultsNemenyiPVal[3, "models"] <- paste("SM / GM", sep = "")
      resultsNemenyiPVal[4, "models"] <- paste("PM / WSM", sep = "")
      resultsNemenyiPVal[5, "models"] <- paste("PM / PM+", sep = "")
      resultsNemenyiPVal[6, "models"] <- paste("SM / WSM", sep = "")
      resultsNemenyiPVal[7, "models"] <- paste("SM / PM+", sep = "")
      resultsNemenyiPVal[8, "models"] <- paste("GM / WSM", sep = "")
      resultsNemenyiPVal[9, "models"] <- paste("GM / PM+", sep = "")
      resultsNemenyiPVal[10, "models"] <- paste("WSM / PM+", sep = "")
      
      resultsNemenyiPVal[1, paste(metricList[m], sep = "")] <- modelCompResults$p.value[1, 1]
      resultsNemenyiPVal[2, paste(metricList[m], sep = "")] <- modelCompResults$p.value[2, 1]
      resultsNemenyiPVal[3, paste(metricList[m], sep = "")] <- modelCompResults$p.value[2, 2]
      resultsNemenyiPVal[4, paste(metricList[m], sep = "")] <- modelCompResults$p.value[3, 1]
      resultsNemenyiPVal[5, paste(metricList[m], sep = "")] <- modelCompResults$p.value[3, 2]
      resultsNemenyiPVal[6, paste(metricList[m], sep = "")] <- modelCompResults$p.value[3, 3]
      resultsNemenyiPVal[7, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 1]
      resultsNemenyiPVal[8, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 2]
      resultsNemenyiPVal[9, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 3]
      resultsNemenyiPVal[10, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 4]
    }
  }
  else if(numberOfModels == 3){
    for(m in 1:length(metricList)){
      modelCompMatrix <- as.matrix(cbind(resultsPM[, metricList[m]], resultsSM[, metricList[m]], resultsGM[, metricList[m]]))
      colnames(modelCompMatrix) <- c("PM", "SM", "GM")
      modelCompResults <- posthoc.friedman.nemenyi.test(modelCompMatrix)
        
      #modelCompResults matrix pVals:
      ##    PM    SM    GM    
      #SM   [1,1] NA    NA   
      #GM   [2,1] [2,2] NA   
      
      resultsNemenyiPVal[1, "models"] <- paste("PM / SM", sep = "")
      resultsNemenyiPVal[2, "models"] <- paste("PM / GM", sep = "")
      resultsNemenyiPVal[3, "models"] <- paste("SM / GM", sep = "")
      
      resultsNemenyiPVal[1, paste(metricList[m], sep = "")] <- modelCompResults$p.value[1, 1]
      resultsNemenyiPVal[2, paste(metricList[m], sep = "")] <- modelCompResults$p.value[2, 1]
      resultsNemenyiPVal[3, paste(metricList[m], sep = "")] <- modelCompResults$p.value[2, 2]
    }
  }
  
  return(resultsNemenyiPVal)
}
getAuthorColorByMetric <- function(plotInputDir, projectName, authorAlias, metricName, numberOfColors, numberOfModels){
  pVal <- 0.05
  # betterCount <- 0
  # worseCount <- 0
  if(metricName == "pf")
    aboveAvgIsGood <- FALSE
  else
    aboveAvgIsGood <- TRUE
  
  if(numberOfModels == 5){
    resultsPM <- getAuthorResults(plotInputDir, projectName, authorAlias, "PM")
    resultsSM <- getAuthorResults(plotInputDir, projectName, authorAlias, "SM")
    resultsGM <- getAuthorResults(plotInputDir, projectName, authorAlias, "GM")
    resultsWSM <- getAuthorResults(plotInputDir, projectName, authorAlias, "WPM")
    resultsMeta <- getAuthorResults(plotInputDir, projectName, authorAlias, "Meta")
    
    #returns a table that contains pValues of PM/SM, PM/GM and SM/GM comparisons
    authorComparisonResults <- getNemenyiComparisonResultOfModels(authorAlias, projectName, numberOfModels, resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta)
    
    medianPM <- medianChangeNaToZero(resultsPM[, metricName])
    medianSM <- medianChangeNaToZero(resultsSM[, metricName])
    medianGM <- medianChangeNaToZero(resultsGM[, metricName])
    medianWSM <- medianChangeNaToZero(resultsWSM[, metricName])
    medianMeta <- medianChangeNaToZero(resultsMeta[, metricName])
    
    # dodgerblue1
    # lightskyblue
    # lightskyblue1
    # lightsteelblue1
    
    # firebrick2
    # indianred1
    # salmon
    # sandybrown
    
    # yellowgreen
    
    PM_SMDiff <- authorComparisonResults[1, metricName] < pVal
    PM_GMDiff <- authorComparisonResults[2, metricName] < pVal
    PM_WSMDiff <- authorComparisonResults[4, metricName] < pVal
    PM_MetaDiff <- authorComparisonResults[5, metricName] < pVal
    
    diffVector <- c(PM_SMDiff, PM_GMDiff, PM_WSMDiff, PM_MetaDiff)
    modelCompNames <- c("PM_SM", "PM_GM", "PM_WSM", "PM_Meta")
    howManyDifference <- length(diffVector[diffVector == TRUE])
    
    if(howManyDifference == 0) #if PM is not any statistically different from others
      tmpColorString <- "'black'"
    else if(howManyDifference == 1){ #if PM is statistically different from only one other model
      modelIndex <- which(diffVector == TRUE)
      medianOfComparedModel <- get(paste("median", strsplit(modelCompNames[modelIndex], "_")[[1]][2], sep = ""))
      
      if(medianPM < medianOfComparedModel)
        tmpColorString <- "'sandybrown'"
      else if(medianPM > medianOfComparedModel)
        tmpColorString <- "'lightsteelblue1'"
      else
        tmpColorString <- "'yellowgreen'"
    }
    else if(howManyDifference == 2){ #if PM is statistically different from two of the other models
      modelIndex <- which(diffVector == TRUE)
      medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
      medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
      
      if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2)
        tmpColorString <- "'salmon'"
      else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2)
        tmpColorString <- "'lightskyblue1'"
      else
        tmpColorString <- "'yellowgreen'"
    }
    else if(howManyDifference == 3){ #if PM is statistically different from three of the other models
      modelIndex <- which(diffVector == TRUE)
      medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
      medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
      medianOfComparedModel3 <- get(paste("median", strsplit(modelCompNames[modelIndex[3]], "_")[[1]][2], sep = ""))
      
      if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2 && medianPM < medianOfComparedModel3)
        tmpColorString <- "'indianred1'"
      else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2 && medianPM > medianOfComparedModel3)
        tmpColorString <- "'lightskyblue'"
      else
        tmpColorString <- "'yellowgreen'"
    }
    else if(howManyDifference == 3){ #if PM is statistically different from all the other models
      modelIndex <- which(diffVector == TRUE)
      medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
      medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
      medianOfComparedModel3 <- get(paste("median", strsplit(modelCompNames[modelIndex[3]], "_")[[1]][2], sep = ""))
      medianOfComparedModel4 <- get(paste("median", strsplit(modelCompNames[modelIndex[4]], "_")[[1]][2], sep = ""))
      
      if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2 && medianPM < medianOfComparedModel3 && medianPM < medianOfComparedModel4)
        tmpColorString <- "'firebrick2'"
      else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2 && medianPM > medianOfComparedModel3 && medianPM > medianOfComparedModel4)
        tmpColorString <- "'dodgerblue1'"
      else
        tmpColorString <- "'yellowgreen'"
    }
    else
      tmpColorString <- "'yellowgreen'"
    
    if(numberOfColors == 3){
      tmpColorString <- gsub("dodgerblue1", "blue", tmpColorString)
      tmpColorString <- gsub("lightskyblue1", "blue", tmpColorString)
      tmpColorString <- gsub("lightskyblue", "blue", tmpColorString)
      tmpColorString <- gsub("lightsteelblue1", "blue", tmpColorString)
      
      tmpColorString <- gsub("firebrick2", "red", tmpColorString)
      tmpColorString <- gsub("indianred1", "red", tmpColorString)
      tmpColorString <- gsub("salmon", "red", tmpColorString)
      tmpColorString <- gsub("sandybrown", "red", tmpColorString)
      
      tmpColorString <- gsub("yellowgreen", "green", tmpColorString)
    }
    
    return (tmpColorString)
    
  }
  else if(numberOfModels == 3){
    resultsPM <- getAuthorResults(plotInputDir, projectName, authorAlias, "PM")
    resultsSM <- getAuthorResults(plotInputDir, projectName, authorAlias, "SM")
    resultsGM <- getAuthorResults(plotInputDir, projectName, authorAlias, "GM")
    
    #returns a table that contains pValues of PM/SM, PM/GM and SM/GM comparisons
    authorComparisonResults <- getNemenyiComparisonResultOfModels(authorAlias, projectName, numberOfModels, resultsPM, resultsSM, resultsGM)
  
    medianPM <- medianChangeNaToZero(resultsPM[, metricName])
    medianSM <- medianChangeNaToZero(resultsSM[, metricName])
    medianGM <- medianChangeNaToZero(resultsGM[, metricName])
    
    # dodgerblue1 --> best
    # lightskyblue
    # lightskyblue1 --
    # lightsteelblue1 --
    
    # firebrick2 --> worst
    # indianred1 
    # salmon --
    # sandybrown --
    
    # yellowgreen
    
    PM_SMDiff <- authorComparisonResults[1, metricName] < pVal
    PM_GMDiff <- authorComparisonResults[2, metricName] < pVal
    
    diffVector <- c(PM_SMDiff, PM_GMDiff)
    modelCompNames <- c("PM_SM", "PM_GM")
    howManyDifference <- length(diffVector[diffVector == TRUE])
    
    if(howManyDifference == 0) #if PM is not any statistically different from others
      tmpColorString <- "'black'"
    else if(howManyDifference == 1){ #if PM is statistically different from only one other model
      modelIndex <- which(diffVector == TRUE)
      medianOfComparedModel <- get(paste("median", strsplit(modelCompNames[modelIndex], "_")[[1]][2], sep = ""))
      
      if(medianPM < medianOfComparedModel)
        tmpColorString <- ifelse(aboveAvgIsGood, "'indianred1'", "'lightskyblue'")
      else if(medianPM > medianOfComparedModel)
        tmpColorString <- ifelse(aboveAvgIsGood, "'lightskyblue'", "'indianred1'")
      else
        tmpColorString <- "'yellowgreen'"
    }
    else if(howManyDifference == 2){ #if PM is statistically different from two of the other models
      modelIndex <- which(diffVector == TRUE)
      medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
      medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
      
      if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2)
        tmpColorString <- ifelse(aboveAvgIsGood, "'firebrick2'", "'dodgerblue1'")
      else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2)
        tmpColorString <- ifelse(aboveAvgIsGood, "'dodgerblue1'", "'firebrick2'")
      else
        tmpColorString <- "'yellowgreen'"
    }
    else
      tmpColorString <- "'yellowgreen'"
   
    if(numberOfColors == 2){
      tmpColorString <- gsub("dodgerblue1", "blue", tmpColorString)
      tmpColorString <- gsub("lightskyblue", "blue", tmpColorString)
      
      tmpColorString <- gsub("firebrick2", "red", tmpColorString)
      tmpColorString <- gsub("indianred1", "red", tmpColorString)
      
      tmpColorString <- gsub("yellowgreen", "green", tmpColorString)
    }
    
    return (tmpColorString)
  }
}

returnAsDate <- function(dateTime){
  # dateTime <- "Wed Feb 1 12:48:12 2017 +0300" #sample date
  # dateTime = "Sun Jun  9 23:12:06 2013" #another sample
  
  dateTimeElements <- unlist(strsplit(dateTime, " ", fixed = TRUE))
  dateTimeElements <- dateTimeElements[dateTimeElements != ""] 
  time <- dateTimeElements[4]
  timeElements <- unlist(strsplit(time, ":", fixed = TRUE))
  
  year <- dateTimeElements[5]
  month <- which(shortMonths %in% dateTimeElements[2])
  dayOfMonth <- dateTimeElements[3]
  
  hour <- timeElements[1]
  min <- timeElements[2]
  sec <- timeElements[3]
  
  #ISOdatetime(Year, Month, Day, Hour, Min, Sec)
  dateTime <- ISOdatetime(year, month, dayOfMonth, hour, min, sec, tz="UTC")
  
  return (dateTime)
}

collapseListOfVectors <- function(listOfVectors, collapseCharForVector, collapseCharForList){
  collapsedString <- character(length = 0)
  for(j in 1:length(listOfVectors)){
    collapsedString <- c(collapsedString, paste(listOfVectors[[j]], collapse = collapseCharForVector))
  }
  return(paste(collapsedString, collapse = collapseCharForList))
}

calculateConfusionMatrix <- function(predictions, positiveLabel, negativeLabel){
  FP <- as.numeric(length(which(predictions$actual == negativeLabel & predictions$predicted == positiveLabel)))
  TN <- as.numeric(length(which(predictions$actual == negativeLabel & predictions$predicted == negativeLabel)))
  FN <- as.numeric(length(which(predictions$actual == positiveLabel & predictions$predicted == negativeLabel)))
  TP <- as.numeric(length(which(predictions$actual == positiveLabel & predictions$predicted == positiveLabel)))
  
  # confMatrix <- table(predictions$actual, predictions$predicted)
  precision <- TP / (TP + FP)
  pd <- TP / (TP + FN) 
  pf <- FP / (FP + TN)
  F1 <- (2 * precision * pd) / (precision + pd)
  MCC <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)) # closer to -1 indicates wrong classification while closer to 1 true classification
  
  # AUC calculation
  library(AUC)
  predictions$actualLabels <- ifelse(predictions$actual == positiveLabel, 1, 0)
  predictions$actualLabels <- as.factor(predictions$actualLabels)
  AUC <- AUC::auc(roc(predictions[, positiveLabel], predictions$actualLabels))
  # pROC::auc(PredPM$actual, PredPM$BUG, levels = c("BUG", "NO_BUG"), direction = ">") #library(pROC)
  
  # Briers Score calculation: smaller scores indicates better forecast
  numberOfTestInstances <- nrow(predictions) 
  predictions$outcomeLabels <- ifelse(predictions$predicted == positiveLabel, 1, 0)
  predictions$difference <- predictions[, positiveLabel] - predictions$outcomeLabels 
  predictions$differenceSquare <- predictions$difference ^ 2
  BScore <- sum(predictions$differenceSquare) / numberOfTestInstances
  # library(DescTools)
  # BrierScore(predictions$outcomeLabels, predictions$BUG, scaled = FALSE)
  
  # bScore <- sum((tmpPredictions$prediction - ifelse(grepl(x = tmpPredictions$predicted, pattern = "TRUE", fixed = TRUE), 1, 0)) ^ 2) / nrow(tmpPredictions)
  
  values <- as.data.frame(matrix(ncol = 4, nrow = 1, c(TP, FP, FN, TN)))
  for(m in metricList)
    values[, m] <- get(m)
  colnames(values) <- c("TP", "FP", "FN", "TN", metricList)
  return(values)
}

logFilter <- function(data){
  # logFilter.setExpression("ifelse(A<=0.000001, log(0.000001), log(A))");
  for(col in 1:ncol(data)){
    if(is.numeric(data[, col]))
      data[, col] <- ifelse(test = data[, col] <= 0.000001, yes = log(0.000001), no = log(data[, col]))
  }
  return(data)
}

getTopFeaturesByInfoGain <- function(data, numberOfSelection){
  #increase the range of numeric cloumns to avoid error
  numCols <- sapply(data, is.numeric)  
  data[numCols] <- data[numCols] * 1000000 
  
  weights <- information.gain(CLASS~., data)
  
  if(all(weights$attr_importance == 0)){
    rankedMetrics <- rownames(weights)[order(weights)]
  } else {
    rankedMetrics <- rownames(weights)[rev(order(weights))]
  }
  
  return(rankedMetrics[1:numberOfSelection])
}

chekcIfAllCellsAreEqual <- function(df){
  x <- as.vector(t(unique(df)))
  isEqual <- all(x == x[1])  
  
  return(isEqual)
}

read.csv.with.err <- function(file){
  data <- tryCatch({
   read.csv(file, header = TRUE)
  }, error = function(err){
    print(dataIsNotExistError)
  })
  
  return(data)
}

medianChangeNaToZero <- function(x){
  x[is.na(x)] <- 0
  median(x)
}

perm = function(n, x) {
  factorial(n) / factorial(n-x)
}
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}
###################################################
