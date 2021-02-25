library(xlsx)
library(caret)

rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
setwd(mainProjectDir)

experimentType <- "MixTime"
experimentSignature <- "MixTime"
devsDataSizeOption.Mix <- "Neq" #Neq, Eq
dataBalancingOption <- "None" # Under, SMOTE
developerSelectionVersion <- "SelectedV2"

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

for(p in 1:length(projectList)){
  projectName <- projectList[p]
  loadProjectData(projectName)
  loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
  
  selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
  changesDfName <- paste(projectName, "Changes", sep = "")
  
  # first, create a general GM and SM data (then we will exclude the folds from SM and GM data)
  tmpGMData <- get(changesDfName)
  tmpSMData <- data.frame()
  
  # older WO
  if(devsDataSizeOption.Mix == "Neq"){
    for(d in 1:numberOfSelectedAuthors){
      authorName <- selectedAuthorNames[d]
      authorAlias <- selectedAuthorAliases[d]

      allInstancesPM <- get(changesDfName)[which(get(changesDfName)$AUTHOR_MASKED == authorAlias), ]
      
      tmpSMData <- rbind(tmpSMData, allInstancesPM)
    }
  }
  # older WITH
  else if(devsDataSizeOption.Mix == "Eq"){
    for(d in 1:numberOfSelectedAuthors){
      authorName <- selectedAuthorNames[d]
      authorAlias <- selectedAuthorAliases[d]
      
      bugChangeCount <- round(minChangeCountAmongSelectedAuthors * get(selectedAuthorsDfName)[d, "BuggyCommitRatio"])
      noBugChangeCount <- minChangeCountAmongSelectedAuthors - bugChangeCount
      
      bugInstancesPM <- get(changesDfName)[sample(which(get(changesDfName)$CLASS == "BUG" & get(changesDfName)$AUTHOR_MASKED == authorAlias), bugChangeCount, replace = FALSE), ]
      noBugInstancesPM <- get(changesDfName)[sample(which(get(changesDfName)$CLASS == "NO_BUG" & get(changesDfName)$AUTHOR_MASKED == authorAlias), noBugChangeCount, replace = FALSE), ]
      
      tmpSMData <- rbind(tmpSMData, bugInstancesPM, noBugInstancesPM)
    }
  }
  
  # second, for each run create PM folds, and save PM, SM, and GM train-test sets
  infoTable <- data.frame(matrix(nrow = 0, ncol = 9))
  colnames(infoTable) <- c("Project", "Author", "Alias", "Run", "Fold", "Model", "Buggy", "NonBuggy", "Total")
    
  for(r in 1:numberOfRuns){
    tmpSMData <- tmpSMData[sample(nrow(tmpSMData), replace = FALSE), ] #shuffle data
    tmpGMData <- tmpGMData[sample(nrow(tmpGMData), replace = FALSE), ] #shuffle data
    
    for(d in 1:numberOfSelectedAuthors){
      authorName <- selectedAuthorNames[d]
      authorAlias <- selectedAuthorAliases[d]
      
      dir.create(paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, sep = ""), recursive = T, showWarnings = FALSE)
      
      if(devsDataSizeOption.Mix == "Eq"){
        bugChangeCount <- round(minChangeCountAmongSelectedAuthors * get(selectedAuthorsDfName)[d, "BuggyCommitRatio"])
        noBugChangeCount <- minChangeCountAmongSelectedAuthors - bugChangeCount
        
        bugInstancesPM <- get(changesDfName)[sample(which(get(changesDfName)$CLASS == "BUG" & get(changesDfName)$AUTHOR_MASKED == authorAlias), bugChangeCount, replace = FALSE), ]
        noBugInstancesPM <- get(changesDfName)[sample(which(get(changesDfName)$CLASS == "NO_BUG" & get(changesDfName)$AUTHOR_MASKED == authorAlias), noBugChangeCount, replace = FALSE), ]
        
        tmpPMData <- rbind(bugInstancesPM, noBugInstancesPM)
      }
      else
        tmpPMData <- get(changesDfName)[which(get(changesDfName)$AUTHOR_MASKED == authorAlias), ]
      
      folds <- createFolds(tmpPMData$CLASS, k = numberOfFolds)
      
      for(f in 1:numberOfFolds){
        foldData <- tmpPMData[folds[[f]], ]
        
        testSet <- foldData
        trainSetPM <- tmpPMData[!(tmpPMData$HASHID %in% testSet$HASHID), ]
        trainSetSM <- tmpSMData[!(tmpSMData$HASHID %in% testSet$HASHID), ]
        trainSetGM <- tmpGMData[!(tmpGMData$HASHID %in% testSet$HASHID), ]
        
        infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "test" ,length(which(testSet$CLASS == "BUG")), length(which(testSet$CLASS == "NO_BUG")), nrow(testSet))
        infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "PM", length(which(trainSetPM$CLASS == "BUG")), length(which(trainSetPM$CLASS == "NO_BUG")), nrow(trainSetPM))
        infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "SM", length(which(trainSetSM$CLASS == "BUG")), length(which(trainSetSM$CLASS == "NO_BUG")), nrow(trainSetSM))
        infoTable[nrow(infoTable) + 1, ] <- c(projectName, authorName, authorAlias, r, f, "GM", length(which(trainSetGM$CLASS == "BUG")), length(which(trainSetGM$CLASS == "NO_BUG")), nrow(trainSetGM))
        
        # below code writes all columns in train-test sets
        # testSet <- write.csv(testSet, paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), row.names = F)
        # trainSetPM <- write.csv(trainSetPM, paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM",f, ".csv", sep = ""), row.names = F)
        # trainSetSM <- write.csv(trainSetSM, paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), row.names = F)
        # trainSetGM <- write.csv(trainSetGM, paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), row.names = F)

        # instead of keeping all columns in train-test sets, I keep only the hash ids
        testSet <- write.csv(data.frame(HASHID = testSet$HASHID), paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), row.names = F)
        trainSetPM <- write.csv(data.frame(HASHID = trainSetPM$HASHID), paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM",f, ".csv", sep = ""), row.names = F)
        trainSetSM <- write.csv(data.frame(HASHID = trainSetSM$HASHID), paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), row.names = F)
        trainSetGM <- write.csv(data.frame(HASHID = trainSetGM$HASHID), paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), row.names = F)
        
        #info
        print(paste("Train-test set generation: ", projectName, " - ", authorName, " - ", authorAlias, " - run:", r, " - fold:", f, sep = ""))
      }
    }
  }
  
  write.csv(x = infoTable, file = paste(processingDataDir, "MixTime_", devsDataSizeOption.Mix, "_TrainTest/", projectName, "/infoTable.csv", sep = ""), row.names = F)
}
