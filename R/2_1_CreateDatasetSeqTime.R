# timeline based commit analysis to create timeline based train test sets

# library(xlsx)
library("openxlsx")

rm(list = ls())

mainProjectDir = "/Users/beken/projects/DP_OS/"
setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

testSetLengthsInMonths <- c(6)

# create interval data (PM-SM-GM-TM) according to the selected authors' personal date sequences 
timeIntervalDataCreation <- FALSE

# prepare train and test sets (PM-SM-GM-WPM-TM) utilizing the interval data
sequentialTimeTrainTestDataPreparation <- TRUE

for(p in 1:length(projectList)){
  projectName <- projectList[p]
  commits <- read.csv(file = paste(changeDataDir, projectName, ".csv", sep = ""), header = TRUE, sep = ",")
  authorsInfo <- openxlsx::read.xlsx(xlsxFile = paste(finalDataDir, "authorInfo.xlsx", sep = ""), sheet = projectName)

  if(timeIntervalDataCreation){
    orderedAuthorList <- rev(authorsInfo$Author)
    shortenedCommits <- commits[, c("HASHID", "AUTHOR_MASKED", "COMMITTER_DATE", "CLASS")]
    shortenedCommits <- shortenedCommits[rev(order(match(shortenedCommits$AUTHOR, orderedAuthorList))),]
    shortenedCommits[, "NEW_DATE"] <- 0
    class(shortenedCommits[, "NEW_DATE"]) <- "Date"
    
    for(t in 1:nrow(shortenedCommits)){
      shortenedCommits[t, "NEW_DATE"] <- returnAsDate(as.character(shortenedCommits[t, "COMMITTER_DATE"]))
    }
    
    selectedAuthors <- read.xlsx(xlsxFile = paste(finalDataDir, "selectedAuthorsSeqTime.xlsx", sep = ""), sheet = projectName)
    
    for(tm in testSetLengthsInMonths){
      howManyInterval <- tm / 3 
      
      dir.create(paste(processingDataDir, "/SeqTime_Intervals/", tm, "Months/", projectName, sep = ""), recursive = T, showWarnings = F)
    
      gmData <- shortenedCommits
      smData <- shortenedCommits[which(shortenedCommits[, "AUTHOR_MASKED"] %in% selectedAuthors[selectedAuthors[, paste("Selected_", tm, "Months", sep = "")] == "T", "Alias"]), ]
    
      for(a in 1:nrow(selectedAuthors)){
        if(selectedAuthors[a, paste("Selected_", tm, "Months", sep = "")] == "T"){
          dir.create(paste(processingDataDir, "/SeqTime_Intervals/", tm, "Months/", projectName, "/", selectedAuthors[a, "Alias"], sep = ""), showWarnings = F)
        
          pmData <- shortenedCommits[which(shortenedCommits[, "AUTHOR_MASKED"] == selectedAuthors[a, "Alias"]), ]
    
          minDate <- min(pmData[, "NEW_DATE"])
          maxDate <- max(pmData[, "NEW_DATE"])
          
          # same logic is used to create same dates as in commitDateInfo.xlsx
          pmDateSequence <- seq(from = minDate, to = maxDate, by = "3 month")
          pmDateSequence <- c(pmDateSequence, maxDate)
          
          for(s in 1:length(pmDateSequence)){
            commitsInInterval <- pmData[pmData$NEW_DATE >= pmDateSequence[s] & pmData$NEW_DATE < pmDateSequence[s + 1], ]
            if(s == (length(pmDateSequence) - 1)) #include the last days' commits into the last interval
              commitsInInterval <- rbind(commitsInInterval, pmData[pmData$NEW_DATE == pmDateSequence[s], ])
            
            buggyCount <- length(which(commitsInInterval$CLASS == "BUG"))
            nonBuggyCount <- length(which(commitsInInterval$CLASS == "NO_BUG"))
            
            # if(buggyCount & nonBuggyCount){
              intervalDataPM <- commits[commits[,1] %in% commitsInInterval$HASHID, ]
              write.csv(x = intervalDataPM, 
                        file = paste(processingDataDir, "SeqTime_Intervals/", tm, "Months/",
                                     projectName, "/", 
                                     selectedAuthors[a, "Alias"], "/", 
                                     selectedAuthors[a, "Alias"], "_Int", s, "_PM.csv" , sep = ""),
                        row.names = FALSE)
            # }
            
            
            commitsInInterval <- smData[smData$NEW_DATE >= pmDateSequence[s] & smData$NEW_DATE < pmDateSequence[s + 1], ]
            if(s == (length(pmDateSequence) - 1)) #include the last days' commits into the last interval
              commitsInInterval <- rbind(commitsInInterval, smData[smData$NEW_DATE == pmDateSequence[s], ])
            
            buggyCount <- length(which(commitsInInterval$CLASS == "BUG"))
            nonBuggyCount <- length(which(commitsInInterval$CLASS == "NO_BUG"))
            
            # if(buggyCount & nonBuggyCount){
              intervalDataSM <- commits[commits[,1] %in% commitsInInterval$HASHID, ]
              write.csv(x = intervalDataSM, 
                        file = paste(processingDataDir, "/SeqTime_Intervals/", tm, "Months/",
                                     projectName, "/", 
                                     selectedAuthors[a, "Alias"], "/", 
                                     selectedAuthors[a, "Alias"], "_Int", s, "_SM.csv" , sep = ""),
                        row.names = FALSE)
            # }
            
            
            commitsInInterval <- gmData[gmData$NEW_DATE >= pmDateSequence[s] & gmData$NEW_DATE < pmDateSequence[s + 1], ]
            if(s == (length(pmDateSequence) - 1)) #include the last days' commits into the last interval
              commitsInInterval <- rbind(commitsInInterval, gmData[gmData$NEW_DATE == pmDateSequence[s], ])
            
            buggyCount <- length(which(commitsInInterval$CLASS == "BUG"))
            nonBuggyCount <- length(which(commitsInInterval$CLASS == "NO_BUG"))
            
            # if(buggyCount & nonBuggyCount){
              intervalDataGM <- commits[commits[,1] %in% commitsInInterval$HASHID, ]
              write.csv(x = intervalDataGM, 
                      file = paste(processingDataDir, "/SeqTime_Intervals/", tm, "Months/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "_Int", s, "_GM.csv" , sep = ""),
                      row.names = FALSE)
            # }
          }
        }
      }
    }
  }
  
  if(sequentialTimeTrainTestDataPreparation){
    for(tm in testSetLengthsInMonths){
      selectedAuthors <- openxlsx::read.xlsx(xlsxFile = paste(finalDataDir, "selectedAuthorsSeqTime.xlsx", sep = ""), sheet = projectName)
      selectedAuthors <- selectedAuthors[selectedAuthors[, paste("Selected_", tm, "Months", sep = "")] == "T", ]
    
      for(a in 1:nrow(selectedAuthors)){
        authorName <- selectedAuthors[a, "Alias"]
        
        testIntervals <- as.character(selectedAuthors[a, paste("TestIntervals_", tm, "Months", sep = "")])
        testIntervals <- unlist(strsplit(testIntervals, "-", fixed = TRUE))
        
        for(interval in testIntervals){
          if(authorName == "TM"){
            datasetTM <- getIntervalDataset(inputDataDir = paste(finalDataDir, "SeqTime_Intervals/", tm, "Months/", sep = ""), 
                                            projectName = projectName, 
                                            authorName = authorName,
                                            interval = interval,
                                            testSetMonths = tm,
                                            modelType = "",
                                            preparedTrainTestOrInterval = "I",
                                            extension = "csv")
            
            trainSetTM <- datasetTM[[1]]
            testSetTM <- datasetTM[[2]]
            
            write.csv(x = trainSetTM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   projectName, "/", 
                                   "TrainTM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            write.csv(x = testSetTM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   projectName, "/", 
                                   "TestTM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
          }
          else{
            dir.create(paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/", "MaxTrainCommit", maxCommitCountInTrainSet, "/", projectName, "/", selectedAuthors[a, "Alias"], sep = ""), recursive = T, showWarnings = F)
            
            datasetPM <- getIntervalDataset(inputDataDir = paste(finalDataDir, "SeqTime_Intervals/", tm, "Months/", sep = ""), 
                                            projectName = projectName, 
                                            authorName = authorName,
                                            interval = interval,
                                            testSetMonths = tm,
                                            modelType = "PM",
                                            preparedTrainTestOrInterval = "I",
                                            extension = "csv")
            
            datasetSM <- getIntervalDataset(inputDataDir = paste(finalDataDir, "SeqTime_Intervals/", tm, "Months/", sep = ""), 
                                            projectName = projectName, 
                                            authorName = authorName,
                                            interval = interval,
                                            testSetMonths = tm,
                                            modelType = "SM",
                                            preparedTrainTestOrInterval = "I",
                                            extension = "csv")
            
            datasetGM <- getIntervalDataset(inputDataDir = paste(finalDataDir, "SeqTime_Intervals/", tm, "Months/", sep = ""), 
                                            projectName = projectName,
                                            authorName = authorName,
                                            interval = interval,
                                            testSetMonths = tm,
                                            modelType = "GM",
                                            preparedTrainTestOrInterval = "I",
                                            extension = "csv")
            
            trainSetPM <- datasetPM[[1]]
            testSetPM <- datasetPM[[2]]
            
            trainSetSM <- datasetSM[[1]]
            testSetSM <- datasetSM[[2]]
            
            trainSetGM <- datasetGM[[1]]
            testSetGM <- datasetGM[[2]]
          
            
            # do not create WPM data for now
            
            # trainSetWPM <- data.frame()
            # otherAuthors <- unique(trainSetGM$AUTHOR_MASKED)
            # otherAuthorInfo <- data.frame(matrix(nrow = 0, ncol = 5))
            # colnames(otherAuthorInfo) <- c("Author", "AvailableCommitCount", "BuggyRatio", "WPM_CommitCount", "WPM_BuggyCommitCount")
            # 
            # for(o in 1:length(otherAuthors)){
            #   otherAuthorName <- otherAuthors[o]
            #   
            #   if(otherAuthorName != authorName){
            #     # otherAuthorBuggyRatio <- authorDf[authorDf$Author == otherAuthorName, "BuggyCommitRatio"]
            #     otherAuthorCommits <- trainSetGM[which(trainSetGM$AUTHOR_MASKED == otherAuthorName), ]
            #     otherAuthorBuggyCommits <- trainSetGM[which(trainSetGM$AUTHOR_MASKED == otherAuthorName & trainSetGM$CLASS == "BUG" ), ]
            #     otherAuthorBuggyRatio <- nrow(otherAuthorBuggyCommits) / nrow(otherAuthorCommits)
            #     
            #     otherAuthorAvailableCommitCount <- nrow(trainSetGM[trainSetGM$AUTHOR_MASKED == otherAuthorName, ])
            #     otherAuthorAvailableBuggyCommitCount <- nrow(trainSetGM[which(trainSetGM$AUTHOR_MASKED == otherAuthorName & trainSetGM$CLASS == "BUG"), ])
            #     
            #     if(otherAuthorAvailableBuggyCommitCount >= otherAuthorAvailableCommitCount * otherAuthorBuggyRatio){
            #       otherAuthorInfo[nrow(otherAuthorInfo) + 1, "Author"] <- as.character(otherAuthorName)
            #       otherAuthorInfo[nrow(otherAuthorInfo), "AvailableCommitCount"] <- otherAuthorAvailableCommitCount
            #       otherAuthorInfo[nrow(otherAuthorInfo), "BuggyRatio"] <- otherAuthorBuggyRatio
            #     }
            #   }
            # }
            # 
            # authorCommitCount50Perc <- (nrow(trainSetPM) + nrow(testSetPM)) / 2
            # otherAuthorsTotalCommitCount <- sum(otherAuthorInfo[, "AvailableCommitCount"])
            # 
            # if(otherAuthorsTotalCommitCount >= authorCommitCount50Perc){
            #   for(o in 1:nrow(otherAuthorInfo)){
            #     otherAuthorName <- otherAuthorInfo[o, "Author"]
            #     otherAuthorBuggyRatio <- otherAuthorInfo[o, "BuggyRatio"]
            #     otherAuthorAvailableCommitCount <- otherAuthorInfo[o, "AvailableCommitCount"]
            #     
            #     otherAuthorWPMCommitCount <- round(authorCommitCount50Perc * otherAuthorAvailableCommitCount / otherAuthorsTotalCommitCount)
            #     otherAuthorWPMBuggyCommitCount <- round(otherAuthorWPMCommitCount * otherAuthorBuggyRatio)
            #     otherAuthorWPMNonBuggyCommitCount <- otherAuthorWPMCommitCount - otherAuthorWPMBuggyCommitCount
            #     
            #     otherAuthorInfo[o, "WPM_CommitCount"] <- otherAuthorWPMCommitCount
            #     otherAuthorInfo[o, "WPM_BuggyCommitCount"] <- otherAuthorWPMBuggyCommitCount
            #     
            #     otherAuthorBuggyCommits <- trainSetGM[which(trainSetGM$AUTHOR_MASKED == otherAuthorName & trainSetGM$CLASS == "BUG" ), ]
            #     otherAuthorNonBuggyCommits <- trainSetGM[which(trainSetGM$AUTHOR_MASKED == otherAuthorName & trainSetGM$CLASS == "NO_BUG" ), ]
            #     
            #     trainSetWPM <- rbind(trainSetWPM,
            #                          otherAuthorBuggyCommits[sample(nrow(otherAuthorBuggyCommits), otherAuthorWPMBuggyCommitCount), ],
            #                          otherAuthorNonBuggyCommits[sample(nrow(otherAuthorNonBuggyCommits), otherAuthorWPMNonBuggyCommitCount), ]
            #     )
            #   }
            # }
            # 
            # trainSetWPM <- rbind(trainSetWPM, trainSetPM)
            # trainSetWPM <- trainSetWPM[sample(nrow(trainSetWPM)), ]
            # 
            # write.csv(x = trainSetWPM, 
            #           file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
            #                        "MaxTrainCommit", maxCommitCountInTrainSet, "/",
            #                        projectName, "/", 
            #                        selectedAuthors[a, "Alias"], "/", 
            #                        selectedAuthors[a, "Alias"], "TrainWPM", interval, ".csv" , sep = ""),
            #           row.names = FALSE)
            write.csv(x = testSetPM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TestWPM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            
            write.csv(x = trainSetPM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TrainPM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            write.csv(x = testSetPM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TestPM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            
            write.csv(x = trainSetSM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TrainSM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            write.csv(x = testSetSM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TestSM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            
            write.csv(x = trainSetGM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TrainGM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
            write.csv(x = testSetGM, 
                      file = paste(processingDataDir, "/SeqTime_TrainTest/", tm, "Months/",
                                   "MaxTrainCommit", maxCommitCountInTrainSet, "/",
                                   projectName, "/", 
                                   selectedAuthors[a, "Alias"], "/", 
                                   selectedAuthors[a, "Alias"], "TestGM", interval, ".csv" , sep = ""),
                      row.names = FALSE)
          }
        }
      }
    }
  }
}
