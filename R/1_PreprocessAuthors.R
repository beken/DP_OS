# 13.02.2021
# Bu scripti Experiment_V4 ve oncesinde kullandim. 
# Tahminimce bir kez calistirdiktan sonra tekrar calistirmaya gerek olmayacak bir script.
# Experiment_V4 JSS 2nd submissiondaki major revisondan onceki analizleri icermekte.
# Bu script halihazirda ayni author isimleri eslestirilmis commit verileri uzerinde 
# author info analizi, author selection, author name masking yapiyor.

# 14.02.2021
# selected authors info print function is added


# mask author names in author info file and change files
# write a author info file that includes all developers' statistics about commit counts and ratios 

# Steps:
# 1. Mask author names and write an author info file with aliases
# 2. Mask author names from change files
# 3. Select authors automatically for MixTime experiment --> MixTime
# 4. Write author commit counts in timeline (3 months intervals) --> SeqTime
# 5. Select authors automatically for SeqTime experiment --> SeqTime
# 5. Step also do determination of test intervals

library(xlsx)
# library(openxlsx)

rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

writeAuthorInfoFileWithAliases <- FALSE
maskAuthorNamesInChangeFiles <- FALSE
selectAuthorsForMixTimeExperiment <- FALSE
writeAuthorCommitCountsInTimeline <- FALSE
selectAuthorsAndDetermineTestIntervalsForSeqTimeExperiment <- FALSE

minCommitCountToSelectTimeInterval <- 50 # --> for SeqTime Experiment
minBuggyCommitCountToSelectTimeInterval <- 5 # --> for SeqTime Experiment
testSetLengthsInMonths <- c(3, 6) # --> for SeqTime Experiment

maskAuthorNames <- function(inputDf, maskedColumnName){
  for(i in 1:nrow(inputDf)){
    inputDf[i, maskedColumnName] <- paste("Dev_", i, sep = "")
  }
  
  return(inputDf)
}

if(selectAuthorsAndDetermineTestIntervalsForSeqTimeExperiment)
  selectedAuthorsWb <- createWorkbook()

for(p in 1:length(projectList)){
  # load information
  projectName <- projectList[p]
  loadProjectData(projectName)
  
  commits <- get(paste(projectName, "Changes", sep = ""))
  
  # generate an author information file with masked author names
  authorInfo <- createAuthorInfoDf(commits, "AUTHOR_NAME", "CLASS", "BUG", "NO_BUG")
  maskedAuthorInfo <- maskAuthorNames(authorInfo, "Alias") #get an alias for each developer
  maskedAuthorInfo <- maskedAuthorInfo[, c(1,8,2,3,4,5,6,7)] #put alias column after dev.name column
  
  if(writeAuthorInfoFileWithAliases){
    # keep author info in csv file to keep the names of the authors correct. when we use excel, name formats are broken.
    if(p == 1){
      # write.csv(maskedAuthorInfo, paste(processingDataDir, "authorInfo", projectName, ".csv", sep = ""), row.names = FALSE)
      xlsx::write.xlsx(maskedAuthorInfo, paste(processingDataDir, "authorInfo.xlsx", sep = ""), sheetName = projectName, row.names = FALSE)
    }
    else{
      # write.csv(maskedAuthorInfo, paste(processingDataDir, "authorInfo", projectName, ".csv", sep = ""), row.names = FALSE)
      xlsx::write.xlsx(maskedAuthorInfo, paste(processingDataDir, "authorInfo.xlsx", sep = ""), sheetName = projectName, row.names = FALSE, append = TRUE)
    }
  }
  
  if(maskAuthorNamesInChangeFiles){
    #mask author names in change files
    for(i in 1:nrow(commits)){
      commits[i, "AUTHOR_MASKED"] <- getAuthorMaskedName(commits[i, "AUTHOR_NAME"], maskedAuthorInfo)
    }
    commits <- commits[, c(1,2,20,3:19)]
    
    write.csv(commits, paste(processingDataDir, projectName, ".csv", sep = ""), row.names = FALSE)  
  }
  
  if(selectAuthorsForMixTimeExperiment){
    # Rules for developer selection are:
    # 1. Determine the developers who made the 80% of the total commits, include them into the "selected developers"
    # 2. Determine the developers whose buggy commit ratio is below the value of 0.1, exclude them from the "selected developers"
    
    maskedAuthorInfo$Selected <- "F"
    maskedAuthorInfo[which(maskedAuthorInfo$CumulativeCommitRatio <= 0.8), "Selected"] <- "T"
    maskedAuthorInfo[which(maskedAuthorInfo$BuggyCommitRatio <= 0.1), "Selected"] <- "F"
    
    if(p == 1){
      xlsx::write.xlsx(maskedAuthorInfo, paste(processingDataDir, "selectedAuthorsMixTime.xlsx", sep = ""), sheetName = projectName, row.names = FALSE)
    }
    else{
      xlsx::write.xlsx(maskedAuthorInfo, paste(processingDataDir, "selectedAuthorsMixTime.xlsx", sep = ""), sheetName = projectName, row.names = FALSE, append = TRUE)
    }
    
  }
  
  if(writeAuthorCommitCountsInTimeline){
    orderedAuthorList <- rev(maskedAuthorInfo$Author)
      
    shortenedCommits <- commits[, c("HASHID", "AUTHOR_MASKED", "COMMITTER_DATE", "CLASS")]
    shortenedCommits <- shortenedCommits[rev(order(match(shortenedCommits$AUTHOR, orderedAuthorList))),]
    shortenedCommits[, "NEW_DATE"] <- 0
    class(shortenedCommits[, "NEW_DATE"]) <- "Date"
    
    for(t in 1:nrow(shortenedCommits)){
      shortenedCommits[t, "NEW_DATE"] <- returnAsDate(as.character(shortenedCommits[t, "COMMITTER_DATE"]))
      shortenedCommits[t, "AUTHOR_INFO"] <- paste(shortenedCommits[t, "AUTHOR_MASKED"], 
                                                  "(",
                                                  maskedAuthorInfo[which(maskedAuthorInfo$Alias == shortenedCommits[t, "AUTHOR_MASKED"]), "CommitCount"], 
                                                  "/",
                                                  maskedAuthorInfo[which(maskedAuthorInfo$Alias == shortenedCommits[t, "AUTHOR_MASKED"]), "BuggyCommitCount"], 
                                                  ")",
                                                  sep = "")
      shortenedCommits[t, "COMMIT_COUNT"] <- maskedAuthorInfo[which(maskedAuthorInfo$Alias == shortenedCommits[t, "AUTHOR_MASKED"]), "CommitCount"]
      shortenedCommits[t, "BUGGY_COMMIT_COUNT"] <- maskedAuthorInfo[which(maskedAuthorInfo$Alias == shortenedCommits[t, "AUTHOR_MASKED"]), "BuggyCommitCount"]
    }
      
    dateInfoTable <- data.frame(matrix(ncol = 0, nrow = 0))
    bugInfoTable <- data.frame(matrix(ncol = 0, nrow = 0))
    
    # GM info
    minDate <- min(shortenedCommits[, "NEW_DATE"])
    maxDate <- max(shortenedCommits[, "NEW_DATE"])
    
    gmDateSequence <- seq(from = minDate, to = maxDate, by = "3 month")
    gmDateSequence <- c(gmDateSequence, maxDate)
    
    dateInfoTable[nrow(dateInfoTable) + 1, 1] <- "GM"
    bugInfoTable[nrow(bugInfoTable) + 1, 1] <- "GM"
    
    for(s in 1:length(gmDateSequence)){
        dateInfoTable[, (s+1)] <- 0
        class(dateInfoTable[, (s+1)]) <- "Date"
        dateInfoTable[nrow(dateInfoTable), (s+1)] <- gmDateSequence[s]
        
        commitsInterval <- shortenedCommits[shortenedCommits$NEW_DATE >= gmDateSequence[s] & shortenedCommits$NEW_DATE <= gmDateSequence[s+1],]
        buggyCount <- length(which(commitsInterval$CLASS == "BUG"))
        nonBuggyCount <- length(which(commitsInterval$CLASS == "NO_BUG"))
        bugInfoTable[nrow(bugInfoTable), (s+1)] <- paste(buggyCount + nonBuggyCount, "/", buggyCount, sep = "")
      }
      
    # PM info
    for(a in 1:nrow(maskedAuthorInfo)){
      pmData <- shortenedCommits[which(shortenedCommits[, "AUTHOR_MASKED"] == maskedAuthorInfo[a, "Alias"]), ]
      
      minDate <- min(pmData[, "NEW_DATE"])
      maxDate <- max(pmData[, "NEW_DATE"])
      
      pmDateSequence <- seq(from = minDate, to = maxDate, by = "3 month")
      pmDateSequence <- c(pmDateSequence, maxDate)
      
      dateInfoTable[nrow(dateInfoTable) + 1, 1] <- paste("PM", maskedAuthorInfo[a, "Alias"], sep = "_")
      bugInfoTable[nrow(bugInfoTable) + 1, 1] <- paste("PM", maskedAuthorInfo[a, "Alias"], sep = "_")
      
      for(s in 1:length(pmDateSequence)){
        # dateInfoTable[, (s+1)] <- 0
        # class(dateInfoTable[, (s+1)]) <- "Date"
        dateInfoTable[nrow(dateInfoTable), (s+1)] <- pmDateSequence[s]
        
        commitsInterval <- pmData[pmData$NEW_DATE >= pmDateSequence[s] & pmData$NEW_DATE <= pmDateSequence[s+1],]
        buggyCount <- length(which(commitsInterval$CLASS == "BUG"))
        nonBuggyCount <- length(which(commitsInterval$CLASS == "NO_BUG"))
        bugInfoTable[nrow(bugInfoTable), (s+1)] <- paste(buggyCount + nonBuggyCount, "/", buggyCount, sep = "")
      }
    }
    
    if(p == 1){
      xlsx::write.xlsx(dateInfoTable, file = paste(processingDataDir, "commitDateInfo.xlsx", sep = ""), sheetName = projectName, row.names = FALSE)
      xlsx::write.xlsx(bugInfoTable, file = paste(processingDataDir, "commitDateInfo.xlsx", sep = ""), sheetName = paste(projectName, "_bugCounts", sep = ""), row.names = FALSE, append = T)
    }
    else{
      xlsx::write.xlsx(dateInfoTable, file = paste(processingDataDir, "commitDateInfo.xlsx", sep = ""), sheetName = projectName, row.names = FALSE, append = T)
      xlsx::write.xlsx(bugInfoTable, file = paste(processingDataDir, "commitDateInfo.xlsx", sep = ""), sheetName = paste(projectName, "_bugCounts", sep = ""), row.names = FALSE, append = T)
    }
  }
  
  if(selectAuthorsAndDetermineTestIntervalsForSeqTimeExperiment){
    commitBugCountsTimeBased <- openxlsx::read.xlsx(xlsxFile = paste(finalDataDir, "commitDateInfo.xlsx", sep = ""), sheet = paste(projectName, "_bugCounts", sep = ""))
    selectedAuthors <- maskedAuthorInfo
    
    for(d in 2:nrow(commitBugCountsTimeBased)){
      developerAlias <- gsub(x = commitBugCountsTimeBased[d, 1], pattern = "PM_", replacement = "", fixed = TRUE)
      
      for(tm in testSetLengthsInMonths){
        howManyInterval <- tm / 3 
        
        selectedAuthors[which(selectedAuthors$Alias == developerAlias), paste("Selected_", tm, "Months", sep = "")] <- "F"
        
        developerTrainCommitCount <- 0
        developerTrainBuggyCount <- 0
        appropriateTestIntervalsList <- list() #character(length = 0)
        
        for(t in 2:(ncol(commitBugCountsTimeBased) - howManyInterval)){
          timeIntervalValues <- unlist(strsplit(x = as.character(commitBugCountsTimeBased[d, t]), split = "/", fixed = TRUE))
          developerTrainCommitCount <- developerTrainCommitCount + as.numeric(timeIntervalValues[1])
          developerTrainBuggyCount <- developerTrainBuggyCount + as.numeric(timeIntervalValues[2])
          
          developerTestCommitCount <- 0
          developerTestBuggyCount <- 0
          candidateTestIntervals <- character(length = 0)
          
          for(hmi in 1:howManyInterval){
            timeIntervalValues <- unlist(strsplit(x = as.character(commitBugCountsTimeBased[d, (t + hmi)]), split = "/", fixed = TRUE))
            developerTestCommitCount <- developerTestCommitCount + as.numeric(timeIntervalValues[1])
            developerTestBuggyCount <- developerTestBuggyCount + as.numeric(timeIntervalValues[2])
            candidateTestIntervals <- c(candidateTestIntervals, (t - 1) + hmi)
          }
          
          # check if the test and trains sets are appropriate for building models
          if(!is.na(developerTrainCommitCount) & 
             !is.na(developerTrainBuggyCount) & 
             !is.na(developerTestCommitCount) & 
             !is.na(developerTestBuggyCount)){
            
            if(developerTrainCommitCount >= minCommitCountToSelectTimeInterval & 
               developerTrainBuggyCount >= minBuggyCommitCountToSelectTimeInterval &
               developerTestCommitCount >= minCommitCountToSelectTimeInterval & 
               developerTestBuggyCount >= minBuggyCommitCountToSelectTimeInterval){
              
              appropriateTestIntervalsList[[length(appropriateTestIntervalsList) + 1]] <- candidateTestIntervals
              
              # cat("candidateTestIntervals", candidateTestIntervals)
              # cat("appropriateTestIntervalsList", appropriateTestIntervalsList)
              
              selectedAuthors[which(selectedAuthors$Alias == developerAlias), paste("Selected_", tm, "Months", sep = "")] <- "T"
              
              # break
            }
          }
        }
        # create test intervals string and add to selectedAuthors excel
        if(length(appropriateTestIntervalsList) > 0){
          testIntervals <- collapseListOfVectors(appropriateTestIntervalsList, "&", "-")
          selectedAuthors[which(selectedAuthors$Alias == developerAlias), paste("TestIntervals_", tm, "Months", sep = "")] <- testIntervals
        }
      }
    }
    
    addWorksheet(wb = selectedAuthorsWb, sheetName = projectName, gridLines = FALSE)
    writeDataTable(wb = selectedAuthorsWb, sheet = p, x = selectedAuthors)
    
    # if(p == 1){
    #   write.xlsx(selectedAuthors, file = paste(processingDataDir, "selectedAuthors_TimeBased.xlsx", sep = ""), sheetName = projectName, row.names = FALSE)
    # }
    # else{
    #   write.xlsx(selectedAuthors, file = paste(processingDataDir, "selectedAuthors_TimeBased.xlsx", sep = ""), sheetName = projectName, row.names = FALSE, append = T)
    # }
    
   
  }
}

if(selectAuthorsAndDetermineTestIntervalsForSeqTimeExperiment)
  saveWorkbook(selectedAuthorsWb, paste(processingDataDir, "selectedAuthorsSeqTime.xlsx", sep = ""), overwrite = TRUE)




# added on 14.02.2021
experimentType <- "MixTime"
experimentSignature <- "MixTime"
developerSelectionVersion <- "SelectedV2"
printSelectedAuthorsInfo <- TRUE

if(printSelectedAuthorsInfo){
  for(i in c("SelectedV1", "SelectedV2")){
    selectedAuthorsInfoTable <- as.data.frame(matrix(ncol = 7, nrow = 0))
    colnames(selectedAuthorsInfoTable) <- c("Project", 
                                            "SelectedAuthorCount", "MaxTotalCommit", "MinTotalCommit", "MaxBuggyCommit", "MinTotalCommit", 
                                            "TotalAuthorCount")
    
    for(p in projectList){
      loadProjectData(projectName = p)
      loadProjectAuthorsData(experimentType, projectName = p, developerSelectionVersion = i)
      
      selectedAuthorsInfoTable[nrow(selectedAuthorsInfoTable) + 1, ] <-
        c(p, numberOfSelectedAuthors, 
          max(get(paste(p, "SelectedAuthors", sep = ""))$CommitCount),
          min(get(paste(p, "SelectedAuthors", sep = ""))$CommitCount),
          max(get(paste(p, "SelectedAuthors", sep = ""))$BuggyCommitCount),
          min(get(paste(p, "SelectedAuthors", sep = ""))$BuggyCommitCount),
          length(unique(get(paste(p, "Changes", sep = ""))$AUTHOR_MASKED)))
    }
    
    if(i == "SelectedV1")
      write.xlsx(x = selectedAuthorsInfoTable, file = paste(processingDataDir, "SelectedAuthorStatistics.xlsx", sep = ""), sheetName = i, row.names = F)
    else
      write.xlsx(x = selectedAuthorsInfoTable, file = paste(processingDataDir, "SelectedAuthorStatistics.xlsx", sep = ""), sheetName = i, row.names = F, append = T)
  }
}
