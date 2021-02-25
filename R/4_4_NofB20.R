require("xlsx")

rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime"
baseExperimentSignature <- "MixTime" #SeqTime, MixTime
samplingOption <- "WITH"
testSetMonths <- ""

setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

# outExperimentSignature <- paste(baseExperimentSignature, "_", samplingOption, sep = "")
outExperimentSignature <- paste(baseExperimentSignature, sep = "")

loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)

calculateNofPofForDevs <- 1
calculateNofPofAll <- 1

if(calculateNofPofForDevs == 1){
  for(algorithmChoice in algorithms){
    if(algorithmChoice == algorithms[1])
      outExperimentSignature <- paste(outExperimentSignature, algorithmChoice, sep = "_")
    else
      outExperimentSignature <- paste(gsub(x = outExperimentSignature, pattern = "\\_[^\\_]*$", replacement = ""), algorithmChoice, sep = "_")
    
    loadModelBuildingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)
    
    for(p in 3:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectAllCommits <- get(paste(projectName, "Changes", sep = ""))
      
      # if(projectName == "Perl")
      #   churnColumnName <- "X.CHURN."
      # else
        churnColumnName <- "CHURN"
      
      for(a in 1:numberOfSelectedDev){
        authorName <- authorNames[a]
        
        # NofB20_PofB20_Results <- data.frame(matrix(ncol = 11, nrow = numberOfRuns))
        # colnames(NofB20_PofB20_Results) <- c("PM_NofB20", "SM_NofB20", "GM_NofB20", "WPM_NofB20", "Meta_NofB20", 
        #                                 "PM_PofB20", "SM_PofB20", "GM_PofB20", "WPM_PofB20", "Meta_PofB20", 
        #                                 "TotalBugs")
        
        NofB20_PofB20_Results <- data.frame(matrix(ncol = 7, nrow = numberOfRuns))
        colnames(NofB20_PofB20_Results) <- c("PM_NofB20", "SM_NofB20", "GM_NofB20",
                                             "PM_PofB20", "SM_PofB20", "GM_PofB20", 
                                             "TotalBugs")
        
        for(r in 1:numberOfRuns){
          GMPredictions <- data.frame()
          SMPredictions <- data.frame()
          PMPredictions <- data.frame()
          # WPMPredictions <- data.frame()
          # MetaPredictions <- data.frame()
          testSet <- data.frame()
          
          #RF prediction results saved cumulatively by mistake (fold10 contains all folds predictions, for ex. fold9 contains 1 to 9)
          if(algorithmChoice == "RF"){ 
            f <- numberOfFolds
            PMPredictions <- rbind(PMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            SMPredictions <- rbind(SMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            GMPredictions <- rbind(GMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            # WPMPredictions <- rbind(WPMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
          }
          else{
            for(f in 1:numberOfFolds) {
              PMPredictions <- rbind(PMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              SMPredictions <- rbind(SMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              GMPredictions <- rbind(GMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              # WPMPredictions <- rbind(WPMPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            }
          }
          
          #I saved predictions of the Meta model into a single PredMeta file (do not need a loop for folds)
          # MetaPredictions <- rbind(MetaPredictions, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredMeta.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
          
          # trainTestDataDir <- paste("/Volumes/BeyzaEken/Projects/DP_OS/data/final/MixTime_TrainTest/", samplingOption, "/", sep = "")
          
          for(f in 1:numberOfFolds){
            testSetCommitIds <- read.csv(paste(trainTestDataDir, projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
            testSet <- rbind(testSet, projectAllCommits[projectAllCommits$HASHID %in% testSetCommitIds[, 1], ])
          }
            
            
          
          totalChurnTest <- sum(testSet[, churnColumnName])
          twentyPercOftotalChurn <- totalChurnTest * 0.2
          totalBugsInTest <- length(which(testSet$CLASS == "BUG"))
          
          testSetAllPredictions <- cbind(testSet,
                                         PM_BugProb = PMPredictions[, "BUG"],
                                         SM_BugProb = SMPredictions[, "BUG"],
                                         GM_BugProb = GMPredictions[, "BUG"])
                                         # WPM_BugProb = WPMPredictions[, "BUG"],
                                         # Meta_BugProb = MetaPredictions[, "BUG"]
          
          testSetAllPredictions <- testSetAllPredictions[rev(order(testSetAllPredictions[, "PM_BugProb"])), ]
          instanceChurn <- 0
          lastCommit_PM <- 0
          while(instanceChurn < twentyPercOftotalChurn){
            instanceChurn <- as.numeric(instanceChurn + testSetAllPredictions[lastCommit_PM + 1, churnColumnName])
            lastCommit_PM <- lastCommit_PM + 1
          }
          twentyPercentTestSet_PM <- testSetAllPredictions[1:lastCommit_PM, ]
          
          testSetAllPredictions <- testSetAllPredictions[rev(order(testSetAllPredictions[, "SM_BugProb"])), ]
          instanceChurn <- 0
          lastCommit_SM <- 0
          while(instanceChurn < twentyPercOftotalChurn){
            instanceChurn <- as.numeric(instanceChurn + testSetAllPredictions[lastCommit_SM + 1, churnColumnName])
            lastCommit_SM <- lastCommit_SM + 1
          }
          twentyPercentTestSet_SM <- testSetAllPredictions[1:lastCommit_SM, ]
          
          testSetAllPredictions <- testSetAllPredictions[rev(order(testSetAllPredictions[, "GM_BugProb"])), ]
          instanceChurn <- 0
          lastCommit_GM <- 0
          while(instanceChurn < twentyPercOftotalChurn){
            instanceChurn <- as.numeric(instanceChurn + testSetAllPredictions[lastCommit_GM + 1, churnColumnName])
            lastCommit_GM <- lastCommit_GM + 1
          }
          twentyPercentTestSet_GM <- testSetAllPredictions[1:lastCommit_GM, ]
          
          # testSetAllPredictions <- testSetAllPredictions[rev(order(testSetAllPredictions[, "WPM_BugProb"])), ]
          # instanceChurn <- 0
          # lastCommit_WPM <- 0
          # while(instanceChurn < twentyPercOftotalChurn){
          #   instanceChurn <- as.numeric(instanceChurn + testSetAllPredictions[lastCommit_WPM + 1, churnColumnName])
          #   lastCommit_WPM <- lastCommit_WPM + 1
          # }
          # twentyPercentTestSet_WPM <- testSetAllPredictions[1:lastCommit_WPM, ]
          # 
          # testSetAllPredictions <- testSetAllPredictions[rev(order(testSetAllPredictions[, "Meta_BugProb"])), ]
          # instanceChurn <- 0
          # lastCommit_Meta <- 0
          # while(instanceChurn < twentyPercOftotalChurn){
          #   instanceChurn <- as.numeric(instanceChurn + testSetAllPredictions[lastCommit_Meta + 1, churnColumnName])
          #   lastCommit_Meta <- lastCommit_Meta + 1
          # }
          # twentyPercentTestSet_Meta <- testSetAllPredictions[1:lastCommit_Meta, ]

          NofB20_PM = length(which(twentyPercentTestSet_PM$CLASS == "BUG" & twentyPercentTestSet_PM$PM_BugProb >= 0.5))
          NofB20_SM = length(which(twentyPercentTestSet_SM$CLASS == "BUG" & twentyPercentTestSet_SM$PM_BugProb >= 0.5))
          NofB20_GM = length(which(twentyPercentTestSet_GM$CLASS == "BUG" & twentyPercentTestSet_GM$PM_BugProb >= 0.5))
          # NofB20_WPM = length(which(twentyPercentTestSet_WPM$CLASS == "BUG" & twentyPercentTestSet_WPM$PM_BugProb >= 0.5))
          # NofB20_Meta = length(which(twentyPercentTestSet_Meta$CLASS == "BUG" & twentyPercentTestSet_Meta$PM_BugProb >= 0.5))
          
          PofB20_PM = NofB20_PM / totalBugsInTest * 100
          PofB20_SM = NofB20_SM / totalBugsInTest * 100
          PofB20_GM = NofB20_GM / totalBugsInTest * 100
          # PofB20_WPM = NofB20_WPM / totalBugsInTest * 100
          # PofB20_Meta = NofB20_Meta / totalBugsInTest * 100
          
          
          NofB20_PofB20_Results[r, "PM_NofB20"] <- NofB20_PM
          NofB20_PofB20_Results[r, "SM_NofB20"] <- NofB20_SM
          NofB20_PofB20_Results[r, "GM_NofB20"] <- NofB20_GM
          # NofB20_PofB20_Results[r, "WPM_NofB20"] <- NofB20_WPM
          # NofB20_PofB20_Results[r, "Meta_NofB20"] <- NofB20_Meta
          
          NofB20_PofB20_Results[r, "PM_PofB20"] <- PofB20_PM
          NofB20_PofB20_Results[r, "SM_PofB20"] <- PofB20_SM
          NofB20_PofB20_Results[r, "GM_PofB20"] <- PofB20_GM
          # NofB20_PofB20_Results[r, "WPM_PofB20"] <- PofB20_WPM
          # NofB20_PofB20_Results[r, "Meta_PofB20"] <- PofB20_Meta
          
          NofB20_PofB20_Results[r, "TotalBugs"] <- totalBugsInTest
        }
        
        write.csv(NofB20_PofB20_Results, paste(modelOutputDir, projectName, "/results/", authorNames[a], "ResultsNofB20.csv", sep = ""), row.names = FALSE)
      }
    }
      
  }
}

if(calculateNofPofAll == 1){
  # modelTypes <- c("PM", "SM", "GM", "WPM", "Meta")
  selectedDevCounts <- c(27, 3, 50, 21, 54, 6)
  # selectedDevCounts <- c(22, 2, 30, 4, 54, 3)
  outExperimentSignature <- paste(baseExperimentSignature, "_", samplingOption, sep = "")
  loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)
  dir.create(paste(plotOutputDir, "NofB20", sep = ""), recursive = T, showWarnings = F)
  
  for(algorithmChoice in algorithms){
    # if(algorithmChoice == algorithms[1])
    #   outExperimentSignature <- paste(outExperimentSignature, algorithmChoice, sep = "_")
    # else
    #   outExperimentSignature <- paste(gsub(x = outExperimentSignature, pattern = "\\_[^\\_]*$", replacement = ""), algorithmChoice, sep = "_")

    # loadModelBuildingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)
    
      
  
    # allDevsResults <- data.frame(matrix(nrow = sum(selectedDevCounts), ncol = length(modelTypes) * 2 + 1))
    # colnames(allDevsResults) <- c("PM_NofB20", "SM_NofB20", "GM_NofB20", "WPM_NofB20", "Meta_NofB20", 
    #                               "PM_PofB20", "SM_PofB20", "GM_PofB20", "WPM_PofB20", "Meta_PofB20", 
    #                               "TotalBugs")
    colnames(allDevsResults) <- c("PM_NofB20", "SM_NofB20", "GM_NofB20",
                                  "PM_PofB20", "SM_PofB20", "GM_PofB20",
                                  "TotalBugs")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
      
      projectResults <- data.frame(matrix(nrow = sum(numberOfSelectedDev), ncol = length(modelTypes) * 2 + 1))
      # colnames(projectResults) <- c("PM_NofB20", "SM_NofB20", "GM_NofB20", "WPM_NofB20", "Meta_NofB20", 
      #                               "PM_PofB20", "SM_PofB20", "GM_PofB20", "WPM_PofB20", "Meta_PofB20", 
      #                               "TotalBugs")
      colnames(projectResults) <- c("PM_NofB20", "SM_NofB20", "GM_NofB20",
                                    "PM_PofB20", "SM_PofB20", "GM_PofB20",
                                    "TotalBugs")
      
      for(a in 1:numberOfSelectedDev){
        authorName <- authorNames[a]
        # devResults <- read.xlsx(paste(outputDir, projectName, "/results/", authorNames[a], "ResultsNofB20.xlsx", sep = ""), sheetIndex = 1)
        devResults <- read.csv(paste(get(paste(algorithmChoice, "_Dir", sep = "")), projectName, "/results/", authorNames[a], "ResultsNofB20.csv", sep = ""))
        
        for(m in 1:(length(modelTypes) * 2)){
          projectResults[a, m] <- median(devResults[, m])
          allDevsResults[sum(selectedDevCounts[1:p]) - selectedDevCounts[p] + a, m] <- median(devResults[, m])
        }
        allDevsResults[sum(selectedDevCounts[1:p]) - selectedDevCounts[p] + a, ncol(allDevsResults)] <- mean(devResults[, ncol(devResults)])
        projectResults[a, ncol(projectResults)] <- mean(devResults[, ncol(devResults)])
      }
      
      finalRowNumber <- nrow(projectResults)
      # for(c in c(1:5,11)) five models
      for(c in c(1:3,7))
        projectResults[finalRowNumber + 1, c] <- sum(projectResults[1:(finalRowNumber), c])
      # for(c in 6:10){
      for(c in 4:6){
        projectResults[finalRowNumber + 1, c] <- sum(median(projectResults[1:(finalRowNumber), c]))
        projectResults[finalRowNumber + 2, c] <- sum(mean(projectResults[1:(finalRowNumber), c]))
      }
      write.xlsx(projectResults, paste(plotOutputDir, "NofB20/", projectName, "_NofB20_", algorithmChoice, ".xlsx", sep = ""))
    }
    
    finalRowNumber <- nrow(allDevsResults)
    # for(c in c(1:5,11)) five models
    for(c in c(1:3,7))
      allDevsResults[finalRowNumber + 1, c] <- sum(allDevsResults[1:(finalRowNumber), c])
    # for(c in 6:10){
    for(c in 4:6){
      allDevsResults[finalRowNumber + 1, c] <- sum(median(allDevsResults[1:(finalRowNumber), c]))
      allDevsResults[finalRowNumber + 2, c] <- sum(mean(allDevsResults[1:(finalRowNumber), c]))
    }
    
    write.xlsx(allDevsResults, paste(plotOutputDir, "NofB20/", "NofB20_", algorithmChoice, ".xlsx", sep = ""))
  }
}

