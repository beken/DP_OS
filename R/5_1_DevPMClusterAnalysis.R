library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(xlsx)
# library(openxlsx)
library(stats)
library(PMCMR)
library(vegan)
library(effsize)
library(ScottKnottESD)
library(RColorBrewer)
library(plot.matrix)
library(RColorBrewer)
library(reshape2)
library(Hmisc)
library(NMF)

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime"
baseExperimentSignature <- "MixTime_UnderSamp" 
samplingOption <- "WO"
testSetMonths <- ""

# outExperimentSignature <- paste(baseExperimentSignature, "_DevClusters_TwoParameters", sep = "")
# outExperimentSignature <- paste(baseExperimentSignature, "_DevGroupBasedInfoGainAnalysis", sep = "")
outExperimentSignature <- paste(baseExperimentSignature, "_DevGroupAnalysis", sep = "")

setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)

cluster.by.commitStatistics <- FALSE
cluster.by.commitStatistics.and.project <- FALSE
cluster.by.metricStatistics <- FALSE
dunnTest <- FALSE

clusterNumbers <- c(3,5,7,10)


if(experimentType == "MixTime"){
  if(cluster.by.commitStatistics){
    ### read all developers' PM SM GM WPM PM+ performances into a single dataframe ###########
    allResultsNB <- data.frame()
    allResultsRF <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      
      if(p == 1){
        allResultsNB <- rbind(allResultsNB, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_NB.xlsx", sep = ""), sheet = p, colNames = T)))
        
        allResultsRF <- rbind(allResultsRF, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_RF.xlsx", sep = ""), sheet = p, colNames = T)))
      }else{
        allResultsNB <- rbind(allResultsNB, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_NB.xlsx", sep = ""), sheet = p)))
        allResultsRF <- rbind(allResultsRF, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_RF.xlsx", sep = ""), sheet = p)))
        
      }
    }
    #####################################################################################
    
    for(cln in clusterNumbers){
      totalCommits <- allResultsNB[which(allResultsNB[, "Model"] == "PM"), "TotalCommit"]
      buggyRatios <- allResultsNB[which(allResultsNB[, "Model"] == "PM"), "BuggyRatio"]
      allData <- cbind(totalCommits, buggyRatios)
      
      model <- kmeans(allData, centers = cln, nstart = 25, iter.max = 50)
      model$size
      model$cluster
      
      allData <- cbind(allData, model$cluster)
      
      for(alg in algorithms){
        allResults <- get(paste("allResults", alg, sep = ""))
        allResults[, "Cluster"] <- rep(model$cluster, 1, each = 5)
        
        openxlsx::write.xlsx(x = allResults,
                             file = paste(plotOutputDir, "Results_", cln, "Clusters_", alg, "_WO.xlsx", sep = ""),
                             row.names = FALSE)
        
        medianTable <- meanTable <- as.data.frame(matrix(ncol = 5 * 7 + 4, nrow = cln + 2))
        medianTable[3:(cln + 2), 1] <- meanTable[3:(cln + 2), 1] <- paste("Cluster", 1:cln, sep = "")
        medianTable[2, 2] <- meanTable[2, 2] <- c("AuthorCount")
        medianTable[2, 3] <- meanTable[2, 3] <- c("BuggyRatio")
        medianTable[2, 4] <- meanTable[2, 4] <- c("TotalCommit")
        medianTable[1, c(5,12,19,24,31)] <- meanTable[1, c(5,12,19,24,31)] <- c("PM", "SM", "GM", "WPM", "PM+")
        medianTable[2, c(5:39)] <- meanTable[2, c(5:39)] <- metricList
        
        for(i in 1:cln){
          clusterPMData <- allResults[which(allResults[, "Cluster"] == i
                                            & allResults[, "Model"] == "PM"), ]
          clusterSMData <- allResults[which(allResults[, "Cluster"] == i 
                                            & allResults[, "Model"] == "SM"), ]
          clusterGMData <- allResults[which(allResults[, "Cluster"] == i 
                                            & allResults[, "Model"] == "GM"), ]
          clusterWPMData <- allResults[which(allResults[, "Cluster"] == i 
                                             & allResults[, "Model"] == "WPM"), ]
          clusterMetaData <- allResults[which(allResults[, "Cluster"] == i 
                                              & allResults[, "Model"] == "PM+"), ]
          
          medianTable[i + 2, 2] <- meanTable[i + 2, 2] <- nrow(clusterPMData)
          meanTable[i + 2, 3] <- round(mean(clusterPMData[, "BuggyRatio"], na.rm = T), 3)
          meanTable[i + 2, 4] <- round(mean(clusterPMData[, "TotalCommit"], na.rm = T), 3)
          medianTable[i + 2, 3] <- round(median(clusterPMData[, "BuggyRatio"], na.rm = T), 3)
          medianTable[i + 2, 4] <- round(median(clusterPMData[, "TotalCommit"], na.rm = T), 3)
          
          for(m in 1:length(metricList)){
            meanPM <- mean(clusterPMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanSM <- mean(clusterSMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanGM <- mean(clusterGMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanWPM <- mean(clusterWPMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanMeta <- mean(clusterMetaData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            
            medianPM <- median(clusterPMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianSM <- median(clusterSMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianGM <- median(clusterGMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianWPM <- median(clusterWPMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianMeta <- median(clusterMetaData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            
            meanTable[i + 2, 4 + m] <- round(meanPM, 3)
            meanTable[i + 2, 11 + m] <- round(meanSM, 3)
            meanTable[i + 2, 18 + m] <- round(meanGM, 3)
            meanTable[i + 2, 25 + m] <- round(meanWPM, 3)
            meanTable[i + 2, 32 + m] <- round(meanMeta, 3)
            
            medianTable[i + 2, 4 + m] <- round(medianPM, 3)
            medianTable[i + 2, 11 + m] <- round(medianSM, 3)
            medianTable[i + 2, 18 + m] <- round(medianGM, 3)
            medianTable[i + 2, 25 + m] <- round(medianWPM, 3)
            medianTable[i + 2, 32 + m] <- round(medianMeta, 3)
          }
        }
        
        xlsx::write.xlsx(x = meanTable, file = paste(plotOutputDir, "ResultsClusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetName = paste(cln, "C-mean", sep = ""), row.names = F, col.names = F, append = T)
        xlsx::write.xlsx(x = medianTable, file = paste(plotOutputDir, "ResultsClusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetName = paste(cln, "C-median", sep = ""), row.names = F, col.names = F, append = T)
      }
    }
  }
  
  if(cluster.by.commitStatistics.and.project){
    ### read all developers' PM SM GM WPM PM+ performances into a single dataframe ###########
    allResultsNB <- data.frame()
    allResultsRF <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      
      if(p == 1){
        allResultsNB <- rbind(allResultsNB, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_NB.xlsx", sep = ""), sheet = p, colNames = T)))
        
        allResultsRF <- rbind(allResultsRF, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_RF.xlsx", sep = ""), sheet = p, colNames = T)))
      }else{
        allResultsNB <- rbind(allResultsNB, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_NB.xlsx", sep = ""), sheet = p)))
        allResultsRF <- rbind(allResultsRF, 
                              cbind(projectName, 
                                    openxlsx::read.xlsx(xlsxFile = paste(resultDataDir, "Results_", baseExperimentSignature, "_", samplingOption, "_RF.xlsx", sep = ""), sheet = p)))
        
      }
    }
    #####################################################################################
    
    for(cln in clusterNumbers){
      totalCommits <- allResultsNB[which(allResultsNB[, "Model"] == "PM"), "TotalCommit"]
      buggyRatios <- allResultsNB[which(allResultsNB[, "Model"] == "PM"), "BuggyRatio"]
      projectNames <- as.numeric(allResultsNB[which(allResultsNB[, "Model"] == "PM"), "projectName"])
      allData <- cbind(totalCommits, buggyRatios, projectNames)
      
      model <- kmeans(allData, centers = cln, nstart = 25, iter.max = 50)
      model$size
      model$cluster
      
      allData <- cbind(allData, model$cluster)
      
      for(alg in algorithms){
        allResults <- get(paste("allResults", alg, sep = ""))
        allResults[, "Cluster"] <- rep(model$cluster, 1, each = 5)
        
        openxlsx::write.xlsx(x = allResults,
                             file = paste(plotOutputDir, "Results_", cln, "Clusters_", alg, "_WO.xlsx", sep = ""),
                             row.names = FALSE)
        
        medianTable <- meanTable <- as.data.frame(matrix(ncol = 5 * 7 + 4, nrow = cln + 2))
        medianTable[3:(cln + 2), 1] <- meanTable[3:(cln + 2), 1] <- paste("Cluster", 1:cln, sep = "")
        medianTable[2, 2] <- meanTable[2, 2] <- c("AuthorCount")
        medianTable[2, 3] <- meanTable[2, 3] <- c("BuggyRatio")
        medianTable[2, 4] <- meanTable[2, 4] <- c("TotalCommit")
        medianTable[1, c(5,12,19,24,31)] <- meanTable[1, c(5,12,19,24,31)] <- c("PM", "SM", "GM", "WPM", "PM+")
        medianTable[2, c(5:39)] <- meanTable[2, c(5:39)] <- metricList
        
        for(i in 1:cln){
          clusterPMData <- allResults[which(allResults[, "Cluster"] == i
                                            & allResults[, "Model"] == "PM"), ]
          clusterSMData <- allResults[which(allResults[, "Cluster"] == i 
                                            & allResults[, "Model"] == "SM"), ]
          clusterGMData <- allResults[which(allResults[, "Cluster"] == i 
                                            & allResults[, "Model"] == "GM"), ]
          clusterWPMData <- allResults[which(allResults[, "Cluster"] == i 
                                             & allResults[, "Model"] == "WPM"), ]
          clusterMetaData <- allResults[which(allResults[, "Cluster"] == i 
                                              & allResults[, "Model"] == "PM+"), ]
          
          medianTable[i + 2, 2] <- meanTable[i + 2, 2] <- nrow(clusterPMData)
          meanTable[i + 2, 3] <- round(mean(clusterPMData[, "BuggyRatio"], na.rm = T), 3)
          meanTable[i + 2, 4] <- round(mean(clusterPMData[, "TotalCommit"], na.rm = T), 3)
          medianTable[i + 2, 3] <- round(median(clusterPMData[, "BuggyRatio"], na.rm = T), 3)
          medianTable[i + 2, 4] <- round(median(clusterPMData[, "TotalCommit"], na.rm = T), 3)
          
          for(m in 1:length(metricList)){
            meanPM <- mean(clusterPMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanSM <- mean(clusterSMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanGM <- mean(clusterGMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanWPM <- mean(clusterWPMData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            meanMeta <- mean(clusterMetaData[, paste("mean-", metricList[m], sep = "")], na.rm = T)
            
            medianPM <- median(clusterPMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianSM <- median(clusterSMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianGM <- median(clusterGMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianWPM <- median(clusterWPMData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            medianMeta <- median(clusterMetaData[, paste("median-", metricList[m], sep = "")], na.rm = T)
            
            meanTable[i + 2, 4 + m] <- round(meanPM, 3)
            meanTable[i + 2, 11 + m] <- round(meanSM, 3)
            meanTable[i + 2, 18 + m] <- round(meanGM, 3)
            meanTable[i + 2, 25 + m] <- round(meanWPM, 3)
            meanTable[i + 2, 32 + m] <- round(meanMeta, 3)
            
            medianTable[i + 2, 4 + m] <- round(medianPM, 3)
            medianTable[i + 2, 11 + m] <- round(medianSM, 3)
            medianTable[i + 2, 18 + m] <- round(medianGM, 3)
            medianTable[i + 2, 25 + m] <- round(medianWPM, 3)
            medianTable[i + 2, 32 + m] <- round(medianMeta, 3)
          }
        }
        
        xlsx::write.xlsx(x = meanTable, file = paste(plotOutputDir, "ResultsClusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetName = paste(cln, "C-mean", sep = ""), row.names = F, col.names = F, append = T)
        xlsx::write.xlsx(x = medianTable, file = paste(plotOutputDir, "ResultsClusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetName = paste(cln, "C-median", sep = ""), row.names = F, col.names = F, append = T)
      }
    }
  }
  
  if(cluster.by.metricStatistics){
    ### read all developers' metrics into a single dataframe ###########
    allDevelopersAllCommits <- data.frame()
    allDevelopersAggregatedCommits <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      tmpCommits <- get(paste(projectName, "Changes", sep = ""))
      tmpSelectedAuthors <- get(paste(projectName, "SelectedAuthors", sep = ""))
      
      tmpCommits <- tmpCommits[tmpCommits[, "AUTHOR_NAME"] %in% tmpSelectedAuthors[, "Author"], ]
      tmpCommits[, "PROJECT"] <- projectName
      
      allDevelopersAllCommits <- rbind(allDevelopersAllCommits, tmpCommits)
      
      for(a in 1:length(authorNames)){
        authorCommits <- tmpCommits[which(tmpCommits[, "AUTHOR_NAME"] == authorNames[a]), ]
        
        # authorStatistics <- colSums(authorCommits[, processMetrics])
        authorStatistics <- colMeans(authorCommits[, processMetrics])
        
        allDevelopersAggregatedCommits[nrow(allDevelopersAggregatedCommits) + 1, "Project"] <- projectName 
        allDevelopersAggregatedCommits[nrow(allDevelopersAggregatedCommits), "Author"] <- authorNames[a]
        allDevelopersAggregatedCommits[nrow(allDevelopersAggregatedCommits), processMetrics] <- authorStatistics
      }
    }
    ############################################################
    
    for(cln in clusterNumbers){
      # totalCommits <- allResultsNB[which(allResultsNB[, "Model"] == "PM"), "TotalCommit"]
      # buggyRatios <- allResultsNB[which(allResultsNB[, "Model"] == "PM"), "BuggyRatio"]
      # projectNames <- as.numeric(allResultsNB[which(allResultsNB[, "Model"] == "PM"), "projectName"])
      # allData <- cbind(totalCommits, buggyRatios, projectNames)

      model <- kmeans(allDevelopersAggregatedCommits[, processMetrics], centers = cln, nstart = 25, iter.max = 50)
      # model <- kmeans(allDevelopersAggregatedCommits[, c("EXP")], centers = 3, nstart = 25, iter.max = 50)
      model$size
      model$cluster
      
      allDevelopersAggregatedCommits <- cbind(allDevelopersAggregatedCommits, model$cluster)
      colnames(allDevelopersAggregatedCommits)[ncol(allDevelopersAggregatedCommits)] <- paste("cln", cln, sep = "_")
    }
    
    developerGroups.NB.PMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups.RF.PMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups.NB.PMvsSMGM <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")

    finalData <- cbind(allDevelopersAggregatedCommits, developerGroups.NB.PMvsGM[, c("pd", "pf", "F1")])
    colnames(finalData)[(ncol(finalData) - 2): ncol(finalData)] <- paste("NB.PMvsGM", rep(c("pd", "pf", "F1")))

    finalData <- cbind(finalData, developerGroups.RF.PMvsGM[, c("pd", "pf", "F1")])
    colnames(finalData)[(ncol(finalData) - 2): ncol(finalData)] <- paste("RF.PMvsGM", rep(c("pd", "pf", "F1")))
    
    finalData <- cbind(finalData, developerGroups.NB.PMvsSMGM[, c("pd", "pf", "F1")])
    colnames(finalData)[(ncol(finalData) - 2): ncol(finalData)] <- paste("NB.PMvsSMGM", rep(c("pd", "pf", "F1")))
    
    xlsx::write.xlsx(x = finalData, file = paste(plotOutputDir, "developerKmeansClustersAndGroups.xlsx", sep = ""), sheetName = "clusters", col.names = T, row.names = F)
  }
  
  if(dunnTest){
    cln <- 10
    alg <- "RF"
    resultsToBeTested <- xlsx::read.xlsx(file = paste(plotOutputDir, "Results_", cln, "Clusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetIndex = 1, header = TRUE)
    
    for(m in metricList){
      
      clusterResultsList <- list()
      
      for(i in 1:cln){
        clusterAuthors <- resultsToBeTested[which(resultsToBeTested$Cluster == i & resultsToBeTested$Model == "PM"), "Author"]
        clusterProjects <- resultsToBeTested[which(resultsToBeTested$Cluster == i & resultsToBeTested$Model == "PM"), "projectName"]
        
        columnResults <- numeric(length = 0)
        
        for(a in 1:length(clusterAuthors)){
          personalResults <- 
            getAuthorResults(inputDir = paste(outputDir, baseExperimentSignature, "_", alg, "/", samplingOption, "/", sep = ""), 
                           projectName = clusterProjects[a],
                           authorName = clusterAuthors[a],
                           modelName = "PM")
          
          columnResults <- c(columnResults, personalResults[, m])
        }
        clusterResultsList[[length(clusterResultsList) + 1]] <- columnResults
      }  
      
      denemeCount <- unlist(clusterResultsList)
      denemeSpray <- c(rep("C1", length(clusterResultsList[[1]])),
                       rep("C2", length(clusterResultsList[[2]])),
                       rep("C3", length(clusterResultsList[[3]])),
                       rep("C4", length(clusterResultsList[[4]])),
                       rep("C5", length(clusterResultsList[[5]])),
                       rep("C6", length(clusterResultsList[[6]])),
                       rep("C7", length(clusterResultsList[[7]])),
                       rep("C8", length(clusterResultsList[[8]])),
                       rep("C9", length(clusterResultsList[[9]])),
                       rep("C10", length(clusterResultsList[[10]]))
                       )
        
      denemeSpray <- as.factor(denemeSpray)
      
      sonuc <- posthoc.kruskal.dunn.test(denemeCount, denemeSpray)
      posthoc.kruskal.dunn.test(count ~ spray, data = InsectSprays, p.adjust="bonf")
      
      dunn.test.control(denemeCount, denemeSpray, p.adjust.method = "bonf")
      
      if(m == "pd")
        xlsx::write.xlsx(x = sonuc$p.value, file = paste(plotOutputDir, "Dunn_", cln, "Clusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetName = m, col.names = T, row.names = T)
      else
        xlsx::write.xlsx(x = sonuc$p.value, file = paste(plotOutputDir, "Dunn_", cln, "Clusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetName = m, col.names = T, row.names = T, append = T)
    }
    
 
    
    
    
  }
  
  # group developers (PM vs GM) ####
  if(groupDevelopersByPMvsGM){
    for(alg in algorithms){
      # create developer groups ####
      developerGroupsPMvsGM <- data.frame(matrix(ncol = 2 + length(metricList), nrow = 0))
      colnames(developerGroupsPMvsGM) <- c("Project", "Developer", metricList)
      
      for(p in 1:length(projectList)){
        projectName <- projectList[p]
        loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
        
        for(a in 1:length(authorNames)){
          if(experimentType == "MixTime")
            authorName <- authorNames[a]
          else
            authorName <- authorAliases[a]
          
          developerGroupsPMvsGM[nrow(developerGroupsPMvsGM) + 1, "Project"] <- projectName
          developerGroupsPMvsGM[nrow(developerGroupsPMvsGM), "Developer"] <- authorName
          
          resultsDir <- paste(alg, "_Dir", sep = "")
          
          resultsPM <- read.csv(file = paste(get(resultsDir), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE)
          resultsGM <- read.csv(file = paste(get(resultsDir), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE)
          
          for(m in 1:length(metricList)){
            if(metricList[m] == "pf") 
              higherIsBetter <- 0
            else
              higherIsBetter <- 1
            
            resultPM <- resultsPM[, metricList[m]]
            resultGM <- resultsGM[, metricList[m]]
            
            # friedman.test(cbind(resultPM, resultGM))
            nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(resultPM, resultGM))
            
            # effectSizeResult <- cohen.d(c(resultPM, resultGM), rep(c("PM","GM"), each = length(resultPM)), hedges.correction=TRUE)
            # paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
            
            if(nemenyiResult$p.value < pVal)
              if(medianChangeNaToZero(resultPM) > medianChangeNaToZero(resultGM))
                developerGroupsPMvsGM[nrow(developerGroupsPMvsGM), metricList[m]] <- if(higherIsBetter) "PM" else "GM"
            else
              developerGroupsPMvsGM[nrow(developerGroupsPMvsGM), metricList[m]] <- if(higherIsBetter) "GM" else "PM"
            else
              developerGroupsPMvsGM[nrow(developerGroupsPMvsGM), metricList[m]] <- "-"
          }
        }
      }
      
      xlsx::write.xlsx(developerGroupsPMvsGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups", row.names = F, col.names = T, showNA = FALSE)
      #############################
      
      # count developers of each group ####
      developerCountsPMvsGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(developerCountsPMvsGM) <- c("", "PM > GM", "PM < GM", "PM = GM")
      
      for(m in 1:length(metricList)){
        developerCountsPMvsGM[m, 1] <- metricList[m]
        developerCountsPMvsGM[m, 2] <- length(which(developerGroupsPMvsGM[, metricList[m]] == "PM"))
        developerCountsPMvsGM[m, 3] <- length(which(developerGroupsPMvsGM[, metricList[m]] == "GM"))
        developerCountsPMvsGM[m, 4] <- length(which(developerGroupsPMvsGM[, metricList[m]] == "-"))
      }
      xlsx::write.xlsx(developerCountsPMvsGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "dev.counts", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      #####################################
      
      # commit counts of each group (min-mean-median-max) ####
      commitCountsPMvsGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(commitCountsPMvsGM) <- c("", "PM > GM", "PM < GM", "PM = GM")
      buggyRatiosPMvsGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(buggyRatiosPMvsGM) <- c("", "PM > GM", "PM < GM", "PM = GM")
      
      for(m in 1:length(metricList)){
        commitCountsPMvsGM[m, 1] <- metricList[m]
        buggyRatiosPMvsGM[m, 1] <- metricList[m]
        
        pmDevs <- developerGroupsPMvsGM[which(developerGroupsPMvsGM[, metricList[m]] == "PM"), ]
        gmDevs <- developerGroupsPMvsGM[which(developerGroupsPMvsGM[, metricList[m]] == "GM"), ]
        eqDevs <- developerGroupsPMvsGM[which(developerGroupsPMvsGM[, metricList[m]] == "-"), ]
        
        commitCountsOfPm <- numeric(length = 0L)
        commitCountsOfGm <- numeric(length = 0L)
        commitCountsOfEq <- numeric(length = 0L)
        
        buggyRatiosOfPm <- numeric(length = 0L)
        buggyRatiosOfGm <- numeric(length = 0L)
        buggyRatiosOfEq <- numeric(length = 0L)
        
        for(r in 1:nrow(pmDevs)){
          devName <- pmDevs[r, "Developer"]
          projectName <- pmDevs[r, "Project"]
          
          authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
          commitCountsOfPm <- c(commitCountsOfPm, authorInfoTable[authorInfoTable$Author == devName, "CommitCount"])
          buggyRatiosOfPm <- c(buggyRatiosOfPm, authorInfoTable[authorInfoTable$Author == devName, "BuggyCommitRatio"])
        }
        
        for(r in 1:nrow(gmDevs)){
          devName <- gmDevs[r, "Developer"]
          projectName <- gmDevs[r, "Project"]
          
          authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
          commitCountsOfGm <- c(commitCountsOfGm, authorInfoTable[authorInfoTable$Author == devName, "CommitCount"])
          buggyRatiosOfGm <- c(buggyRatiosOfGm, authorInfoTable[authorInfoTable$Author == devName, "BuggyCommitRatio"])
        }   
        
        for(r in 1:nrow(eqDevs)){
          devName <- eqDevs[r, "Developer"]
          projectName <- eqDevs[r, "Project"]
          
          authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
          commitCountsOfEq <- c(commitCountsOfEq, authorInfoTable[authorInfoTable$Author == devName, "CommitCount"])
          buggyRatiosOfEq <- c(buggyRatiosOfEq, authorInfoTable[authorInfoTable$Author == devName, "BuggyCommitRatio"])
        }
        
        commitCountsPMvsGM[m, 2] <- paste(round(min(commitCountsOfPm), 2), round(mean(commitCountsOfPm), 2), round(median(commitCountsOfPm), 2), round(max(commitCountsOfPm), 2), sep = " - ")
        commitCountsPMvsGM[m, 3] <- paste(round(min(commitCountsOfGm), 2), round(mean(commitCountsOfGm), 2), round(median(commitCountsOfGm), 2), round(max(commitCountsOfGm), 2), sep = " - ")
        commitCountsPMvsGM[m, 4] <- paste(round(min(commitCountsOfEq), 2), round(mean(commitCountsOfEq), 2), round(median(commitCountsOfEq), 2), round(max(commitCountsOfEq), 2), sep = " - ")
        
        buggyRatiosPMvsGM[m, 2] <- paste(round(min(buggyRatiosOfPm), 2), round(mean(buggyRatiosOfPm), 2), round(median(buggyRatiosOfPm), 2), round(max(buggyRatiosOfPm), 2), sep = " - ")
        buggyRatiosPMvsGM[m, 3] <- paste(round(min(buggyRatiosOfPm), 2), round(mean(buggyRatiosOfPm), 2), round(median(buggyRatiosOfPm), 2), round(max(buggyRatiosOfPm), 2), sep = " - ")
        buggyRatiosPMvsGM[m, 4] <- paste(round(min(buggyRatiosOfPm), 2), round(mean(buggyRatiosOfPm), 2), round(median(buggyRatiosOfPm), 2), round(max(buggyRatiosOfPm), 2), sep = " - ")
      }
      xlsx::write.xlsx(commitCountsPMvsGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "commit.counts", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      xlsx::write.xlsx(buggyRatiosPMvsGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "buggy.ratios", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      #######################################################
      
      # prediction performances of each group ####
      predictionPerformancePMvsGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(predictionPerformancePMvsGM) <- c("", "PM > GM", "PM < GM", "PM = GM")
      
      resultsDir <- paste(alg, "_Dir", sep = "")
      
      for(m in 1:length(metricList)){
        predictionPerformancePMvsGM[m, 1] <- metricList[m]
        
        pmDevs <- developerGroupsPMvsGM[which(developerGroupsPMvsGM[, metricList[m]] == "PM"), ]
        gmDevs <- developerGroupsPMvsGM[which(developerGroupsPMvsGM[, metricList[m]] == "GM"), ]
        eqDevs <- developerGroupsPMvsGM[which(developerGroupsPMvsGM[, metricList[m]] == "-"), ]
        
        performancesOfPm <- numeric(length = 0L)
        performancesOfGm <- numeric(length = 0L)
        performancesOfEq <- numeric(length = 0L)
        
        for(r in 1:nrow(pmDevs)){
          devName <- pmDevs[r, "Developer"]
          projectName <- pmDevs[r, "Project"]
          
          performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), projectName, "/results/", devName, "ResultsPM.csv", sep = ""), header = TRUE)
          performancesOfPm <- c(performancesOfPm, performanceResultsOfAuthor[, metricList[m]])
        }
        
        for(r in 1:nrow(gmDevs)){
          devName <- gmDevs[r, "Developer"]
          projectName <- gmDevs[r, "Project"]
          
          performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), projectName, "/results/", devName, "ResultsPM.csv", sep = ""), header = TRUE)
          performancesOfGm <- c(performancesOfGm, performanceResultsOfAuthor[, metricList[m]])
        }
        
        for(r in 1:nrow(eqDevs)){
          devName <- eqDevs[r, "Developer"]
          projectName <- eqDevs[r, "Project"]
          
          performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), projectName, "/results/", devName, "ResultsPM.csv", sep = ""), header = TRUE)
          performancesOfEq <- c(performancesOfEq, performanceResultsOfAuthor[, metricList[m]])
        }
        
        predictionPerformancePMvsGM[m, 2] <- paste(round(min(performancesOfPm, na.rm = T), 2), round(mean(performancesOfPm, na.rm = T), 2), round(median(performancesOfPm, na.rm = T), 2), round(max(performancesOfPm, na.rm = T), 2), sep = " - ")
        predictionPerformancePMvsGM[m, 3] <- paste(round(min(performancesOfGm, na.rm = T), 2), round(mean(performancesOfGm, na.rm = T), 2), round(median(performancesOfGm, na.rm = T), 2), round(max(performancesOfGm, na.rm = T), 2), sep = " - ")
        predictionPerformancePMvsGM[m, 4] <- paste(round(min(performancesOfEq, na.rm = T), 2), round(mean(performancesOfEq, na.rm = T), 2), round(median(performancesOfEq, na.rm = T), 2), round(max(performancesOfEq, na.rm = T), 2), sep = " - ")
      }
      xlsx::write.xlsx(predictionPerformancePMvsGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "performances", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      ###########################################
    }
  }
  
  if(infoGainByDeveloperGroups.Density){
    totalMetricCount <- 13 
    alg <- "NB"
    
    infoGainRanks <- xlsx::read.xlsx(paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "ranks")
    developerGroupsPMvsGM <- read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    
    developerGroups <- c("PM", "GM", "-")
    
    # dir.create(path = paste(plotOutputDir, "InfoGainDensity_GroupBased/", sep = ""), showWarnings = FALSE, recursive = TRUE)
    
    metricList <- c("pd", "pf", "F1")
    
    # pdf(file = paste(plotOutputDir, "InfoGainDensity_GroupBased.pdf", sep = ""), width = 23, height = 27)
    # par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    pdf(file = paste(plotOutputDir, alg, "_InfoGainDensity_GroupBased_PdPfF1.pdf", sep = ""), width = 23, height = 15)
    par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(mt in 1:length(metricList)){
      for(gr in 1:length(developerGroups)){
        infoGainRankCountsGroupBased <- data.frame(matrix(ncol = totalMetricCount, nrow = totalMetricCount))
        colnames(infoGainRankCountsGroupBased) <- processMetrics
        rownames(infoGainRankCountsGroupBased) <- c(paste("Rank", rep(1:totalMetricCount), sep = ""))
        
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == developerGroups[gr], ]$Developer
        groupRanks <- infoGainRanks[infoGainRanks$Developer %in% authorNames, ]
        
        for(m in 1:length(processMetrics)){
          for(r in 1:length(processMetrics)){
            infoGainRankCountsGroupBased[paste("Rank", r, sep = ""), processMetrics[m]] <- length(which(groupRanks[, paste("Rank", r, sep = "")] == processMetrics[m]))
          }
        }
        
        colnames(infoGainRankCountsGroupBased) <- toupper(colnames(infoGainRankCountsGroupBased))
        rownames(infoGainRankCountsGroupBased) <- c(paste("Rank", rep(1:totalMetricCount), sep = " "))
        
        if(mt == 1 & gr == 1){
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = developerGroups[gr], las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
          title(ylab = metricList[mt], line = 4, cex.lab = 1.2)
        }
        else if (mt == 1)
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = developerGroups[gr], las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
        else if(gr == 1){
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = "", las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
          title(ylab = metricList[mt], line = 4, cex.lab = 1.2)
        }
        else
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = "", las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
      }
    }
    dev.off()
  }
  
  if(infoGainByDeveloperGroups.SK){
    alg <- "NB"
    
    infoGainOrders <- xlsx::read.xlsx(paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "orders")
    developerGroupsPMvsGM <- read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    
    developerGroups <- c("PM", "GM", "-")
    metricList <- c("pd", "pf", "F1")
    
    # pdf(file = paste(plotOutputDir, "InfoGainDensity_GroupBased.pdf", sep = ""), width = 23, height = 27)
    # par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    pdf(file = paste(plotOutputDir, alg, "_InfoGainRanks_GroupBased_SK_PdPfF1.pdf", sep = ""), width = 23, height = 15)
    par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
    
    for(mt in 1:length(metricList)){
      for(gr in 1:length(developerGroups)){
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == developerGroups[gr], ]$Developer
        groupOrders <- infoGainOrders[infoGainOrders$Developer %in% authorNames, ]
        
        modelCompMatrix <- as.matrix(groupOrders[, processMetrics])
        modelCompResults <- sk_esd(modelCompMatrix)
        rownames(modelCompResults$m.inf) <- toupper(rownames(modelCompResults$m.inf))
        rownames(modelCompResults$m.inf) <- gsub(pattern = "NFC", replacement = "NPC", x = rownames(modelCompResults$m.inf), fixed = T)
        # modelCompResults$m.inf <- modelCompResults$m.inf[nrow(modelCompResults$m.inf):1, ]
        if(developerGroups[gr] == "-")
          title <- paste("PM = GM (", metricList[mt], ")", sep = "")
        else if (developerGroups[gr] == "GM")
          title <- paste("PM < GM (", metricList[mt], ")", sep = "")
        else if (developerGroups[gr] == "PM")
          title <- paste("PM > GM (", metricList[mt], ")", sep = "")
        
        plot(modelCompResults, ylim = c(0, 13), ylab = "Rank", las = 2, xlab = "", title = title)
      }
    }
    dev.off()
  }
  
  
  
  if(metricValuesComparisonByDeveloperGroups.OnlyNB){
    alg <- "NB"
    # metricList <- c("pd", "pf", "F1")
    
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    allProjectsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      allProjectsChanges <- rbind(allProjectsChanges, get(paste(projectName, "Changes", sep = "")))
    }
    
    pdf(file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_AllProjects_Plot.pdf", sep = ""), width = 23, height = 30)
    par(mfrow = c(length(processMetrics), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(m in 1:length(processMetrics)){
      processMetricComparison <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        # PM 
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
        pmValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
        # GM 
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Developer
        gmValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
        # - 
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
        equalValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
        
        numberOfPlots <- length(pmValues) + length(gmValues) + length(equalValues)
        
        if(numberOfPlots > 0){
          boxplot(pmValues, gmValues, equalValues, cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
          title(ylab = paste(processMetrics[m], metricList[mt]))
          
          # Mann-Whitney U Test
          
          mannWhitneyUResultPmGm <- wilcox.test(pmValues, gmValues) 
          mannWhitneyUResultPmEq <- wilcox.test(pmValues, equalValues) 
          mannWhitneyUResultGmEq <- wilcox.test(gmValues, equalValues) 
          effectSizeResultPmGm <- cohen.d(pmValues, gmValues, hedges.correction = TRUE)
          effectSizeResultPmEq <- cohen.d(pmValues, equalValues, hedges.correction = TRUE)
          effectSizeResultGmEq <- cohen.d(gmValues, equalValues, hedges.correction = TRUE)
          
          latestRow <- nrow(processMetricComparison) + 1
          processMetricComparison[latestRow + 1, 1] <- metricList[mt]
          processMetricComparison[latestRow + 1, 2] <- "Pm vs Gm"
          processMetricComparison[latestRow + 1, 3] <- "Pm vs Eq"
          processMetricComparison[latestRow + 1, 4] <- "Gm vs Eq"
          processMetricComparison[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
          processMetricComparison[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
          processMetricComparison[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5) 
          processMetricComparison[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
          processMetricComparison[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
          processMetricComparison[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
          
          
          # Dunn's test
          
          # values <- c(pmValues, gmValues, equalValues)
          # grps <- as.factor(c(rep("PM", length(pmValues)), rep("GM", length(gmValues)), rep("-", length(equalValues))))
          # 
          # dunnResult <- posthoc.kruskal.dunn.test(values, grps, p.adjust="bonf")
          # 
          # latestRow <- nrow(processMetricComparison) + 1
          # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
          # processMetricComparison[latestRow + 1, 2:4] <- c("", colnames(dunnResult$p.value))
          # processMetricComparison[(latestRow + 1):(latestRow + 3), 2] <- c("", rownames(dunnResult$p.value))
          # processMetricComparison[(latestRow + 2):(latestRow + 3), 3:4] <- dunnResult$p.value
        }
        else
          boxplot(0, cex.axis = 1.5, cex.lab = 1.5, names = c("NO"))
      }
      
      testName <- "MannWhitneyUTest" # DunnTest
      
      if(m == 1)
        xlsx::write.xlsx(x = processMetricComparison,
                         file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                         sheetName = processMetrics[m],
                         col.names = FALSE, row.names = FALSE)
      else
        xlsx::write.xlsx(x = processMetricComparison,
                         file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                         sheetName = processMetrics[m],
                         col.names = FALSE, row.names = FALSE,
                         append = TRUE)
    }
    dev.off()
    
  }
  
  if(commitCountsComparisonByDeveloperGroups.OnlyNB){
    alg <- "NB"
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    allProjectsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      allProjectsChanges <- rbind(allProjectsChanges, get(paste(projectName, "Changes", sep = "")))
    }
    
    pdf(file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_AllProjects_Plot.pdf", sep = ""), width = 35, height = 10)
    par(mfrow = c(2, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    # total commit values ####
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsGM[, metricList[mt]]
      
      # PM 
      authorNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
      totalCommitValuesPm <- numeric(length = 0L)
      buggyCommitRatiosPm <- numeric(length = 0L)
      for(aName in authorNamesPm){
        totalCommitValuesPm <- c(totalCommitValuesPm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # GM
      authorNamesGm <- developerGroupsPMvsGM[which(devGroupsNB == "GM"), "Developer"]
      totalCommitValuesGm <- numeric(length = 0L)
      buggyCommitRatiosGm <- numeric(length = 0L)
      for(aName in authorNamesGm){
        totalCommitValuesGm <- c(totalCommitValuesGm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - 
      authorNamesEq <- as.character(developerGroupsPMvsGM[which(devGroupsNB == "-"), "Developer"])
      totalCommitValuesEq <- numeric(length = 0L)
      buggyCommitRatiosEq <- numeric(length = 0L)
      for(aName in authorNamesEq){
        totalCommitValuesEq <- c(totalCommitValuesEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
      title(ylab = paste("Total commit values", metricList[mt]))
      
      mannWhitneyUResultPmGm <- wilcox.test(totalCommitValuesPm, totalCommitValuesGm) 
      mannWhitneyUResultPmEq <- wilcox.test(totalCommitValuesPm, totalCommitValuesEq) 
      mannWhitneyUResultGmEq <- wilcox.test(totalCommitValuesGm, totalCommitValuesEq) 
      effectSizeResultPmGm <- cohen.d(totalCommitValuesPm, totalCommitValuesGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(totalCommitValuesPm, totalCommitValuesEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(totalCommitValuesGm, totalCommitValuesEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                     sheetName = "Total commit values",
                     col.names = FALSE, row.names = FALSE)
    
    #########################
    
    # buggy commit ratios ####    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsGM[, metricList[mt]]
      
      # PM
      authorNamesPm <- developerGroupsPMvsGM[which(devGroupsNB == "PM"), "Developer"]
      totalCommitValuesPm <- numeric(length = 0L)
      buggyCommitRatiosPm <- numeric(length = 0L)
      for(aName in authorNamesPm){
        totalCommitValuesPm <- c(totalCommitValuesPm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # GM
      authorNamesGm <- developerGroupsPMvsGM[which(devGroupsNB == "GM"), "Developer"]
      totalCommitValuesGm <- numeric(length = 0L)
      buggyCommitRatiosGm <- numeric(length = 0L)
      for(aName in authorNamesGm){
        totalCommitValuesGm <- c(totalCommitValuesGm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # -
      authorNamesEq <- as.character(developerGroupsPMvsGM[which(devGroupsNB == "-"), "Developer"])
      totalCommitValuesEq <- numeric(length = 0L)
      buggyCommitRatiosEq <- numeric(length = 0L)
      for(aName in authorNamesEq){
        totalCommitValuesEq <- c(totalCommitValuesEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      boxplot(buggyCommitRatiosPm, buggyCommitRatiosGm, buggyCommitRatiosEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
      title(ylab = paste("Buggy commit ratios", metricList[mt]))
      
      mannWhitneyUResultPmGm <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosGm) 
      mannWhitneyUResultPmEq <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosEq) 
      mannWhitneyUResultGmEq <- wilcox.test(buggyCommitRatiosGm, buggyCommitRatiosEq) 
      effectSizeResultPmGm <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(buggyCommitRatiosGm, buggyCommitRatiosEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                     sheetName = "Buggy commit ratios",
                     col.names = FALSE, row.names = FALSE,
                     append = TRUE)
    #########################
    
    dev.off()
  }
  
  if(performancesComparisonByDeveloperGroups.OnlyNB.PM){
    alg <- "NB"
    resultsDir <- paste(alg, "_Dir", sep = "")
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    pdf(file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_Plot_PM.pdf", sep = ""), width = 35, height = 6)
    par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      # PM 
      authorNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
      projectNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Project
      performanceValuesPm <- numeric(length = 0L)
      for(a in 1:length(authorNamesPm)){
        aName <- authorNamesPm[a]
        pName <- projectNamesPm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesPm <- c(performanceValuesPm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # GM
      authorNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Developer
      projectNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Project
      performanceValuesGm <- numeric(length = 0L)
      for(a in 1:length(authorNamesGm)){
        aName <- authorNamesGm[a]
        pName <- projectNamesGm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesGm <- c(performanceValuesGm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - 
      authorNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
      projectNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Project
      performanceValuesEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesEq)){
        aName <- authorNamesEq[a]
        pName <- projectNamesEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesEq <- c(performanceValuesEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      boxplot(performanceValuesPm, performanceValuesGm, performanceValuesEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
      title(ylab = paste("Performance values", metricList[mt]))
      
      mannWhitneyUResultPmGm <- wilcox.test(performanceValuesPm, performanceValuesGm) 
      mannWhitneyUResultPmEq <- wilcox.test(performanceValuesPm, performanceValuesEq) 
      mannWhitneyUResultGmEq <- wilcox.test(performanceValuesGm, performanceValuesEq) 
      effectSizeResultPmGm <- cohen.d(performanceValuesPm, performanceValuesGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(performanceValuesPm, performanceValuesEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(performanceValuesGm, performanceValuesEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_", testName, "_PM.xlsx", sep = ""),
                     sheetName = "Performance values",
                     col.names = FALSE, row.names = FALSE)
    
    dev.off()
  }
  
  if(performancesComparisonByDeveloperGroups.OnlyNB.GM){
    alg <- "NB"
    resultsDir <- paste(alg, "_Dir", sep = "")
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    pdf(file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_Plot_GM.pdf", sep = ""), width = 35, height = 6)
    par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      # PM 
      authorNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
      projectNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Project
      performanceValuesPm <- numeric(length = 0L)
      for(a in 1:length(authorNamesPm)){
        aName <- authorNamesPm[a]
        pName <- projectNamesPm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesPm <- c(performanceValuesPm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # GM
      authorNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Developer
      projectNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Project
      performanceValuesGm <- numeric(length = 0L)
      for(a in 1:length(authorNamesGm)){
        aName <- authorNamesGm[a]
        pName <- projectNamesGm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesGm <- c(performanceValuesGm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - 
      authorNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
      projectNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Project
      performanceValuesEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesEq)){
        aName <- authorNamesEq[a]
        pName <- projectNamesEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesEq <- c(performanceValuesEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      boxplot(performanceValuesPm, performanceValuesGm, performanceValuesEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
      title(ylab = paste("Performance values", metricList[mt]))
      
      mannWhitneyUResultPmGm <- wilcox.test(performanceValuesPm, performanceValuesGm) 
      mannWhitneyUResultPmEq <- wilcox.test(performanceValuesPm, performanceValuesEq) 
      mannWhitneyUResultGmEq <- wilcox.test(performanceValuesGm, performanceValuesEq) 
      effectSizeResultPmGm <- cohen.d(performanceValuesPm, performanceValuesGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(performanceValuesPm, performanceValuesEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(performanceValuesGm, performanceValuesEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_", testName, "GM.xlsx", sep = ""),
                     sheetName = "Performance values",
                     col.names = FALSE, row.names = FALSE)
    
    dev.off()
  }
  
  if(metricValuesComparisonByDeveloperGroups.BothNBRF){
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    allProjectsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      allProjectsChanges <- rbind(allProjectsChanges, get(paste(projectName, "Changes", sep = "")))
    }
    
    pdf(file = paste(plotOutputDir, "BothNBRF_MetricValues_GroupBased_AllProjects_Plot.pdf", sep = ""), width = 35, height = 45)
    par(mfrow = c(length(processMetrics), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(m in 1:length(processMetrics)){
      processMetricComparison <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- developerGroupsPMvsGM.NB[, metricList[mt]]
        devGroupsRF <- developerGroupsPMvsGM.RF[, metricList[mt]]
        
        # PM & PM
        authorNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
        pmPmValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNamesPmPm, processMetrics[m]]
        
        # GM & GM
        authorNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
        gmGmValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNamesGmGm, processMetrics[m]]
        
        # - & GM or GM & -
        authorNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
        authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
        equalGmValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNamesGmEq, processMetrics[m]]
        
        # - & PM or PM & -
        authorNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
        authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
        equalPmValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNamesPmEq, processMetrics[m]]
        
        # - & - 
        authorNamesEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
        authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
        equalEqualValues <- allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% authorNamesEqEq, processMetrics[m]]
        
        lengthOfResults <- length(pmPmValues) + length(gmGmValues) + length(equalGmValues) + length(equalPmValues) + length(equalEqualValues)
        
        if(lengthOfResults > 0){
          boxplot(pmPmValues, gmGmValues, equalGmValues, equalPmValues, equalEqualValues,
                  cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
          title(ylab = paste(processMetrics[m], metricList[mt]))
          
          # Mann-Whitney U Test
          mannWhitneyUResultPmPmGmGm <- wilcox.test(pmPmValues, gmGmValues) 
          mannWhitneyUResultPmPmEqGm <- wilcox.test(pmPmValues, equalGmValues) 
          mannWhitneyUResultPmPmEqPm <- wilcox.test(pmPmValues, equalPmValues) 
          mannWhitneyUResultPmPmEqEq <- wilcox.test(pmPmValues, equalEqualValues) 
          
          mannWhitneyUResultGmGmEqGm <- wilcox.test(gmGmValues, equalGmValues) 
          mannWhitneyUResultGmGmEqPm <- wilcox.test(gmGmValues, equalPmValues) 
          mannWhitneyUResultGmGmEqEq <- wilcox.test(gmGmValues, equalEqualValues) 
          
          mannWhitneyUResultEqGmEqPm <- wilcox.test(equalGmValues, equalPmValues) 
          mannWhitneyUResultEqGmEqEq <- wilcox.test(equalGmValues, equalEqualValues) 
          
          mannWhitneyUResultEqPmEqEq <- wilcox.test(equalPmValues, equalEqualValues) 
          
          
          effectSizeResultPmPmGmGm <- cohen.d(pmPmValues, gmGmValues, hedges.correction = TRUE)
          effectSizeResultPmPmEqGm <- cohen.d(pmPmValues, equalGmValues, hedges.correction = TRUE)
          effectSizeResultPmPmEqPm <- cohen.d(pmPmValues, equalPmValues, hedges.correction = TRUE)
          effectSizeResultPmPmEqEq <- cohen.d(pmPmValues, equalEqualValues, hedges.correction = TRUE)
          
          effectSizeResultGmGmEqGm <- cohen.d(gmGmValues, equalGmValues, hedges.correction = TRUE)
          effectSizeResultGmGmEqPm <- cohen.d(gmGmValues, equalPmValues, hedges.correction = TRUE) 
          effectSizeResultGmGmEqEq <- cohen.d(gmGmValues, equalEqualValues, hedges.correction = TRUE)
          
          effectSizeResultEqGmEqPm <- cohen.d(equalGmValues, equalPmValues, hedges.correction = TRUE)
          effectSizeResultEqGmEqEq <- cohen.d(equalGmValues, equalEqualValues, hedges.correction = TRUE)
          
          effectSizeResultEqPmEqEq <- cohen.d(equalPmValues, equalEqualValues, hedges.correction = TRUE)
          
          
          latestRow <- nrow(processMetricComparison) + 1
          processMetricComparison[latestRow + 1, 1] <- metricList[mt]
          processMetricComparison[latestRow + 1, 2] <- "PmPm vs GmGm"
          processMetricComparison[latestRow + 1, 3] <- "PmPm vs EqGm"
          processMetricComparison[latestRow + 1, 4] <- "PmPm vs EqPm"
          processMetricComparison[latestRow + 1, 5] <- "PmPm vs EqEq"
          
          processMetricComparison[latestRow + 1, 6] <- "GmGm vs EqGm"
          processMetricComparison[latestRow + 1, 7] <- "GmGm vs EqPm"
          processMetricComparison[latestRow + 1, 8] <- "GmGm vs EqEq"
          
          processMetricComparison[latestRow + 1, 9] <- "EqGm vs EqPm"
          processMetricComparison[latestRow + 1, 10] <- "EqGm vs EqEq"
          
          processMetricComparison[latestRow + 1, 11] <- "EqPm vs EqEq"
          
          
          processMetricComparison[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
          processMetricComparison[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
          processMetricComparison[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
          processMetricComparison[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
          
          processMetricComparison[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
          processMetricComparison[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
          processMetricComparison[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
          
          processMetricComparison[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
          processMetricComparison[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
          
          processMetricComparison[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
          
          
          processMetricComparison[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
          processMetricComparison[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
          processMetricComparison[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
          processMetricComparison[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
          
          processMetricComparison[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
          processMetricComparison[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
          processMetricComparison[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
          
          processMetricComparison[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
          processMetricComparison[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
          
          processMetricComparison[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
        }
        else
          boxplot(0, cex.axis = 1.5, cex.lab = 1.5, names = c("NO"))
      }
      
      testName <- "MannWhitneyUTest" # DunnTest
      
      if(m == 1)
        xlsx::write.xlsx(x = processMetricComparison,
                         file = paste(plotOutputDir, "BothNBRF_MetricValues_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                         sheetName = processMetrics[m],
                         col.names = FALSE, row.names = FALSE)
      else
        xlsx::write.xlsx(x = processMetricComparison,
                         file = paste(plotOutputDir, "BothNBRF_MetricValues_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                         sheetName = processMetrics[m],
                         col.names = FALSE, row.names = FALSE,
                         append = TRUE)
    }
    
    dev.off()
  }
  
  if(commitCountsComparisonByDeveloperGroups.BothNBRF){
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    # alg <- "NB"
    # metricList <- c("pd", "pf", "F1")
    testName <- "MannWhitneyUTest" # DunnTest
    
    allProjectsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      allProjectsChanges <- rbind(allProjectsChanges, get(paste(projectName, "Changes", sep = "")))
    }
    
    pdf(file = paste(plotOutputDir, "BothNBRF_CommitCounts_GroupBased_AllProjects_Plot.pdf", sep = ""), width = 35, height = 12)
    par(mfrow = c(2, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    # total commit values ####
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsGM.NB[, metricList[mt]]
      devGroupsRF <- developerGroupsPMvsGM.RF[, metricList[mt]]
      
      # PM & PM
      authorNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
      totalCommitValuesPmPm <- numeric(length = 0L)
      buggyCommitRatiosPmPm <- numeric(length = 0L)
      for(aName in authorNamesPmPm){
        totalCommitValuesPmPm <- c(totalCommitValuesPmPm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosPmPm <- c(buggyCommitRatiosPmPm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # GM & GM
      authorNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
      totalCommitValuesGmGm <- numeric(length = 0L)
      buggyCommitRatiosGmGm <- numeric(length = 0L)
      for(aName in authorNamesGmGm){
        totalCommitValuesGmGm <- c(totalCommitValuesGmGm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosGmGm <- c(buggyCommitRatiosGmGm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - & GM or GM & -
      authorNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
      authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
      totalCommitValuesGmEq <- numeric(length = 0L)
      buggyCommitRatiosGmEq <- numeric(length = 0L)
      for(aName in authorNamesGmEq){
        totalCommitValuesGmEq <- c(totalCommitValuesGmEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosGmEq <- c(buggyCommitRatiosGmEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - & PM or PM & -
      authorNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
      authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
      totalCommitValuesPmEq <- numeric(length = 0L)
      buggyCommitRatiosPmEq <- numeric(length = 0L)
      for(aName in authorNamesPmEq){
        totalCommitValuesPmEq <- c(totalCommitValuesPmEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosPmEq <- c(buggyCommitRatiosPmEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - & - 
      authorNamesEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
      authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
      totalCommitValuesEqEq <- numeric(length = 0L)
      buggyCommitRatiosEqEq <- numeric(length = 0L)
      for(aName in authorNamesEqEq){
        totalCommitValuesEqEq <- c(totalCommitValuesEqEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosEqEq <- c(buggyCommitRatiosEqEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      boxplot(totalCommitValuesPmPm, totalCommitValuesGmGm, totalCommitValuesGmEq, totalCommitValuesPmEq, totalCommitValuesEqEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
      title(ylab = paste("Total commit values", metricList[mt]))
      
      mannWhitneyUResultPmPmGmGm <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesGmGm)
      mannWhitneyUResultPmPmEqGm <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesGmEq) 
      mannWhitneyUResultPmPmEqPm <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesPmEq) 
      mannWhitneyUResultPmPmEqEq <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesEqEq) 
      
      mannWhitneyUResultGmGmEqGm <- wilcox.test(totalCommitValuesGmGm, totalCommitValuesGmEq) 
      mannWhitneyUResultGmGmEqPm <- wilcox.test(totalCommitValuesGmGm, totalCommitValuesPmEq) 
      mannWhitneyUResultGmGmEqEq <- wilcox.test(totalCommitValuesGmGm, totalCommitValuesEqEq) 
      
      mannWhitneyUResultEqGmEqPm <- wilcox.test(totalCommitValuesGmEq, totalCommitValuesPmEq) 
      mannWhitneyUResultEqGmEqEq <- wilcox.test(totalCommitValuesGmEq, totalCommitValuesEqEq) 
      
      mannWhitneyUResultEqPmEqEq <- wilcox.test(totalCommitValuesPmEq, totalCommitValuesEqEq) 
      
      effectSizeResultPmPmGmGm <- cohen.d(totalCommitValuesPmPm, totalCommitValuesGmGm, hedges.correction = TRUE)
      effectSizeResultPmPmEqGm <- cohen.d(totalCommitValuesPmPm, totalCommitValuesGmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqPm <- cohen.d(totalCommitValuesPmPm, totalCommitValuesPmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqEq <- cohen.d(totalCommitValuesPmPm, totalCommitValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultGmGmEqGm <- cohen.d(totalCommitValuesGmGm, totalCommitValuesGmEq, hedges.correction = TRUE)
      effectSizeResultGmGmEqPm <- cohen.d(totalCommitValuesGmGm, totalCommitValuesPmEq, hedges.correction = TRUE) 
      effectSizeResultGmGmEqEq <- cohen.d(totalCommitValuesGmGm, totalCommitValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqGmEqPm <- cohen.d(totalCommitValuesGmEq, totalCommitValuesPmEq, hedges.correction = TRUE)
      effectSizeResultEqGmEqEq <- cohen.d(totalCommitValuesGmEq, totalCommitValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqPmEqEq <- cohen.d(totalCommitValuesPmEq, totalCommitValuesEqEq, hedges.correction = TRUE)
      
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "PmPm vs GmGm"
      comparisonTable[latestRow + 1, 3] <- "PmPm vs EqGm"
      comparisonTable[latestRow + 1, 4] <- "PmPm vs EqPm"
      comparisonTable[latestRow + 1, 5] <- "PmPm vs EqEq"
      
      comparisonTable[latestRow + 1, 6] <- "GmGm vs EqGm"
      comparisonTable[latestRow + 1, 7] <- "GmGm vs EqPm"
      comparisonTable[latestRow + 1, 8] <- "GmGm vs EqEq"
      
      comparisonTable[latestRow + 1, 9] <- "EqGm vs EqPm"
      comparisonTable[latestRow + 1, 10] <- "EqGm vs EqEq"
      
      comparisonTable[latestRow + 1, 11] <- "EqPm vs EqEq"
      
      
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
      comparisonTable[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
      comparisonTable[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, "BothNBRF_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                     sheetName = "Total commit values",
                     col.names = FALSE, row.names = FALSE)
    
    #########################
    
    # buggy commit ratios ####    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsGM.NB[, metricList[mt]]
      devGroupsRF <- developerGroupsPMvsGM.RF[, metricList[mt]]
      
      # PM & PM
      authorNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
      totalCommitValuesPmPm <- numeric(length = 0L)
      buggyCommitRatiosPmPm <- numeric(length = 0L)
      for(aName in authorNamesPmPm){
        totalCommitValuesPmPm <- c(totalCommitValuesPmPm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosPmPm <- c(buggyCommitRatiosPmPm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # GM & GM
      authorNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
      totalCommitValuesGmGm <- numeric(length = 0L)
      buggyCommitRatiosGmGm <- numeric(length = 0L)
      for(aName in authorNamesGmGm){
        totalCommitValuesGmGm <- c(totalCommitValuesGmGm, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosGmGm <- c(buggyCommitRatiosGmGm, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - & GM or GM & -
      authorNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
      authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
      totalCommitValuesGmEq <- numeric(length = 0L)
      buggyCommitRatiosGmEq <- numeric(length = 0L)
      for(aName in authorNamesGmEq){
        totalCommitValuesGmEq <- c(totalCommitValuesGmEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosGmEq <- c(buggyCommitRatiosGmEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - & PM or PM & -
      authorNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
      authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
      totalCommitValuesPmEq <- numeric(length = 0L)
      buggyCommitRatiosPmEq <- numeric(length = 0L)
      for(aName in authorNamesPmEq){
        totalCommitValuesPmEq <- c(totalCommitValuesPmEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosPmEq <- c(buggyCommitRatiosPmEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - & - 
      authorNamesEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
      authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
      totalCommitValuesEqEq <- numeric(length = 0L)
      buggyCommitRatiosEqEq <- numeric(length = 0L)
      for(aName in authorNamesEqEq){
        totalCommitValuesEqEq <- c(totalCommitValuesEqEq, nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
        buggyCommitRatiosEqEq <- c(buggyCommitRatiosEqEq, length(which(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      boxplot(buggyCommitRatiosPmPm, buggyCommitRatiosGmGm, buggyCommitRatiosGmEq, buggyCommitRatiosPmEq, buggyCommitRatiosEqEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
      title(ylab = paste("Buggy commit ratios", metricList[mt]))
      
      mannWhitneyUResultPmPmGmGm <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosGmGm)
      mannWhitneyUResultPmPmEqGm <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosGmEq) 
      mannWhitneyUResultPmPmEqPm <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosPmEq) 
      mannWhitneyUResultPmPmEqEq <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosEqEq) 
      
      mannWhitneyUResultGmGmEqGm <- wilcox.test(buggyCommitRatiosGmGm, buggyCommitRatiosGmEq) 
      mannWhitneyUResultGmGmEqPm <- wilcox.test(buggyCommitRatiosGmGm, buggyCommitRatiosPmEq) 
      mannWhitneyUResultGmGmEqEq <- wilcox.test(buggyCommitRatiosGmGm, buggyCommitRatiosEqEq) 
      
      mannWhitneyUResultEqGmEqPm <- wilcox.test(buggyCommitRatiosGmEq, buggyCommitRatiosPmEq) 
      mannWhitneyUResultEqGmEqEq <- wilcox.test(buggyCommitRatiosGmEq, buggyCommitRatiosEqEq) 
      
      mannWhitneyUResultEqPmEqEq <- wilcox.test(buggyCommitRatiosPmEq, buggyCommitRatiosEqEq) 
      
      effectSizeResultPmPmGmGm <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosGmGm, hedges.correction = TRUE)
      effectSizeResultPmPmEqGm <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosGmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqPm <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosPmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqEq <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosEqEq, hedges.correction = TRUE)
      
      effectSizeResultGmGmEqGm <- cohen.d(buggyCommitRatiosGmGm, buggyCommitRatiosGmEq, hedges.correction = TRUE)
      effectSizeResultGmGmEqPm <- cohen.d(buggyCommitRatiosGmGm, buggyCommitRatiosPmEq, hedges.correction = TRUE) 
      effectSizeResultGmGmEqEq <- cohen.d(buggyCommitRatiosGmGm, buggyCommitRatiosEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqGmEqPm <- cohen.d(buggyCommitRatiosGmEq, buggyCommitRatiosPmEq, hedges.correction = TRUE)
      effectSizeResultEqGmEqEq <- cohen.d(buggyCommitRatiosGmEq, buggyCommitRatiosEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqPmEqEq <- cohen.d(buggyCommitRatiosPmEq, buggyCommitRatiosEqEq, hedges.correction = TRUE)
      
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "PmPm vs GmGm"
      comparisonTable[latestRow + 1, 3] <- "PmPm vs EqGm"
      comparisonTable[latestRow + 1, 4] <- "PmPm vs EqPm"
      comparisonTable[latestRow + 1, 5] <- "PmPm vs EqEq"
      
      comparisonTable[latestRow + 1, 6] <- "GmGm vs EqGm"
      comparisonTable[latestRow + 1, 7] <- "GmGm vs EqPm"
      comparisonTable[latestRow + 1, 8] <- "GmGm vs EqEq"
      
      comparisonTable[latestRow + 1, 9] <- "EqGm vs EqPm"
      comparisonTable[latestRow + 1, 10] <- "EqGm vs EqEq"
      
      comparisonTable[latestRow + 1, 11] <- "EqPm vs EqEq"
      
      
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
      comparisonTable[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
      comparisonTable[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
    }
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, "BothNBRF_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
                     sheetName = "Buggy commit ratios",
                     col.names = FALSE, row.names = FALSE,
                     append = TRUE)
    #########################
    
    dev.off()
  }
  
  if(performancesComparisonByDeveloperGroups.BothNBRF.PM){
    alg <- "NB"
    testName <- "MannWhitneyUTest" # DunnTest 
    resultsDir <- paste(alg, "_Dir", sep = "")
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    pdf(file = paste(plotOutputDir, "BothNBRF_Performances_GroupBased_AllProjects_Plot_PM.pdf", sep = ""), width = 35, height = 6)
    par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsGM.NB[, metricList[mt]]
      devGroupsRF <- developerGroupsPMvsGM.RF[, metricList[mt]]
      
      # PM & PM
      authorNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
      projectNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Project"]
      performanceValuesPmPm <- numeric(length = 0L)
      for(a in 1:length(authorNamesPmPm)){
        aName <- authorNamesPmPm[a]
        pName <- projectNamesPmPm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesPmPm <- c(performanceValuesPmPm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # GM & GM
      authorNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
      projectNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Project"]
      performanceValuesGmGm <- numeric(length = 0L)
      for(a in 1:length(authorNamesGmGm)){
        aName <- authorNamesGmGm[a]
        pName <- projectNamesGmGm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesGmGm <- c(performanceValuesGmGm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - & GM or GM & -
      authorNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
      authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
      projectNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Project"])
      projectNamesGmEq <- c(projectNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Project"]))
      performanceValuesGmEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesGmEq)){
        aName <- authorNamesGmEq[a]
        pName <- projectNamesGmEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesGmEq <- c(performanceValuesGmEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - & PM or PM & -
      authorNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
      authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
      projectNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Project"])
      projectNamesPmEq <- c(projectNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Project"]))
      performanceValuesPmEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesPmEq)){
        aName <- authorNamesPmEq[a]
        pName <- projectNamesPmEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesPmEq <- c(performanceValuesPmEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - & - 
      authorNamesEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
      authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
      projectNameEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Project"])
      projectNameEqEq <- c(projectNameEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Project"]))
      performanceValuesEqEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesEqEq)){
        aName <- authorNamesEqEq[a]
        pName <- projectNameEqEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesEqEq <- c(performanceValuesEqEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      boxplot(performanceValuesPmPm, performanceValuesGmGm, performanceValuesGmEq, performanceValuesPmEq, performanceValuesEqEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
      title(ylab = paste("Total commit values", metricList[mt]))
      
      mannWhitneyUResultPmPmGmGm <- wilcox.test(performanceValuesPmPm, performanceValuesGmGm)
      mannWhitneyUResultPmPmEqGm <- wilcox.test(performanceValuesPmPm, performanceValuesGmEq) 
      mannWhitneyUResultPmPmEqPm <- wilcox.test(performanceValuesPmPm, performanceValuesPmEq) 
      mannWhitneyUResultPmPmEqEq <- wilcox.test(performanceValuesPmPm, performanceValuesEqEq) 
      
      mannWhitneyUResultGmGmEqGm <- wilcox.test(performanceValuesGmGm, performanceValuesGmEq) 
      mannWhitneyUResultGmGmEqPm <- wilcox.test(performanceValuesGmGm, performanceValuesPmEq) 
      mannWhitneyUResultGmGmEqEq <- wilcox.test(performanceValuesGmGm, performanceValuesEqEq) 
      
      mannWhitneyUResultEqGmEqPm <- wilcox.test(performanceValuesGmEq, performanceValuesPmEq) 
      mannWhitneyUResultEqGmEqEq <- wilcox.test(performanceValuesGmEq, performanceValuesEqEq) 
      
      mannWhitneyUResultEqPmEqEq <- wilcox.test(performanceValuesPmEq, performanceValuesEqEq) 
      
      effectSizeResultPmPmGmGm <- cohen.d(performanceValuesPmPm, performanceValuesGmGm, hedges.correction = TRUE)
      effectSizeResultPmPmEqGm <- cohen.d(performanceValuesPmPm, performanceValuesGmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqPm <- cohen.d(performanceValuesPmPm, performanceValuesPmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqEq <- cohen.d(performanceValuesPmPm, performanceValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultGmGmEqGm <- cohen.d(performanceValuesGmGm, performanceValuesGmEq, hedges.correction = TRUE)
      effectSizeResultGmGmEqPm <- cohen.d(performanceValuesGmGm, performanceValuesPmEq, hedges.correction = TRUE) 
      effectSizeResultGmGmEqEq <- cohen.d(performanceValuesGmGm, performanceValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqGmEqPm <- cohen.d(performanceValuesGmEq, performanceValuesPmEq, hedges.correction = TRUE)
      effectSizeResultEqGmEqEq <- cohen.d(performanceValuesGmEq, performanceValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqPmEqEq <- cohen.d(performanceValuesPmEq, performanceValuesEqEq, hedges.correction = TRUE)
      
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "PmPm vs GmGm"
      comparisonTable[latestRow + 1, 3] <- "PmPm vs EqGm"
      comparisonTable[latestRow + 1, 4] <- "PmPm vs EqPm"
      comparisonTable[latestRow + 1, 5] <- "PmPm vs EqEq"
      
      comparisonTable[latestRow + 1, 6] <- "GmGm vs EqGm"
      comparisonTable[latestRow + 1, 7] <- "GmGm vs EqPm"
      comparisonTable[latestRow + 1, 8] <- "GmGm vs EqEq"
      
      comparisonTable[latestRow + 1, 9] <- "EqGm vs EqPm"
      comparisonTable[latestRow + 1, 10] <- "EqGm vs EqEq"
      
      comparisonTable[latestRow + 1, 11] <- "EqPm vs EqEq"
      
      
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
      comparisonTable[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
      comparisonTable[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, "BothNBRF_PerformanceValues_GroupBased_AllProjects_", testName, "_PM.xlsx", sep = ""),
                     sheetName = "Total commit values",
                     col.names = FALSE, row.names = FALSE)
    
    dev.off()
  }
  
  if(performancesComparisonByDeveloperGroups.BothNBRF.GM){
    alg <- "NB"
    testName <- "MannWhitneyUTest" # DunnTest 
    resultsDir <- paste(alg, "_Dir", sep = "")
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    pdf(file = paste(plotOutputDir, "BothNBRF_Performances_GroupBased_AllProjects_Plot_GM.pdf", sep = ""), width = 35, height = 6)
    par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsGM.NB[, metricList[mt]]
      devGroupsRF <- developerGroupsPMvsGM.RF[, metricList[mt]]
      
      # PM & PM
      authorNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
      projectNamesPmPm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Project"]
      performanceValuesPmPm <- numeric(length = 0L)
      for(a in 1:length(authorNamesPmPm)){
        aName <- authorNamesPmPm[a]
        pName <- projectNamesPmPm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesPmPm <- c(performanceValuesPmPm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # GM & GM
      authorNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
      projectNamesGmGm <- developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Project"]
      performanceValuesGmGm <- numeric(length = 0L)
      for(a in 1:length(authorNamesGmGm)){
        aName <- authorNamesGmGm[a]
        pName <- projectNamesGmGm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesGmGm <- c(performanceValuesGmGm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - & GM or GM & -
      authorNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
      authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
      projectNamesGmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Project"])
      projectNamesGmEq <- c(projectNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Project"]))
      performanceValuesGmEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesGmEq)){
        aName <- authorNamesGmEq[a]
        pName <- projectNamesGmEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesGmEq <- c(performanceValuesGmEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - & PM or PM & -
      authorNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
      authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
      projectNamesPmEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Project"])
      projectNamesPmEq <- c(projectNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Project"]))
      performanceValuesPmEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesPmEq)){
        aName <- authorNamesPmEq[a]
        pName <- projectNamesPmEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesPmEq <- c(performanceValuesPmEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - & - 
      authorNamesEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
      authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
      projectNameEqEq <- as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Project"])
      projectNameEqEq <- c(projectNameEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Project"]))
      performanceValuesEqEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesEqEq)){
        aName <- authorNamesEqEq[a]
        pName <- projectNameEqEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesEqEq <- c(performanceValuesEqEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      boxplot(performanceValuesPmPm, performanceValuesGmGm, performanceValuesGmEq, performanceValuesPmEq, performanceValuesEqEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
      title(ylab = paste("Total commit values", metricList[mt]))
      
      mannWhitneyUResultPmPmGmGm <- wilcox.test(performanceValuesPmPm, performanceValuesGmGm)
      mannWhitneyUResultPmPmEqGm <- wilcox.test(performanceValuesPmPm, performanceValuesGmEq) 
      mannWhitneyUResultPmPmEqPm <- wilcox.test(performanceValuesPmPm, performanceValuesPmEq) 
      mannWhitneyUResultPmPmEqEq <- wilcox.test(performanceValuesPmPm, performanceValuesEqEq) 
      
      mannWhitneyUResultGmGmEqGm <- wilcox.test(performanceValuesGmGm, performanceValuesGmEq) 
      mannWhitneyUResultGmGmEqPm <- wilcox.test(performanceValuesGmGm, performanceValuesPmEq) 
      mannWhitneyUResultGmGmEqEq <- wilcox.test(performanceValuesGmGm, performanceValuesEqEq) 
      
      mannWhitneyUResultEqGmEqPm <- wilcox.test(performanceValuesGmEq, performanceValuesPmEq) 
      mannWhitneyUResultEqGmEqEq <- wilcox.test(performanceValuesGmEq, performanceValuesEqEq) 
      
      mannWhitneyUResultEqPmEqEq <- wilcox.test(performanceValuesPmEq, performanceValuesEqEq) 
      
      effectSizeResultPmPmGmGm <- cohen.d(performanceValuesPmPm, performanceValuesGmGm, hedges.correction = TRUE)
      effectSizeResultPmPmEqGm <- cohen.d(performanceValuesPmPm, performanceValuesGmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqPm <- cohen.d(performanceValuesPmPm, performanceValuesPmEq, hedges.correction = TRUE)
      effectSizeResultPmPmEqEq <- cohen.d(performanceValuesPmPm, performanceValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultGmGmEqGm <- cohen.d(performanceValuesGmGm, performanceValuesGmEq, hedges.correction = TRUE)
      effectSizeResultGmGmEqPm <- cohen.d(performanceValuesGmGm, performanceValuesPmEq, hedges.correction = TRUE) 
      effectSizeResultGmGmEqEq <- cohen.d(performanceValuesGmGm, performanceValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqGmEqPm <- cohen.d(performanceValuesGmEq, performanceValuesPmEq, hedges.correction = TRUE)
      effectSizeResultEqGmEqEq <- cohen.d(performanceValuesGmEq, performanceValuesEqEq, hedges.correction = TRUE)
      
      effectSizeResultEqPmEqEq <- cohen.d(performanceValuesPmEq, performanceValuesEqEq, hedges.correction = TRUE)
      
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "PmPm vs GmGm"
      comparisonTable[latestRow + 1, 3] <- "PmPm vs EqGm"
      comparisonTable[latestRow + 1, 4] <- "PmPm vs EqPm"
      comparisonTable[latestRow + 1, 5] <- "PmPm vs EqEq"
      
      comparisonTable[latestRow + 1, 6] <- "GmGm vs EqGm"
      comparisonTable[latestRow + 1, 7] <- "GmGm vs EqPm"
      comparisonTable[latestRow + 1, 8] <- "GmGm vs EqEq"
      
      comparisonTable[latestRow + 1, 9] <- "EqGm vs EqPm"
      comparisonTable[latestRow + 1, 10] <- "EqGm vs EqEq"
      
      comparisonTable[latestRow + 1, 11] <- "EqPm vs EqEq"
      
      
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
      comparisonTable[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
      comparisonTable[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
      
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
      comparisonTable[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
      comparisonTable[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
      comparisonTable[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
      
      comparisonTable[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, "BothNBRF_PerformanceValues_GroupBased_AllProjects_", testName, "_GM.xlsx", sep = ""),
                     sheetName = "Total commit values",
                     col.names = FALSE, row.names = FALSE)
    
    dev.off()
  }
  
  
  if(metricValuesComparisonByDeveloperGroups.OnlyNB.eachProject){
    dir.create(path = paste(plotOutputDir, "ForEachProject", sep = ""), showWarnings = FALSE, recursive = TRUE)
    
    alg <- "NB"
    # metricList <- c("pd", "pf", "F1")
    
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectsChanges <- get(paste(projectName, "Changes", sep = ""))
      
      pdf(file = paste(plotOutputDir, "ForEachProject/", alg, "_MetricValues_GroupBased_", projectName, "_Plot.pdf", sep = ""), width = 23, height = 30)
      par(mfrow = c(length(processMetrics), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      for(m in 1:length(processMetrics)){
        processMetricComparison <- as.data.frame(matrix(ncol = 0, nrow = 0))
        
        for(mt in 1:length(metricList)){
          # PM 
          authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
          pmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
          # GM 
          authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Developer
          gmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
          # - 
          authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
          equalValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
          
          numberOfPlots <- length(pmValues) + length(gmValues) + length(equalValues)
          
          if(numberOfPlots > 0){
            boxplot(pmValues, gmValues, equalValues, cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
            title(ylab = paste(processMetrics[m], metricList[mt]))
            
            # Mann-Whitney U Test
            # 
            # mannWhitneyUResultPmGm <- wilcox.test(pmValues, gmValues) 
            # mannWhitneyUResultPmEq <- wilcox.test(pmValues, equalValues) 
            # mannWhitneyUResultGmEq <- wilcox.test(gmValues, equalValues) 
            # effectSizeResultPmGm <- cohen.d(pmValues, gmValues, hedges.correction = TRUE)
            # effectSizeResultPmEq <- cohen.d(pmValues, equalValues, hedges.correction = TRUE)
            # effectSizeResultGmEq <- cohen.d(gmValues, equalValues, hedges.correction = TRUE)
            # 
            # latestRow <- nrow(processMetricComparison) + 1
            # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
            # processMetricComparison[latestRow + 1, 2] <- "Pm vs Gm"
            # processMetricComparison[latestRow + 1, 3] <- "Pm vs Eq"
            # processMetricComparison[latestRow + 1, 4] <- "Gm vs Eq"
            # processMetricComparison[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
            # processMetricComparison[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
            # processMetricComparison[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5) 
            # processMetricComparison[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
            # processMetricComparison[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
            # processMetricComparison[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
            
            
            # Dunn's test
            
            # values <- c(pmValues, gmValues, equalValues)
            # grps <- as.factor(c(rep("PM", length(pmValues)), rep("GM", length(gmValues)), rep("-", length(equalValues))))
            # 
            # dunnResult <- posthoc.kruskal.dunn.test(values, grps, p.adjust="bonf")
            # 
            # latestRow <- nrow(processMetricComparison) + 1
            # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
            # processMetricComparison[latestRow + 1, 2:4] <- c("", colnames(dunnResult$p.value))
            # processMetricComparison[(latestRow + 1):(latestRow + 3), 2] <- c("", rownames(dunnResult$p.value))
            # processMetricComparison[(latestRow + 2):(latestRow + 3), 3:4] <- dunnResult$p.value
          }
          else
            boxplot(0, cex.axis = 1.5, cex.lab = 1.5, names = c("NO"))
        }
        
        # testName <- "MannWhitneyUTest" # DunnTest
        # 
        # if(m == 1)
        #   xlsx::write.xlsx(x = processMetricComparison,
        #                    file = paste(plotOutputDir, "ForEachProject/", alg, "_MetricValues_GroupBased_", projectName, "_", testName, ".xlsx", sep = ""),
        #                    sheetName = processMetrics[m],
        #                    col.names = FALSE, row.names = FALSE)
        # else
        #   xlsx::write.xlsx(x = processMetricComparison,
        #                    file = paste(plotOutputDir, "ForEachProject/", alg, "_MetricValues_GroupBased_", projectName, "_", testName, ".xlsx", sep = ""),
        #                    sheetName = processMetrics[m],
        #                    col.names = FALSE, row.names = FALSE,
        #                    append = TRUE)
      }
      dev.off()
    }
  }
  
  if(commitCountsComparisonByDeveloperGroups.OnlyNB.eachProject){
    alg <- "NB"
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectsChanges <- get(paste(projectName, "Changes", sep = ""))
      
      projectDeveloperGroupsPMvsGM <- developerGroupsPMvsGM[which(developerGroupsPMvsGM$Project == projectName), ]
      
      pdf(file = paste(plotOutputDir, "ForEachProject/", alg, "_CommitCounts_GroupBased_", projectName, "_Plot.pdf", sep = ""), width = 35, height = 10)
      par(mfrow = c(2, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      # total commit values ####
      comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- projectDeveloperGroupsPMvsGM[, metricList[mt]]
        
        # PM 
        authorNamesPm <- projectDeveloperGroupsPMvsGM[projectDeveloperGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
        totalCommitValuesPm <- numeric(length = 0L)
        buggyCommitRatiosPm <- numeric(length = 0L)
        for(aName in authorNamesPm){
          totalCommitValuesPm <- c(totalCommitValuesPm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # GM
        authorNamesGm <- projectDeveloperGroupsPMvsGM[which(devGroupsNB == "GM"), "Developer"]
        totalCommitValuesGm <- numeric(length = 0L)
        buggyCommitRatiosGm <- numeric(length = 0L)
        for(aName in authorNamesGm){
          totalCommitValuesGm <- c(totalCommitValuesGm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - 
        authorNamesEq <- as.character(projectDeveloperGroupsPMvsGM[which(devGroupsNB == "-"), "Developer"])
        totalCommitValuesEq <- numeric(length = 0L)
        buggyCommitRatiosEq <- numeric(length = 0L)
        for(aName in authorNamesEq){
          totalCommitValuesEq <- c(totalCommitValuesEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq,
                cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
        title(ylab = paste("Total commit values", metricList[mt]))
        
        # mannWhitneyUResultPmGm <- wilcox.test(totalCommitValuesPm, totalCommitValuesGm) 
        # mannWhitneyUResultPmEq <- wilcox.test(totalCommitValuesPm, totalCommitValuesEq) 
        # mannWhitneyUResultGmEq <- wilcox.test(totalCommitValuesGm, totalCommitValuesEq) 
        # effectSizeResultPmGm <- cohen.d(totalCommitValuesPm, totalCommitValuesGm, hedges.correction = TRUE)
        # effectSizeResultPmEq <- cohen.d(totalCommitValuesPm, totalCommitValuesEq, hedges.correction = TRUE)
        # effectSizeResultGmEq <- cohen.d(totalCommitValuesGm, totalCommitValuesEq, hedges.correction = TRUE)
        # 
        # latestRow <- nrow(comparisonTable) + 1
        # comparisonTable[latestRow + 1, 1] <- metricList[mt]
        # comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
        # comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
        # comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
        # comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
        # comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
        # comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
        # comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
        # comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
        # comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
      }   
      
      # xlsx::write.xlsx(x = comparisonTable,
      #                  file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
      #                  sheetName = "Total commit values",
      #                  col.names = FALSE, row.names = FALSE)
      
      #########################
      
      # buggy commit ratios ####    
      comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- projectDeveloperGroupsPMvsGM[, metricList[mt]]
        
        # PM
        authorNamesPm <- projectDeveloperGroupsPMvsGM[which(devGroupsNB == "PM"), "Developer"]
        totalCommitValuesPm <- numeric(length = 0L)
        buggyCommitRatiosPm <- numeric(length = 0L)
        for(aName in authorNamesPm){
          totalCommitValuesPm <- c(totalCommitValuesPm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # GM
        authorNamesGm <- projectDeveloperGroupsPMvsGM[which(devGroupsNB == "GM"), "Developer"]
        totalCommitValuesGm <- numeric(length = 0L)
        buggyCommitRatiosGm <- numeric(length = 0L)
        for(aName in authorNamesGm){
          totalCommitValuesGm <- c(totalCommitValuesGm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # -
        authorNamesEq <- as.character(projectDeveloperGroupsPMvsGM[which(devGroupsNB == "-"), "Developer"])
        totalCommitValuesEq <- numeric(length = 0L)
        buggyCommitRatiosEq <- numeric(length = 0L)
        for(aName in authorNamesEq){
          totalCommitValuesEq <- c(totalCommitValuesEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        boxplot(buggyCommitRatiosPm, buggyCommitRatiosGm, buggyCommitRatiosEq,
                cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
        title(ylab = paste("Buggy commit ratios", metricList[mt]))
        
        # mannWhitneyUResultPmGm <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosGm) 
        # mannWhitneyUResultPmEq <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosEq) 
        # mannWhitneyUResultGmEq <- wilcox.test(buggyCommitRatiosGm, buggyCommitRatiosEq) 
        # effectSizeResultPmGm <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosGm, hedges.correction = TRUE)
        # effectSizeResultPmEq <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosEq, hedges.correction = TRUE)
        # effectSizeResultGmEq <- cohen.d(buggyCommitRatiosGm, buggyCommitRatiosEq, hedges.correction = TRUE)
        # 
        # latestRow <- nrow(comparisonTable) + 1
        # comparisonTable[latestRow + 1, 1] <- metricList[mt]
        # comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
        # comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
        # comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
        # comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
        # comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
        # comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
        # comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
        # comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
        # comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
      }
      
      # xlsx::write.xlsx(x = comparisonTable,
      #                  file = paste(plotOutputDir, "ForEachProject/", alg, "_CommitCounts_GroupBased_", projectName, "_", testName, ".xlsx", sep = ""),
      #                  sheetName = "Buggy commit ratios",
      #                  col.names = FALSE, row.names = FALSE,
      #                  append = TRUE)
      #########################
      
      dev.off()
    }
  }
  
  if(metricValuesComparisonByDeveloperGroups.BothNBRF.eachProject){
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectsChanges <- get(paste(projectName, "Changes", sep = ""))
      
      projectDeveloperGroupsPMvsGM.NB <- developerGroupsPMvsGM.NB[which(developerGroupsPMvsGM.NB$Project == projectName), ]
      projectDeveloperGroupsPMvsGM.RF <- developerGroupsPMvsGM.RF[which(developerGroupsPMvsGM.RF$Project == projectName), ]
      
      pdf(file = paste(plotOutputDir, "ForEachProject/BothNBRF_MetricValues_GroupBased_", projectName, "_Plot.pdf", sep = ""), width = 35, height = 45)
      par(mfrow = c(length(processMetrics), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      for(m in 1:length(processMetrics)){
        processMetricComparison <- as.data.frame(matrix(ncol = 0, nrow = 0))
        
        for(mt in 1:length(metricList)){
          devGroupsNB <- projectDeveloperGroupsPMvsGM.NB[, metricList[mt]]
          devGroupsRF <- projectDeveloperGroupsPMvsGM.RF[, metricList[mt]]
          
          # PM & PM
          authorNamesPmPm <- projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
          pmPmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNamesPmPm, processMetrics[m]]
          
          # GM & GM
          authorNamesGmGm <- projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
          gmGmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNamesGmGm, processMetrics[m]]
          
          # - & GM or GM & -
          authorNamesGmEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
          authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
          equalGmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNamesGmEq, processMetrics[m]]
          
          # - & PM or PM & -
          authorNamesPmEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
          authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
          equalPmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNamesPmEq, processMetrics[m]]
          
          # - & - 
          authorNamesEqEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
          authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
          equalEqualValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNamesEqEq, processMetrics[m]]
          
          lengthOfResults <- length(pmPmValues) + length(gmGmValues) + length(equalGmValues) + length(equalPmValues) + length(equalEqualValues)
          
          if(lengthOfResults > 0){
            boxplot(pmPmValues, gmGmValues, equalGmValues, equalPmValues, equalEqualValues,
                    cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
            title(ylab = paste(processMetrics[m], metricList[mt]))
            
            # # Mann-Whitney U Test
            # mannWhitneyUResultPmPmGmGm <- wilcox.test(pmPmValues, gmGmValues) 
            # mannWhitneyUResultPmPmEqGm <- wilcox.test(pmPmValues, equalGmValues) 
            # mannWhitneyUResultPmPmEqPm <- wilcox.test(pmPmValues, equalPmValues) 
            # mannWhitneyUResultPmPmEqEq <- wilcox.test(pmPmValues, equalEqualValues) 
            # 
            # mannWhitneyUResultGmGmEqGm <- wilcox.test(gmGmValues, equalGmValues) 
            # mannWhitneyUResultGmGmEqPm <- wilcox.test(gmGmValues, equalPmValues) 
            # mannWhitneyUResultGmGmEqEq <- wilcox.test(gmGmValues, equalEqualValues) 
            # 
            # mannWhitneyUResultEqGmEqPm <- wilcox.test(equalGmValues, equalPmValues) 
            # mannWhitneyUResultEqGmEqEq <- wilcox.test(equalGmValues, equalEqualValues) 
            # 
            # mannWhitneyUResultEqPmEqEq <- wilcox.test(equalPmValues, equalEqualValues) 
            # 
            # 
            # effectSizeResultPmPmGmGm <- cohen.d(pmPmValues, gmGmValues, hedges.correction = TRUE)
            # effectSizeResultPmPmEqGm <- cohen.d(pmPmValues, equalGmValues, hedges.correction = TRUE)
            # effectSizeResultPmPmEqPm <- cohen.d(pmPmValues, equalPmValues, hedges.correction = TRUE)
            # effectSizeResultPmPmEqEq <- cohen.d(pmPmValues, equalEqualValues, hedges.correction = TRUE)
            # 
            # effectSizeResultGmGmEqGm <- cohen.d(gmGmValues, equalGmValues, hedges.correction = TRUE)
            # effectSizeResultGmGmEqPm <- cohen.d(gmGmValues, equalPmValues, hedges.correction = TRUE) 
            # effectSizeResultGmGmEqEq <- cohen.d(gmGmValues, equalEqualValues, hedges.correction = TRUE)
            # 
            # effectSizeResultEqGmEqPm <- cohen.d(equalGmValues, equalPmValues, hedges.correction = TRUE)
            # effectSizeResultEqGmEqEq <- cohen.d(equalGmValues, equalEqualValues, hedges.correction = TRUE)
            # 
            # effectSizeResultEqPmEqEq <- cohen.d(equalPmValues, equalEqualValues, hedges.correction = TRUE)
            # 
            # 
            # latestRow <- nrow(processMetricComparison) + 1
            # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
            # processMetricComparison[latestRow + 1, 2] <- "PmPm vs GmGm"
            # processMetricComparison[latestRow + 1, 3] <- "PmPm vs EqGm"
            # processMetricComparison[latestRow + 1, 4] <- "PmPm vs EqPm"
            # processMetricComparison[latestRow + 1, 5] <- "PmPm vs EqEq"
            # 
            # processMetricComparison[latestRow + 1, 6] <- "GmGm vs EqGm"
            # processMetricComparison[latestRow + 1, 7] <- "GmGm vs EqPm"
            # processMetricComparison[latestRow + 1, 8] <- "GmGm vs EqEq"
            # 
            # processMetricComparison[latestRow + 1, 9] <- "EqGm vs EqPm"
            # processMetricComparison[latestRow + 1, 10] <- "EqGm vs EqEq"
            # 
            # processMetricComparison[latestRow + 1, 11] <- "EqPm vs EqEq"
            # 
            # 
            # processMetricComparison[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
            # processMetricComparison[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
            # processMetricComparison[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
            # processMetricComparison[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
            # 
            # processMetricComparison[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
            # processMetricComparison[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
            # processMetricComparison[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
            # 
            # processMetricComparison[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
            # processMetricComparison[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
            # 
            # processMetricComparison[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
            # 
            # 
            # processMetricComparison[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
            # processMetricComparison[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
            # processMetricComparison[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
            # processMetricComparison[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
            # 
            # processMetricComparison[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
            # processMetricComparison[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
            # processMetricComparison[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
            # 
            # processMetricComparison[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
            # processMetricComparison[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
            # 
            # processMetricComparison[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
          }
          else
            boxplot(0, cex.axis = 1.5, cex.lab = 1.5, names = c("NO"))
        }
        
        testName <- "MannWhitneyUTest" # DunnTest
        
        # if(m == 1)
        #   xlsx::write.xlsx(x = processMetricComparison,
        #                    file = paste(plotOutputDir, "BothNBRF_MetricValues_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
        #                    sheetName = processMetrics[m],
        #                    col.names = FALSE, row.names = FALSE)
        # else
        #   xlsx::write.xlsx(x = processMetricComparison,
        #                    file = paste(plotOutputDir, "BothNBRF_MetricValues_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
        #                    sheetName = processMetrics[m],
        #                    col.names = FALSE, row.names = FALSE,
        #                    append = TRUE)
      }
      dev.off()
    }
  }
  
  if(commitCountsComparisonByDeveloperGroups.BothNBRF.eachProject){
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    testName <- "MannWhitneyUTest" # DunnTest
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectsChanges <- get(paste(projectName, "Changes", sep = ""))
      
      projectDeveloperGroupsPMvsGM.NB <- developerGroupsPMvsGM.NB[which(developerGroupsPMvsGM.NB$Project == projectName), ]
      projectDeveloperGroupsPMvsGM.RF <- developerGroupsPMvsGM.RF[which(developerGroupsPMvsGM.RF$Project == projectName), ]
      
      pdf(file = paste(plotOutputDir, "ForEachProject/BothNBRF_CommitCounts_GroupBased_", projectName, "_Plot.pdf", sep = ""), width = 35, height = 12)
      par(mfrow = c(2, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      # total commit values ####
      comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- projectDeveloperGroupsPMvsGM.NB[, metricList[mt]]
        devGroupsRF <- projectDeveloperGroupsPMvsGM.RF[, metricList[mt]]
        
        # PM & PM
        authorNamesPmPm <- projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
        totalCommitValuesPmPm <- numeric(length = 0L)
        buggyCommitRatiosPmPm <- numeric(length = 0L)
        for(aName in authorNamesPmPm){
          totalCommitValuesPmPm <- c(totalCommitValuesPmPm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPmPm <- c(buggyCommitRatiosPmPm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # GM & GM
        authorNamesGmGm <- projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
        totalCommitValuesGmGm <- numeric(length = 0L)
        buggyCommitRatiosGmGm <- numeric(length = 0L)
        for(aName in authorNamesGmGm){
          totalCommitValuesGmGm <- c(totalCommitValuesGmGm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGmGm <- c(buggyCommitRatiosGmGm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - & GM or GM & -
        authorNamesGmEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
        authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
        totalCommitValuesGmEq <- numeric(length = 0L)
        buggyCommitRatiosGmEq <- numeric(length = 0L)
        for(aName in authorNamesGmEq){
          totalCommitValuesGmEq <- c(totalCommitValuesGmEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGmEq <- c(buggyCommitRatiosGmEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - & PM or PM & -
        authorNamesPmEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
        authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
        totalCommitValuesPmEq <- numeric(length = 0L)
        buggyCommitRatiosPmEq <- numeric(length = 0L)
        for(aName in authorNamesPmEq){
          totalCommitValuesPmEq <- c(totalCommitValuesPmEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPmEq <- c(buggyCommitRatiosPmEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - & - 
        authorNamesEqEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
        authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
        totalCommitValuesEqEq <- numeric(length = 0L)
        buggyCommitRatiosEqEq <- numeric(length = 0L)
        for(aName in authorNamesEqEq){
          totalCommitValuesEqEq <- c(totalCommitValuesEqEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosEqEq <- c(buggyCommitRatiosEqEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        boxplot(totalCommitValuesPmPm, totalCommitValuesGmGm, totalCommitValuesGmEq, totalCommitValuesPmEq, totalCommitValuesEqEq,
                cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
        title(ylab = paste("Total commit values", metricList[mt]))
        
        # mannWhitneyUResultPmPmGmGm <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesGmGm)
        # mannWhitneyUResultPmPmEqGm <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesGmEq) 
        # mannWhitneyUResultPmPmEqPm <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesPmEq) 
        # mannWhitneyUResultPmPmEqEq <- wilcox.test(totalCommitValuesPmPm, totalCommitValuesEqEq) 
        # 
        # mannWhitneyUResultGmGmEqGm <- wilcox.test(totalCommitValuesGmGm, totalCommitValuesGmEq) 
        # mannWhitneyUResultGmGmEqPm <- wilcox.test(totalCommitValuesGmGm, totalCommitValuesPmEq) 
        # mannWhitneyUResultGmGmEqEq <- wilcox.test(totalCommitValuesGmGm, totalCommitValuesEqEq) 
        # 
        # mannWhitneyUResultEqGmEqPm <- wilcox.test(totalCommitValuesGmEq, totalCommitValuesPmEq) 
        # mannWhitneyUResultEqGmEqEq <- wilcox.test(totalCommitValuesGmEq, totalCommitValuesEqEq) 
        # 
        # mannWhitneyUResultEqPmEqEq <- wilcox.test(totalCommitValuesPmEq, totalCommitValuesEqEq) 
        # 
        # effectSizeResultPmPmGmGm <- cohen.d(totalCommitValuesPmPm, totalCommitValuesGmGm, hedges.correction = TRUE)
        # effectSizeResultPmPmEqGm <- cohen.d(totalCommitValuesPmPm, totalCommitValuesGmEq, hedges.correction = TRUE)
        # effectSizeResultPmPmEqPm <- cohen.d(totalCommitValuesPmPm, totalCommitValuesPmEq, hedges.correction = TRUE)
        # effectSizeResultPmPmEqEq <- cohen.d(totalCommitValuesPmPm, totalCommitValuesEqEq, hedges.correction = TRUE)
        # 
        # effectSizeResultGmGmEqGm <- cohen.d(totalCommitValuesGmGm, totalCommitValuesGmEq, hedges.correction = TRUE)
        # effectSizeResultGmGmEqPm <- cohen.d(totalCommitValuesGmGm, totalCommitValuesPmEq, hedges.correction = TRUE) 
        # effectSizeResultGmGmEqEq <- cohen.d(totalCommitValuesGmGm, totalCommitValuesEqEq, hedges.correction = TRUE)
        # 
        # effectSizeResultEqGmEqPm <- cohen.d(totalCommitValuesGmEq, totalCommitValuesPmEq, hedges.correction = TRUE)
        # effectSizeResultEqGmEqEq <- cohen.d(totalCommitValuesGmEq, totalCommitValuesEqEq, hedges.correction = TRUE)
        # 
        # effectSizeResultEqPmEqEq <- cohen.d(totalCommitValuesPmEq, totalCommitValuesEqEq, hedges.correction = TRUE)
        # 
        # 
        # latestRow <- nrow(comparisonTable) + 1
        # comparisonTable[latestRow + 1, 1] <- metricList[mt]
        # comparisonTable[latestRow + 1, 2] <- "PmPm vs GmGm"
        # comparisonTable[latestRow + 1, 3] <- "PmPm vs EqGm"
        # comparisonTable[latestRow + 1, 4] <- "PmPm vs EqPm"
        # comparisonTable[latestRow + 1, 5] <- "PmPm vs EqEq"
        # 
        # comparisonTable[latestRow + 1, 6] <- "GmGm vs EqGm"
        # comparisonTable[latestRow + 1, 7] <- "GmGm vs EqPm"
        # comparisonTable[latestRow + 1, 8] <- "GmGm vs EqEq"
        # 
        # comparisonTable[latestRow + 1, 9] <- "EqGm vs EqPm"
        # comparisonTable[latestRow + 1, 10] <- "EqGm vs EqEq"
        # 
        # comparisonTable[latestRow + 1, 11] <- "EqPm vs EqEq"
        # 
        # 
        # comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
        # comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
        # comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
        # comparisonTable[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
        # comparisonTable[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
        # comparisonTable[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
        # comparisonTable[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
        # comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
        # comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
        # comparisonTable[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
        # 
        # comparisonTable[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
        # comparisonTable[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
        # comparisonTable[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
        # 
        # comparisonTable[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
        # comparisonTable[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
        # 
        # comparisonTable[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
      }   
      
      # xlsx::write.xlsx(x = comparisonTable,
      #                  file = paste(plotOutputDir, "BothNBRF_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
      #                  sheetName = "Total commit values",
      #                  col.names = FALSE, row.names = FALSE)
      
      #########################
      
      # buggy commit ratios ####    
      comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- projectDeveloperGroupsPMvsGM.NB[, metricList[mt]]
        devGroupsRF <- projectDeveloperGroupsPMvsGM.RF[, metricList[mt]]
        
        # PM & PM
        authorNamesPmPm <- projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "PM")), "Developer"]
        totalCommitValuesPmPm <- numeric(length = 0L)
        buggyCommitRatiosPmPm <- numeric(length = 0L)
        for(aName in authorNamesPmPm){
          totalCommitValuesPmPm <- c(totalCommitValuesPmPm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPmPm <- c(buggyCommitRatiosPmPm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # GM & GM
        authorNamesGmGm <- projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "GM")), "Developer"]
        totalCommitValuesGmGm <- numeric(length = 0L)
        buggyCommitRatiosGmGm <- numeric(length = 0L)
        for(aName in authorNamesGmGm){
          totalCommitValuesGmGm <- c(totalCommitValuesGmGm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGmGm <- c(buggyCommitRatiosGmGm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - & GM or GM & -
        authorNamesGmEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "GM")), "Developer"])
        authorNamesGmEq <- c(authorNamesGmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "GM"), which(devGroupsRF == "-")), "Developer"]))
        totalCommitValuesGmEq <- numeric(length = 0L)
        buggyCommitRatiosGmEq <- numeric(length = 0L)
        for(aName in authorNamesGmEq){
          totalCommitValuesGmEq <- c(totalCommitValuesGmEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGmEq <- c(buggyCommitRatiosGmEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - & PM or PM & -
        authorNamesPmEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "PM")), "Developer"])
        authorNamesPmEq <- c(authorNamesPmEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "PM"), which(devGroupsRF == "-")), "Developer"]))
        totalCommitValuesPmEq <- numeric(length = 0L)
        buggyCommitRatiosPmEq <- numeric(length = 0L)
        for(aName in authorNamesPmEq){
          totalCommitValuesPmEq <- c(totalCommitValuesPmEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPmEq <- c(buggyCommitRatiosPmEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - & - 
        authorNamesEqEq <- as.character(projectDeveloperGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"])
        authorNamesEqEq <- c(authorNamesEqEq, as.character(developerGroupsPMvsGM.NB[intersect(which(devGroupsNB == "-"), which(devGroupsRF == "-")), "Developer"]))
        totalCommitValuesEqEq <- numeric(length = 0L)
        buggyCommitRatiosEqEq <- numeric(length = 0L)
        for(aName in authorNamesEqEq){
          totalCommitValuesEqEq <- c(totalCommitValuesEqEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosEqEq <- c(buggyCommitRatiosEqEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        boxplot(buggyCommitRatiosPmPm, buggyCommitRatiosGmGm, buggyCommitRatiosGmEq, buggyCommitRatiosPmEq, buggyCommitRatiosEqEq,
                cex.axis = 1.5, cex.lab = 1.5, names = c("PM PM", "GM GM", "- GM", "- PM", "- -"), outline = FALSE)
        title(ylab = paste("Buggy commit ratios", metricList[mt]))
        
        # mannWhitneyUResultPmPmGmGm <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosGmGm)
        # mannWhitneyUResultPmPmEqGm <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosGmEq) 
        # mannWhitneyUResultPmPmEqPm <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosPmEq) 
        # mannWhitneyUResultPmPmEqEq <- wilcox.test(buggyCommitRatiosPmPm, buggyCommitRatiosEqEq) 
        # 
        # mannWhitneyUResultGmGmEqGm <- wilcox.test(buggyCommitRatiosGmGm, buggyCommitRatiosGmEq) 
        # mannWhitneyUResultGmGmEqPm <- wilcox.test(buggyCommitRatiosGmGm, buggyCommitRatiosPmEq) 
        # mannWhitneyUResultGmGmEqEq <- wilcox.test(buggyCommitRatiosGmGm, buggyCommitRatiosEqEq) 
        # 
        # mannWhitneyUResultEqGmEqPm <- wilcox.test(buggyCommitRatiosGmEq, buggyCommitRatiosPmEq) 
        # mannWhitneyUResultEqGmEqEq <- wilcox.test(buggyCommitRatiosGmEq, buggyCommitRatiosEqEq) 
        # 
        # mannWhitneyUResultEqPmEqEq <- wilcox.test(buggyCommitRatiosPmEq, buggyCommitRatiosEqEq) 
        # 
        # effectSizeResultPmPmGmGm <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosGmGm, hedges.correction = TRUE)
        # effectSizeResultPmPmEqGm <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosGmEq, hedges.correction = TRUE)
        # effectSizeResultPmPmEqPm <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosPmEq, hedges.correction = TRUE)
        # effectSizeResultPmPmEqEq <- cohen.d(buggyCommitRatiosPmPm, buggyCommitRatiosEqEq, hedges.correction = TRUE)
        # 
        # effectSizeResultGmGmEqGm <- cohen.d(buggyCommitRatiosGmGm, buggyCommitRatiosGmEq, hedges.correction = TRUE)
        # effectSizeResultGmGmEqPm <- cohen.d(buggyCommitRatiosGmGm, buggyCommitRatiosPmEq, hedges.correction = TRUE) 
        # effectSizeResultGmGmEqEq <- cohen.d(buggyCommitRatiosGmGm, buggyCommitRatiosEqEq, hedges.correction = TRUE)
        # 
        # effectSizeResultEqGmEqPm <- cohen.d(buggyCommitRatiosGmEq, buggyCommitRatiosPmEq, hedges.correction = TRUE)
        # effectSizeResultEqGmEqEq <- cohen.d(buggyCommitRatiosGmEq, buggyCommitRatiosEqEq, hedges.correction = TRUE)
        # 
        # effectSizeResultEqPmEqEq <- cohen.d(buggyCommitRatiosPmEq, buggyCommitRatiosEqEq, hedges.correction = TRUE)
        # 
        # 
        # latestRow <- nrow(comparisonTable) + 1
        # comparisonTable[latestRow + 1, 1] <- metricList[mt]
        # comparisonTable[latestRow + 1, 2] <- "PmPm vs GmGm"
        # comparisonTable[latestRow + 1, 3] <- "PmPm vs EqGm"
        # comparisonTable[latestRow + 1, 4] <- "PmPm vs EqPm"
        # comparisonTable[latestRow + 1, 5] <- "PmPm vs EqEq"
        # 
        # comparisonTable[latestRow + 1, 6] <- "GmGm vs EqGm"
        # comparisonTable[latestRow + 1, 7] <- "GmGm vs EqPm"
        # comparisonTable[latestRow + 1, 8] <- "GmGm vs EqEq"
        # 
        # comparisonTable[latestRow + 1, 9] <- "EqGm vs EqPm"
        # comparisonTable[latestRow + 1, 10] <- "EqGm vs EqEq"
        # 
        # comparisonTable[latestRow + 1, 11] <- "EqPm vs EqEq"
        # 
        # 
        # comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmPmGmGm$p.value, 5)
        # comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmPmEqGm$p.value, 5)
        # comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultPmPmEqPm$p.value, 5)
        # comparisonTable[latestRow + 2, 5] <- round(mannWhitneyUResultPmPmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 2, 6] <- round(mannWhitneyUResultGmGmEqGm$p.value, 5)
        # comparisonTable[latestRow + 2, 7] <- round(mannWhitneyUResultGmGmEqPm$p.value, 5)
        # comparisonTable[latestRow + 2, 8] <- round(mannWhitneyUResultGmGmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 2, 9] <- round(mannWhitneyUResultEqGmEqPm$p.value, 5)
        # comparisonTable[latestRow + 2, 10] <- round(mannWhitneyUResultEqGmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 2, 11] <- round(mannWhitneyUResultEqPmEqEq$p.value, 5)
        # 
        # comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmPmGmGm$estimate, 3), effectSizeResultPmPmGmGm$magnitude)
        # comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmPmEqGm$estimate, 3), effectSizeResultPmPmEqGm$magnitude)
        # comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultPmPmEqPm$estimate, 3), effectSizeResultPmPmEqPm$magnitude)
        # comparisonTable[latestRow + 3, 5] <- paste(round(effectSizeResultPmPmEqEq$estimate, 3), effectSizeResultPmPmEqEq$magnitude)
        # 
        # comparisonTable[latestRow + 3, 6] <- paste(round(effectSizeResultGmGmEqGm$estimate, 3), effectSizeResultGmGmEqGm$magnitude)
        # comparisonTable[latestRow + 3, 7] <- paste(round(effectSizeResultGmGmEqPm$estimate, 3), effectSizeResultGmGmEqPm$magnitude)
        # comparisonTable[latestRow + 3, 8] <- paste(round(effectSizeResultGmGmEqEq$estimate, 3), effectSizeResultGmGmEqEq$magnitude)
        # 
        # comparisonTable[latestRow + 3, 9] <- paste(round(effectSizeResultEqGmEqPm$estimate, 3), effectSizeResultEqGmEqPm$magnitude)
        # comparisonTable[latestRow + 3, 10] <- paste(round(effectSizeResultEqGmEqEq$estimate, 3), effectSizeResultEqGmEqEq$magnitude)
        # 
        # comparisonTable[latestRow + 3, 11] <- paste(round(effectSizeResultEqPmEqEq$estimate, 3), effectSizeResultEqPmEqEq$magnitude)
      }
      
      # xlsx::write.xlsx(x = comparisonTable,
      #                  file = paste(plotOutputDir, "BothNBRF_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
      #                  sheetName = "Buggy commit ratios",
      #                  col.names = FALSE, row.names = FALSE,
      #                  append = TRUE)
      #########################
      
      dev.off()
    }
  }
  
  
  if(metricValuesHeatMap){
    developerGroupsPMvsGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroupsPMvsGM.RF <- xlsx::read.xlsx(paste(plotOutputDir, "RF_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    
    allSelectedAuthorsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectChanges <- cbind(get(paste(projectName, "Changes", sep = "")), PROJECT = projectName)
      allSelectedAuthorsChanges <- rbind(allSelectedAuthorsChanges, projectChanges[projectChanges$AUTHOR_NAME %in% authorNames, ])
    }
    
    authorsMetrics <- allSelectedAuthorsChanges[, c("PROJECT", "AUTHOR_MASKED", processMetrics)] 
    authorsMetrics$AUTHOR <- paste(authorsMetrics$PROJECT, authorsMetrics$AUTHOR_MASKED)
    authorsMetrics <- aggregate(authorsMetrics[, processMetrics], by = list(AUTHOR = authorsMetrics$AUTHOR), FUN = mean)
    
    for(r in 1:nrow(authorsMetrics)){
      projectName <- strsplit(x = authorsMetrics[r, "AUTHOR"],split = " ")[[1]][1]
      authorAlias <- strsplit(x = authorsMetrics[r, "AUTHOR"],split = " ")[[1]][2]
      
      authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
      
      authorName <- authorInfoTable[authorInfoTable$Alias == authorAlias, "Author"]
      
      commitCountOfAuthor <- authorInfoTable[authorInfoTable$Alias == authorAlias, "CommitCount"]
      buggyCommitRatioOfAuthor <- round(x = authorInfoTable[authorInfoTable$Alias == authorAlias, "BuggyCommitRatio"], digits = 2)
      devGroupAccordingToNB <- developerGroupsPMvsGM.NB[which(developerGroupsPMvsGM.NB$Developer == as.character(authorName)), "pd"]
      devGroupAccordingToRF <- developerGroupsPMvsGM.RF[which(developerGroupsPMvsGM.RF$Developer == as.character(authorName)), "pd"]
      
      authorsMetrics[r, "AUTHOR"] <- paste(authorsMetrics[r, "AUTHOR"], 
                                           commitCountOfAuthor,
                                           buggyCommitRatioOfAuthor, 
                                           devGroupAccordingToNB,
                                           devGroupAccordingToRF)
    }
    
    authorsMetricsMatrix <- authorsMetrics
    rownames(authorsMetricsMatrix) = authorsMetricsMatrix$AUTHOR #make.names(authorsMetricsMatrix$AUTHOR, unique = T)
    authorsMetricsMatrix = authorsMetricsMatrix %>% select(-AUTHOR) %>% as.matrix()
    
    
    aheatmap(authorsMetricsMatrix, color = "-RdBu:50", scale = "col", breaks = 0,
             annRow = authorsMetrics["AUTHOR"], annColors = "Set2", 
             distfun = "pearson", treeheight = c(200, 50), 
             fontsize = 13, cexCol = .7, 
             filename = paste(plotOutputDir, "mean_heatmap.pdf", sep = ""), width = 10, height = 26)
  }
  ################################ #
  
  # group developers (PM vs SM/GM) ####
  if(groupDevelopersByPMvsSMGM){
    for(alg in algorithms){
      # create developer groups ####
      developerGroupsPMvsSMGM <- data.frame(matrix(ncol = 2 + length(metricList), nrow = 0))
      colnames(developerGroupsPMvsSMGM) <- c("Project", "Developer", metricList)
      
      for(p in 1:length(projectList)){
        projectName <- projectList[p]
        loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
        
        for(a in 1:length(authorNames)){
          if(experimentType == "MixTime")
            authorName <- authorNames[a]
          else
            authorName <- authorAliases[a]
          
          developerGroupsPMvsSMGM[nrow(developerGroupsPMvsSMGM) + 1, "Project"] <- projectName
          developerGroupsPMvsSMGM[nrow(developerGroupsPMvsSMGM), "Developer"] <- authorName
          
          resultsDir <- paste(alg, "_Dir", sep = "")
          
          resultsPM <- read.csv(file = paste(get(resultsDir), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE)
          resultsSM <- read.csv(file = paste(get(resultsDir), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE)
          resultsGM <- read.csv(file = paste(get(resultsDir), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE)
          
          for(m in 1:length(metricList)){
            if(metricList[m] == "pf") 
              higherIsBetter <- 0
            else
              higherIsBetter <- 1
            
            resultPM <- resultsPM[, metricList[m]]
            resultSM <- resultsSM[, metricList[m]]
            resultGM <- resultsGM[, metricList[m]]
            
            # ikili yada uclu karsilastirma yapinca farkli sonuclar cikabiliyor
            # nemenyiResult.PMvsGM <- posthoc.friedman.nemenyi.test(cbind(resultPM, resultGM))
            # nemenyiResult.PMvsSM <- posthoc.friedman.nemenyi.test(cbind(resultPM, resultSM))

            compResults <- posthoc.friedman.nemenyi.test(as.matrix(cbind(resultPM, resultSM, resultGM)))            
            #compResults matrix pVals:
            ##    PM    SM    GM    
            #SM   [1,1] NA    NA   
            #GM   [2,1] [2,2] NA  
            
            nemenyiResult.PMvsGM <- compResults$p.value[2, 1]
            nemenyiResult.PMvsSM <- compResults$p.value[1, 1]
            
            # effectSizeResult <- cohen.d(c(resultPM, resultGM), rep(c("PM","GM"), each = length(resultPM)), hedges.correction=TRUE)
            # paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
            
            pmBeatGm <- gmBeatPm <- pmBeatSm <- smBeatPm <- pmEqualGm <- pmEqualSm <- FALSE
            
            if(nemenyiResult.PMvsGM < pVal){
              if(medianChangeNaToZero(resultPM) > medianChangeNaToZero(resultGM))
                if(higherIsBetter)
                  pmBeatGm <- TRUE
                else
                  gmBeatPm <- TRUE
              else
                if(higherIsBetter)
                  gmBeatPm <- TRUE
                else
                  pmBeatGm <- TRUE
            }
            else
              pmEqualGm <- TRUE
            
            if(nemenyiResult.PMvsSM < pVal){
              if(medianChangeNaToZero(resultPM) > medianChangeNaToZero(resultSM))
                if(higherIsBetter)
                  pmBeatSm <- TRUE
                else
                  smBeatPm <- TRUE
              else
                if(higherIsBetter)
                  smBeatPm <- TRUE
                else
                  pmBeatSm <- TRUE
            }
            else
              pmEqualSm <- TRUE
            
            # PM outperforms at least one traditional model
            if(pmBeatGm | pmBeatSm)
              developerGroupsPMvsSMGM[nrow(developerGroupsPMvsSMGM), metricList[m]] <- "PM"
            # PM performs worse than both traditional models
            if(gmBeatPm & smBeatPm)
              developerGroupsPMvsSMGM[nrow(developerGroupsPMvsSMGM), metricList[m]] <- "Traditional"
            # PM performs worse than one traditional model but equals to other
            if((pmEqualGm & smBeatPm) | (pmEqualSm & gmBeatPm))
              developerGroupsPMvsSMGM[nrow(developerGroupsPMvsSMGM), metricList[m]] <- "Traditional"
            # PM performs equal to both traditional models
            if(pmEqualGm & pmEqualSm)
              developerGroupsPMvsSMGM[nrow(developerGroupsPMvsSMGM), metricList[m]] <- "-"
          }
        }
      }
      
      xlsx::write.xlsx(developerGroupsPMvsSMGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups", row.names = F, col.names = T, showNA = FALSE)
      #############################
      
      # count developers of each group ####
      developerCountsPMvsSMGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(developerCountsPMvsSMGM) <- c("", "PM > SM & PM > GM", "PM < SM & PM < GM", "PM = GM")
      
      for(m in 1:length(metricList)){
        developerCountsPMvsSMGM[m, 1] <- metricList[m]
        developerCountsPMvsSMGM[m, 2] <- length(which(developerGroupsPMvsSMGM[, metricList[m]] == "PM"))
        developerCountsPMvsSMGM[m, 3] <- length(which(developerGroupsPMvsSMGM[, metricList[m]] == "Traditional"))
        developerCountsPMvsSMGM[m, 4] <- length(which(developerGroupsPMvsSMGM[, metricList[m]] == "-"))
      }
      xlsx::write.xlsx(developerCountsPMvsSMGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "dev.counts", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      ####################################
      
      # commit counts of each group (min-mean-median-max) ####
      commitCountsPMvsSMGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(commitCountsPMvsSMGM) <- c("", "PM > SM/GM", "PM < SM/GM", "PM = SM/GM")
      buggyRatiosPMvsSMGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(buggyRatiosPMvsSMGM) <- c("", "PM > SM/GM", "PM < SM/GM", "PM = SM/GM")
      
      for(m in 1:length(metricList)){
        commitCountsPMvsSMGM[m, 1] <- metricList[m]
        buggyRatiosPMvsSMGM[m, 1] <- metricList[m]
        
        pmDevs <- developerGroupsPMvsSMGM[which(developerGroupsPMvsSMGM[, metricList[m]] == "PM"), ]
        gmDevs <- developerGroupsPMvsSMGM[which(developerGroupsPMvsSMGM[, metricList[m]] == "Traditional"), ]
        eqDevs <- developerGroupsPMvsSMGM[which(developerGroupsPMvsSMGM[, metricList[m]] == "-"), ]
        
        commitCountsOfPm <- numeric(length = 0L)
        commitCountsOfGm <- numeric(length = 0L)
        commitCountsOfEq <- numeric(length = 0L)
        
        buggyRatiosOfPm <- numeric(length = 0L)
        buggyRatiosOfGm <- numeric(length = 0L)
        buggyRatiosOfEq <- numeric(length = 0L)
        
        if(experimentType == "MixTime"){
          for(r in 1:nrow(pmDevs)){
            devName <- pmDevs[r, "Developer"]
            projectName <- pmDevs[r, "Project"]
            
            authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
            commitCountsOfPm <- c(commitCountsOfPm, authorInfoTable[authorInfoTable$Author == devName, "CommitCount"])
            buggyRatiosOfPm <- c(buggyRatiosOfPm, authorInfoTable[authorInfoTable$Author == devName, "BuggyCommitRatio"])
          }
          
          for(r in 1:nrow(gmDevs)){
            devName <- gmDevs[r, "Developer"]
            projectName <- gmDevs[r, "Project"]
            
            authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
            commitCountsOfGm <- c(commitCountsOfGm, authorInfoTable[authorInfoTable$Author == devName, "CommitCount"])
            buggyRatiosOfGm <- c(buggyRatiosOfGm, authorInfoTable[authorInfoTable$Author == devName, "BuggyCommitRatio"])
          }   
          
          for(r in 1:nrow(eqDevs)){
            devName <- eqDevs[r, "Developer"]
            projectName <- eqDevs[r, "Project"]
            
            authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
            commitCountsOfEq <- c(commitCountsOfEq, authorInfoTable[authorInfoTable$Author == devName, "CommitCount"])
            buggyRatiosOfEq <- c(buggyRatiosOfEq, authorInfoTable[authorInfoTable$Author == devName, "BuggyCommitRatio"])
          }
        }
        else{
          if(nrow(pmDevs) > 0)
            for(r in 1:nrow(pmDevs)){
              devName <- pmDevs[r, "Developer"]
              projectName <- pmDevs[r, "Project"]
              
              authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
              commitCountsOfPm <- c(commitCountsOfPm, authorInfoTable[authorInfoTable$Alias == devName, "CommitCount"])
              buggyRatiosOfPm <- c(buggyRatiosOfPm, authorInfoTable[authorInfoTable$Alias == devName, "BuggyCommitRatio"])
            }
          
          if(nrow(gmDevs) > 0)
            for(r in 1:nrow(gmDevs)){
              devName <- gmDevs[r, "Developer"]
              projectName <- gmDevs[r, "Project"]
              
              authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
              commitCountsOfGm <- c(commitCountsOfGm, authorInfoTable[authorInfoTable$Alias == devName, "CommitCount"])
              buggyRatiosOfGm <- c(buggyRatiosOfGm, authorInfoTable[authorInfoTable$Alias == devName, "BuggyCommitRatio"])
            }   
          
          if(nrow(eqDevs) > 0)
            for(r in 1:nrow(eqDevs)){
            devName <- eqDevs[r, "Developer"]
            projectName <- eqDevs[r, "Project"]
            
            authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
            commitCountsOfEq <- c(commitCountsOfEq, authorInfoTable[authorInfoTable$Alias == devName, "CommitCount"])
            buggyRatiosOfEq <- c(buggyRatiosOfEq, authorInfoTable[authorInfoTable$Alias == devName, "BuggyCommitRatio"])
          } 
        }
        
        commitCountsPMvsSMGM[m, 2] <- paste(round(min(commitCountsOfPm), 2), round(mean(commitCountsOfPm), 2), round(median(commitCountsOfPm), 2), round(max(commitCountsOfPm), 2), sep = " - ")
        commitCountsPMvsSMGM[m, 3] <- paste(round(min(commitCountsOfGm), 2), round(mean(commitCountsOfGm), 2), round(median(commitCountsOfGm), 2), round(max(commitCountsOfGm), 2), sep = " - ")
        commitCountsPMvsSMGM[m, 4] <- paste(round(min(commitCountsOfEq), 2), round(mean(commitCountsOfEq), 2), round(median(commitCountsOfEq), 2), round(max(commitCountsOfEq), 2), sep = " - ")
        
        buggyRatiosPMvsSMGM[m, 2] <- paste(round(min(buggyRatiosOfPm), 2), round(mean(buggyRatiosOfPm), 2), round(median(buggyRatiosOfPm), 2), round(max(buggyRatiosOfPm), 2), sep = " - ")
        buggyRatiosPMvsSMGM[m, 3] <- paste(round(min(buggyRatiosOfGm), 2), round(mean(buggyRatiosOfGm), 2), round(median(buggyRatiosOfGm), 2), round(max(buggyRatiosOfGm), 2), sep = " - ")
        buggyRatiosPMvsSMGM[m, 4] <- paste(round(min(buggyRatiosOfEq), 2), round(mean(buggyRatiosOfEq), 2), round(median(buggyRatiosOfEq), 2), round(max(buggyRatiosOfEq), 2), sep = " - ")
      }
      xlsx::write.xlsx(commitCountsPMvsSMGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "commit.counts", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      xlsx::write.xlsx(buggyRatiosPMvsSMGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "buggy.ratios", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      #######################################################
      
      # prediction performances of each group ####
      predictionPerformancePMvsSMGM <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(predictionPerformancePMvsSMGM) <- c("", "PM > SM/GM", "PM < SM/GM", "PM = SM/GM")
      
      resultsDir <- paste(alg, "_Dir", sep = "")
      
      for(m in 1:length(metricList)){
        predictionPerformancePMvsSMGM[m, 1] <- metricList[m]
        
        pmDevs <- developerGroupsPMvsSMGM[which(developerGroupsPMvsSMGM[, metricList[m]] == "PM"), ]
        gmDevs <- developerGroupsPMvsSMGM[which(developerGroupsPMvsSMGM[, metricList[m]] == "Traditional"), ]
        eqDevs <- developerGroupsPMvsSMGM[which(developerGroupsPMvsSMGM[, metricList[m]] == "-"), ]
        
        performancesOfPm <- numeric(length = 0L)
        performancesOfGm <- numeric(length = 0L)
        performancesOfEq <- numeric(length = 0L)
        
        for(r in 1:nrow(pmDevs)){
          devName <- pmDevs[r, "Developer"]
          projectName <- pmDevs[r, "Project"]
          
          performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), projectName, "/results/", devName, "ResultsPM.csv", sep = ""), header = TRUE)
          performancesOfPm <- c(performancesOfPm, performanceResultsOfAuthor[, metricList[m]])
        }
        
        for(r in 1:nrow(gmDevs)){
          devName <- gmDevs[r, "Developer"]
          projectName <- gmDevs[r, "Project"]
          
          performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), projectName, "/results/", devName, "ResultsPM.csv", sep = ""), header = TRUE)
          performancesOfGm <- c(performancesOfGm, performanceResultsOfAuthor[, metricList[m]])
        }
        
        for(r in 1:nrow(eqDevs)){
          devName <- eqDevs[r, "Developer"]
          projectName <- eqDevs[r, "Project"]
          
          performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), projectName, "/results/", devName, "ResultsPM.csv", sep = ""), header = TRUE)
          performancesOfEq <- c(performancesOfEq, performanceResultsOfAuthor[, metricList[m]])
        }
        
        predictionPerformancePMvsSMGM[m, 2] <- paste(round(min(performancesOfPm, na.rm = T), 2), round(mean(performancesOfPm, na.rm = T), 2), round(median(performancesOfPm, na.rm = T), 2), round(max(performancesOfPm, na.rm = T), 2), sep = " - ")
        predictionPerformancePMvsSMGM[m, 3] <- paste(round(min(performancesOfGm, na.rm = T), 2), round(mean(performancesOfGm, na.rm = T), 2), round(median(performancesOfGm, na.rm = T), 2), round(max(performancesOfGm, na.rm = T), 2), sep = " - ")
        predictionPerformancePMvsSMGM[m, 4] <- paste(round(min(performancesOfEq, na.rm = T), 2), round(mean(performancesOfEq, na.rm = T), 2), round(median(performancesOfEq, na.rm = T), 2), round(max(performancesOfEq, na.rm = T), 2), sep = " - ")
      }
      xlsx::write.xlsx(predictionPerformancePMvsSMGM, file = paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "performances", row.names = F, col.names = T, showNA = FALSE, append = TRUE)
      ###########################################
    }
  }
  
  if(infoGainByDeveloperGroups.Density){
    totalMetricCount <- 13 
    alg <- "NB"
    
    infoGainRanks <- xlsx::read.xlsx(paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "ranks")
    developerGroupsPMvsGM <- read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    
    developerGroups <- c("PM", "Traditional", "-")
    
    # dir.create(path = paste(plotOutputDir, "InfoGainDensity_GroupBased/", sep = ""), showWarnings = FALSE, recursive = TRUE)
    
    metricList <- c("pd", "pf", "F1")
    
    # pdf(file = paste(plotOutputDir, "InfoGainDensity_GroupBased.pdf", sep = ""), width = 23, height = 27)
    # par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    pdf(file = paste(plotOutputDir, alg, "_InfoGainDensity_GroupBased_PdPfF1.pdf", sep = ""), width = 23, height = 15)
    par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(mt in 1:length(metricList)){
      for(gr in 1:length(developerGroups)){
        infoGainRankCountsGroupBased <- data.frame(matrix(ncol = totalMetricCount, nrow = totalMetricCount))
        colnames(infoGainRankCountsGroupBased) <- processMetrics
        rownames(infoGainRankCountsGroupBased) <- c(paste("Rank", rep(1:totalMetricCount), sep = ""))
        
        authorNames <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == developerGroups[gr], ]$Developer
        groupRanks <- infoGainRanks[infoGainRanks$Developer %in% authorNames, ]
        
        for(m in 1:length(processMetrics)){
          for(r in 1:length(processMetrics)){
            infoGainRankCountsGroupBased[paste("Rank", r, sep = ""), processMetrics[m]] <- length(which(groupRanks[, paste("Rank", r, sep = "")] == processMetrics[m]))
          }
        }
        
        colnames(infoGainRankCountsGroupBased) <- toupper(colnames(infoGainRankCountsGroupBased))
        rownames(infoGainRankCountsGroupBased) <- c(paste("Rank", rep(1:totalMetricCount), sep = " "))
        
        if(mt == 1 & gr == 1){
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = developerGroups[gr], las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
          title(ylab = metricList[mt], line = 4, cex.lab = 1.2)
        }
        else if (mt == 1)
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = developerGroups[gr], las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
        else if(gr == 1){
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = "", las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
          title(ylab = metricList[mt], line = 4, cex.lab = 1.2)
        }
        else
          plot(as.matrix(infoGainRankCountsGroupBased), xlab = "", ylab = "", main = "", las = 1, digits = 0, text.cell = list(cex = 0.8), col = brewer.pal(n = 9, name = "Blues"))
      }
    }
    dev.off()
  }
  
  if(infoGainByDeveloperGroups.SK){
    alg <- "NB"
    # alg <- "RF"
    
    infoGainOrders <- xlsx::read.xlsx(paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "orders")
    developerGroupsPMvsSMGM <- read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    
    developerGroups <- c("PM", "Traditional", "-")
    metricList <- c("pd", "pf", "F1")
    
    # pdf(file = paste(plotOutputDir, "InfoGainDensity_GroupBased.pdf", sep = ""), width = 23, height = 27)
    # par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    pdf(file = paste(plotOutputDir, alg, "_InfoGainRanks_GroupBased_SK_PdPfF1.pdf", sep = ""), width = 23, height = 15)
    par(mfrow = c(length(metricList), length(developerGroups)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
    
    for(mt in 1:length(metricList)){
      for(gr in 1:length(developerGroups)){
        authorNames <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == developerGroups[gr], ]$Developer
        groupOrders <- infoGainOrders[infoGainOrders$Developer %in% authorNames, ]
        
        modelCompMatrix <- as.matrix(groupOrders[, processMetrics])
        modelCompResults <- sk_esd(modelCompMatrix)
        rownames(modelCompResults$m.inf) <- toupper(rownames(modelCompResults$m.inf))
        rownames(modelCompResults$m.inf) <- gsub(pattern = "NFC", replacement = "NPC", x = rownames(modelCompResults$m.inf), fixed = T)
        # modelCompResults$m.inf <- modelCompResults$m.inf[nrow(modelCompResults$m.inf):1, ]
        if(developerGroups[gr] == "-")
          title <- paste("PM = SM/GM (", metricList[mt], ")", sep = "")
        else if (developerGroups[gr] == "Traditional")
          title <- paste("PM < SM/GM (", metricList[mt], ")", sep = "")
        else if (developerGroups[gr] == "PM")
          title <- paste("PM > SM/GM (", metricList[mt], ")", sep = "")
        plot(modelCompResults, ylim = c(0, 13), ylab = "Rank", las = 2, xlab = "", title = title)
      }
    }
    dev.off()
  }
  
  
  if(metricValuesComparisonByDeveloperGroups.OnlyNB){
    alg <- "NB"
    # alg <- "RF"
    metricList <- c("pd", "pf", "F1")
    # metricList <- c("pd", "pf", "precision", "F1", "AUC", "MCC", "BScore")
    # processMetrics <- c("ADD", "DEL", "CHURN", "Ent", "ND", "NS","NF", "NDEV", "AGE", "NFC", "EXP", "REXP", "SEXP")
    processMetrics <- c("NDEV", "NFC", "EXP", "REXP", "SEXP")
    writeExcel <- FALSE
    
    if(experimentType == "MixTime")
      auhtorNameOrMask <- "AUTHOR_NAME"
    else
      auhtorNameOrMask <- "AUTHOR_MASKED"
    
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "Traditional", "-")
    
    allProjectsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      allProjectsChanges <- rbind(allProjectsChanges, get(paste(projectName, "Changes", sep = "")))
    }
    
    pdf(file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_Plot_SMGM_5Metrics.pdf", sep = ""), width = 17, height = 30)
    # pdf(file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_Plot_SMGM.pdf", sep = ""), width = 20, height = 40)
    par(mfrow = c(length(processMetrics), length(metricList)), mar = c(6, 8, 5, 1), oma = c(6, 8, 1, 1), cex.lab = 3, cex.axis = 3) 
    
    for(m in 1:length(processMetrics)){
      processMetricComparison <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        # PM 
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
        pmValues <- allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% authorNames, processMetrics[m]]
        # GM 
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "Traditional", ]$Developer
        gmValues <- allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% authorNames, processMetrics[m]]
        # - 
        authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
        equalValues <- allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% authorNames, processMetrics[m]]
        
        numberOfPlots <- length(pmValues) + length(gmValues) + length(equalValues)
        
        if(numberOfPlots > 0){
          # if(mt == 1){
          #   boxplot(pmValues, gmValues, equalValues, cex.axis = 2.5, cex.lab = 2.5, las = 2, #horizontal = T,
          #           names = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"), outline = FALSE)
          # }
          # else{
          #   boxplot(pmValues, gmValues, equalValues, cex.axis = 2.5, cex.lab = 2.5, horizontal = T, las = 1, outline = FALSE)
          # }
          # if(processMetrics[m] == "NFC")
          #   title(xlab = "NPC", line = 4)
          # else
          #   title(xlab = processMetrics[m], line = 4)
          # if(m == 1)
          #   title(main = metricList[mt], cex.main = 2.5)
          
          
          if(m == 5){
            boxplot(pmValues, gmValues, equalValues, xaxt = "n",
                    cex.axis = 3, cex.lab = 3, names = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"), outline = FALSE)
            text(x = 1:3 + 0.7,
                 ## Move labels to just below bottom of chart.
                 y = par("usr")[3] - 0.45,
                 ## Use names from the data list.
                 labels = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"),
                 ## Change the clipping region.
                 xpd = NA,
                 ## Rotate the labels by 35 degrees.
                 srt = 35,
                 ## Adjust the labels to almost 100% right-justified.
                 adj = 1.2,
                 ## Increase label size.
                 cex = 3)
          }
          else
            boxplot(pmValues, gmValues, equalValues, xaxt = "n",
                    cex.axis = 3, cex.lab = 3, outline = FALSE)
          if(m == 1)
            title(metricList[mt], cex.main = 3, line = 3, font.main = 1)
          
          if(mt == 1)
            title(ylab = ifelse(processMetrics[m] == "NFC", "NPC", processMetrics[m]), line = 5, cex.lab = 3, font.main = 1)
          
          
          # Mann-Whitney U Test
          
          mannWhitneyUResultPmGm <- wilcox.test(pmValues, gmValues) 
          mannWhitneyUResultPmEq <- wilcox.test(pmValues, equalValues) 
          mannWhitneyUResultGmEq <- wilcox.test(gmValues, equalValues) 
          effectSizeResultPmGm <- cohen.d(pmValues, gmValues, hedges.correction = TRUE)
          effectSizeResultPmEq <- cohen.d(pmValues, equalValues, hedges.correction = TRUE)
          effectSizeResultGmEq <- cohen.d(gmValues, equalValues, hedges.correction = TRUE)
          
          latestRow <- nrow(processMetricComparison) + 1
          processMetricComparison[latestRow + 1, 1] <- metricList[mt]
          processMetricComparison[latestRow + 1, 2] <- "Pm vs Gm"
          processMetricComparison[latestRow + 1, 3] <- "Pm vs Eq"
          processMetricComparison[latestRow + 1, 4] <- "Gm vs Eq"
          processMetricComparison[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
          processMetricComparison[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
          processMetricComparison[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5) 
          processMetricComparison[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
          processMetricComparison[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
          processMetricComparison[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
          
          
          # Dunn's test
          
          # values <- c(pmValues, gmValues, equalValues)
          # grps <- as.factor(c(rep("PM", length(pmValues)), rep("GM", length(gmValues)), rep("-", length(equalValues))))
          # 
          # dunnResult <- posthoc.kruskal.dunn.test(values, grps, p.adjust="bonf")
          # 
          # latestRow <- nrow(processMetricComparison) + 1
          # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
          # processMetricComparison[latestRow + 1, 2:4] <- c("", colnames(dunnResult$p.value))
          # processMetricComparison[(latestRow + 1):(latestRow + 3), 2] <- c("", rownames(dunnResult$p.value))
          # processMetricComparison[(latestRow + 2):(latestRow + 3), 3:4] <- dunnResult$p.value
        }
        else
          boxplot(0, cex.axis = 1.5, cex.lab = 1.5, names = c("NO"))
      }
      
      testName <- "MannWhitneyUTest" # DunnTest
      
      if(writeExcel){
        if(m == 1)
          xlsx::write.xlsx(x = processMetricComparison,
                           file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_", testName, "_SMGM.xlsx", sep = ""),
                           sheetName = processMetrics[m],
                           col.names = FALSE, row.names = FALSE)
        else
          xlsx::write.xlsx(x = processMetricComparison,
                           file = paste(plotOutputDir, alg, "_MetricValues_GroupBased_", testName, "_SMGM.xlsx", sep = ""),
                           sheetName = processMetrics[m],
                           col.names = FALSE, row.names = FALSE,
                           append = TRUE)
      }
    }
    dev.off()
  }
  
  if(commitCountsComparisonByDeveloperGroups.OnlyNB){
    alg <- "NB"
    # alg <- "RF"
    metricList <- c("pd", "pf", "F1")
    # metricList <- c("pd", "pf", "precision", "F1", "AUC", "MCC", "BScore")
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsSMGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "Traditional", "-")
    
    if(experimentType == "MixTime")
      auhtorNameOrMask <- "AUTHOR_NAME"
    else
      auhtorNameOrMask <- "AUTHOR_MASKED"
    
    allProjectsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      allProjectsChanges <- rbind(allProjectsChanges, get(paste(projectName, "Changes", sep = "")))
    }
    
    # total commit values ####
    pdf(file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_Plot_SMGM.pdf", sep = ""), width = 17, height = 7)
    par(mfrow = c(1, length(metricList)), mar = c(6, 7, 5, 1), oma = c(6, 6, 4, 1), cex.lab = 3, cex.axis = 3)
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsSMGM[, metricList[mt]]
      
      # PM 
      authorNamesPm <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "PM", ]$Developer
      totalCommitValuesPm <- numeric(length = 0L)
      buggyCommitRatiosPm <- numeric(length = 0L)
      for(aName in authorNamesPm){
        totalCommitValuesPm <- c(totalCommitValuesPm, nrow(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, ]))
        buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # Traditional
      authorNamesGm <- developerGroupsPMvsSMGM[which(devGroupsNB == "Traditional"), "Developer"]
      totalCommitValuesGm <- numeric(length = 0L)
      buggyCommitRatiosGm <- numeric(length = 0L)
      for(aName in authorNamesGm){
        totalCommitValuesGm <- c(totalCommitValuesGm, nrow(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, ]))
        buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # - 
      authorNamesEq <- as.character(developerGroupsPMvsSMGM[which(devGroupsNB == "-"), "Developer"])
      totalCommitValuesEq <- numeric(length = 0L)
      buggyCommitRatiosEq <- numeric(length = 0L)
      for(aName in authorNamesEq){
        totalCommitValuesEq <- c(totalCommitValuesEq, nrow(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, ]))
        buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))      }

      # if(mt == 1)
      #   boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq, horizontal = T, las = 1,
      #           cex.axis = 2.5, cex.lab = 2.5, names = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"), outline = FALSE)
      # else
      #   boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq, horizontal = T, las = 1,
      #           cex.axis = 2.5, cex.lab = 2.5, outline = FALSE)
      # title(metricList[mt], cex.main = 2.5, line = 2)

      boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq, xaxt = "n",
              cex.axis = 3, cex.lab = 3, names = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"), outline = FALSE)
      if(mt == 1)
        title(ylab = "Number of commits", line = 5, cex.lab = 3)
      title(metricList[mt], cex.main = 3, line = 3, font.main = 1)
      text(x = 1:3 + 0.7,
           ## Move labels to just below bottom of chart.
           y = par("usr")[3] - 0.45,
           ## Use names from the data list.
           labels = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"),
           ## Change the clipping region.
           xpd = NA,
           ## Rotate the labels by 35 degrees.
           srt = 35,
           ## Adjust the labels to almost 100% right-justified.
           adj = 1.2,
           ## Increase label size.
           cex = 3)
      

      # if(mt == 1){
      #   boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq,
      #           horizontal = TRUE, las = 2,
      #           cex.axis = 1.5, cex.lab = 1.5, names = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"), outline = FALSE)
      #   title(xlab = "Number of commits", line = 4)
      #   title(main = metricList[mt])
      # }else{
      #   boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq,
      #           horizontal = TRUE, las = 2,
      #           cex.axis = 1.5, cex.lab = 1.5, outline = FALSE)
      #   title(xlab = "Number of commits", line = 4)
      #   title(main = metricList[mt])
      # }
      
      mannWhitneyUResultPmGm <- wilcox.test(totalCommitValuesPm, totalCommitValuesGm) 
      mannWhitneyUResultPmEq <- wilcox.test(totalCommitValuesPm, totalCommitValuesEq) 
      mannWhitneyUResultGmEq <- wilcox.test(totalCommitValuesGm, totalCommitValuesEq) 
      effectSizeResultPmGm <- cohen.d(totalCommitValuesPm, totalCommitValuesGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(totalCommitValuesPm, totalCommitValuesEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(totalCommitValuesGm, totalCommitValuesEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_", testName, "_SMGM.xlsx", sep = ""),
                     sheetName = "Total commit values",
                     col.names = FALSE, row.names = FALSE)
    dev.off()
    #########################
    
    # buggy commit ratios ####    
    pdf(file = paste(plotOutputDir, alg, "_BuggyRatios_GroupBased_Plot_SMGM.pdf", sep = ""), width = length(metricList)*2, height = 3)
    par(mfrow = c(1, length(metricList)), mar = c(2, 3, 3, 0), oma = c(1, 2, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      devGroupsNB <- developerGroupsPMvsSMGM[, metricList[mt]]
      
      # PM
      authorNamesPm <- developerGroupsPMvsSMGM[which(devGroupsNB == "PM"), "Developer"]
      totalCommitValuesPm <- numeric(length = 0L)
      buggyCommitRatiosPm <- numeric(length = 0L)
      for(aName in authorNamesPm){
        totalCommitValuesPm <- c(totalCommitValuesPm, nrow(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, ]))
        buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # Traditional
      authorNamesGm <- developerGroupsPMvsSMGM[which(devGroupsNB == "Traditional"), "Developer"]
      totalCommitValuesGm <- numeric(length = 0L)
      buggyCommitRatiosGm <- numeric(length = 0L)
      for(aName in authorNamesGm){
        totalCommitValuesGm <- c(totalCommitValuesGm, nrow(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, ]))
        buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      # -
      authorNamesEq <- as.character(developerGroupsPMvsSMGM[which(devGroupsNB == "-"), "Developer"])
      totalCommitValuesEq <- numeric(length = 0L)
      buggyCommitRatiosEq <- numeric(length = 0L)
      for(aName in authorNamesEq){
        totalCommitValuesEq <- c(totalCommitValuesEq, nrow(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, ]))
        buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(allProjectsChanges[allProjectsChanges[, auhtorNameOrMask] %in% aName, "CLASS"] == "BUG")) / nrow(allProjectsChanges[allProjectsChanges$AUTHOR_NAME %in% aName, ]))
      }
      
      boxplot(buggyCommitRatiosPm, buggyCommitRatiosGm, buggyCommitRatiosEq,
              cex.axis = 0.9, cex.lab = 1.0, names = c("PM", "Traditional", "-"), outline = FALSE)
      title(ylab = "Buggy commit ratios", metricList[mt])
      
      # if(mt == 1){
      #   boxplot(buggyCommitRatiosPm, buggyCommitRatiosGm, buggyCommitRatiosEq,
      #           horizontal = TRUE, las = 2,
      #           cex.axis = 1.5, cex.lab = 1.5, names = c("PM > SM/GM", "PM < SM/GM", "PM = SM/GM"), outline = FALSE)
      #   title(xlab = "Buggy commit ratios", line = 4)
      #   title(main = metricList[mt])
      # }else{
      #   boxplot(buggyCommitRatiosPm, buggyCommitRatiosGm, buggyCommitRatiosEq,
      #           horizontal = TRUE, las = 2,
      #           cex.axis = 1.5, cex.lab = 1.5, outline = FALSE)
      #   title(xlab = "Buggy commit ratios", line = 4)
      #   title(main = metricList[mt])
      # }
      
      mannWhitneyUResultPmGm <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosGm) 
      mannWhitneyUResultPmEq <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosEq) 
      mannWhitneyUResultGmEq <- wilcox.test(buggyCommitRatiosGm, buggyCommitRatiosEq) 
      effectSizeResultPmGm <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(buggyCommitRatiosGm, buggyCommitRatiosEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_BuggyRatios_GroupBased_", testName, "_SMGM.xlsx", sep = ""),
                     sheetName = "Buggy commit ratios",
                     col.names = FALSE, row.names = FALSE,
                     append = TRUE)
    dev.off()
    #########################
  }
  
  if(performancesComparisonByDeveloperGroups.OnlyNB.PM){
    alg <- "NB"
    metricList <- c("pd", "pf", "F1")
    resultsDir <- paste(alg, "_Dir", sep = "")
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsSMGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "Traditional", "-")
    
    pdf(file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_Plot_PM_SMGM.pdf", sep = ""), width = 10, height = 4)
    par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      # PM 
      authorNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
      projectNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Project
      performanceValuesPm <- numeric(length = 0L)
      for(a in 1:length(authorNamesPm)){
        aName <- authorNamesPm[a]
        pName <- projectNamesPm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesPm <- c(performanceValuesPm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # Traditional
      authorNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "Traditional", ]$Developer
      projectNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "Traditional", ]$Project
      performanceValuesGm <- numeric(length = 0L)
      for(a in 1:length(authorNamesGm)){
        aName <- authorNamesGm[a]
        pName <- projectNamesGm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesGm <- c(performanceValuesGm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - 
      authorNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
      projectNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Project
      performanceValuesEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesEq)){
        aName <- authorNamesEq[a]
        pName <- projectNamesEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)
        performanceValuesEq <- c(performanceValuesEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      boxplot(performanceValuesPm, performanceValuesGm, performanceValuesEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "Traditional", "-"), outline = FALSE)
      title(ylab = paste("Performance values", metricList[mt]))
      
      mannWhitneyUResultPmGm <- wilcox.test(performanceValuesPm, performanceValuesGm) 
      mannWhitneyUResultPmEq <- wilcox.test(performanceValuesPm, performanceValuesEq) 
      mannWhitneyUResultGmEq <- wilcox.test(performanceValuesGm, performanceValuesEq) 
      effectSizeResultPmGm <- cohen.d(performanceValuesPm, performanceValuesGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(performanceValuesPm, performanceValuesEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(performanceValuesGm, performanceValuesEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_", testName, "_PM_SMGM.xlsx", sep = ""),
                     sheetName = "Performance values",
                     col.names = FALSE, row.names = FALSE)
    
    dev.off()
  }
  
  if(performancesComparisonByDeveloperGroups.OnlyNB.GM){
    alg <- "NB"
    metricList <- c("pd", "pf", "F1")
    resultsDir <- paste(alg, "_Dir", sep = "")
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsSMGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "Traditional", "-")
    
    pdf(file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_Plot_GM_SMGM.pdf", sep = ""), width = 10, height = 4)
    par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
    
    for(mt in 1:length(metricList)){
      # PM 
      authorNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
      projectNamesPm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Project
      performanceValuesPm <- numeric(length = 0L)
      for(a in 1:length(authorNamesPm)){
        aName <- authorNamesPm[a]
        pName <- projectNamesPm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesPm <- c(performanceValuesPm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # Traditional
      authorNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "Traditional", ]$Developer
      projectNamesGm <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "Traditional", ]$Project
      performanceValuesGm <- numeric(length = 0L)
      for(a in 1:length(authorNamesGm)){
        aName <- authorNamesGm[a]
        pName <- projectNamesGm[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesGm <- c(performanceValuesGm, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      # - 
      authorNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
      projectNamesEq <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Project
      performanceValuesEq <- numeric(length = 0L)
      for(a in 1:length(authorNamesEq)){
        aName <- authorNamesEq[a]
        pName <- projectNamesEq[a]
        performanceResultsOfAuthor <- read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)
        performanceValuesEq <- c(performanceValuesEq, performanceResultsOfAuthor[, metricList[mt]])
      }
      
      boxplot(performanceValuesPm, performanceValuesGm, performanceValuesEq,
              cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "Traditional", "-"), outline = FALSE)
      title(ylab = paste("Performance values", metricList[mt]))
      
      mannWhitneyUResultPmGm <- wilcox.test(performanceValuesPm, performanceValuesGm) 
      mannWhitneyUResultPmEq <- wilcox.test(performanceValuesPm, performanceValuesEq) 
      mannWhitneyUResultGmEq <- wilcox.test(performanceValuesGm, performanceValuesEq) 
      effectSizeResultPmGm <- cohen.d(performanceValuesPm, performanceValuesGm, hedges.correction = TRUE)
      effectSizeResultPmEq <- cohen.d(performanceValuesPm, performanceValuesEq, hedges.correction = TRUE)
      effectSizeResultGmEq <- cohen.d(performanceValuesGm, performanceValuesEq, hedges.correction = TRUE)
      
      latestRow <- nrow(comparisonTable) + 1
      comparisonTable[latestRow + 1, 1] <- metricList[mt]
      comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
      comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
      comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
      comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
      comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
      comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
      comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
      comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
      comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
    }   
    
    xlsx::write.xlsx(x = comparisonTable,
                     file = paste(plotOutputDir, alg, "_Performances_GroupBased_AllProjects_", testName, "GM_SMGM.xlsx", sep = ""),
                     sheetName = "Performance values",
                     col.names = FALSE, row.names = FALSE)
    
    dev.off()
  }
  
  if(performancesComparisonByDeveloperGroups.OnlyNB.PMSMGM){
    alg <- "NB"
    # alg <- "RF"
    metricList <- c("pd", "pf", "F1")
    resultsDir <- paste(alg, "_Dir", sep = "")
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsSMGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "Traditional", "-")
    
    pdf(file = paste(plotOutputDir, alg, "_Performances_GroupBased_Plot_PMSMGM.pdf", sep = ""), width = 13, height = 4)
    par(mfrow = c(1, length(metricList)), mar = c(5, 4, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(mt in 1:length(metricList)){
      # PM 
      authorNamesPm <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "PM", ]$Developer
      projectNamesPm <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "PM", ]$Project
      performanceValuesPm.PM <- performanceValuesPm.SM <- performanceValuesPm.GM <- numeric(length = 0L)
      for(a in 1:length(authorNamesPm)){
        aName <- authorNamesPm[a]
        pName <- projectNamesPm[a]
        performanceValuesPm.PM <- c(performanceValuesPm.PM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)[, metricList[mt]])
        performanceValuesPm.SM <- c(performanceValuesPm.SM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsSM.csv", sep = ""), header = TRUE)[, metricList[mt]])
        performanceValuesPm.GM <- c(performanceValuesPm.GM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)[, metricList[mt]])
      }
      
      # Traditional
      authorNamesGm <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "Traditional", ]$Developer
      projectNamesGm <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "Traditional", ]$Project
      performanceValuesGm.PM <- performanceValuesGm.SM <- performanceValuesGm.GM <- numeric(length = 0L)
      for(a in 1:length(authorNamesGm)){
        aName <- authorNamesGm[a]
        pName <- projectNamesGm[a]
        performanceValuesGm.PM <- c(performanceValuesGm.PM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)[, metricList[mt]])
        performanceValuesGm.SM <- c(performanceValuesGm.SM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsSM.csv", sep = ""), header = TRUE)[, metricList[mt]])
        performanceValuesGm.GM <- c(performanceValuesGm.GM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)[, metricList[mt]])
      }
      
      # - 
      authorNamesEq <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "-", ]$Developer
      projectNamesEq <- developerGroupsPMvsSMGM[developerGroupsPMvsSMGM[, metricList[mt]] == "-", ]$Project
      performanceValuesEq.PM <- performanceValuesEq.SM <- performanceValuesEq.GM <- numeric(length = 0L)
      for(a in 1:length(authorNamesEq)){
        aName <- authorNamesEq[a]
        pName <- projectNamesEq[a]
        performanceValuesEq.PM <- c(performanceValuesEq.PM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsPM.csv", sep = ""), header = TRUE)[, metricList[mt]])
        performanceValuesEq.SM <- c(performanceValuesEq.SM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsSM.csv", sep = ""), header = TRUE)[, metricList[mt]])
        performanceValuesEq.GM <- c(performanceValuesEq.GM, 
                                    read.csv(file = paste(get(resultsDir), pName, "/results/", aName, "ResultsGM.csv", sep = ""), header = TRUE)[, metricList[mt]])
      }
      
      boxplot(performanceValuesPm.PM, performanceValuesPm.SM, performanceValuesPm.GM,
              performanceValuesGm.PM, performanceValuesGm.SM, performanceValuesGm.GM,
              performanceValuesEq.PM, performanceValuesEq.SM, performanceValuesEq.GM, 
              at = c(1,2,3,5,6,7,9,10,11),
              cex.axis = 0.9, cex.lab = 1.1, names = c("PM", "SM", "GM", "PM", "SM", "GM","PM", "SM", "GM"), outline = FALSE)
              # cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "", "", "Traditional", "", "", "-", "", ""), outline = FALSE)
      title(ylab = "Performance values")
      title(xlab = "PM > SM/GM              PM < SM/GM               PM = SM/GM")
      title(main = metricList[mt])
    }
    dev.off()
  }
  
  
  # not modified
  if(metricValuesComparisonByDeveloperGroups.OnlyNB.eachProject){
    dir.create(path = paste(plotOutputDir, "ForEachProject", sep = ""), showWarnings = FALSE, recursive = TRUE)
    
    alg <- "NB"
    # metricList <- c("pd", "pf", "F1")
    
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectsChanges <- get(paste(projectName, "Changes", sep = ""))
      
      pdf(file = paste(plotOutputDir, "ForEachProject/", alg, "_MetricValues_GroupBased_", projectName, "_Plot.pdf", sep = ""), width = 23, height = 30)
      par(mfrow = c(length(processMetrics), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      for(m in 1:length(processMetrics)){
        processMetricComparison <- as.data.frame(matrix(ncol = 0, nrow = 0))
        
        for(mt in 1:length(metricList)){
          # PM 
          authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
          pmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
          # GM 
          authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "GM", ]$Developer
          gmValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
          # - 
          authorNames <- developerGroupsPMvsGM[developerGroupsPMvsGM[, metricList[mt]] == "-", ]$Developer
          equalValues <- projectsChanges[projectsChanges$AUTHOR_NAME %in% authorNames, processMetrics[m]]
          
          numberOfPlots <- length(pmValues) + length(gmValues) + length(equalValues)
          
          if(numberOfPlots > 0){
            boxplot(pmValues, gmValues, equalValues, cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
            title(ylab = paste(processMetrics[m], metricList[mt]))
            
            # Mann-Whitney U Test
            # 
            # mannWhitneyUResultPmGm <- wilcox.test(pmValues, gmValues) 
            # mannWhitneyUResultPmEq <- wilcox.test(pmValues, equalValues) 
            # mannWhitneyUResultGmEq <- wilcox.test(gmValues, equalValues) 
            # effectSizeResultPmGm <- cohen.d(pmValues, gmValues, hedges.correction = TRUE)
            # effectSizeResultPmEq <- cohen.d(pmValues, equalValues, hedges.correction = TRUE)
            # effectSizeResultGmEq <- cohen.d(gmValues, equalValues, hedges.correction = TRUE)
            # 
            # latestRow <- nrow(processMetricComparison) + 1
            # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
            # processMetricComparison[latestRow + 1, 2] <- "Pm vs Gm"
            # processMetricComparison[latestRow + 1, 3] <- "Pm vs Eq"
            # processMetricComparison[latestRow + 1, 4] <- "Gm vs Eq"
            # processMetricComparison[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
            # processMetricComparison[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
            # processMetricComparison[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5) 
            # processMetricComparison[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
            # processMetricComparison[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
            # processMetricComparison[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
            
            
            # Dunn's test
            
            # values <- c(pmValues, gmValues, equalValues)
            # grps <- as.factor(c(rep("PM", length(pmValues)), rep("GM", length(gmValues)), rep("-", length(equalValues))))
            # 
            # dunnResult <- posthoc.kruskal.dunn.test(values, grps, p.adjust="bonf")
            # 
            # latestRow <- nrow(processMetricComparison) + 1
            # processMetricComparison[latestRow + 1, 1] <- metricList[mt]
            # processMetricComparison[latestRow + 1, 2:4] <- c("", colnames(dunnResult$p.value))
            # processMetricComparison[(latestRow + 1):(latestRow + 3), 2] <- c("", rownames(dunnResult$p.value))
            # processMetricComparison[(latestRow + 2):(latestRow + 3), 3:4] <- dunnResult$p.value
          }
          else
            boxplot(0, cex.axis = 1.5, cex.lab = 1.5, names = c("NO"))
        }
        
        # testName <- "MannWhitneyUTest" # DunnTest
        # 
        # if(m == 1)
        #   xlsx::write.xlsx(x = processMetricComparison,
        #                    file = paste(plotOutputDir, "ForEachProject/", alg, "_MetricValues_GroupBased_", projectName, "_", testName, ".xlsx", sep = ""),
        #                    sheetName = processMetrics[m],
        #                    col.names = FALSE, row.names = FALSE)
        # else
        #   xlsx::write.xlsx(x = processMetricComparison,
        #                    file = paste(plotOutputDir, "ForEachProject/", alg, "_MetricValues_GroupBased_", projectName, "_", testName, ".xlsx", sep = ""),
        #                    sheetName = processMetrics[m],
        #                    col.names = FALSE, row.names = FALSE,
        #                    append = TRUE)
      }
      dev.off()
    }
  }
  # not modified
  if(commitCountsComparisonByDeveloperGroups.OnlyNB.eachProject){
    alg <- "NB"
    testName <- "MannWhitneyUTest" # DunnTest
    developerGroupsPMvsGM <- xlsx::read.xlsx(paste(plotOutputDir, alg, "_developerGroupsPMvsGM.xlsx", sep = ""), sheetName = "groups")
    developerGroups <- c("PM", "GM", "-")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectsChanges <- get(paste(projectName, "Changes", sep = ""))
      
      projectDeveloperGroupsPMvsGM <- developerGroupsPMvsGM[which(developerGroupsPMvsGM$Project == projectName), ]
      
      pdf(file = paste(plotOutputDir, "ForEachProject/", alg, "_CommitCounts_GroupBased_", projectName, "_Plot.pdf", sep = ""), width = 35, height = 10)
      par(mfrow = c(2, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      # total commit values ####
      comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- projectDeveloperGroupsPMvsGM[, metricList[mt]]
        
        # PM 
        authorNamesPm <- projectDeveloperGroupsPMvsGM[projectDeveloperGroupsPMvsGM[, metricList[mt]] == "PM", ]$Developer
        totalCommitValuesPm <- numeric(length = 0L)
        buggyCommitRatiosPm <- numeric(length = 0L)
        for(aName in authorNamesPm){
          totalCommitValuesPm <- c(totalCommitValuesPm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # GM
        authorNamesGm <- projectDeveloperGroupsPMvsGM[which(devGroupsNB == "GM"), "Developer"]
        totalCommitValuesGm <- numeric(length = 0L)
        buggyCommitRatiosGm <- numeric(length = 0L)
        for(aName in authorNamesGm){
          totalCommitValuesGm <- c(totalCommitValuesGm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # - 
        authorNamesEq <- as.character(projectDeveloperGroupsPMvsGM[which(devGroupsNB == "-"), "Developer"])
        totalCommitValuesEq <- numeric(length = 0L)
        buggyCommitRatiosEq <- numeric(length = 0L)
        for(aName in authorNamesEq){
          totalCommitValuesEq <- c(totalCommitValuesEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        boxplot(totalCommitValuesPm, totalCommitValuesGm, totalCommitValuesEq,
                cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
        title(ylab = paste("Total commit values", metricList[mt]))
        
        # mannWhitneyUResultPmGm <- wilcox.test(totalCommitValuesPm, totalCommitValuesGm) 
        # mannWhitneyUResultPmEq <- wilcox.test(totalCommitValuesPm, totalCommitValuesEq) 
        # mannWhitneyUResultGmEq <- wilcox.test(totalCommitValuesGm, totalCommitValuesEq) 
        # effectSizeResultPmGm <- cohen.d(totalCommitValuesPm, totalCommitValuesGm, hedges.correction = TRUE)
        # effectSizeResultPmEq <- cohen.d(totalCommitValuesPm, totalCommitValuesEq, hedges.correction = TRUE)
        # effectSizeResultGmEq <- cohen.d(totalCommitValuesGm, totalCommitValuesEq, hedges.correction = TRUE)
        # 
        # latestRow <- nrow(comparisonTable) + 1
        # comparisonTable[latestRow + 1, 1] <- metricList[mt]
        # comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
        # comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
        # comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
        # comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
        # comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
        # comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
        # comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
        # comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
        # comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
      }   
      
      # xlsx::write.xlsx(x = comparisonTable,
      #                  file = paste(plotOutputDir, alg, "_CommitCounts_GroupBased_AllProjects_", testName, ".xlsx", sep = ""),
      #                  sheetName = "Total commit values",
      #                  col.names = FALSE, row.names = FALSE)
      
      #########################
      
      # buggy commit ratios ####    
      comparisonTable <- as.data.frame(matrix(ncol = 0, nrow = 0))
      
      for(mt in 1:length(metricList)){
        devGroupsNB <- projectDeveloperGroupsPMvsGM[, metricList[mt]]
        
        # PM
        authorNamesPm <- projectDeveloperGroupsPMvsGM[which(devGroupsNB == "PM"), "Developer"]
        totalCommitValuesPm <- numeric(length = 0L)
        buggyCommitRatiosPm <- numeric(length = 0L)
        for(aName in authorNamesPm){
          totalCommitValuesPm <- c(totalCommitValuesPm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosPm <- c(buggyCommitRatiosPm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # GM
        authorNamesGm <- projectDeveloperGroupsPMvsGM[which(devGroupsNB == "GM"), "Developer"]
        totalCommitValuesGm <- numeric(length = 0L)
        buggyCommitRatiosGm <- numeric(length = 0L)
        for(aName in authorNamesGm){
          totalCommitValuesGm <- c(totalCommitValuesGm, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosGm <- c(buggyCommitRatiosGm, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        # -
        authorNamesEq <- as.character(projectDeveloperGroupsPMvsGM[which(devGroupsNB == "-"), "Developer"])
        totalCommitValuesEq <- numeric(length = 0L)
        buggyCommitRatiosEq <- numeric(length = 0L)
        for(aName in authorNamesEq){
          totalCommitValuesEq <- c(totalCommitValuesEq, nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
          buggyCommitRatiosEq <- c(buggyCommitRatiosEq, length(which(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, "CLASS"] == "BUG")) / nrow(projectsChanges[projectsChanges$AUTHOR_NAME %in% aName, ]))
        }
        
        boxplot(buggyCommitRatiosPm, buggyCommitRatiosGm, buggyCommitRatiosEq,
                cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "GM", "-"), outline = FALSE)
        title(ylab = paste("Buggy commit ratios", metricList[mt]))
        
        # mannWhitneyUResultPmGm <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosGm) 
        # mannWhitneyUResultPmEq <- wilcox.test(buggyCommitRatiosPm, buggyCommitRatiosEq) 
        # mannWhitneyUResultGmEq <- wilcox.test(buggyCommitRatiosGm, buggyCommitRatiosEq) 
        # effectSizeResultPmGm <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosGm, hedges.correction = TRUE)
        # effectSizeResultPmEq <- cohen.d(buggyCommitRatiosPm, buggyCommitRatiosEq, hedges.correction = TRUE)
        # effectSizeResultGmEq <- cohen.d(buggyCommitRatiosGm, buggyCommitRatiosEq, hedges.correction = TRUE)
        # 
        # latestRow <- nrow(comparisonTable) + 1
        # comparisonTable[latestRow + 1, 1] <- metricList[mt]
        # comparisonTable[latestRow + 1, 2] <- "Pm vs Gm"
        # comparisonTable[latestRow + 1, 3] <- "Pm vs Eq"
        # comparisonTable[latestRow + 1, 4] <- "Gm vs Eq"
        # comparisonTable[latestRow + 2, 2] <- round(mannWhitneyUResultPmGm$p.value, 5)
        # comparisonTable[latestRow + 2, 3] <- round(mannWhitneyUResultPmEq$p.value, 5)
        # comparisonTable[latestRow + 2, 4] <- round(mannWhitneyUResultGmEq$p.value, 5)
        # comparisonTable[latestRow + 3, 2] <- paste(round(effectSizeResultPmGm$estimate, 3), effectSizeResultPmGm$magnitude)
        # comparisonTable[latestRow + 3, 3] <- paste(round(effectSizeResultPmEq$estimate, 3), effectSizeResultPmEq$magnitude)
        # comparisonTable[latestRow + 3, 4] <- paste(round(effectSizeResultGmEq$estimate, 3), effectSizeResultGmEq$magnitude)
      }
      
      # xlsx::write.xlsx(x = comparisonTable,
      #                  file = paste(plotOutputDir, "ForEachProject/", alg, "_CommitCounts_GroupBased_", projectName, "_", testName, ".xlsx", sep = ""),
      #                  sheetName = "Buggy commit ratios",
      #                  col.names = FALSE, row.names = FALSE,
      #                  append = TRUE)
      #########################
      
      dev.off()
    }
  }
  
  if(metricValuesHeatMap){
    developerGroupsPMvsSMGM.NB <- xlsx::read.xlsx(paste(plotOutputDir, "NB_developerGroupsPMvsSMGM.xlsx", sep = ""), sheetName = "groups")
    
    allSelectedAuthorsChanges <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      projectChanges <- cbind(get(paste(projectName, "Changes", sep = "")), PROJECT = projectName)
      allSelectedAuthorsChanges <- rbind(allSelectedAuthorsChanges, projectChanges[projectChanges$AUTHOR_NAME %in% authorNames, ])
    }
    
    authorsMetrics <- allSelectedAuthorsChanges[, c("PROJECT", "AUTHOR_MASKED", processMetrics)] 
    authorsMetrics$AUTHOR <- paste(authorsMetrics$PROJECT, authorsMetrics$AUTHOR_MASKED)
    authorsMetrics <- aggregate(authorsMetrics[, processMetrics], by = list(AUTHOR = authorsMetrics$AUTHOR), FUN = mean)
    
    for(r in 1:nrow(authorsMetrics)){
      projectName <- strsplit(x = authorsMetrics[r, "AUTHOR"],split = " ")[[1]][1]
      authorAlias <- strsplit(x = authorsMetrics[r, "AUTHOR"],split = " ")[[1]][2]
      
      authorInfoTable <- get(paste(projectName, "SelectedAuthors", sep = ""))
      
      authorName <- authorInfoTable[authorInfoTable$Alias == authorAlias, "Author"]
      
      commitCountOfAuthor <- authorInfoTable[authorInfoTable$Alias == authorAlias, "CommitCount"]
      buggyCommitRatioOfAuthor <- round(x = authorInfoTable[authorInfoTable$Alias == authorAlias, "BuggyCommitRatio"], digits = 2)
      devGroupAccordingToNB <- developerGroupsPMvsSMGM.NB[which(developerGroupsPMvsSMGM.NB$Developer == as.character(authorName)), "pd"]
    
      authorsMetrics[r, "AUTHOR"] <- paste(authorsMetrics[r, "AUTHOR"], 
                                           commitCountOfAuthor,
                                           buggyCommitRatioOfAuthor, 
                                           devGroupAccordingToNB)
    }
    
    authorsMetricsMatrix <- authorsMetrics
    rownames(authorsMetricsMatrix) = authorsMetricsMatrix$AUTHOR #make.names(authorsMetricsMatrix$AUTHOR, unique = T)
    authorsMetricsMatrix = authorsMetricsMatrix %>% select(-AUTHOR) %>% as.matrix()
    
    
    aheatmap(authorsMetricsMatrix, color = "-RdBu:50", scale = "col", breaks = 0,
             annRow = authorsMetrics["AUTHOR"], annColors = "Set2", 
             distfun = "pearson", treeheight = c(200, 50), 
             fontsize = 13, cexCol = .7, 
             filename = paste(plotOutputDir, "mean_heatmap_SMGM.pdf", sep = ""), width = 10, height = 26)
  }
  #################################

}
