library(xlsx)
library(FSelector)
library(plot.matrix)
library(RColorBrewer)
library(reshape2)
library(Hmisc)
library(ScottKnottESD)

# library(openxlsx)
# detach("package:openxlsx", unload = TRUE)

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime" #SeqTime, MixTime
baseExperimentSignature <- "MixTime"
experimentSignature <- "MixTime_InfoGain"
samplingOption <- "WO" #WITH, WO
testSetMonths <- ""
useIntervalDataOrPreparedTrainTestData <- "P"
setwd(mainProjectDir) 

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

# loadDataProcessingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, experimentSignature, samplingOption)
loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature = experimentSignature, samplingOption)

# totalMetricCount <- 18 #(latent features 5 rank)
# totalMetricCount <- 23 #10 topics
totalMetricCount <- 13 

applyFSelector <- FALSE
analyzeResults <- FALSE
analyzeWekaResults <- FALSE
applyLogFilter <- FALSE #inside the applyFSelector
prepareTrainTestDataFiles <- FALSE
prepareTrainTestDataSelectTop5Weka <- FALSE

applyFSelector.Without10x10CV <- FALSE
applyFSelector.With10x10CV <- FALSE
sk.InfoGainRanks.Without10x10CV <- FALSE

columnsToBeRemoved <- c(1,2,3,4,5,9)


if(applyFSelector.Without10x10CV){
  if(experimentType == "MixTime"){
    infoGainRanks <- data.frame(matrix(ncol = totalMetricCount + 3, nrow = 0))
    colnames(infoGainRanks) <- c("Project", "Developer", "Model", paste("Rank", rep(1:totalMetricCount), sep = ""))
   
    infoGainOrders <- data.frame(matrix(ncol = totalMetricCount + 3, nrow = 0))
    colnames(infoGainOrders) <- c("Project", "Developer", "Model", processMetrics)

    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      gmChanges <- get(paste(projectName, "Changes", sep = ""))
      smChanges <- data.frame()
      
      for(a in 1:numberOfSelectedDev){
        authorName <- authorNames[a]
        
        print(paste("info gain evaluation: ", projectName, " - ", authorName, sep = ""))
        
        pmChanges <- gmChanges[gmChanges$AUTHOR_NAME == authorName, ]
        smChanges <- rbind(smChanges, pmChanges)
        
        # PM
        pmChanges <- pmChanges[, -columnsToBeRemoved] #only numeric features and class remained
        
        #increase the range of numeric cloumns to avoid error
        numCols <- sapply(pmChanges, is.numeric)
        pmChanges[numCols] <- pmChanges[numCols] * 1000000
        
        weights <- information.gain(CLASS~., pmChanges)
        # weights <- information.gain(CLASS~., pmChanges, unit = "log2")
        # weights <- gain.ratio(CLASS~., pmChanges)
        
        if(all(weights$attr_importance == 0)){
          rankedMetrics <- rownames(weights)[order(weights)]
        } else {
          rankedMetrics <- rownames(weights)[rev(order(weights))]
        }
        
        infoGainRanks[nrow(infoGainRanks) + 1, "Project"] <- projectName
        infoGainRanks[nrow(infoGainRanks), "Developer"] <- authorName
        infoGainRanks[nrow(infoGainRanks), "Model"] <- "PM"
        infoGainRanks[nrow(infoGainRanks), c(1:totalMetricCount) + 3] <- rankedMetrics
        
        infoGainOrders[nrow(infoGainOrders) + 1, "Project"] <- projectName
        infoGainOrders[nrow(infoGainOrders), "Developer"] <- authorName
        infoGainOrders[nrow(infoGainOrders), "Model"] <- "PM"
        for(m in 1:length(processMetrics)){
          infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% processMetrics[m])
        }  
      }
        
      # SM
      print(paste("info gain evaluation: ", projectName, " - ", "SM", sep = ""))
      
      smChanges <- smChanges[, -columnsToBeRemoved] #only numeric features and class remained
      
      #increase the range of numeric cloumns to avoid error
      numCols <- sapply(smChanges, is.numeric)
      smChanges[numCols] <- smChanges[numCols] * 1000000
      
      weights <- information.gain(CLASS~., smChanges)
      # weights <- information.gain(CLASS~., smChanges, unit = "log2")
      # weights <- gain.ratio(CLASS~., smChanges)
      
      if(all(weights$attr_importance == 0)){
        rankedMetrics <- rownames(weights)[order(weights)]
      } else {
        rankedMetrics <- rownames(weights)[rev(order(weights))]
      }
      
      infoGainRanks[nrow(infoGainRanks) + 1, "Project"] <- projectName
      infoGainRanks[nrow(infoGainRanks), "Developer"] <- "SM"
      infoGainRanks[nrow(infoGainRanks), "Model"] <- "SM"
      infoGainRanks[nrow(infoGainRanks), c(1:totalMetricCount) + 3] <- rankedMetrics
      
      infoGainOrders[nrow(infoGainOrders) + 1, "Project"] <- projectName
      infoGainOrders[nrow(infoGainOrders), "Developer"] <- "SM"
      infoGainOrders[nrow(infoGainOrders), "Model"] <- "SM"
      for(m in 1:length(processMetrics)){
        infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% processMetrics[m])
      }
      
      
      # GM
      print(paste("info gain evaluation: ", projectName, " - ", "GM", sep = ""))
      
      gmChanges <- gmChanges[, -columnsToBeRemoved] #only numeric features and class remained
      
      #increase the range of numeric cloumns to avoid error
      numCols <- sapply(gmChanges, is.numeric)
      gmChanges[numCols] <- gmChanges[numCols] * 1000000
      
      weights <- information.gain(CLASS~., gmChanges)
      # weights <- information.gain(CLASS~., gmChanges, unit = "log2")
      # weights <- gain.ratio(CLASS~., gmChanges)
      
      if(all(weights$attr_importance == 0)){
        rankedMetrics <- rownames(weights)[order(weights)]
      } else {
        rankedMetrics <- rownames(weights)[rev(order(weights))]
      }
      
      infoGainRanks[nrow(infoGainRanks) + 1, "Project"] <- projectName
      infoGainRanks[nrow(infoGainRanks), "Developer"] <- "GM"
      infoGainRanks[nrow(infoGainRanks), "Model"] <- "GM"
      infoGainRanks[nrow(infoGainRanks), c(1:totalMetricCount) + 3] <- rankedMetrics
      
      infoGainOrders[nrow(infoGainOrders) + 1, "Project"] <- projectName
      infoGainOrders[nrow(infoGainOrders), "Developer"] <- "GM"
      infoGainOrders[nrow(infoGainOrders), "Model"] <- "GM"
      for(m in 1:length(processMetrics)){
        infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% processMetrics[m])
      }
    }
    
    dir.create(path = paste(plotDir, "MixTime_InfoGain/", samplingOption, sep = ""), showWarnings = FALSE, recursive = TRUE)
    write.xlsx(x = infoGainRanks, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "ranks", row.names = FALSE)
    write.xlsx(x = infoGainOrders, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "orders", row.names = FALSE, append = TRUE)
    
    # PM ranks and density plot
    infoGainRankCountsPM <- data.frame(matrix(ncol = totalMetricCount, nrow = totalMetricCount))
    colnames(infoGainRankCountsPM) <- processMetrics
    rownames(infoGainRankCountsPM) <- c(paste("Rank", rep(1:totalMetricCount), sep = ""))
    
    onlyPmRanks <- infoGainRanks[infoGainRanks$Model == "PM", ]
    
    for(m in 1:length(processMetrics)){
      for(r in 1:length(processMetrics)){
        infoGainRankCountsPM[paste("Rank", r, sep = ""), processMetrics[m]] <- length(which(onlyPmRanks[, paste("Rank", r, sep = "")] == processMetrics[m]))
      }
    }
    
    write.xlsx(x = infoGainRankCountsPM, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "rankCountsPM", row.names = TRUE, append = TRUE)
    
    
    pdf(file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/infoGainRankCountsPM.pdf", sep = ""), height = 6, width = 7)     
    par(mar=c(5.1, 5.1, 4.1, 4.1), cex = 1) # adapt margins
    
    colnames(infoGainRankCountsPM) <- toupper(colnames(infoGainRankCountsPM))
    colnames(infoGainRankCountsPM) <- gsub(pattern = "NFC", replacement = "NPC", x = colnames(infoGainRankCountsPM), fixed = T)
    rownames(infoGainRankCountsPM) <- c(paste("Rank", rep(1:totalMetricCount), sep = "-"))
    
    plot(as.matrix(infoGainRankCountsPM), xlab = "", ylab = "", main = "", las = 2, digits = 0, text.cell = list(cex = 1), col = brewer.pal(n = 9, name = "Greys"))
    
    dev.off()
  }
}

if(applyFSelector.With10x10CV){
  columnsToBeRemoved <- c(1,2,3,4,8)
  
  infoGainRanks <- data.frame(matrix(ncol = totalMetricCount + 5, nrow = 0))
  colnames(infoGainRanks) <- c("Project", "Developer", "Model", "Run", "Fold", paste("Rank", rep(1:totalMetricCount), sep = ""))
  
  infoGainOrders <- data.frame(matrix(ncol = totalMetricCount + 5, nrow = 0))
  colnames(infoGainOrders) <- c("Project", "Developer", "Model", "Run", "Fold", processMetrics)
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
    
    for(a in 1:numberOfSelectedDev){
      authorName <- authorNames[a]
      
      for(r in 1:numberOfRuns){
        for(f in 1:numberOfFolds){
          print(paste("info gain evaluation: ", projectName, " - ", authorName, sep = ""))
          
          fileDir <- paste(trainTestDataDir, projectName, "/", authorName, "/Run", r, "/", sep = "")
          
          # PM
          trainData <- read.csv(file = paste(fileDir, authorName, "TrainPM", f, ".csv", sep = ""), header = TRUE)
          trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
          
          #increase the range of numeric cloumns to avoid error
          numCols <- sapply(trainData, is.numeric)
          trainData[numCols] <- trainData[numCols] * 1000000
          
          weights <- information.gain(CLASS~., trainData)
          # weights <- information.gain(CLASS~., trainData, unit = "log2")
          # weights <- gain.ratio(CLASS~., trainData)
          
          if(all(weights$attr_importance == 0)){
            rankedMetrics <- rownames(weights)[order(weights)]
          } else {
            rankedMetrics <- rownames(weights)[rev(order(weights))]
          }
          
          infoGainRanks[nrow(infoGainRanks) + 1, "Project"] <- projectName
          infoGainRanks[nrow(infoGainRanks), "Developer"] <- authorName
          infoGainRanks[nrow(infoGainRanks), "Model"] <- "PM"
          infoGainRanks[nrow(infoGainRanks), "Run"] <- r
          infoGainRanks[nrow(infoGainRanks), "Fold"] <- f
          infoGainRanks[nrow(infoGainRanks), c(1:totalMetricCount) + 3] <- rankedMetrics
          
          infoGainOrders[nrow(infoGainOrders) + 1, "Project"] <- projectName
          infoGainOrders[nrow(infoGainOrders), "Developer"] <- authorName
          infoGainOrders[nrow(infoGainOrders), "Model"] <- "PM"
          infoGainOrders[nrow(infoGainOrders), "Run"] <- r
          infoGainOrders[nrow(infoGainOrders), "Fold"] <- f
          for(m in 1:length(processMetrics)){
            if(projectName == "Perl")
              infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% paste("X.", processMetrics[m], ".", sep = ""))
            else
              infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% processMetrics[m])
          }
          
          # SM
          trainData <- read.csv(file = paste(fileDir, authorName, "TrainSM", f, ".csv", sep = ""), header = TRUE)
          trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
          
          #increase the range of numeric cloumns to avoid error
          numCols <- sapply(trainData, is.numeric)
          trainData[numCols] <- trainData[numCols] * 1000000
          
          weights <- information.gain(CLASS~., trainData)
          # weights <- information.gain(CLASS~., trainData, unit = "log2")
          # weights <- gain.ratio(CLASS~., trainData)
          
          if(all(weights$attr_importance == 0)){
            rankedMetrics <- rownames(weights)[order(weights)]
          } else {
            rankedMetrics <- rownames(weights)[rev(order(weights))]
          }
          
          infoGainRanks[nrow(infoGainRanks) + 1, "Project"] <- projectName
          infoGainRanks[nrow(infoGainRanks), "Developer"] <- authorName
          infoGainRanks[nrow(infoGainRanks), "Model"] <- "SM"
          infoGainRanks[nrow(infoGainRanks), "Run"] <- r
          infoGainRanks[nrow(infoGainRanks), "Fold"] <- f
          infoGainRanks[nrow(infoGainRanks), c(1:totalMetricCount) + 3] <- rankedMetrics
          
          infoGainOrders[nrow(infoGainOrders) + 1, "Project"] <- projectName
          infoGainOrders[nrow(infoGainOrders), "Developer"] <- authorName
          infoGainOrders[nrow(infoGainOrders), "Model"] <- "SM"
          infoGainOrders[nrow(infoGainOrders), "Run"] <- r
          infoGainOrders[nrow(infoGainOrders), "Fold"] <- f
          for(m in 1:length(processMetrics)){
            if(projectName == "Perl")
              infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% paste("X.", processMetrics[m], ".", sep = ""))
            else
              infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% processMetrics[m])
          }
          
          # GM
          trainData <- read.csv(file = paste(fileDir, authorName, "TrainGM", f, ".csv", sep = ""), header = TRUE)
          trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
          
          #increase the range of numeric cloumns to avoid error
          numCols <- sapply(trainData, is.numeric)
          trainData[numCols] <- trainData[numCols] * 1000000
          
          weights <- information.gain(CLASS~., trainData)
          # weights <- information.gain(CLASS~., trainData, unit = "log2")
          # weights <- gain.ratio(CLASS~., trainData)
          
          if(all(weights$attr_importance == 0)){
            rankedMetrics <- rownames(weights)[order(weights)]
          } else {
            rankedMetrics <- rownames(weights)[rev(order(weights))]
          }
          
          infoGainRanks[nrow(infoGainRanks) + 1, "Project"] <- projectName
          infoGainRanks[nrow(infoGainRanks), "Developer"] <- authorName
          infoGainRanks[nrow(infoGainRanks), "Model"] <- "GM"
          infoGainRanks[nrow(infoGainRanks), "Run"] <- r
          infoGainRanks[nrow(infoGainRanks), "Fold"] <- f
          infoGainRanks[nrow(infoGainRanks), c(1:totalMetricCount) + 3] <- rankedMetrics
          
          infoGainOrders[nrow(infoGainOrders) + 1, "Project"] <- projectName
          infoGainOrders[nrow(infoGainOrders), "Developer"] <- authorName
          infoGainOrders[nrow(infoGainOrders), "Model"] <- "GM"
          infoGainOrders[nrow(infoGainOrders), "Run"] <- r
          infoGainOrders[nrow(infoGainOrders), "Fold"] <- f
          for(m in 1:length(processMetrics)){
            if(projectName == "Perl")
              infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% paste("X.", processMetrics[m], ".", sep = ""))
            else
              infoGainOrders[nrow(infoGainOrders), processMetrics[m]] <- which(rankedMetrics %in% processMetrics[m])
          }
        }
      }
    }
  }
  
  # write.xlsx(x = infoGainRanks, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain_10x10.xlsx", sep = ""), sheetName = "ranks", row.names = FALSE)
  # write.xlsx(x = infoGainOrders, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain_10x10.xlsx", sep = ""), sheetName = "orders", row.names = FALSE, append = TRUE)
  
  openxlsx::write.xlsx(x = infoGainRanks, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain_10x10_ranks.xlsx", sep = ""), sheetName = "ranks", row.names = FALSE)
  openxlsx::write.xlsx(x = infoGainOrders, file = paste(plotDir, "MixTime_InfoGain/", samplingOption, "/InfoGain_10x10_orders.xlsx", sep = ""), sheetName = "orders", row.names = FALSE)
}

if(sk.InfoGainRanks.Without10x10CV){
  infoGainOrders <- read.xlsx(paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain.xlsx", sep = ""), sheetName = "orders")
  
  onlyPmOrders <- infoGainOrders[infoGainOrders$Model == "PM", ]
  
  pdf(file = paste(plotOutputDir, "infoGainRanks_Metrics_SK.pdf", sep = ""), width = 7, height = 7)
  par(mfrow = c(1, 1), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
  
  modelCompMatrix <- as.matrix(onlyPmOrders[, processMetrics])
  modelCompResults <- sk_esd(modelCompMatrix)
  rownames(modelCompResults$m.inf) <- toupper(rownames(modelCompResults$m.inf))
  rownames(modelCompResults$m.inf) <- gsub(pattern = "NFC", replacement = "NPC", x = rownames(modelCompResults$m.inf), fixed = T)
  # modelCompResults$ord <- rev(modelCompResults$ord)
  modelCompResults$groups <- rev(modelCompResults$groups)
  modelCompResults$m.inf <- modelCompResults$m.inf[nrow(modelCompResults$m.inf):1, ]
  plot(modelCompResults, ylim = c(0, 13), ylab = "Rank", las = 2, xlab = "", title = "")
  mtext("Metrics", side = 1, line = 4)
  
  dev.off()
  
  # pdf(file = paste(plotOutputDir, "infoGainRanks_Developers_SK.pdf", sep = ""), width = 30, height = 7)
  # par(mfrow = c(1, 1), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
  # 
  # modelCompMatrix <- t(as.matrix(onlyPmOrders[, processMetrics]))
  # colnames(modelCompMatrix) <- onlyPmOrders$Developer
  # modelCompResults <- sk_esd(modelCompMatrix)
  # plot(modelCompResults, ylim = c(0, 13), ylab = "Rank", las = 2, xlab = "Metrics", title = "")
  # 
  # dev.off()
  
}

if(sk.InfoGainRanks.Devs.10x10CV){
  # infoGainOrders <- read.xlsx(paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain_10x10.xlsx", sep = ""), sheetName = "orders")
  infoGainOrders <- openxlsx::read.xlsx(xlsxFile = paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/InfoGain_10x10.xlsx", sep = ""), sheet = "orders")
  
  onlyPmOrders <- infoGainOrders[infoGainOrders$Model == "PM", ]
  
  loadAllAuthorsInfo(experimentType)
  allSelectedAuthors <- allAuthorsInfo[allAuthorsInfo$Selected == "T" & allAuthorsInfo$projectName != "Rails", ]$Author
  
  modelCompMatrix <- as.data.frame(matrix(nrow = 1300, ncol = length(allSelectedAuthors)))
  
  for(a in 1:length(allSelectedAuthors)){
    authorInfoGainOrders <- onlyPmOrders[onlyPmOrders$Developer == allSelectedAuthors[a], ]
      
    authorOrders <- numeric(length = 0L)
    for(i in 1:nrow(authorInfoGainOrders)){
      authorOrders <- c(authorOrders, as.numeric(authorInfoGainOrders[i, processMetrics]))
    }
    
    modelCompMatrix[, a] <- authorOrders
  }
  
  # pdf(file = paste(plotOutputDir, "infoGain_Devs_SK.pdf", sep = ""), width = 7, height = 7)
  # par(mfrow = c(1, 1), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
  # 
  modelCompMatrix <- as.matrix(modelCompMatrix)
  colnames(modelCompMatrix) <- allSelectedAuthors
  modelCompResults <- sk_esd(modelCompMatrix)
  plot(modelCompResults)
  # 
  # dev.off()
}

if(applyFSelector){
  if(experimentType == "MixedTime"){
    allInfoGainResults <- data.frame(matrix(ncol = totalMetricCount + 5, nrow = 0))
    colnames(allInfoGainResults)[1:5] <- c("Project", "Developer", "Model", "Iteration", "Fold")
    
    for(p in 1:length(projectList)) {
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      for(a in 1:numberOfSelectedDev){
        authorName <- authorNames[a]
        
        for(r in 1:numberOfRuns){
          for(f in 1:numberOfFolds){
            print(paste("info gain evaluation: ", projectName, " - ", authorName, " - Run: ", r, " - Fold: ", f, sep = ""))
            
            fileDir <- paste(inputDataDir, "/", projectName, "/", authorName, "/Run", r, "/", sep = "")
            
            # PM
            trainData <- read.csv(file = paste(fileDir, authorName, "TrainPM", f, ".csv", sep = ""), header = TRUE)
            trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
            
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainData, is.numeric)  
            trainData[numCols] <- trainData[numCols] * 1000000 
            
            weights <- information.gain(CLASS~., trainData)
            # weights <- information.gain(CLASS~., trainData, unit = "log2")
            # weights <- gain.ratio(CLASS~., trainData)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
            allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "PM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
            
            
            # SM
            trainData <- read.csv(file = paste(fileDir, authorName, "TrainSM", f, ".csv", sep = ""), header = TRUE)
            trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
            
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainData, is.numeric)  
            trainData[numCols] <- trainData[numCols] * 1000000 
            
            weights <- information.gain(CLASS~., trainData)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
            allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "SM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
            
            
            # GM
            trainData <- read.csv(file = paste(fileDir, authorName, "TrainGM", f, ".csv", sep = ""), header = TRUE)
            trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
            
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainData, is.numeric)  
            trainData[numCols] <- trainData[numCols] * 1000000
            
            weights <- information.gain(CLASS~., trainData)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
            allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "GM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
            
            
            # WSM
            trainData <- read.csv(file = paste(fileDir, authorName, "TrainWSM", f, ".csv", sep = ""), header = TRUE)
            trainData <- trainData[, -columnsToBeRemoved] #only numeric features and class remained
            
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainData, is.numeric)  
            trainData[numCols] <- trainData[numCols] * 1000000 
            
            weights <- information.gain(CLASS~., trainData)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
            allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "WPM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
          }
        }
      }
    }
  }
  else if(experimentType == "SequentialTime"){
    allInfoGainResults <- data.frame(matrix(ncol = totalMetricCount + 4, nrow = 0))
    colnames(allInfoGainResults)[1:4] <- c("Project", "Developer", "Model", "Interval")
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      for(a in 1:numberOfSelectedDev){
        authorName <- authorNames[a]
        testIntervals <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))[a, paste("TestIntervals_", testSetMonths, "Months", sep = "")])
        testIntervals <- unlist(strsplit(testIntervals, "-", fixed = TRUE))
        
        if(authorName == "TM"){
          for(interval in testIntervals){
            print(paste("info gain evaluation: ", projectName, " - ", authorName, " - test_interval: ", interval, sep = ""))
            
            if(useIntervalDataOrPreparedTrainTestData == "I")
              datasetTM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "", "I", "csv")    
            if(useIntervalDataOrPreparedTrainTestData == "P")
              datasetTM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "", "P", "csv")    
            
            trainSetTM <- datasetTM[[1]][, -columnsToBeRemoved]
            testSetTM <- datasetTM[[2]][, -columnsToBeRemoved]
            
            # PM
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainSetTM, is.numeric)  
            trainSetTM[numCols] <- trainSetTM[numCols] * 1000000 
            
            weights <- information.gain(CLASS~., trainSetTM)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Interval"] <- interval
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "PM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(5:(totalMetricCount + 4))] <- rankedMetrics
            
          }
        }
        else{
          for(interval in testIntervals){
            print(paste("info gain evaluation: ", projectName, " - ", authorName, " - test_interval: ", interval, sep = ""))
            
            if(useIntervalDataOrPreparedTrainTestData == "I"){
              datasetPM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "PM", "I", "csv")    
              datasetSM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "SM", "I", "csv")        
              datasetGM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "GM", "I", "csv")    
            }
            if(useIntervalDataOrPreparedTrainTestData == "P"){
              datasetPM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "PM", "P", "csv")    
              datasetSM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "SM", "P", "csv")    
              datasetGM <- getIntervalDataset(inputDataDir, projectName, authorName, interval, testSetMonths, "GM", "P", "csv")    
            }
            
            trainSetPM <- datasetPM[[1]][, -columnsToBeRemoved]
            testSetPM <- datasetPM[[2]][, -columnsToBeRemoved]
            
            trainSetSM <- datasetSM[[1]][, -columnsToBeRemoved]
            testSetSM <- datasetSM[[2]][, -columnsToBeRemoved]
            
            trainSetGM <- datasetGM[[1]][, -columnsToBeRemoved]
            testSetGM <- datasetGM[[2]][, -columnsToBeRemoved]
            
            
            # PM
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainSetPM, is.numeric)  
            trainSetPM[numCols] <- trainSetPM[numCols] * 1000000 
            
            weights <- information.gain(CLASS~., trainSetPM)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Interval"] <- interval
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "PM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(5:(totalMetricCount + 4))] <- rankedMetrics
            
            # SM
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainSetSM, is.numeric)  
            trainSetSM[numCols] <- trainSetSM[numCols] * 1000000 
            
            weights <- information.gain(CLASS~., trainSetSM)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Interval"] <- interval
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "SM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(5:(totalMetricCount + 4))] <- rankedMetrics
            
            # GM
            #increase the range of numeric cloumns to avoid error
            numCols <- sapply(trainSetGM, is.numeric)  
            trainSetGM[numCols] <- trainSetGM[numCols] * 1000000
            
            weights <- information.gain(CLASS~., trainSetGM)
            
            if(all(weights$attr_importance == 0)){
              rankedMetrics <- rownames(weights)[order(weights)]
            } else {
              rankedMetrics <- rownames(weights)[rev(order(weights))]
            }
            
            allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
            allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
            allInfoGainResults[nrow(allInfoGainResults), "Interval"] <- interval
            allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "GM"
            
            allInfoGainResults[nrow(allInfoGainResults), c(5:(totalMetricCount + 4))] <- rankedMetrics
          }
        }
      }
    }
  }
  
  write.xlsx(x = allInfoGainResults, file = paste(outputDataDir, "/FSelectorInfoGainResults.xlsx", sep = ""), sheetName = "infoGain", row.names = FALSE)
  
  # summaryResults <- data.frame(matrix(ncol = 13, nrow = 5))
  # gainCountsWoAllModule <- allInfoGainResults[-which(allInfoGainResults$Project == "all"), ]
  # #rev(sort(table(gainCountsWoAllModule$X6)))[1:5]
  # 
  # tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X6)))), rev(sort(table(gainCountsWoAllModule$X6))), sep = "/")
  # summaryResults[1, 1:length(tbl)] <- tbl
  # 
  # tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X7)))), rev(sort(table(gainCountsWoAllModule$X7))), sep = "/")
  # summaryResults[2, 1:length(tbl)] <- tbl
  # 
  # tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X8)))), rev(sort(table(gainCountsWoAllModule$X8))), sep = "/")
  # summaryResults[3, 1:length(tbl)] <- tbl
  # 
  # tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X9)))), rev(sort(table(gainCountsWoAllModule$X9))), sep = "/")
  # summaryResults[4, 1:length(tbl)] <- tbl
  # 
  # tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X10)))), rev(sort(table(gainCountsWoAllModule$X10))), sep = "/")
  # summaryResults[5, 1:length(tbl)] <- tbl
  # 
  # write.xlsx(x = allInfoGainResults, file = paste(wekaResultsDir, "wekaAttrRanking_", samplingOption, ".xlsx", sep = ""), sheetName = "infoGain", row.names = FALSE)
  # write.xlsx(x = summaryResults, file = paste(wekaResultsDir, "wekaAttrRanking_", samplingOption, ".xlsx", sep = ""), sheetName = "summary", row.names = FALSE, append = TRUE)
  # 
}

if(analyzeResults){
  inputDataDir <- paste(finalDataDir, "MixTime_InfoGain/", samplingOption, "/", sep = "")
  
  infoGainResults <- openxlsx::read.xlsx(xlsxFile = paste(inputDataDir, "FSelectorInfoGainResults.xlsx", sep = ""), sheet = 1)
  
  infoGainResults <- infoGainResults[infoGainResults$Model == "GM", ]
  
  summaryResults <- data.frame(matrix(ncol = 13, nrow = 5))
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X6)))), rev(sort(table(infoGainResults$X6))), sep = "/")
  summaryResults[1, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X7)))), rev(sort(table(infoGainResults$X7))), sep = "/")
  summaryResults[2, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X8)))), rev(sort(table(infoGainResults$X8))), sep = "/")
  summaryResults[3, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X9)))), rev(sort(table(infoGainResults$X9))), sep = "/")
  summaryResults[4, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X10)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[5, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X11)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[6, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X12)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[7, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X13)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[8, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X14)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[9, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X15)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[10, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X16)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[11, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X17)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[12, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(infoGainResults$X18)))), rev(sort(table(infoGainResults$X10))), sep = "/")
  summaryResults[13, 1:length(tbl)] <- tbl
  
  
  write.xlsx(x = summaryResults, file = paste(outputDataDir, "infoGainAnalyzeGM.xlsx", sep = ""))
  
  
  
  rankTable <- data.frame(matrix(ncol = 13, nrow = 5))
  # colnames(rankTable) <- c("Add", "Del", "Churn", "Ent", "Nd", "Ns","Nf", "Ndev", "Age", "Nfc", "Exp", "Rexp", "Sexp")
  colnames(rankTable) <- c("ADD", "DEL", "CHURN", "Ent", "ND", "NS","NF", "NDEV", "AGE", "NFC", "EXP", "REXP", "SEXP")
  rownames(rankTable) <- c("1st Rank", "2nd Rank", "3rd Rank", "4th Rank", "5th Rank")
  
  totalNumberOfResults <- nrow(infoGainResults)
  
  offset <- 5
  for(r in 1:nrow(rankTable)){
    for(c in 1:ncol(rankTable)){
      tbl <- table(infoGainResults[, r + offset])
      count <- tbl[names(tbl) == colnames(rankTable)[c]]
      if(length(count) > 0)
        rankTable[r, c] <- round(count / totalNumberOfResults, digits = 3) * 100
    }
  }
  
  write.xlsx(x = rankTable, file = paste(outputDataDir, "infoGainAnalyzeTableGM.xlsx", sep = ""))
  
}

if(analyzeWekaResults){
  inputDataDir <- paste(finalDataDir, "MixTime_InfoGain/")
    
  for(p in 1:length(projectList)) {
    #some variable names for easy usage
    projectName <- projectList[p]
    loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
    
    # create folder for results
    dir.create(outputDataDir, showWarnings = FALSE)
    
    for(a in 1:numberOfSelectedDev){
      authorName <- as.character(get(selectedAuthorsDfName)[a, "Author"])
      
      for(r in 1:numberOfRuns){
        for(f in 1:numberOfFolds){
          print(paste("info gain evaluation: ", projectName, " - ", authorName, " - Run: ", r, " - Fold: ", f, sep = ""))
          
          fileDir <- paste(wekaResultsDir, "/dataset_ranked/", projectName, "/", authorName, "/Run", r, "/", sep = "")
          
          # PM
          rankedTrainData <- read.csv(file = paste(fileDir, authorName, "TrainPM", f, ".csv", sep = ""), header = TRUE)
          rankedMetrics <- colnames(rankedTrainData)[-ncol(rankedTrainData)]
          
          allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
          allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
          allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
          allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
          allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "PM"
          
          allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
          
          
          # SM
          rankedTrainData <- read.csv(file = paste(fileDir, authorName, "TrainSM", f, ".csv", sep = ""), header = TRUE)
          rankedMetrics <- colnames(rankedTrainData)[-ncol(rankedTrainData)]
          
          allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
          allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
          allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
          allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
          allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "SM"
          
          allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
          
          
          # GM
          rankedTrainData <- read.csv(file = paste(fileDir, authorName, "TrainGM", f, ".csv", sep = ""), header = TRUE)
          rankedMetrics <- colnames(rankedTrainData)[-ncol(rankedTrainData)]
          
          allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
          allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
          allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
          allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
          allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "GM"
          
          allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
          
          
          # WSM
          rankedTrainData <- read.csv(file = paste(fileDir, authorName, "TrainWSM", f, ".csv", sep = ""), header = TRUE)
          rankedMetrics <- colnames(rankedTrainData)[-ncol(rankedTrainData)]
          
          allInfoGainResults[nrow(allInfoGainResults) + 1, "Project"] <- projectName
          allInfoGainResults[nrow(allInfoGainResults), "Developer"] <- authorName
          allInfoGainResults[nrow(allInfoGainResults), "Iteration"] <- r
          allInfoGainResults[nrow(allInfoGainResults), "Fold"] <- f
          allInfoGainResults[nrow(allInfoGainResults), "Model"] <- "WPM"
          
          allInfoGainResults[nrow(allInfoGainResults), c(6:(totalMetricCount + 5))] <- rankedMetrics
          
        }
      }
    }
  }
  
  summaryResults <- data.frame(matrix(ncol = 13, nrow = 5))
  gainCountsWoAllModule <- allInfoGainResults[-which(allInfoGainResults$Project == "all"), ]
  #rev(sort(table(gainCountsWoAllModule$X6)))[1:5]
  
  tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X6)))), rev(sort(table(gainCountsWoAllModule$X6))), sep = "/")
  summaryResults[1, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X7)))), rev(sort(table(gainCountsWoAllModule$X7))), sep = "/")
  summaryResults[2, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X8)))), rev(sort(table(gainCountsWoAllModule$X8))), sep = "/")
  summaryResults[3, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X9)))), rev(sort(table(gainCountsWoAllModule$X9))), sep = "/")
  summaryResults[4, 1:length(tbl)] <- tbl
  
  tbl <- paste(names(rev(sort(table(gainCountsWoAllModule$X10)))), rev(sort(table(gainCountsWoAllModule$X10))), sep = "/")
  summaryResults[5, 1:length(tbl)] <- tbl
  
  write.xlsx(x = allInfoGainResults, file = paste(wekaResultsDir, "wekaAttrRanking_", samplingOption, ".xlsx", sep = ""), sheetName = "infoGain", row.names = FALSE)
  write.xlsx(x = summaryResults, file = paste(wekaResultsDir, "wekaAttrRanking_", samplingOption, ".xlsx", sep = ""), sheetName = "summary", row.names = FALSE, append = TRUE)
}

if(prepareTrainTestDataFiles){
  outputFolder <- paste(resultsDir, "/wekaResults/trainTest_discretized/", sep = "")
  inputFolder <- paste(resultsDir, "/wekaResults/dataset_discretized/", sep = "")
  
  # create folder for results
  dir.create(outputFolder, showWarnings = FALSE)
  
  for(p in 1:length(projectList)) {
    #some variable names for easy usage
    projectName <- projectList[p]
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    
    #read selected authors and keep them in a table and a list
    authorData <- read.xlsx(file = paste(baseDir, "selectedAuthors.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData$Selected == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
    
    numberOfSelectedDev <- length(get(selectedAuthorsDfName)$Author)
    
    for(a in 1:numberOfSelectedDev){
      authorName <- as.character(get(selectedAuthorsDfName)[a, "Author"])
      
      for(r in 1:numberOfRuns){
        inputDataDir <- paste(inputFolder, projectName, "/", authorName, "/Run", r, sep = "")
        trainTestDataDir <- paste(outputFolder, projectName, "/", authorName, "/Run", r, sep = "")
        
        dir.create(trainTestDataDir, recursive = T, showWarnings = F)
        
        for(f in 1:numberOfFolds){
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainPM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainPM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData)
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainPM", f, ".csv", sep = ""), row.names = FALSE)
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainSM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainSM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData)
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainSM", f, ".csv", sep = ""), row.names = FALSE)
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainGM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainGM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData)
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainGM", f, ".csv", sep = ""), row.names = FALSE)
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainWSM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainWSM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData)
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainWSM", f, ".csv", sep = ""), row.names = FALSE)
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "Test", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData)
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "Test", f, ".csv", sep = ""), row.names = FALSE)
        }
      }
    }
  }
}

if(prepareTrainTestDataSelectTop5Weka){
  outputFolder <- paste(resultsDir, "/wekaResults/trainTest_discretized_top5/", sep = "")
  inputFolder <- paste(resultsDir, "/wekaResults/dataset_discretized_ranked/", sep = "")
  previousDataFolder <- paste(resultsDir, "/wekaResults/dataset_discretized/", sep = "")
  
  # create folder for results
  dir.create(outputFolder, showWarnings = FALSE)
  
  for(p in 1:length(projectList)) {
    #some variable names for easy usage
    projectName <- projectList[p]
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    
    #read selected authors and keep them in a table and a list
    authorData <- read.xlsx(file = paste(baseDir, "selectedAuthors.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData$Selected == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
    
    numberOfSelectedDev <- length(get(selectedAuthorsDfName)$Author)
    
    for(a in 1:numberOfSelectedDev){
      authorName <- as.character(get(selectedAuthorsDfName)[a, "Author"])
      
      for(r in 1:numberOfRuns){
        inputDataDir <- paste(inputFolder, projectName, "/", authorName, "/Run", r, sep = "")
        trainTestDataDir <- paste(outputFolder, projectName, "/", authorName, "/Run", r, sep = "")
        
        dir.create(trainTestDataDir, recursive = T, showWarnings = F)
        
        for(f in 1:numberOfFolds){
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainPM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainPM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData[, c(1:5)], transformedData[, ncol(transformedData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainPM", f, ".csv", sep = ""), row.names = FALSE)
          
          selectedFeatures <- colnames(transformedData)[1:5]
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          previousData <- read.csv(file = paste(previousDataFolder, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], previousData[, selectedFeatures], originalData[, ncol(originalData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TestPM", f, ".csv", sep = ""), row.names = FALSE)
          
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainSM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainSM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData[, c(1:5)], transformedData[, ncol(transformedData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainSM", f, ".csv", sep = ""), row.names = FALSE)
          
          selectedFeatures <- colnames(transformedData)[1:5]
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          previousData <- read.csv(file = paste(previousDataFolder, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], previousData[, selectedFeatures], originalData[, ncol(originalData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TestSM", f, ".csv", sep = ""), row.names = FALSE)
          
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainGM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainGM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData[, c(1:5)], transformedData[, ncol(transformedData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainGM", f, ".csv", sep = ""), row.names = FALSE)
          
          selectedFeatures <- colnames(transformedData)[1:5]
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          previousData <- read.csv(file = paste(previousDataFolder, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], previousData[, selectedFeatures], originalData[, ncol(originalData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TestGM", f, ".csv", sep = ""), row.names = FALSE)
          
          
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "TrainWSM", f, ".csv", sep = ""))
          transformedData <- read.csv(file = paste(inputDataDir, "/", authorName, "TrainWSM", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], transformedData[, c(1:5)], transformedData[, ncol(transformedData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TrainWSM", f, ".csv", sep = ""), row.names = FALSE)
          
          selectedFeatures <- colnames(transformedData)[1:5]
          originalData <- read.csv(file = paste(foldsDir, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          previousData <- read.csv(file = paste(previousDataFolder, "/", projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""))
          finalData <- cbind(originalData[, c(1:5)], previousData[, selectedFeatures], originalData[, ncol(originalData)])
          colnames(finalData)[ncol(finalData)] <- "CLASS"
          write.csv(x = finalData, file = paste(trainTestDataDir, "/", authorName, "TestWSM", f, ".csv", sep = ""), row.names = FALSE)
        }
      }
    }
  }
}


