library(xlsx)
# library(openxlsx)
library(ScottKnottESD)

rm(list = ls())

mainProjectDir = "/Users/beken/Projects/DP_OS/"
dataDir = paste(mainProjectDir, "data/", sep = "")
finalDataDir = paste(mainProjectDir, "data/final/", sep = "")
outputDir = paste(mainProjectDir, "output/", sep = "")
plotDir = paste(mainProjectDir, "plots/", sep = "")
# trainTestDataDir = paste(finalDataDir, "/TrainTestData/", sep = "")
setwd(mainProjectDir)
source("./R/0_GeneralFunctions.R")

# testSetLengthsInMonths <- c(3,6)
testSetLengthsInMonths <- c(3)
projects <- c("Gimp", "Maven2", "Perl", "PostgreSql", "Rails", "Rhino")

algorithms <- c("NB", "RF", "HyperSMURF")
metricList <- c("recall", "pf", "precision", "F1")




plotSeparatedAlgorithmsCombinedDevelopers <- FALSE
plotSeparatedAlgorithmsSeparatedDevelopers <- FALSE
plotCombinedAlgorithmsSeparatedDevelopers <- FALSE
singlePlot <- FALSE
scotKnott <- FALSE
compareExperimentationTypes <- TRUE

for(tm in testSetLengthsInMonths){
  trainTestDataDir = paste(finalDataDir, "TrainTest_", tm, "Months/", sep = "")
  howManyInterval <- tm / 3 
  
  # outputDir = paste(outputDir, "predictions", algorithms , tm, "Months/", sep = "")
  plotDir = paste(mainProjectDir, "plots/", tm, "Months/", sep = "")
  dir.create(plotDir, showWarnings = FALSE, recursive = TRUE)
  
  
  if(plotSeparatedAlgorithmsCombinedDevelopers){
    NB_Dir <- paste(outputDir, "predictionsNB/", tm, "Months/", sep ="")
    RF_Dir <- paste(outputDir, "predictionsRF/", tm, "Months/", sep ="")
    HyperSMURF_Dir <- paste(outputDir, "predictionsHyperSMURF/", tm, "Months/", sep ="")
    
    for(p in projects){
      projectName <- p
      selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
      authorData <- read.xlsx(file = paste(finalDataDir, "selectedAuthorsTimeBased.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
      assign(selectedAuthorsDfName, authorData[which(authorData[, paste("Selected_", tm, "Months", sep = "")] == "T"), ])
      authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Alias)
      
      results_NB_PM <- data.frame()
      results_NB_SM <- data.frame()
      results_NB_GM <- data.frame()
      
      results_RF_PM <- data.frame()
      results_RF_SM <- data.frame()
      results_RF_GM <- data.frame()
      
      results_HyperSMURF_PM <- data.frame()
      results_HyperSMURF_SM <- data.frame()
      results_HyperSMURF_GM <- data.frame()
      
      for(a in 1:length(authorNames)){
        authorName <- authorNames[a]
        
        results_NB_PM <- rbind(results_NB_PM, read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        results_NB_SM <- rbind(results_NB_SM, read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        results_NB_GM <- rbind(results_NB_GM, read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        
        results_RF_PM <- rbind(results_RF_PM, read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        results_RF_SM <- rbind(results_RF_SM, read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        results_RF_GM <- rbind(results_RF_GM, read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        
        results_HyperSMURF_PM <- rbind(results_HyperSMURF_PM, read.xlsx(file = paste(HyperSMURF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        results_HyperSMURF_SM <- rbind(results_HyperSMURF_SM, read.xlsx(file = paste(HyperSMURF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
        results_HyperSMURF_GM <- rbind(results_HyperSMURF_GM, read.xlsx(file = paste(HyperSMURF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE))
      }
    }
    
    pdf(file = paste(plotDir, "SeparatedAlgsCombinedDevs.pdf", sep = ""), width = 10, height = 20)
    par(mfrow = c(4, 3), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(m in metricList){
      for(alg in algorithms){
        results_PM <- get(paste("results_", alg, "_PM", sep = ""))[, m]
        results_SM <- get(paste("results_", alg, "_PM", sep = ""))[, m]  
        results_GM <- get(paste("results_", alg, "_SM", sep = ""))[, m]  
        
        boxplot(results_PM, results_SM, results_GM, ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5,
                names = c('PM', 'SM', 'GM'))
        title(ylab = paste(alg, m), cex.lab = 1.7)
      }
    }
    
    dev.off()
  }
  
  
  if(plotSeparatedAlgorithmsSeparatedDevelopers){
    NB_Dir <- paste(outputDir, "predictionsNB/", tm, "Months/", sep ="")
    RF_Dir <- paste(outputDir, "predictionsRF/", tm, "Months/", sep ="")
    HyperSMURF_Dir <- paste(outputDir, "predictionsHyperSMURF/", tm, "Months/", sep ="")
    
    for(p in projects){
      projectName <- p
      selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
      authorData <- read.xlsx(file = paste(finalDataDir, "selectedAuthorsTimeBased.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
      assign(selectedAuthorsDfName, authorData[which(authorData[, paste("Selected_", tm, "Months", sep = "")] == "T"), ])
      authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Alias)
      
      for(a in authorNames){
        authorName <- a
        
        results_NB_PM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        results_NB_SM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        results_NB_GM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        
        results_RF_PM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        results_RF_SM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        results_RF_GM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        
        results_hyperSMURF_PM <- read.xlsx(file = paste(HyperSMURF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        results_hyperSMURF_SM <- read.xlsx(file = paste(HyperSMURF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        results_hyperSMURF_GM <- read.xlsx(file = paste(HyperSMURF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
        
        pdf(file = paste(plotDir, authorName, ".pdf", sep = ""), width = 10, height = 20)
        par(mfrow = c(4, 3), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
        
        for(m in metricList){
          boxplot(results_NB_PM[, m], results_NB_SM[, m], results_NB_GM[, m], ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
                  names = c('PM', 'SM', 'GM'))
          title(ylab = paste("NB", m), cex.lab = 1.7)
          
          boxplot(results_RF_PM[, m], results_RF_SM[, m], results_RF_GM[, m], ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
                  names = c('PM', 'SM', 'GM'))
          title(ylab = paste("RF", m), cex.lab = 1.7)
          
          boxplot(results_hyperSMURF_PM[, m], results_hyperSMURF_SM[, m], results_hyperSMURF_GM[, m], ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
                  names = c('PM', 'SM', 'GM'))
          title(ylab = paste("HyperSMURF", m), cex.lab = 1.7)
        }
        
        dev.off()
        
      }
    }
  }
 
}  








if(plotCombinedAlgorithmsSeparatedDevelopers){
  NB_Dir <- paste(outputDir, "NB/", sep = "")
  RF_Dir <- paste(outputDir, "RF/", sep = "")
  hyperSMURF_Dir <- paste(outputDir, "hyperSMURF/", sep = "")
  
  for(p in projects){
    projectName <- p
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(dataDir, "selectedAuthorsTimeBased.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData$Selected == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
  
    pdf(file = paste(plotDir, "SeparatedDevsCombinedAlgs.pdf", sep = ""), width = 10, height = 20)
    par(mfrow = c(length(authorNames), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(a in authorNames){
      authorName <- a
      
      results_NB_PM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_RF_PM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_hyperSMURF_PM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_hyperSMURF_SM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_hyperSMURF_GM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      
      # bir developerin tum algoritmalarinin birlesmis sonuclari
      results_PM <- rbind(results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      results_SM <- rbind(results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      results_GM <- rbind(results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
      
      
      for(m in metricList){
        boxplot(results_PM[, m], results_SM[, m], results_GM[, m], ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
                names = c('PM', 'SM', 'GM'))
        title(ylab = paste(authorName, m), cex.lab = 1.7)
      }
    }
    dev.off()
  }
}

if(singlePlot){
  results_PM <- data.frame()
  results_SM <- data.frame()
  results_GM <- data.frame()
  
  NB_Dir <- paste(outputDir, "NB/", sep = "")
  RF_Dir <- paste(outputDir, "RF/", sep = "")
  hyperSMURF_Dir <- paste(outputDir, "hyperSMURF/", sep = "")
  
  for(p in projectList){
    projectName <- p
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(dataDir, "selectedAuthorsTimeBased.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData$Selected == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
    
    for(a in authorNames){
      authorName <- a
      
      results_NB_PM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_RF_PM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_hyperSMURF_PM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_hyperSMURF_SM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_hyperSMURF_GM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
    
      results_PM <- rbind(results_PM, results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      results_SM <- rbind(results_SM, results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      results_GM <- rbind(results_GM, results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
    }
  }
  
  pdf(file = paste(plotDir, "SinglePlot.pdf", sep = ""), width = 10, height = 4)
  par(mfrow = c(1, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
  
  for(m in metricList){
    boxplot(results_PM[, m], results_SM[, m], results_GM[, m], ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
            names = c('PM', 'SM', 'GM'))
    title(ylab = paste("", m), cex.lab = 1.7)
  }
  dev.off()
}

if(scotKnott){
  results_PM <- data.frame()
  results_SM <- data.frame()
  results_GM <- data.frame()
  
  NB_Dir <- paste(outputDir, "NB/", sep = "")
  RF_Dir <- paste(outputDir, "RF/", sep = "")
  hyperSMURF_Dir <- paste(outputDir, "hyperSMURF/", sep = "")
  
  for(p in projectList){
    projectName <- p
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(dataDir, "selectedAuthorsTimeBased.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData$Selected == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
    
    for(a in authorNames){
      authorName <- a
      
      results_NB_PM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_RF_PM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_hyperSMURF_PM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_hyperSMURF_SM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_hyperSMURF_GM <- read.xlsx(file = paste(hyperSMURF_Dir, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      
      results_PM <- rbind(results_PM, results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      results_SM <- rbind(results_SM, results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      results_GM <- rbind(results_GM, results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
      
      pdf(file = paste(plotDir, authorName, "SK-ESD.pdf", sep = ""), width = 10, height = 4)
      par(mfrow = c(2, 2), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
      
      
      for(m in metricList){
        modelCompMatrix <- as.matrix(cbind(results_NB_PM[,m],results_NB_SM[,m],results_NB_GM[,m],
                                           results_RF_PM[,m],results_RF_SM[,m],results_RF_GM[,m],
                                           results_hyperSMURF_PM[,m],results_hyperSMURF_SM[,m],results_hyperSMURF_GM[,m]))
        # colnames(modelCompMatrix) <- algorithms[c(4,7)]
        modelCompResults <- sk_esd(modelCompMatrix)
        plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")

      }
      dev.off()
      
    }
  }
  
}


# mixed time vs sequential time experimentations
if(compareExperimentationTypes){
  NB_Dir_SeqTime <- paste(outputDir, "predictionsNB/", tm, "Months/", sep ="")
  RF_Dir_SeqTime <- paste(outputDir, "predictionsRF/", tm, "Months/", sep ="")
  HyperSMURF_Dir_SeqTime <- paste(outputDir, "predictionsHyperSMURF/", tm, "Months/", sep ="")

  results_PM_SeqTime <- data.frame()
  results_SM_SeqTime <- data.frame()
  results_GM_SeqTime <- data.frame()

  for(p in 1:length(projects)){
    projectName <- projects[p]
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(finalDataDir, "selectedAuthorsSeqTime.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData[, paste("Selected_", tm, "Months", sep = "")] == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Alias)

    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]

      results_NB_PM <- read.xlsx(file = paste(NB_Dir_SeqTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir_SeqTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir_SeqTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)

      results_RF_PM <- read.xlsx(file = paste(RF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)

      # results_hyperSMURF_PM <- read.xlsx(file = paste(HyperSMURF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_SM <- read.xlsx(file = paste(HyperSMURF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_GM <- read.xlsx(file = paste(HyperSMURF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)


      results_PM_SeqTime <- rbind(results_PM_SeqTime, results_NB_PM, results_RF_PM)
      results_SM_SeqTime <- rbind(results_SM_SeqTime, results_NB_SM, results_RF_SM)
      results_GM_SeqTime <- rbind(results_GM_SeqTime, results_NB_GM, results_RF_GM)
      
      # results_PM_SeqTime <- rbind(results_PM_SeqTime, results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      # results_SM_SeqTime <- rbind(results_SM_SeqTime, results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      # results_GM_SeqTime <- rbind(results_GM_SeqTime, results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
    }

  }
  
  
  NB_Dir_MixTime <- paste(mainProjectDir, "MixedTime/Experiment_V2/WoSampling/PredictionsNB/", sep ="")
  RF_Dir_MixTime <- paste(mainProjectDir, "MixedTime/Experiment_V2/WoSampling/PredictionsRF/", sep ="")
  # HyperSMURF_Dir_MixTime <- paste(outputDir, "predictionsHyperSMURF/", tm, "Months/", sep ="")
  
  results_PM_MixTime <- data.frame()
  results_SM_MixTime <- data.frame()
  results_GM_MixTime <- data.frame()
  
  for(p in 1:length(projects)){
    projectName <- projects[p]
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(finalDataDir, "selectedAuthorsMixTime.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData)
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
    
    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]
      
      results_NB_PM <- read.xlsx(file = paste(NB_Dir_MixTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir_MixTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir_MixTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_RF_PM <- read.xlsx(file = paste(RF_Dir_MixTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir_MixTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir_MixTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      # results_hyperSMURF_PM <- read.xlsx(file = paste(HyperSMURF_Dir_MixTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_SM <- read.xlsx(file = paste(HyperSMURF_Dir_MixTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_GM <- read.xlsx(file = paste(HyperSMURF_Dir_MixTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_PM_MixTime <- rbind(results_PM_MixTime, results_NB_PM, results_RF_PM)
      results_SM_MixTime <- rbind(results_SM_MixTime, results_NB_SM, results_RF_SM)
      results_GM_MixTime <- rbind(results_GM_MixTime, results_NB_GM, results_RF_GM)
      
      # results_PM_MixTime <- rbind(results_PM_MixTime, results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      # results_SM_MixTime <- rbind(results_SM_MixTime, results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      # results_GM_MixTime <- rbind(results_GM_MixTime, results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
    }
    
  }
  
  
  pdf(file = paste(plotDir, "ExperimentationComp.pdf", sep = ""), width = 10, height = 7)
  par(mfrow = c(2, 4), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
  
  
  for(m in metricList){
    modelCompMatrix <- as.matrix(cbind(results_PM_MixTime[,m],results_SM_MixTime[,m],results_GM_MixTime[,m],
                                       results_PM_SeqTime[,m],results_SM_SeqTime[,m],results_GM_SeqTime[,m]))
    colnames(modelCompMatrix) <- c("PM_Mix", "SM_Mix", "GM_Mix", "PM_Seq", "SM_Seq", "GM_Seq")
    modelCompResults <- sk_esd(modelCompMatrix)
    plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")
    
  }
  dev.off()
  
  pdf(file = paste(plotDir, "ExperimentationComp_Boxplots.pdf", sep = ""), width = 20, height = 10)
  par(mfrow = c(2, length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
  
  for(m in metricList){
    boxplot(results_PM_MixTime[, m], results_SM_MixTime[, m], results_GM_MixTime[, m], 
            results_PM_SeqTime[,m],results_SM_SeqTime[,m],results_GM_SeqTime[,m],
            ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
            names = c("PM_Mix", "SM_Mix", "GM_Mix", "PM_Seq", "SM_Seq", "GM_Seq"))
    title(ylab = paste("", m), cex.lab = 1.7)
  }
  dev.off()
  
}

# mixed time vs sequential time experimentations
if(compareExperimentationTypesSepAlg){
  NB_Dir_SeqTime <- paste(outputDir, "predictionsNB/", tm, "Months/", sep ="")
  RF_Dir_SeqTime <- paste(outputDir, "predictionsRF/", tm, "Months/", sep ="")
  HyperSMURF_Dir_SeqTime <- paste(outputDir, "predictionsHyperSMURF/", tm, "Months/", sep ="")
  
  results_PM_NB_SeqTime <- data.frame()
  results_SM_NB_SeqTime <- data.frame()
  results_GM_NB_SeqTime <- data.frame()
  
  results_PM_RF_SeqTime <- data.frame()
  results_SM_RF_SeqTime <- data.frame()
  results_GM_RF_SeqTime <- data.frame()
  
  for(p in 1:length(projects)){
    projectName <- projects[p]
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(finalDataDir, "selectedAuthorsSeqTime.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData[which(authorData[, paste("Selected_", tm, "Months", sep = "")] == "T"), ])
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Alias)
    
    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]
      
      results_NB_PM <- read.xlsx(file = paste(NB_Dir_SeqTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir_SeqTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir_SeqTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_RF_PM <- read.xlsx(file = paste(RF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      # results_hyperSMURF_PM <- read.xlsx(file = paste(HyperSMURF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_SM <- read.xlsx(file = paste(HyperSMURF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_GM <- read.xlsx(file = paste(HyperSMURF_Dir_SeqTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      

      results_PM_NB_SeqTime <- rbind(results_PM_NB_SeqTime, results_NB_PM)
      results_SM_NB_SeqTime <- rbind(results_SM_NB_SeqTime, results_NB_SM)
      results_GM_NB_SeqTime <- rbind(results_GM_NB_SeqTime, results_NB_GM)
      
      results_PM_RF_SeqTime <- rbind(results_PM_RF_SeqTime, results_RF_PM)
      results_SM_RF_SeqTime <- rbind(results_SM_RF_SeqTime, results_RF_SM)
      results_GM_RF_SeqTime <- rbind(results_GM_RF_SeqTime, results_RF_GM)
      
      # results_PM_SeqTime <- rbind(results_PM_SeqTime, results_NB_PM, results_RF_PM)
      # results_SM_SeqTime <- rbind(results_SM_SeqTime, results_NB_SM, results_RF_SM)
      # results_GM_SeqTime <- rbind(results_GM_SeqTime, results_NB_GM, results_RF_GM)
      # results_PM_SeqTime <- rbind(results_PM_SeqTime, results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      # results_SM_SeqTime <- rbind(results_SM_SeqTime, results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      # results_GM_SeqTime <- rbind(results_GM_SeqTime, results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
    }
    
  }
  
  
  NB_Dir_MixTime <- paste(mainProjectDir, "MixedTime/Experiment_V2/WoSampling/PredictionsNB/", sep ="")
  RF_Dir_MixTime <- paste(mainProjectDir, "MixedTime/Experiment_V2/WoSampling/PredictionsRF/", sep ="")
  # HyperSMURF_Dir_MixTime <- paste(outputDir, "predictionsHyperSMURF/", tm, "Months/", sep ="")
  
  results_PM_NB_MixTime <- data.frame()
  results_SM_NB_MixTime <- data.frame()
  results_GM_NB_MixTime <- data.frame()
  
  results_PM_RF_MixTime <- data.frame()
  results_SM_RF_MixTime <- data.frame()
  results_GM_RF_MixTime <- data.frame()
  
  for(p in 1:length(projects)){
    projectName <- projects[p]
    selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
    authorData <- read.xlsx(file = paste(finalDataDir, "selectedAuthorsMixTime.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE)
    assign(selectedAuthorsDfName, authorData)
    authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
    
    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]
      
      results_NB_PM <- read.xlsx(file = paste(NB_Dir_MixTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_SM <- read.xlsx(file = paste(NB_Dir_MixTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_NB_GM <- read.xlsx(file = paste(NB_Dir_MixTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      results_RF_PM <- read.xlsx(file = paste(RF_Dir_MixTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_SM <- read.xlsx(file = paste(RF_Dir_MixTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      results_RF_GM <- read.xlsx(file = paste(RF_Dir_MixTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      # results_hyperSMURF_PM <- read.xlsx(file = paste(HyperSMURF_Dir_MixTime, projectName, "/results/", authorName, "ResultsPM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_SM <- read.xlsx(file = paste(HyperSMURF_Dir_MixTime, projectName, "/results/", authorName, "ResultsSM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      # results_hyperSMURF_GM <- read.xlsx(file = paste(HyperSMURF_Dir_MixTime, projectName, "/results/", authorName, "ResultsGM.xlsx", sep = ""), sheetIndex = 1, header = TRUE)
      
      
      results_PM_NB_MixTime <- rbind(results_PM_NB_MixTime, results_NB_PM)
      results_SM_NB_MixTime <- rbind(results_SM_NB_MixTime, results_NB_SM)
      results_GM_NB_MixTime <- rbind(results_GM_NB_MixTime, results_NB_GM)
      
      results_PM_RF_MixTime <- rbind(results_PM_RF_MixTime, results_RF_PM)
      results_SM_RF_MixTime <- rbind(results_SM_RF_MixTime, results_RF_SM)
      results_GM_RF_MixTime <- rbind(results_GM_RF_MixTime, results_RF_GM)
      
      
      
      # results_PM_MixTime <- rbind(results_PM_MixTime, results_NB_PM, results_RF_PM)
      # results_SM_MixTime <- rbind(results_SM_MixTime, results_NB_SM, results_RF_SM)
      # results_GM_MixTime <- rbind(results_GM_MixTime, results_NB_GM, results_RF_GM)
      
      # results_PM_MixTime <- rbind(results_PM_MixTime, results_NB_PM, results_RF_PM, results_hyperSMURF_PM)
      # results_SM_MixTime <- rbind(results_SM_MixTime, results_NB_SM, results_RF_SM, results_hyperSMURF_SM)
      # results_GM_MixTime <- rbind(results_GM_MixTime, results_NB_GM, results_RF_GM, results_hyperSMURF_GM)
    }
    
  }
  
  
  pdf(file = paste(plotDir, "ExperimentationComp_SepAlg.pdf", sep = ""), width = 10, height = 7)
  par(mfrow = c(2, 4), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 

  for(m in metricList){
    modelCompMatrix <- as.matrix(cbind(results_PM_NB_MixTime[,m],results_SM_NB_MixTime[,m],results_GM_NB_MixTime[,m],
                                       results_PM_RF_MixTime[,m],results_SM_RF_MixTime[,m],results_GM_RF_MixTime[,m],
                                 results_PM_NB_SeqTime[,m],results_SM_NB_SeqTime[,m],results_GM_NB_SeqTime[,m],
                                 results_PM_RF_SeqTime[,m],results_SM_RF_SeqTime[,m],results_GM_RF_SeqTime[,m]))
   
    colnames(modelCompMatrix) <- c("PM_NB_Mix", "SM_NB_Mix", "GM_NB_Mix", 
                                   "PM_RF_Mix", "SM_RF_Mix", "GM_RF_Mix",
                                   "PM_NB_Seq", "SM_NB_Seq", "GM_NB_Seq", 
                                   "PM_RF_Seq", "SM_RF_Seq", "GM_RF_Seq")
    modelCompResults <- sk_esd(modelCompMatrix)
    plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")
    
  }
  dev.off()
  
  
  pdf(file = paste(plotDir, "ExperimentationComp_SepAlg_Boxplots.pdf", sep = ""), width = 20, height = 7)
  par(mfrow = c(1, 4), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
  
    for(m in metricList){
      boxplot(results_PM_NB_MixTime[,m],results_SM_NB_MixTime[,m],results_GM_NB_MixTime[,m],
              results_PM_RF_MixTime[,m],results_SM_RF_MixTime[,m],results_GM_RF_MixTime[,m],
              results_PM_NB_SeqTime[,m],results_SM_NB_SeqTime[,m],results_GM_NB_SeqTime[,m],
              results_PM_RF_SeqTime[,m],results_SM_RF_SeqTime[,m],results_GM_RF_SeqTime[,m],
              ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, 
              names = c("PM_NB_Mix", "SM_NB_Mix", "GM_NB_Mix", 
                        "PM_RF_Mix", "SM_RF_Mix", "GM_RF_Mix",
                        "PM_NB_Seq", "SM_NB_Seq", "GM_NB_Seq", 
                        "PM_RF_Seq", "SM_RF_Seq", "GM_RF_Seq"))
      title(ylab = paste("", m), cex.lab = 1.4)
    

  }
  dev.off()
  
  }


