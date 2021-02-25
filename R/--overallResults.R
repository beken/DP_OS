#overall results

library(xlsx)

rm(list=ls()) #clean the environment

mainStoreDir = "/Users/beken/R/ChangeData/Experiment_V2/"

NBWithDir = "/Users/beken/R/ChangeData/Experiment_V2/PredictionsNB/"
RFWithDir = "/Users/beken/R/ChangeData/Experiment_V2/PredictionsRF/"
NBWoDir = "/Users/beken/R/ChangeData/Experiment_V2/WoSampling/PredictionsNB/"
RFWoDir = "/Users/beken/R/ChangeData/Experiment_V2/WoSampling/PredictionsRF/"

directories <- c(NBWithDir, RFWithDir, NBWoDir, RFWoDir)

NofPofOption <- 1

if(NofPofOption == 0){
  metricList <- c("Pd", "Pf", "Precision", "F1")
  projectList <- c("Gimp", "Maven2", "Perl", "PostgreSql", "Rails", "Rhino")
  
  overallResultsTable <- data.frame(matrix(nrow = 6*5+2, ncol = 4*2+2))
  overallResultsTable[1, 3] <- "NB"
  overallResultsTable[1, 7] <- "RF"
  overallResultsTable[2, 3:6] <- c("pd", "pf", "precision", "F1")
  overallResultsTable[2, 7:10] <- c("pd", "pf", "precision", "F1")
  
  overallSepResultsTable <- data.frame(matrix(nrow = 6*5+3, ncol = 4*4+2))
  overallSepResultsTable[1, c(3, 7)] <- "With sampling"
  overallSepResultsTable[1, c(11, 15)] <- "Without sampling"
  overallSepResultsTable[2, c(3, 11)] <- "NB"
  overallSepResultsTable[2, c(7, 15)] <- "RF"
  overallSepResultsTable[3, 3:6] <- c("pd", "pf", "precision", "F1")
  overallSepResultsTable[3, 7:10] <- c("pd", "pf", "precision", "F1")
  overallSepResultsTable[3, 11:14] <- c("pd", "pf", "precision", "F1")
  overallSepResultsTable[3, 15:18] <- c("pd", "pf", "precision", "F1")
  
  
  for(p in 1:length(projectList)) {
    overallResultsTable[5 * (p - 1) + 3, 1] <- projectList[p]
    overallResultsTable[5 * (p - 1) + 3, 2] <- "PM"
    overallResultsTable[5 * (p - 1) + 4, 2] <- "SM"
    overallResultsTable[5 * (p - 1) + 5, 2] <- "GM"
    overallResultsTable[5 * (p - 1) + 6, 2] <- "WSM"
    overallResultsTable[5 * (p - 1) + 7, 2] <- "PM+"
    
    resultsAllNBWith <- read.xlsx(file = paste(NBWithDir, "ResultsAllNBWith v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllNBWo <- read.xlsx(file = paste(NBWoDir, "ResultsAllNBWo v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllRFWith <- read.xlsx(file = paste(RFWithDir, "ResultsAllRFWith v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllRFWo <- read.xlsx(file = paste(RFWoDir, "ResultsAllRFWo v2.xlsx", sep = ""), sheetIndex = projectList[p])
    
    for(m in 1:length(metricList)){
      overAllPM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" PM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" SM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" GM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" WSM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" Meta$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      
      overAllPM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" PM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" SM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" GM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" WSM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" Meta$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      
      overAllPM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" PM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" SM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" GM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" WSM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" Meta$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      
      overAllPM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" PM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" SM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" GM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" WSM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" Meta$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      
      
      overallResultsTable[5 * (p - 1) + 3, m + 2] <- median(c(overAllPM_NBWith, overAllPM_NBWo))
      overallResultsTable[5 * (p - 1) + 4, m + 2] <- median(c(overAllSM_NBWith, overAllSM_NBWo))
      overallResultsTable[5 * (p - 1) + 5, m + 2] <- median(c(overAllGM_NBWith, overAllGM_NBWo))
      overallResultsTable[5 * (p - 1) + 6, m + 2] <- median(c(overAllWSM_NBWith, overAllWSM_NBWo))
      overallResultsTable[5 * (p - 1) + 7, m + 2] <- median(c(overAllMeta_NBWith, overAllMeta_NBWo))
      
      overallResultsTable[5 * (p - 1) + 3, m + 6] <- median(c(overAllPM_RFWith, overAllPM_RFWo))
      overallResultsTable[5 * (p - 1) + 4, m + 6] <- median(c(overAllSM_RFWith, overAllSM_RFWo))
      overallResultsTable[5 * (p - 1) + 5, m + 6] <- median(c(overAllGM_RFWith, overAllGM_RFWo))
      overallResultsTable[5 * (p - 1) + 6, m + 6] <- median(c(overAllWSM_RFWith, overAllWSM_RFWo))
      overallResultsTable[5 * (p - 1) + 7, m + 6] <- median(c(overAllMeta_RFWith, overAllMeta_RFWo))
    }
  }
  
  for(p in 1:length(projectList)) {
    overallSepResultsTable[5 * (p - 1) + 4, 1] <- projectList[p]
    overallSepResultsTable[5 * (p - 1) + 4, 2] <- "PM"
    overallSepResultsTable[5 * (p - 1) + 5, 2] <- "SM"
    overallSepResultsTable[5 * (p - 1) + 6, 2] <- "GM"
    overallSepResultsTable[5 * (p - 1) + 7, 2] <- "WSM"
    overallSepResultsTable[5 * (p - 1) + 8, 2] <- "PM+"
    
    resultsAllNBWith <- read.xlsx(file = paste(NBWithDir, "ResultsAllNBWith v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllNBWo <- read.xlsx(file = paste(NBWoDir, "ResultsAllNBWo v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllRFWith <- read.xlsx(file = paste(RFWithDir, "ResultsAllRFWith v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllRFWo <- read.xlsx(file = paste(RFWoDir, "ResultsAllRFWo v2.xlsx", sep = ""), sheetIndex = projectList[p])
    
    for(m in 1:length(metricList)){
      overAllPM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" PM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" SM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" GM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" WSM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" Meta$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
      
      overAllPM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" PM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" SM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" GM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" WSM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" Meta$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
      
      overAllPM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" PM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" SM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" GM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" WSM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" Meta$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
      
      overAllPM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" PM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllSM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" SM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllGM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" GM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllWSM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" WSM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      overAllMeta_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" Meta$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
      
      
      overallSepResultsTable[5 * (p - 1) + 4, m + 2] <-median(overAllPM_NBWith)
      overallSepResultsTable[5 * (p - 1) + 5, m + 2] <- median(overAllSM_NBWith)
      overallSepResultsTable[5 * (p - 1) + 6, m + 2] <- median(overAllGM_NBWith)
      overallSepResultsTable[5 * (p - 1) + 7, m + 2] <- median(overAllWSM_NBWith)
      overallSepResultsTable[5 * (p - 1) + 8, m + 2] <- median(overAllMeta_NBWith)
      
      overallSepResultsTable[5 * (p - 1) + 4, m + 6] <- median(overAllPM_RFWith)
      overallSepResultsTable[5 * (p - 1) + 5, m + 6] <- median(overAllSM_RFWith)
      overallSepResultsTable[5 * (p - 1) + 6, m + 6] <- median(overAllGM_RFWith)
      overallSepResultsTable[5 * (p - 1) + 7, m + 6] <- median(overAllWSM_RFWith)
      overallSepResultsTable[5 * (p - 1) + 8, m + 6] <- median(overAllMeta_RFWith)
      
      overallSepResultsTable[5 * (p - 1) + 4, m + 10] <- median(overAllPM_NBWo)
      overallSepResultsTable[5 * (p - 1) + 5, m + 10] <- median(overAllSM_NBWo)
      overallSepResultsTable[5 * (p - 1) + 6, m + 10] <- median(overAllGM_NBWo)
      overallSepResultsTable[5 * (p - 1) + 7, m + 10] <- median(overAllWSM_NBWo)
      overallSepResultsTable[5 * (p - 1) + 8, m + 10] <- median(overAllMeta_NBWo)
      
      overallSepResultsTable[5 * (p - 1) + 4, m + 14] <- median(overAllPM_RFWo)
      overallSepResultsTable[5 * (p - 1) + 5, m + 14] <- median(overAllSM_RFWo)
      overallSepResultsTable[5 * (p - 1) + 6, m + 14] <- median(overAllGM_RFWo)
      overallSepResultsTable[5 * (p - 1) + 7, m + 14] <- median(overAllWSM_RFWo)
      overallSepResultsTable[5 * (p - 1) + 8, m + 14] <- median(overAllMeta_RFWo)
    }
  }
  
  resultsFile <- paste(mainStoreDir, "OverAllResults.xlsx", sep = "")
  write.xlsx(overallResultsTable, file = resultsFile, sheetName = "Overall", row.names = FALSE, col.names = FALSE, showNA = FALSE)
  write.xlsx(overallSepResultsTable, file = resultsFile, sheetName = "Overall-Separated", append = TRUE, row.names = FALSE, col.names = FALSE, showNA = FALSE)
}

if(NofPofOption == 1){
  #only separated table for now
  
  metricList <- c("Pd", "Pf", "Precision", "F1", "NofB20", "PofB20")
  projectList <- c("Gimp", "Maven2", "Perl", "PostgreSql", "Rails", "Rhino")
  
  # overallResultsTable <- data.frame(matrix(nrow = 6*5+2, ncol = 6*2+2)) #nrow = #projects * #models + 2 ----- #ncol = #metrics * #algorithms + 2
  # overallResultsTable[1, 3] <- "NB"
  # overallResultsTable[1, 9] <- "RF"
  # overallResultsTable[2, 3:8] <- c("pd", "pf", "precision", "F1", "NofB20", "PofB20")
  # overallResultsTable[2, 9:14] <- c("pd", "pf", "precision", "F1", "NofB20", "PofB20")
  # 
  overallSepResultsTable <- data.frame(matrix(nrow = 6*5+3, ncol = 6*4+2)) #nrow = #projects * #models + 2 ----- #ncol = #metrics * #algorithms * #dataSamplingMethods + 2
  overallSepResultsTable[1, c(3, 9)] <- "With sampling"
  overallSepResultsTable[1, c(15, 21)] <- "Without sampling"
  overallSepResultsTable[2, c(3, 15)] <- "NB"
  overallSepResultsTable[2, c(9, 21)] <- "RF"
  overallSepResultsTable[3, 3:8] <- c("pd", "pf", "precision", "F1", "NofB20", "PofB20")
  overallSepResultsTable[3, 9:14] <- c("pd", "pf", "precision", "F1", "NofB20", "PofB20")
  overallSepResultsTable[3, 15:20] <- c("pd", "pf", "precision", "F1", "NofB20", "PofB20")
  overallSepResultsTable[3, 21:26] <- c("pd", "pf", "precision", "F1", "NofB20", "PofB20")
  
  for(p in 1:length(projectList)){
    overallSepResultsTable[5 * (p - 1) + 4, 1] <- projectList[p]
    overallSepResultsTable[5 * (p - 1) + 4, 2] <- "PM"
    overallSepResultsTable[5 * (p - 1) + 5, 2] <- "SM"
    overallSepResultsTable[5 * (p - 1) + 6, 2] <- "GM"
    overallSepResultsTable[5 * (p - 1) + 7, 2] <- "WSM"
    overallSepResultsTable[5 * (p - 1) + 8, 2] <- "PM+"
    
    resultsAllNBWith <- read.xlsx(file = paste(NBWithDir, "ResultsAllNBWith v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllNBWo <- read.xlsx(file = paste(NBWoDir, "ResultsAllNBWo v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllRFWith <- read.xlsx(file = paste(RFWithDir, "ResultsAllRFWith v2.xlsx", sep = ""), sheetIndex = projectList[p])
    resultsAllRFWo <- read.xlsx(file = paste(RFWoDir, "ResultsAllRFWo v2.xlsx", sep = ""), sheetIndex = projectList[p])
    
    for(m in 1:length(metricList)){
      if(m == 1 | m == 2 | m == 3 | m == 4){
        overAllPM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" PM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllSM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" SM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllGM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" GM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllWSM_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" WSM$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllMeta_NBWith <- (as.numeric(as.character(resultsAllNBWith[grepl(" Meta$", resultsAllNBWith[, 1]), paste("median.", metricList[m], sep = "")])))
        
        overAllPM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" PM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllSM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" SM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllGM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" GM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllWSM_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" WSM$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllMeta_NBWo <- (as.numeric(as.character(resultsAllNBWo[grepl(" Meta$", resultsAllNBWo[, 1]), paste("median.", metricList[m], sep = "")])))
        
        overAllPM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" PM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllSM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" SM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllGM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" GM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllWSM_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" WSM$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllMeta_RFWith <- (as.numeric(as.character(resultsAllRFWith[grepl(" Meta$", resultsAllRFWith[, 1]), paste("median.", metricList[m], sep = "")])))
        
        overAllPM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" PM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllSM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" SM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllGM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" GM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllWSM_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" WSM$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
        overAllMeta_RFWo <- (as.numeric(as.character(resultsAllRFWo[grepl(" Meta$", resultsAllRFWo[, 1]), paste("median.", metricList[m], sep = "")])))
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 2] <- median(overAllPM_NBWith)
        overallSepResultsTable[5 * (p - 1) + 5, m + 2] <- median(overAllSM_NBWith)
        overallSepResultsTable[5 * (p - 1) + 6, m + 2] <- median(overAllGM_NBWith)
        overallSepResultsTable[5 * (p - 1) + 7, m + 2] <- median(overAllWSM_NBWith)
        overallSepResultsTable[5 * (p - 1) + 8, m + 2] <- median(overAllMeta_NBWith)
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 8] <- median(overAllPM_RFWith)
        overallSepResultsTable[5 * (p - 1) + 5, m + 8] <- median(overAllSM_RFWith)
        overallSepResultsTable[5 * (p - 1) + 6, m + 8] <- median(overAllGM_RFWith)
        overallSepResultsTable[5 * (p - 1) + 7, m + 8] <- median(overAllWSM_RFWith)
        overallSepResultsTable[5 * (p - 1) + 8, m + 8] <- median(overAllMeta_RFWith)
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 14] <- median(overAllPM_NBWo)
        overallSepResultsTable[5 * (p - 1) + 5, m + 14] <- median(overAllSM_NBWo)
        overallSepResultsTable[5 * (p - 1) + 6, m + 14] <- median(overAllGM_NBWo)
        overallSepResultsTable[5 * (p - 1) + 7, m + 14] <- median(overAllWSM_NBWo)
        overallSepResultsTable[5 * (p - 1) + 8, m + 14] <- median(overAllMeta_NBWo)
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 20] <- median(overAllPM_RFWo)
        overallSepResultsTable[5 * (p - 1) + 5, m + 20] <- median(overAllSM_RFWo)
        overallSepResultsTable[5 * (p - 1) + 6, m + 20] <- median(overAllGM_RFWo)
        overallSepResultsTable[5 * (p - 1) + 7, m + 20] <- median(overAllWSM_RFWo)
        overallSepResultsTable[5 * (p - 1) + 8, m + 20] <- median(overAllMeta_RFWo)
      }
      else if(m == 5){
          resultsNofNBWith <- read.xlsx(file = paste(NBWithDir, projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
          resultsNofNBWo <- read.xlsx(file = paste(NBWoDir,  projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
          resultsNofRFWith <- read.xlsx(file = paste(RFWithDir,  projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
          resultsNofRFWo <- read.xlsx(file = paste(RFWoDir,  projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
          
          overallSepResultsTable[5 * (p - 1) + 4, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 2]
          overallSepResultsTable[5 * (p - 1) + 5, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 3]
          overallSepResultsTable[5 * (p - 1) + 6, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 4]
          overallSepResultsTable[5 * (p - 1) + 7, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 5]
          overallSepResultsTable[5 * (p - 1) + 8, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 6]
          
          overallSepResultsTable[5 * (p - 1) + 4, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 2]
          overallSepResultsTable[5 * (p - 1) + 5, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 3]
          overallSepResultsTable[5 * (p - 1) + 6, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 4]
          overallSepResultsTable[5 * (p - 1) + 7, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 5]
          overallSepResultsTable[5 * (p - 1) + 8, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 6]
          
          overallSepResultsTable[5 * (p - 1) + 4, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 2]
          overallSepResultsTable[5 * (p - 1) + 5, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 3]
          overallSepResultsTable[5 * (p - 1) + 6, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 4]
          overallSepResultsTable[5 * (p - 1) + 7, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 5]
          overallSepResultsTable[5 * (p - 1) + 8, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 6]
          
          overallSepResultsTable[5 * (p - 1) + 4, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 2]
          overallSepResultsTable[5 * (p - 1) + 5, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 3]
          overallSepResultsTable[5 * (p - 1) + 6, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 4]
          overallSepResultsTable[5 * (p - 1) + 7, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 5]
          overallSepResultsTable[5 * (p - 1) + 8, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 6]
      }
      else if(m == 6){
        resultsNofNBWith <- read.xlsx(file = paste(NBWithDir, projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
        resultsNofNBWo <- read.xlsx(file = paste(NBWoDir,  projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
        resultsNofRFWith <- read.xlsx(file = paste(RFWithDir,  projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
        resultsNofRFWo <- read.xlsx(file = paste(RFWoDir,  projectList[p], "/results/", projectList[p], "NofB20.xlsx", sep = ""), sheetIndex = 1)
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 2] / resultsNofNBWith[nrow(resultsNofNBWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 5, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 3] / resultsNofNBWith[nrow(resultsNofNBWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 6, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 4] / resultsNofNBWith[nrow(resultsNofNBWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 7, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 5] / resultsNofNBWith[nrow(resultsNofNBWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 8, m + 2] <- resultsNofNBWith[nrow(resultsNofNBWith) - 1, 6] / resultsNofNBWith[nrow(resultsNofNBWith) - 1, 12] * 100
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 2] / resultsNofNBWo[nrow(resultsNofNBWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 5, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 3] / resultsNofNBWo[nrow(resultsNofNBWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 6, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 4] / resultsNofNBWo[nrow(resultsNofNBWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 7, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 5] / resultsNofNBWo[nrow(resultsNofNBWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 8, m + 8] <- resultsNofNBWo[nrow(resultsNofNBWo) - 1, 6] / resultsNofNBWo[nrow(resultsNofNBWo) - 1, 12] * 100
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 2] / resultsNofRFWith[nrow(resultsNofRFWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 5, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 3] / resultsNofRFWith[nrow(resultsNofRFWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 6, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 4] / resultsNofRFWith[nrow(resultsNofRFWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 7, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 5] / resultsNofRFWith[nrow(resultsNofRFWith) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 8, m + 14] <- resultsNofRFWith[nrow(resultsNofRFWith) - 1, 6] / resultsNofRFWith[nrow(resultsNofRFWith) - 1, 12] * 100
        
        overallSepResultsTable[5 * (p - 1) + 4, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 2] / resultsNofRFWo[nrow(resultsNofRFWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 5, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 3] / resultsNofRFWo[nrow(resultsNofRFWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 6, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 4] / resultsNofRFWo[nrow(resultsNofRFWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 7, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 5] / resultsNofRFWo[nrow(resultsNofRFWo) - 1, 12] * 100
        overallSepResultsTable[5 * (p - 1) + 8, m + 20] <- resultsNofRFWo[nrow(resultsNofRFWo) - 1, 6] / resultsNofRFWo[nrow(resultsNofRFWo) - 1, 12] * 100
      }
    }
  }
  
  resultsFile <- paste(mainStoreDir, "OverAllNofResults.xlsx", sep = "")
  write.xlsx(overallSepResultsTable, file = resultsFile, sheetName = "Overall-Separated", append = TRUE, row.names = FALSE, col.names = FALSE, showNA = FALSE)
}
