# evaluate the prediction results 

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime"
baseExperimentSignature <- "MixTime_UnderSamp" #SeqTime, MixTime
samplingOption <- "WO"
testSetMonths <- ""

setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

outExperimentSignature <- paste(baseExperimentSignature)

algorithms <- c("RF")

for(algorithmChoice in algorithms){
  if(algorithmChoice == algorithms[1])
    outExperimentSignature <- paste(outExperimentSignature, algorithmChoice, sep = "_")
  else
    outExperimentSignature <- paste(gsub(x = outExperimentSignature, pattern = "\\_[^\\_]*$", replacement = ""), algorithmChoice, sep = "_")
  
  loadModelBuildingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)
  
  if(experimentType == "MixTime"){
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      dir.create(paste(modelOutputDir, projectName, "/results", sep = ""), showWarnings = FALSE, recursive = TRUE)
      
      for(d in 1:numberOfSelectedDev){
        authorName <- authorNames[d]
        
        resultsPM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsPM) <- c("TP", "FP", "FN", "TN", metricList)
        resultsSM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsSM) <- c("TP", "FP", "FN", "TN", metricList)
        resultsGM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsGM) <- c("TP", "FP", "FN", "TN", metricList)
        resultsWPM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsWPM) <- c("TP", "FP", "FN", "TN", metricList)
        
        for(r in 1:numberOfRuns){
          PredPM <- data.frame()
          PredSM <- data.frame()
          PredGM <- data.frame()
          PredWPM <- data.frame()
          
          # for(f in 1:numberOfFolds){
          #   foldPredictionsPM <- read.csv(file = paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""))
          #   foldPredictionsSM <- read.csv(file = paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""))
          #   foldPredictionsGM <- read.csv(file = paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""))
          #   foldPredictionsWPM <- read.csv(file = paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""))
          # 
          #   PredPM <- rbind(PredPM, foldPredictionsPM)
          #   PredSM <- rbind(PredSM, foldPredictionsSM)
          #   PredGM <- rbind(PredGM, foldPredictionsGM)
          #   PredWPM <- rbind(PredWPM, foldPredictionsWPM)
          # }
          
          # ATTENTION: BELOW CONDITION IS NOT VALID FOR EVERY RF RESULTS, CHECK BEFORE RUNNING THE EVALUATION
          # if(algorithmChoice == "RF"){
          #   f <- numberOfFolds
          #   PredPM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
          #   PredSM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
          #   PredGM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
          #   PredWPM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)          }
          # else if(algorithmChoice == "NB"){
            for(f in 1:numberOfFolds){
              PredPM <- rbind(PredPM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              PredSM <- rbind(PredSM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              PredGM <- rbind(PredGM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              PredWPM <- rbind(PredWPM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            }
          # }
          
          resultsPM <- rbind(resultsPM, calculateConfusionMatrix(PredPM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
          resultsSM <- rbind(resultsSM, calculateConfusionMatrix(PredSM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
          resultsGM <- rbind(resultsGM, calculateConfusionMatrix(PredGM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
          resultsWPM <- rbind(resultsWPM, calculateConfusionMatrix(PredWPM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
        }
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), x = resultsPM, row.names = FALSE)
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), x = resultsSM, row.names = FALSE)
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), x = resultsGM, row.names = FALSE)
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), x = resultsWPM, row.names = FALSE)
        
        print(paste("evaluating the predictions... ", algorithmChoice, " - ", projectName, " - ", authorName, sep = ""))
      }
    }
  }
}
