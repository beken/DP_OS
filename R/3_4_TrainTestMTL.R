### train test

library(xlsx)
library(RMTL)

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime"
baseExperimentSignature <- "MixTime" #SeqTime, MixTime
samplingOption <- "WO"
testSetMonths <- ""
# preparedTrainTestOrInterval <- "P" #P,I
# applyLogFilter <- FALSE
# applyDiscretization <- FALSE
# onlyLatentFeautes <- FALSE
# onlyTopics <- FALSE
# topNFeatures <- TRUE
# numberOfFeauresToBeSelected <- 10
# underOverSamplingOption <- 0 #(None, Under, Over, Both)
outExperimentSignature <- paste(baseExperimentSignature, "MTL", sep = "_")
setwd(mainProjectDir) 

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

loadModelBuildingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)


if(experimentType == "MixTime"){
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
    
    dir.create(paste(modelOutputDir, projectName, "/results", sep = ""), showWarnings = FALSE, recursive = TRUE)
    
    
    #first: create prediction files
    for(r in 1:numberOfRuns){
      #create directories to keep prediction results
      dir.create(paste(modelOutputDir, projectName, "/Run", r, sep = ""), showWarnings = FALSE, recursive = TRUE)
      
      allFoldPredictions <- list()
      
      for(f in 1:numberOfFolds){
        #info
        print(paste("Building model: ", projectName, " - run:", r, " - fold:", f, sep = ""))
        
        aggregatedData <- list()
        aggregatedData$Train <- list()
        aggregatedData$TrainLabels <- list()
        aggregatedData$Test <- list()
        aggregatedData$TestLabels <- list()
        
        for(d in 1:numberOfSelectedDev){
          authorName <- authorNames[d]
          
          testSet <- read.csv(paste(trainTestDataDir , projectName, "/", authorName, "/Run", r, "/", authorName, "Test", f, ".csv", sep = ""), header = TRUE)
          trainSetPM <- read.csv(paste(trainTestDataDir , projectName, "/", authorName, "/Run", r, "/", authorName, "TrainPM",f, ".csv", sep = ""), header = TRUE)
          
          aggregatedData$Train[[d]] <- as.matrix(trainSetPM[, -c(columnsToBeRemoved, ncol(trainSetPM))])
          aggregatedData$TrainLabels[[d]] <- as.matrix(ifelse(trainSetPM[, ncol(trainSetPM)] == "BUG", 1, -1))
          aggregatedData$Test[[d]] <- as.matrix(testSet[, -c(columnsToBeRemoved, ncol(testSet))])
          aggregatedData$TestLabels[[d]] <- as.matrix(ifelse(testSet[, ncol(testSet)] == "BUG", 1, -1))
        }
        
        # cvfit <- cvMTL(aggregatedData$Train, aggregatedData$TrainLabels, type = "Classification", Regularization = "L21", G = NULL)
        # model = MTL(aggregatedData$Train, aggregatedData$TrainLabels, type = "Classification", Regularization = "L21", Lam1 = cvfit$Lam1.min, Lam1_seq = cvfit$Lam1_seq, G = NULL)
        model = MTL(aggregatedData$Train, aggregatedData$TrainLabels, type = "Classification", Regularization = "CMTL", Lam1 = 0.1, Lam1_seq = NULL, G = NULL, k = 2)
        
        # model = MTL(aggregatedData$Train, aggregatedData$TrainLabels, type = "Classification",
        # Regularization = "L21", Lam1 = 0.1, Lam1_seq =NULL)
        
        # plotObj(model)
        
        predictionResults <- predict(model, aggregatedData$Test)
        predictedLabels <- lapply(1:numberOfSelectedDev, function(x) (round(predictionResults[[x]]) - 0.5) * 2)
        
        print(paste0("the test error is: ", calcError(model, newX = aggregatedData$Test, newY = aggregatedData$TestLabels)))
        # str(predict(model, aggregatedData$Test))
        # task_num <- length(aggregatedData$Test)
        # residue <- lapply(1:task_num, function(x) aggregatedData$TestLabels[[x]] - (round(predictionResults[[x]]) - 0.5) * 2)
        # error <- sapply(residue, function(x) {
        #   sum(x != 0)/length(x)
        # })
        
        actualPredicted <- lapply(1:numberOfSelectedDev, function(x) cbind(aggregatedData$TestLabels[[x]], predictedLabels[[x]]))
        allFoldPredictions[[f]] <- actualPredicted
        
        # newPredsToBeAdded <- cbind(as.character(tmpTestSet$CLASS), as.data.frame(predictionsPM))
        # colnames(newPredsToBeAdded)[1:2] <- c("actual", "predicted")
        # PredPM <- rbind(PredPM, newPredsToBeAdded)
        # write.csv(PredPM, paste(predictionResultsDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""))
      }
      
      for(d in 1:numberOfSelectedDev){
        authorName <- authorNames[d]
        
        #info
        print(paste("Writing prediction results model: ", projectName, " - run:", r, " - ", authorName, sep = ""))
        
        predictionsOfAuthor <- data.frame()
        
        for(f in 1:numberOfFolds){
          predictionsOfAuthor <- rbind(predictionsOfAuthor, allFoldPredictions[[f]][[d]])
        }
        colnames(predictionsOfAuthor) <- c("actual", "predicted")
        
        write.csv(file = paste(modelOutputDir, projectName, "/Run", r, "/", authorName, "_Pred.csv", sep = ""), x = predictionsOfAuthor, row.names = FALSE)
      }
    }
    
    #second: read previously created prediction files and calculate confusion matrix for each run
    #create directories to keep results
    dir.create(paste(modelOutputDir, projectName, "/results", sep = ""), showWarnings = FALSE, recursive = TRUE)
    
    for(d in 1:numberOfSelectedDev){
      authorName <- authorNames[d]
      
      #info
      print(paste("Calculating confusion matrix: ", authorName, sep = ""))
      
      results <- as.data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors=FALSE)
      colnames(results) <- c("TP", "FP", "FN", "TN", "precision", "recall", "pf", "F1")
      
      for(r in 1:numberOfRuns){
        predictionsOfAuthor <- read.csv(file = paste(modelOutputDir, projectName, "/Run", r, "/", authorName, "_Pred.csv", sep = ""))
        results <- rbind(results, calculateConfusionMatrix(predictionsOfAuthor, 1, -1))
      }
      
      write.csv(x = results, file = paste(modelOutputDir, projectName, "/results/", authorName, "Results.csv", sep = ""), row.names = FALSE)
    }
    
 
  }
}

  
