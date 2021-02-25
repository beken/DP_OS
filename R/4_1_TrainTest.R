# Train test operations

library(xlsx)
library(caret)
library(FSelector)
library(RWeka)
library(e1071)
library(xgboost)
library(hyperSMURF)
library(h2o)
library(ROSE)
library(DMwR)

# h2o.init(
#   nthreads = -1,          ## -1: use all available threads
#   min_mem_size = "4G")    ## specify the memory size for the H2O cloud

# h2o.shutdown(prompt = FALSE)


rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
setwd(mainProjectDir)

experimentType <- "MixTime"
devsDataSizeOption.Mix <- "Neq" #Neq, Eq
dataBalancingOption <- "SMOTE" # None, Under, SMOTE
developerSelectionVersion <- "SelectedV2"
preparedTrainTestOrInterval <- "P" #P,I
applyLogFilter <- FALSE

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

baseExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix,
                                 ifelse(dataBalancingOption == "None", "", paste("_", dataBalancingOption, sep = "")), 
                                 sep = "")
outExperimentSignature <- paste(baseExperimentSignature,
                                ifelse(developerSelectionVersion == "SelectedV2", "DevsV2", "DevsV1"),
                                sep = "_")

for(algorithmChoice in algorithms){
  if(algorithmChoice == algorithms[1])
    outExperimentSignature <- paste(outExperimentSignature, algorithmChoice, sep = "_")
  else
    outExperimentSignature <- paste(gsub(x = outExperimentSignature, pattern = "\\_[^\\_]*$", replacement = ""), algorithmChoice, sep = "_")

  loadModelBuildingEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature)
  
  if(experimentType == "MixTime"){
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(projectName)
      loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
      
      projectAllCommits <- get(paste(projectName, "Changes", sep = ""))
      
      dir.create(paste(modelOutputDir, projectName, "/results", sep = ""), showWarnings = FALSE, recursive = TRUE)

      for(d in 1:numberOfSelectedAuthors){
        authorName <- selectedAuthorNames[d]
        authorAlias <- selectedAuthorAliases[d]
       
        # bazen sadece elimdeki hazir verilerle train yapmak isteyebilirim
        # (SMOTE ciktisi yavas oldugu icin bazı developer verileri hazir olmayabilir)
        # o zaman bu developerin Run10/TrainGM10.csv dosyasi var mi diye bakarim
        # cunku bu dosya bir developer icin en son kaydedilen dosya, eger bu varsa bu developerin tum verileri muhtemelen hazir demektir
        # yoksa o developer training adimini atlarim ve bir sonraki developer ile devam ederim (continue with the next step of the loop)
        authorDataDir <- paste(trainTestDataDir, projectName, "/", authorAlias, "/Run10/", authorAlias, "_TrainGM10", ".csv", sep = "")
        if(!file.exists(authorDataDir)){
          print(paste("Author data is not available @", authorDataDir))
          next;
        }
        
        resultsPM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsPM) <- c("TP", "FP", "FN", "TN", metricList)
        resultsSM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsSM) <- c("TP", "FP", "FN", "TN", metricList)
        resultsGM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsGM) <- c("TP", "FP", "FN", "TN", metricList)
        # resultsWPM <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        # colnames(resultsWPM) <- c("TP", "FP", "FN", "TN", metricList)
        
        for(r in 1:numberOfRuns){
          print(paste(projectName, " - ", authorName, " - ", authorAlias, " - run:", r, sep = ""))
          
          dir.create(paste(modelOutputDir, projectName, "/", authorAlias, "/Run", r, sep = ""), showWarnings = FALSE, recursive = TRUE)
          
          PredPM <- data.frame()
          PredSM <- data.frame()
          PredGM <- data.frame()
          # PredWPM <- data.frame()
          
          for(f in 1:numberOfFolds){
            if(dataBalancingOption == "None"){
              # read hash ids of train-test sets 
              testSet.HashIds <- read.csv(paste(trainTestDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              trainSetPM.HashIds <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM",f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              trainSetSM.HashIds <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              trainSetGM.HashIds <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              # trainSetWPM <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorName, "_TrainWPM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              
              # gather other columns of data (features) matched with the hash ids
              testSet <- projectAllCommits[projectAllCommits$HASHID %in% testSet.HashIds[, 1], c(processMetrics, "CLASS")]
              trainSetPM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetPM.HashIds[, 1], c(processMetrics, "CLASS")]
              trainSetSM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetSM.HashIds[, 1], c(processMetrics, "CLASS")]
              trainSetGM <- projectAllCommits[projectAllCommits$HASHID %in% trainSetGM.HashIds[, 1], c(processMetrics, "CLASS")]
              
              # shuffle data
              trainSetPM <- trainSetPM[sample(nrow(trainSetPM), replace = FALSE), ]
              trainSetSM <- trainSetSM[sample(nrow(trainSetSM), replace = FALSE), ]
              trainSetGM <- trainSetGM[sample(nrow(trainSetGM), replace = FALSE), ]
            }
            else # if(dataBalancingOption == "Under"){
            {
              # train-test sets 
              testSet <- read.csv(paste(trainTestDataDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_Test", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              trainSetPM <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainPM",f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              trainSetSM <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainSM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
              trainSetGM <- read.csv(paste(trainTestDataDir , projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_TrainGM", f, ".csv", sep = ""), header = TRUE)#, stringsAsFactors = FALSE)
            }
            
            # log filtering option is not used
            # results of log filtering are not reported in the paper, we just tried it
            if(applyLogFilter){
              trainSetPM <- logFilter(trainSetPM) 
              trainSetSM <- logFilter(trainSetSM) 
              trainSetGM <- logFilter(trainSetGM) 
              testSet <- logFilter(testSet) 
            }
            
            switch(algorithmChoice,
                   NB = {
                     modelPMNB <- naiveBayes(x = trainSetPM[, 1:(length(trainSetPM) - 1)], y = as.factor(trainSetPM$CLASS))
                     foldPredictionsPM <- cbind(as.character(testSet$CLASS),
                                            as.data.frame(predict(modelPMNB, newdata = testSet, type = "class")),
                                            as.data.frame(predict(modelPMNB, newdata = testSet, type = "raw")))
                     colnames(foldPredictionsPM)[1:2] <- c("actual", "predicted")
                     PredPM <- rbind(PredPM, foldPredictionsPM)
                     
                     modelSMNB <- naiveBayes(x = trainSetSM[, 1:(length(trainSetSM) - 1)], y = as.factor(trainSetSM$CLASS))
                     foldPredictionsSM <- cbind(as.character(testSet$CLASS),
                                            as.data.frame(predict(modelSMNB, newdata = testSet, type = "class")),
                                            as.data.frame(predict(modelSMNB, newdata = testSet, type = "raw")))
                     colnames(foldPredictionsSM)[1:2] <- c("actual", "predicted")
                     PredSM <- rbind(PredSM, foldPredictionsSM)
                     
                     modelGMNB <- naiveBayes(x = trainSetGM[, 1:(length(trainSetGM) - 1)], y = as.factor(trainSetGM$CLASS))
                     foldPredictionsGM <- cbind(as.character(testSet$CLASS),
                                            as.data.frame(predict(modelGMNB, newdata = testSet, type = "class")),
                                            as.data.frame(predict(modelGMNB, newdata = testSet, type = "raw")))
                     colnames(foldPredictionsGM)[1:2] <- c("actual", "predicted")
                     PredGM <- rbind(PredGM, foldPredictionsGM)
                     
                     # modelWPMNB <- naiveBayes(x = trainSetWPM[, 1:(length(trainSetWPM) - 1)], y = as.factor(trainSetWPM$CLASS))
                     # foldPredictionsWPM <- cbind(as.character(testSet$CLASS),
                     #                        as.data.frame(predict(modelWPMNB, newdata = testSet, type = "class")),
                     #                        as.data.frame(predict(modelWPMNB, newdata = testSet, type = "raw")))
                     # colnames(foldPredictionsWPM)[1:2] <- c("actual", "predicted")
                     # PredWPM <- rbind(PredWPM, foldPredictionsWPM)
                   },
                   RF = {
                     modelPMRF <- h2o.randomForest(training_frame = as.h2o(trainSetPM), x = 1:(ncol(trainSetPM) - 1), y = ncol(trainSetPM), ntrees = 50, seed = 1000000, nfolds = 0, max_depth = 10)
                     predictionsPM <- predict(modelPMRF, as.h2o(tmpTestSet))
                     foldPredictionsPM <- cbind(as.character(tmpTestSet$CLASS), as.data.frame(predictionsPM))
                     colnames(foldPredictionsPM)[1:2] <- c("actual", "predicted")
                     PredPM <- rbind(PredPM, foldPredictionsPM)
                     
                     modelSMRF <- h2o.randomForest(training_frame = as.h2o(tmpTrainSetSM), x = 1:(ncol(tmpTrainSetSM) - 1), y = ncol(tmpTrainSetSM), ntrees = 50, seed = 1000000, nfolds = 0, max_depth = 10)
                     predictionsSM <- predict(modelSMRF, as.h2o(tmpTestSet))
                     foldPredictionsSM <- cbind(as.character(tmpTestSet$CLASS), as.data.frame(predictionsSM))
                     colnames(foldPredictionsSM)[1:2] <- c("actual", "predicted")
                     PredSM <- rbind(PredSM, foldPredictionsSM)
                     
                     modelGMRF <- h2o.randomForest(training_frame = as.h2o(tmpTrainSetGM), x = 1:(ncol(tmpTrainSetGM) - 1), y = ncol(tmpTrainSetGM), seed = 1000000, nfolds = 0, max_depth = 10)
                     predictionsGM <- predict(modelGMRF, as.h2o(tmpTestSet))
                     foldPredictionsGM <- cbind(as.character(tmpTestSet$CLASS), as.data.frame(predictionsGM))
                     colnames(foldPredictionsGM)[1:2] <- c("actual", "predicted")
                     PredGM <- rbind(PredGM, foldPredictionsGM)
                     
                     # modelWPMRF <- h2o.randomForest(training_frame = as.h2o(tmpTrainSetWPM), x = 1:(ncol(tmpTrainSetWPM) - 1), y = ncol(tmpTrainSetWPM), seed = 1000000, nfolds = 0, max_depth = 10)
                     # predictionsWPM <- predict(modelWPMRF, as.h2o(tmpTestSet))
                     # foldPredictionsWPM <- cbind(as.character(tmpTestSet$CLASS), as.data.frame(predictionsWPM))
                     # colnames(foldPredictionsWPM)[1:2] <- c("actual", "predicted")
                     # PredWPM <- rbind(PredWPM, foldPredictionsWPM)
                   },
                   HyperSMURF = {
                     # https://cran.r-project.org/web/packages/hyperSMURF/hyperSMURF.pdf
                     # fp: multiplicative factor for the SMOTE oversampling of the minority class. If fp<1 no oversampling is performed.
                     # 18.02.2021: hypersmurf training sirasinda oversampling fp = 0.5 alinca oversampling yapmamis olduk.
                     # majority classi hep NO_BUG aldik, bunu düzeltelim
                     
                     buggyCount <- length(which(trainSetPM$CLASS == "BUG"))
                     noBuggyCount <- length(which(trainSetPM$CLASS == "NO_BUG"))
                     # if the majority class is NO_BUG
                     if(buggyCount < noBuggyCount){
                       tmpLabels <- ifelse(trainSetPM$CLASS == "BUG", 1, 0)
                       ratio <- length(which(trainSetPM$CLASS == "NO_BUG")) / length(which(trainSetPM$CLASS == "BUG"))
                       modelHyper <- hyperSMURF.train(trainSetPM[, 1:(length(trainSetPM) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                       foldPredictionsPM <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                       colnames(foldPredictionsPM) <- c("actual", "predicted", "BUG", "NO_BUG")
                       PredPM <- rbind(PredPM, foldPredictionsPM)
                       
                       print("do nothing")
                     }
                     # if the majority class is BUG
                     else{
                       tmpLabels <- ifelse(trainSetPM$CLASS == "NO_BUG", 1, 0)
                       ratio <- length(which(trainSetPM$CLASS == "BUG")) / length(which(trainSetPM$CLASS == "NO_BUG"))
                       modelHyper <- hyperSMURF.train(trainSetPM[, 1:(length(trainSetPM) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "NO_BUG", "BUG");
                       foldPredictionsPM <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), 1 - predProbs, predProbs)
                       colnames(foldPredictionsPM) <- c("actual", "predicted", "BUG", "NO_BUG")
                       PredPM <- rbind(PredPM, foldPredictionsPM)
                     }
                     
                     buggyCount <- length(which(trainSetSM$CLASS == "BUG"))
                     noBuggyCount <- length(which(trainSetSM$CLASS == "NO_BUG"))
                     # if the majority class is NO_BUG
                     if(buggyCount < noBuggyCount){
                       tmpLabels <- ifelse(trainSetSM$CLASS == "BUG", 1, 0)
                       ratio <- length(which(trainSetSM$CLASS == "NO_BUG")) / length(which(trainSetSM$CLASS == "BUG"))
                       modelHyper <- hyperSMURF.train(trainSetSM[, 1:(length(trainSetSM) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                       foldPredictionsSM <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                       colnames(foldPredictionsSM) <- c("actual", "predicted", "BUG", "NO_BUG")
                       PredSM <- rbind(PredSM, foldPredictionsSM)
                       
                       print("do nothing")
                     }
                     # if the majority class is BUG
                     else{
                       tmpLabels <- ifelse(trainSetSM$CLASS == "NO_BUG", 1, 0)
                       ratio <- length(which(trainSetSM$CLASS == "BUG")) / length(which(trainSetSM$CLASS == "NO_BUG"))  
                       modelHyper <- hyperSMURF.train(trainSetSM[, 1:(length(trainSetSM) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "NO_BUG", "BUG");
                       foldPredictionsSM <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), 1 - predProbs, predProbs)
                       colnames(foldPredictionsSM) <- c("actual", "predicted", "BUG", "NO_BUG")
                       PredSM <- rbind(PredSM, foldPredictionsSM)
                     }
                      
                     buggyCount <- length(which(trainSetGM$CLASS == "BUG"))
                     noBuggyCount <- length(which(trainSetGM$CLASS == "NO_BUG"))
                     # if the majority class is NO_BUG
                     if(buggyCount < noBuggyCount){
                       tmpLabels <- ifelse(trainSetGM$CLASS == "BUG", 1, 0)
                       ratio <- length(which(trainSetGM$CLASS == "NO_BUG")) / length(which(trainSetGM$CLASS == "BUG"))
                       modelHyper <- hyperSMURF.train(trainSetGM[, 1:(length(trainSetGM) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                       foldPredictionsGM <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                       colnames(foldPredictionsGM) <- c("actual", "predicted", "BUG", "NO_BUG")
                       PredGM <- rbind(PredGM, foldPredictionsGM)

                       print("do nothing")
                     }
                     # if the majority class is BUG
                     else{
                       tmpLabels <- ifelse(trainSetGM$CLASS == "NO_BUG", 1, 0)
                       ratio <- length(which(trainSetGM$CLASS == "BUG")) / length(which(trainSetGM$CLASS == "NO_BUG"))  
                       modelHyper <- hyperSMURF.train(trainSetGM[, 1:(length(trainSetGM) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "NO_BUG", "BUG");
                       foldPredictionsGM <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), 1 - predProbs, predProbs)
                       colnames(foldPredictionsGM) <- c("actual", "predicted", "BUG", "NO_BUG")
                       PredGM <- rbind(PredGM, foldPredictionsGM)
                     }
                   },
                   stop("Enter something that switches me!")
            )
            
            write.csv(foldPredictionsPM, paste(modelOutputDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_PredPM", f, ".csv", sep = ""))
            write.csv(foldPredictionsSM, paste(modelOutputDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_PredSM", f, ".csv", sep = ""))
            write.csv(foldPredictionsGM, paste(modelOutputDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_PredGM", f, ".csv", sep = ""))
            # write.csv(foldPredictionsWPM, paste(modelOutputDir, projectName, "/", authorAlias, "/Run", r, "/", authorAlias, "_PredWPM", f, ".csv", sep = ""))
          }
          
          resultsPM <- rbind(resultsPM, calculateConfusionMatrix(PredPM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
          resultsSM <- rbind(resultsSM, calculateConfusionMatrix(PredSM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
          resultsGM <- rbind(resultsGM, calculateConfusionMatrix(PredGM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
          # resultsWPM <- rbind(resultsWPM, calculateConfusionMatrix(PredWPM, positiveLabel = "BUG", negativeLabel = "NO_BUG"))
        }
        
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorAlias, "_ResultsPM.csv", sep = ""), x = resultsPM, row.names = FALSE)
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorAlias, "_ResultsSM.csv", sep = ""), x = resultsSM, row.names = FALSE)
        write.csv(file = paste(modelOutputDir, projectName, "/results/", authorAlias, "_ResultsGM.csv", sep = ""), x = resultsGM, row.names = FALSE)
        # write.csv(file = paste(modelOutputDir, projectName, "/results/", authorAlias, "_ResultsWPM.csv", sep = ""), x = resultsWPM, row.names = FALSE)
      }
    }
  } 
  else if(experimentType == "SeqTime"){
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      dir.create(paste(modelOutputDir, projectName, "/results", sep = ""), showWarnings = FALSE, recursive = TRUE)
      
      for(d in 1:numberOfSelectedDev){
        authorName <- authorAliases[d] 
        
        testIntervals <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))[d, paste("TestIntervals_", testSetMonths, "Months", sep = "")])
        testIntervals <- unlist(strsplit(testIntervals, "-", fixed = TRUE))
        
        #TM is not exist for OS yet
        if(authorName == "TM"){ 
          dir.create(paste(modelOutputDir, projectName, "/", sep = ""), recursive = T, showWarnings = F)
          
          results <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
          colnames(results) <- c("TP", "FP", "FN", "TN", metricList)
          
          for(interval in testIntervals){
            dataset <- getIntervalDataset(trainTestDataDir, projectName, authorName, interval, testSetMonths, 
                                          "", preparedTrainTestOrInterval, "csv")
            
            trainSet <- dataset[[1]][, -columnsToBeRemoved.Seq]
            testSet <- dataset[[2]][, -columnsToBeRemoved.Seq]
            
            switch(dataBalancingOption,
                   Over = {
                     trainSet <- upSample(x = trainSet[, -ncol(trainSet)], y = trainSet$CLASS)     
                     colnames(trainSet)[ncol(trainSet)] = "CLASS"
                     trainSet <- trainSet[sample(nrow(trainSet)), ]
                   },
                   Under = {
                     trainSet <- ovun.sample(CLASS~ ., trainSet, method = "under")$data
                     trainSet <- trainSet[sample(nrow(trainSet)), ]
                   },
                   Both = { 
                     trainSet <- SMOTE(CLASS~ ., trainSet, k = 1)
                     trainSet <- trainSet[sample(nrow(trainSet)), ]
                   }
            )
            
            if(applyLogFilter){
              trainSet <- logFilter(trainSet) 
              testSet <- logFilter(testSet) 
            }
            
            if(topNFeatures){
              topfeautes <- getTopFeaturesByInfoGain(trainSet, numberOfFeauresToBeSelected)
              trainSet <- trainSet[, c(topfeautes, "CLASS")]
              testSet <- testSet[, c(topfeautes, "CLASS")]
            }
            
            # numericCols <- sapply(trainSet, is.numeric)
            foldPredictions <- data.frame()
            Pred <- data.frame()
            switch(algorithmChoice,
                   NB = {
                     modelNB <- naiveBayes(x = trainSet[, 1:(length(trainSet) - 1)], y = trainSet$CLASS)
                     foldPredictions <- cbind(as.character(testSet$CLASS),
                                              as.data.frame(predict(modelNB, newdata = testSet, type = "class")),
                                              as.data.frame(predict(modelNB, newdata = testSet, type = "raw")))
                     colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                     Pred <- rbind(Pred, foldPredictions)
                   },
                   RF = {
                     if(!chekcIfAllCellsAreEqual(trainSet[, -ncol(trainSet)])){
                       modelRF <- h2o.randomForest(training_frame = as.h2o(trainSet), x = 1:(ncol(trainSet) - 1), y = ncol(trainSet), build_tree_one_node = F, nbins = 10, ntrees = 500, seed = 1000000, max_depth = 50)
                       predictions <- predict(modelRF, as.h2o(testSet))
                       foldPredictions <- cbind(as.character(testSet$CLASS), as.data.frame(predictions))
                       colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                       Pred <- rbind(Pred, foldPredictions)
                     }
                   },
                   HyperSMURF = {
                     if(!chekcIfAllCellsAreEqual(trainSet[, -ncol(trainSet)])){
                       tmpLabels <- ifelse(trainSet$CLASS == "BUG", 1, 0)
                       ratio <- length(which(trainSet$CLASS == "NO_BUG")) / length(which(trainSet$CLASS == "BUG"))  
                       modelHyper <- hyperSMURF.train(trainSet[, 1:(length(trainSet) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                       predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                       predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                       foldPredictions <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                       colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                       Pred <- rbind(Pred, foldPredictions)
                     }
                   },
                   XGBoost = {
                     modelXGBoost <- xgboost(data = as.matrix(trainSet[, 1:(length(trainSet) - 1)]), 
                                             label = ifelse(trainSet$CLASS == "BUG", 1, 0),
                                             subsample = 0.5,
                                             max.depth = 6, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)
                     predProbs <- predict(modelXGBoost, as.matrix(testSet[, 1:(length(testSet) - 1)]))
                     predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                     foldPredictions <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                     colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                     Pred <- rbind(Pred, foldPredictions)
                   }
            )
            write.csv(Pred, paste(modelOutputDir, projectName, "/", "Test", interval, "_PredTM", ".csv", sep = ""))
            
            #info
            print(paste(algorithmChoice, " - ", projectName, " - ", authorName, " - test_interval:", interval, sep = ""))
            
            if(nrow(Pred) > 0)
              results <- rbind(results, calculateConfusionMatrix(Pred, "BUG", "NO_BUG"))
          }
          write.csv(file = paste(modelOutputDir, projectName, "/results/", "ResultsTM.csv", sep = ""), x = results, row.names = FALSE)
        }
        else{
          dir.create(paste(modelOutputDir, projectName, "/", authorName, sep = ""), recursive = T, showWarnings = F)
          
          for(modelType in modelTypes){
            results <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
            colnames(results) <- c("TP", "FP", "FN", "TN", metricList)
            
            for(interval in testIntervals){
              dataset <- getIntervalDataset(trainTestDataDir, projectName, authorName, interval, testSetMonths, modelType, preparedTrainTestOrInterval, "csv")    
   
              if(dataset != dataIsNotExist){
                trainSet <- dataset[[1]][, -columnsToBeRemoved.Seq]
                testSet <- dataset[[2]][, -columnsToBeRemoved.Seq]
                
                switch(dataBalancingOption,
                       Over = {
                         trainSet <- upSample(x = trainSet[, -ncol(trainSet)], y = trainSet$CLASS)     
                         colnames(trainSet)[ncol(trainSet)] = "CLASS"
                         trainSet <- trainSet[sample(nrow(trainSet)), ]
                       },
                       Under = {
                         # ATTENTION
                         bugRatioInTrainingSet <- length(which(trainSet$CLASS == "NO_BUG")) / nrow(trainSet)
                         if(bugRatioInTrainingSet < 0.4){
                           trainSet <- ovun.sample(CLASS~ ., trainSet, method = "under")$data
                           trainSet <- trainSet[sample(nrow(trainSet)), ]
                         }
                       },
                       Both = { 
                         trainSet <- SMOTE(CLASS~ ., trainSet, k = 1)
                         trainSet <- trainSet[sample(nrow(trainSet)), ]
                       }
                )
                
                if(applyLogFilter){
                  trainSet <- logFilter(trainSet) 
                  testSet <- logFilter(testSet) 
                }
                
                if(topNFeatures){
                  topfeautes <- getTopFeaturesByInfoGain(trainSet, numberOfFeauresToBeSelected)
                  trainSet <- trainSet[, c(topfeautes, "CLASS")]
                  testSet <- testSet[, c(topfeautes, "CLASS")]
                }
                
                # numericCols <- sapply(trainSet, is.numeric)
                # ACTUALLY, in seq time exp. there is not need to keep foldPredictions, so I reset the content of foldPredictions below
                # but it is same content as Pred
                foldPredictions <- data.frame()
                Pred <- data.frame()
                switch(algorithmChoice,
                       NB = {
                         modelNB <- naiveBayes(x = trainSet[, 1:(length(trainSet) - 1)], y = trainSet$CLASS)
                         foldPredictions <- cbind(as.character(testSet$CLASS),
                                                  as.data.frame(predict(modelNB, newdata = testSet, type = "class")),
                                                  as.data.frame(predict(modelNB, newdata = testSet, type = "raw")))
                         colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                         Pred <- rbind(Pred, foldPredictions)
                       },
                       RF = {
                         if(!chekcIfAllCellsAreEqual(trainSet[, -ncol(trainSet)])){
                           # modelRF <- h2o.randomForest(training_frame = as.h2o(trainSet), x = 1:(ncol(trainSet) - 1), y = ncol(trainSet), build_tree_one_node = F, nbins = 10, ntrees = 500, seed = 1000000, max_depth = 50)
                           modelRF <- h2o.randomForest(training_frame = as.h2o(trainSet), x = 1:(ncol(trainSet) - 1), y = ncol(trainSet), build_tree_one_node = F, nbins = 10, ntrees = 50, seed = 1000000, max_depth = 50)
                           predictions <- predict(modelRF, as.h2o(testSet))
                           foldPredictions <- cbind(as.character(testSet$CLASS), as.data.frame(predictions))
                           colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                           Pred <- rbind(Pred, foldPredictions)
                         }
                       },
                       HyperSMURF = {
                         if(!chekcIfAllCellsAreEqual(trainSet[, -ncol(trainSet)])){
                           tmpLabels <- ifelse(trainSet$CLASS == "BUG", 1, 0)
                           ratio <- length(which(trainSet$CLASS == "NO_BUG")) / length(which(trainSet$CLASS == "BUG"))  
                           modelHyper <- hyperSMURF.train(trainSet[, 1:(length(trainSet) - 1)], tmpLabels, n.part = 5, fp = 0.5, ratio = 1, k = 5, ntree = 5, mtry = 5, cutoff = c(0.5, 0.5), seed = 0, file = "")
                           predProbs <- hyperSMURF.test(testSet[, 1:(length(testSet) - 1)], modelHyper)
                           predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                           foldPredictions <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                           colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                           Pred <- rbind(Pred, foldPredictions)
                         }
                       },
                       XGBoost = {
                         modelXGBoost <- xgboost(data = as.matrix(trainSet[, 1:(length(trainSet) - 1)]), 
                                                 label = ifelse(trainSet$CLASS == "BUG", 1, 0),
                                                 subsample = 0.5,
                                                 max.depth = 6, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = 2)
                         predProbs <- predict(modelXGBoost, as.matrix(testSet[, 1:(length(testSet) - 1)]))
                         predictions <- ifelse(predProbs > 0.5, "BUG", "NO_BUG");
                         foldPredictions <- cbind(as.character(testSet$CLASS), as.data.frame(predictions), predProbs, 1 - predProbs)
                         colnames(foldPredictions) <- c("actual", "predicted", "BUG", "NO_BUG")
                         Pred <- rbind(Pred, foldPredictions)
                       }
                )
                write.csv(foldPredictions, paste(modelOutputDir, projectName, "/", authorName, "/", authorName, "_Test", interval, "_Pred", modelType, ".csv", sep = ""))
                
                #info
                print(paste(algorithmChoice, " - ", projectName, " - ", authorName, " - test_interval:", interval, sep = ""))
                
                if(nrow(Pred) > 0)
                  results <- rbind(results, calculateConfusionMatrix(Pred, "BUG", "NO_BUG"))
              }
            }              
            write.csv(file = paste(modelOutputDir, projectName, "/results/", authorName, "Results", modelType, ".csv", sep = ""), x = results, row.names = FALSE)
          }
        }
      }
    }
  }
}







