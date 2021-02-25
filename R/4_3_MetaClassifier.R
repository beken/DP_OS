# PM+ classifier

# ATTENTION: check the RF prediction results (if they are saved cumulatively or not) before running the script

# This script gives two outputs:
# PredMeta: combined fold by fold prediction results, that means there will be only one PredMeta file 
# resultsMeta: 10 run evaluation results (pd,pf,precision etc.)



rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime"
baseExperimentSignature <- "MixTime" #SeqTime, MixTime
samplingOption <- "WITH"
testSetMonths <- ""

applyLogFilter <- FALSE
applyDiscretization <- FALSE
onlyLatentFeautes <- FALSE
onlyTopics <- FALSE
topNFeatures <- FALSE
numberOfFeauresToBeSelected <- 10

useIntervalDataOrPreparedTrainTestData <- "P" #P,I

setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")


outExperimentSignature <- paste(baseExperimentSignature)

algorithms <- ("NB")

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
        
        resultsMeta <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsMeta) <- c("TP", "FP", "FN", "TN", metricList)
        
        for(r in 1:numberOfRuns){
          PredPM <- data.frame()
          PredSM <- data.frame()
          PredGM <- data.frame()
          PredWPM <- data.frame()
          
          if(algorithmChoice == "RF"){ #RF prediction results saved cumulatively by mistake (fold10 contains all folds predictions, for ex. fold9 contains 1 to 9)
            f <- numberOfFolds
            PredPM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
            PredSM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
            PredGM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
            PredWPM <- read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
          }
          else{
            for(f in 1:numberOfFolds){
              PredPM <- rbind(PredPM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              PredSM <- rbind(PredSM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              PredGM <- rbind(PredGM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
              PredWPM <- rbind(PredWPM, read.csv(paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            }
          }
          
          PredMeta <- data.frame(matrix(nrow = 0, ncol = 5))
          colnames(PredMeta) <- c("instance", "actual", "predicted", "BUG", "NO_BUG")
            
          numberOfTestInstances <- nrow(PredPM)
          
          for(i in 1:numberOfTestInstances){
            weightOf_NoBug <- mean(c(PredPM[i, ]$NO_BUG, PredSM[i, ]$NO_BUG, PredGM[i, ]$NO_BUG, PredWPM[i, ]$NO_BUG))
            weightOf_Bug <- mean(c(PredPM[i, ]$BUG, PredSM[i, ]$BUG, PredGM[i, ]$BUG, PredWPM[i, ]$BUG))
            
            metaLabel <- ifelse(weightOf_Bug > weightOf_NoBug, yes = "BUG", no = "NO_BUG")
            metaProb <- ifelse(weightOf_Bug > weightOf_NoBug, yes = weightOf_Bug, no = weightOf_NoBug)
            
            if(weightOf_Bug == weightOf_NoBug){
              # prediction labelina gore en yuksek probability (confidence) sahibi olanin etiketini ve probabilitysini 
              # PM+ tahmini olarak seciyoruz
              # bunu yaparken belli bir ordera gore secim yapiyoruz
              # bu order su durumda onemli olabilir; eger PM SM GM WPM hepsi ayni olasilikla bir prediction yaptilarsa 
              # hepsi farkli olasilikta ise zaten en yuksegini secmis oluyoruz
              
              print(paste("There is a tie -- ", projectList[p], " -- ", authorName, " -- Run", r, " -- Instance", i, " -- ", weightOf_Bug, " : ", weightOf_NoBug, sep = ""))
              
              #If two different predictions tie, we favor the predictions in this order: PCC, weighted PCC and CC. [Jiang et al.]
              #our order: PM, WPM, SM, GM
              
              modelOrderArray <- c("PM", "WPM", "SM", "GM")
              predArray <- c(
                PredPM[i, PredPM[i, "predicted"]],
                PredSM[i, PredSM[i, "predicted"]],
                PredGM[i, PredGM[i, "predicted"]],
                PredWPM[i, PredWPM[i, "predicted"]])
              
              metaLabel <- get(paste("Pred", modelOrderArray[which.max(predArray)], sep = ""))[i, "predicted"]
              metaProb <- predArray[which.max(predArray)]
            }
            # TODO:ATTENTION: bu asagidaki else kismi neden eklenmis onceden? hatirlayamadim, sildim simdilik, cunku sonuc sacma bir sekilde yanlis olacak silmezsem
            # else{
            #   metaLabel <- "NO_BUG"
            #   metaProb <- max(c(PredPM[i, ]$NO_BUG, PredSM[i, ]$NO_BUG, PredGM[i, ]$NO_BUG, PredWPM[i, ]$NO_BUG))
            # }
            
            PredMeta[nrow(PredMeta) + 1, "instance"] <- PredPM[i, 1]
            PredMeta[nrow(PredMeta), "actual"] <- PredPM[i, "actual"]
            PredMeta[nrow(PredMeta), "predicted"] <- metaLabel
            PredMeta[nrow(PredMeta), metaLabel] <- metaProb
            
            if(metaLabel == "BUG")
              PredMeta[nrow(PredMeta), "NO_BUG"] <- 1 - metaProb
            else
              PredMeta[nrow(PredMeta), "BUG"] <- 1 - metaProb
          } 
          
          write.csv(PredMeta, file = paste(modelOutputDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredMeta.csv", sep = ""), row.names = FALSE)
          resultsMeta <- rbind(resultsMeta, calculateConfusionMatrix(PredMeta, "BUG", "NO_BUG"))
        }
        write.csv(resultsMeta, file = paste(modelOutputDir, projectName, "/", "results/", authorName, "ResultsMeta.csv", sep = ""), row.names = FALSE)
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
        
        resultsMeta <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
        colnames(resultsMeta) <- c("TP", "FP", "FN", "TN", metricList)
        
        if(authorName == "TM"){
          
        }
        else{
          dir.create(paste(modelOutputDir, projectName, "/", authorName, sep = ""), recursive = T, showWarnings = F)
          
          resultsMeta <- as.data.frame(matrix(nrow = 0, ncol = 4 + length(metricList)), stringsAsFactors = FALSE)
          colnames(resultsMeta) <- c("TP", "FP", "FN", "TN", metricList)
          
          for(interval in testIntervals){
            PredPM <- read.csv.with.err(paste(modelOutputDir, projectName, "/", authorName, "/", authorName, "_Test", interval, "_PredPM", ".csv", sep = ""))
            PredSM <- read.csv.with.err(paste(modelOutputDir, projectName, "/", authorName, "/", authorName, "_Test", interval, "_PredSM", ".csv", sep = ""))
            PredGM <- read.csv.with.err(paste(modelOutputDir, projectName, "/", authorName, "/", authorName, "_Test", interval, "_PredGM", ".csv", sep = ""))
            PredWPM <- read.csv.with.err(paste(modelOutputDir, projectName, "/", authorName, "/", authorName, "_Test", interval, "_PredWPM", ".csv", sep = ""))
            
            if(PredPM != dataIsNotExist & PredSM != dataIsNotExist & PredGM != dataIsNotExist & PredGM != dataIsNotExist){
              PredMeta <- data.frame(matrix(nrow = 0, ncol = 5))
              colnames(PredMeta) <- c("instance", "actual", "predicted", "BUG", "NO_BUG")
              
              numberOfTestInstances <- nrow(PredGM)
              
              for(i in 1:numberOfTestInstances){
                weightOf_NoBug <- mean(c(PredPM[i, ]$NO_BUG, PredSM[i, ]$NO_BUG, PredGM[i, ]$NO_BUG, PredWPM[i, ]$NO_BUG))
                weightOf_Bug <- mean(c(PredPM[i, ]$BUG, PredSM[i, ]$BUG, PredGM[i, ]$BUG, PredWPM[i, ]$BUG))
                
                metaLabel <- ifelse(weightOf_Bug > weightOf_NoBug, yes = "BUG", no = "NO_BUG")
                metaProb <- ifelse(weightOf_Bug > weightOf_NoBug, yes = weightOf_Bug, no = weightOf_NoBug)
                
                if(weightOf_Bug == weightOf_NoBug){
                  # prediction labelina gore en yuksek probability (confidence) sahibi olanin etiketini ve probabilitysini 
                  # PM+ tahmini olarak seciyoruz
                  # bunu yaparken belli bir ordera gore secim yapiyoruz
                  # bu order su durumda onemli olabilir; eger PM SM GM WPM hepsi ayni olasilikla bir prediction yaptilarsa 
                  # hepsi farkli olasilikta ise zaten en yuksegini secmis oluyoruz
                  
                  print(paste("There is a tie -- ", projectList[p], " -- ", authorName, " -- TestInterval", interval, " -- Instance", i, " -- ", weightOf_Bug, " : ", weightOf_NoBug, sep = ""))
                  
                  #If two different predictions tie, we favor the predictions in this order: PCC, weighted PCC and CC. [Jiang et al.]
                  #our order: PM, WPM, SM, GM
                  
                  modelOrderArray <- c("PM", "WPM", "SM", "GM")
                  predArray <- c(
                    PredPM[i, PredPM[i, "predicted"]],
                    PredSM[i, PredSM[i, "predicted"]],
                    PredGM[i, PredGM[i, "predicted"]],
                    PredWPM[i, PredWPM[i, "predicted"]])
                  
                  metaLabel <- get(paste("Pred", modelOrderArray[which.max(predArray)], sep = ""))[i, "predicted"]
                  metaProb <- predArray[which.max(predArray)]
                }
                # TODO:ATTENTION: bu asagidaki else kismi neden eklenmis onceden? hatirlayamadim, sildim simdilik, cunku sonuc sacma bir sekilde yanlis olacak silmezsem
                # else{
                #   metaLabel <- "NO_BUG"
                #   metaProb <- max(c(PredPM[i, ]$NO_BUG, PredSM[i, ]$NO_BUG, PredGM[i, ]$NO_BUG, PredWPM[i, ]$NO_BUG))
                # }
                
                PredMeta[nrow(PredMeta) + 1, "instance"] <- PredPM[i, 1]
                PredMeta[nrow(PredMeta), "actual"] <- as.character(PredPM[i, "actual"])
                PredMeta[nrow(PredMeta), "predicted"] <- metaLabel
                PredMeta[nrow(PredMeta), as.character(metaLabel)] <- metaProb
                
                if(metaLabel == "BUG")
                  PredMeta[nrow(PredMeta), "NO_BUG"] <- 1 - metaProb
                else
                  PredMeta[nrow(PredMeta), "BUG"] <- 1 - metaProb
              } 
              
              resultsMeta <- rbind(resultsMeta, calculateConfusionMatrix(PredMeta, "BUG", "NO_BUG"))
              write.csv(PredMeta, file = paste(modelOutputDir, projectName, "/", authorName, "/", authorName, "_Test", interval, "PredMeta.csv", sep = ""), row.names = F)
              #info
              print(paste(algorithmChoice, " - ", projectName, " - ", authorName, " - test_interval:", interval, sep = ""))
            }
            else
              print(paste("PM, SM, GM or WPM predictions are empty. So, PM+ is not calculated for", projectName, authorName, interval))
          }
          
          write.csv(file = paste(modelOutputDir, projectName, "/results/", authorName, "ResultsMeta", ".csv", sep = ""), x = resultsMeta, row.names = FALSE)
        }
      }
    }
  }
}

# getPredictionProbs <- function(instancePrediction){
#   if(instancePrediction["predicted"] == "NO_BUG"){
#     noBugProb <- instancePrediction["prediction"]
#     bugProb <- 1 - noBugProb
#   }
#   else if(instancePrediction["predicted"] == "BUG"){
#     bugProb <- instancePrediction["prediction"]
#     noBugProb <- 1 - bugProb
#   }
#   
#   predList <- c(bugProb, noBugProb)
#   names(predList) <- c("BUG", "NO_BUG")
#   
#   return(predList)
# }


# for(p in 1:length(projectList)){
#   #some variable names for easy usage
#   projectName <- projectList[p]
#   selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
#   changesDfName <- paste(projectName, "Changes", sep = "")
#   
#   #set working directory for the project
#   workingDir <- paste(baseDir, projectList[p], sep = "")
#   setwd(workingDir)
#   dir.create(file.path("./results/"), showWarnings = FALSE)
#   
#   #read selected authors and keep them in a table and a list, also keep the number of selected authors
#   assign(selectedAuthorsDfName, 
#          read.xlsx(file = paste(mainStoreDir, "selectedAuthors.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE))
#   authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
#   numberOfSelectedDev <- length(get(selectedAuthorsDfName)$Author)
#   
#   #because of Rails data has a few buggy change data, we split Rails data into 3 folds
#   if(projectName == "Rails")
#     numberOfFolds <- 3
#   else
#     numberOfFolds <- 10
#   
#   for(a in 1:numberOfSelectedDev){
#     authorName <- as.character(get(selectedAuthorsDfName)[a, "Author"])
#     resultsMeta <- data.frame() #will keep the 10 iteration results
#     
#     for(r in 1:numberOfRuns){
#       GMPredictions <- data.frame()
#       SMPredictions <- data.frame()
#       PMPredictions <- data.frame()
#       WPMPredictions <- data.frame()
#       
#       if(algorithmName == "RF"){ #RF prediction results saved cumulatively by mistake (fold10 contains all folds predictions, for ex. fold9 contains 1 to 9)
#         f <- numberOfFolds #3 for Rails 10 for others
#         PMPredictions <- rbind(PMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#         SMPredictions <- rbind(SMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#         GMPredictions <- rbind(GMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#         WPMPredictions <- rbind(WPMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#       } 
#       else{
#         for(f in 1:numberOfFolds){
#           PMPredictions <- rbind(PMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#           SMPredictions <- rbind(SMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#           GMPredictions <- rbind(GMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#           WPMPredictions <- rbind(WPMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
#         }
#       }
#       
#       PMPredictions <- editClassLabels(PMPredictions)
#       SMPredictions <- editClassLabels(SMPredictions)
#       GMPredictions <- editClassLabels(GMPredictions)
#       WPMPredictions <- editClassLabels(WPMPredictions)
#       
#       metaPredictions <- data.frame(matrix(nrow = 0, ncol = 4))
#       colnames(metaPredictions) <- c("instance", "actual", "predicted", "prediction")
#       
#       if(algorithmName == "NB"){
#         #loop in instances (loop length equals to minChangeCount)
#         for(i in 1:length(PMPredictions[, 1])){
#           weightOf_NoBug <- mean(c(getPredictionProbs(PMPredictions[i, ])$NO_BUG, getPredictionProbs(SMPredictions[i, ])$NO_BUG, getPredictionProbs(GMPredictions[i, ])$NO_BUG, getPredictionProbs(WPMPredictions[i, ])$NO_BUG))
#           weightOf_Bug <- mean(c(getPredictionProbs(PMPredictions[i, ])$BUG, getPredictionProbs(SMPredictions[i, ])$BUG, getPredictionProbs(GMPredictions[i, ])$BUG, getPredictionProbs(WPMPredictions[i, ])$BUG))
#           
#           metaLabel <- ifelse(weightOf_Bug > weightOf_NoBug, yes = "BUG", no = "NO_BUG")
#           metaProb <- ifelse(weightOf_Bug > weightOf_NoBug, yes = weightOf_Bug, no = weightOf_NoBug)
#           
#           if(weightOf_Bug == weightOf_NoBug){
#             print(paste("There is a tie -- ", projectList[p], " -- ", authorName, " -- Run", r, " -- Instance", i, " -- ", weightOf_Bug, " : ", weightOf_NoBug, sep = ""))
#             
#             #If two different predictions tie, we favor the predictions in this order: PCC, weighted PCC and CC. [Jiang et al.]
#             #our order: PM, WPM, SM, GM
#             
#             modelOrderArray <- c("PM", "WPM", "SM", "GM")
#             predArray <- c(PMPredictions[i, "prediction"], WPMPredictions[i, "prediction"], SMPredictions[i, "prediction"], GMPredictions[i, "prediction"]) 
#             
#             metaLabel <- get(paste(modelOrderArray[which.max(predArray)], "Predictions", sep = ""))[i, "predicted"]
#             metaProb <- predArray[which.max(predArray)]
#           } 
#           
#           metaPredictions[nrow(metaPredictions) + 1, "instance"] <- PMPredictions[i, "inst."]
#           metaPredictions[nrow(metaPredictions), "actual"] <- PMPredictions[i, "actual"]
#           metaPredictions[nrow(metaPredictions), "predicted"] <- metaLabel
#           metaPredictions[nrow(metaPredictions), "prediction"] <- metaProb
#         }
#       }
#       else if(algorithmName == "RF"){
#         for(i in 1:length(PMPredictions[, 1])){
#           # weightOf_NoBug <- length(which(c(PMPredictions[i, ]$NO_BUG, SMPredictions[i, ]$NO_BUG, GMPredictions[i, ]$NO_BUG, WPMPredictions[i, ]$NO_BUG) > 0.5))
#           # weightOf_Bug <- length(which(c(PMPredictions[i, ]$BUG, SMPredictions[i, ]$BUG, GMPredictions[i, ]$BUG, WPMPredictions[i, ]$BUG) > 0.5))
#           # 
#           # if(weightOf_Bug > 3){
#           #   metaLabel <- "BUG"
#           #   metaProb <- max(c(PMPredictions[i, ]$BUG, SMPredictions[i, ]$BUG, GMPredictions[i, ]$BUG, WPMPredictions[i, ]$BUG))
#           # }
#           
#           weightOf_NoBug <- mean(c(PMPredictions[i, ]$NO_BUG, SMPredictions[i, ]$NO_BUG, GMPredictions[i, ]$NO_BUG, WPMPredictions[i, ]$NO_BUG))
#           weightOf_Bug <- mean(c(PMPredictions[i, ]$BUG, SMPredictions[i, ]$BUG, GMPredictions[i, ]$BUG, WPMPredictions[i, ]$BUG))
#           
#           metaLabel <- ifelse(weightOf_Bug > weightOf_NoBug, yes = "BUG", no = "NO_BUG")
#           metaProb <- ifelse(weightOf_Bug > weightOf_NoBug, yes = weightOf_Bug, no = weightOf_NoBug)
#           
#           if(weightOf_Bug == weightOf_NoBug){
#             print(paste("There is a tie -- ", projectList[p], " -- ", authorName, " -- Run", r, " -- Instance", i, " -- ", weightOf_Bug, " : ", weightOf_NoBug, sep = ""))
#             
#             #If two different predictions tie, we favor the predictions in this order: PCC, weighted PCC and CC. [Jiang et al.]
#             #our order: PM, WPM, SM, GM
#             
#             modelOrderArray <- c("PM", "WPM", "SM", "GM")
#             predArray <- c(
#               PMPredictions[i, PMPredictions[i, "predicted"]],
#               WPMPredictions[i, WPMPredictions[i, "predicted"]],
#               SMPredictions[i, SMPredictions[i, "predicted"]],
#               GMPredictions[i, GMPredictions[i, "predicted"]])
#             
#             metaLabel <- get(paste(modelOrderArray[which.max(predArray)], "Predictions", sep = ""))[i, "predicted"]
#             metaProb <- predArray[which.max(predArray)]
#           }
#           else{
#             metaLabel <- "NO_BUG"
#             metaProb <- max(c(PMPredictions[i, ]$NO_BUG, SMPredictions[i, ]$NO_BUG, GMPredictions[i, ]$NO_BUG, WPMPredictions[i, ]$NO_BUG))
#           }
#           
#           metaPredictions[nrow(metaPredictions) + 1, "instance"] <- PMPredictions[i, 1]
#           metaPredictions[nrow(metaPredictions), "actual"] <- PMPredictions[i, "actual"]
#           metaPredictions[nrow(metaPredictions), "predicted"] <- metaLabel
#           metaPredictions[nrow(metaPredictions), "prediction"] <- metaProb
#         }
#       }
#       
#       write.csv(metaPredictions, file = paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredMeta.csv", sep = ""), row.names = FALSE)
#       resultsMeta <- rbind(resultsMeta, calculateConfusionMatrix(metaPredictions))
#     }
#     write.xlsx(resultsMeta, file = paste("./results/", authorName, "ResultsMeta.xlsx", sep = ""), row.names = FALSE)
#   }
# }

