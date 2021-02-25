### Calculate pd, pf, precision, F1 and plot
### This is the version after the weighted models are introduced

library(xlsx)
library(stringi)
library(PMCMR)

rm(list=ls()) #clean the environment

mainStoreDir = "/Users/beken/R/ChangeData/Experiment_V2/" #selectedAuthors, all change info can be found here
baseDir = "/Users/beken/R/ChangeData/Experiment_V2/PredictionsNB/"
algorithmName = "NB" #NB,RF
samplingOption = "WITH" #WO,WITH
numberOfRuns <- 10
numberOfFolds <- 10
metricList <- c("recall", "pf", "precision", "F1")
projectList <- c("Gimp", "Maven2", "Perl", "PostgreSql", "Rails", "Rhino")
savePredictionResultsConfMatrix <- "N" #N
setwd(baseDir)


changeNaToZero <- function(dataFrame) {
  dataFrame[is.na(dataFrame)] <- 0
  return (dataFrame)
}
medianWithoutNA <- function(x) {
  median(x[which(!is.na(x))])
}
meanWithoutNA <- function(x) {
  mean(x[which(!is.na(x))])
}
medianChangeNaToZero <- function(x) {
  x[is.na(x)] <- 0
  median(x)
}
meanChangeNaToZero <- function(x) {
  x[is.na(x)] <- 0
  mean(x)
}
minChangeNaToZero <- function(x) {
  x[is.na(x)] <- 0
  min(x)
}
maxChangeNaToZero <- function(x) {
  x[is.na(x)] <- 0
  max(x)
}

editClassLabels <- function(argDataFrame) {
  argDataFrame$predicted[argDataFrame$predicted == "1:NO_BUG"] <- "NO_BUG"
  argDataFrame$predicted[argDataFrame$predicted == "2:NO_BUG"] <- "NO_BUG"
  argDataFrame$predicted[argDataFrame$predicted == "1:BUG"] <- "BUG"
  argDataFrame$predicted[argDataFrame$predicted == "2:BUG"] <- "BUG"
  
  argDataFrame$actual[argDataFrame$actual == "1:NO_BUG"] <- "NO_BUG"
  argDataFrame$actual[argDataFrame$actual == "2:NO_BUG"] <- "NO_BUG"
  argDataFrame$actual[argDataFrame$actual == "1:BUG"] <- "BUG"
  argDataFrame$actual[argDataFrame$actual == "2:BUG"] <- "BUG"
  
  return(argDataFrame)
}
calculateConfusionMatrix <- function(argPredictions) {
  FP <- length(which(argPredictions$actual == "NO_BUG" & argPredictions$predicted == "BUG"))
  TN <- length(which(argPredictions$actual == "NO_BUG" & argPredictions$predicted == "NO_BUG"))
  FN <- length(which(argPredictions$actual == "BUG" & argPredictions$predicted == "NO_BUG"))
  TP <- length(which(argPredictions$actual == "BUG" & argPredictions$predicted == "BUG"))
  
  confMatrix <- table(argPredictions$actual, argPredictions$predicted)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN) #pd
  pf <- FP / (FP + TN)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  values <- as.data.frame(matrix(ncol = 8, nrow = 1, c(TP, FP, FN, TN, recall, pf, precision, F1)))
  colnames(values) <- c("TP", "FP", "FN", "TN", "recall", "pf", "precision", "F1")
  return(values)
}

readAuthorTestResultsAllIterations <- function(projectName, authorName, modelName) {
  results <- read.xlsx(file = paste(baseDir, projectName, "/results/", authorName, "Results", modelName, ".xlsx", sep = ""), sheetIndex = 1, header = TRUE)
  return (results)
}
getFriedmanComparisonResultOfModels <- function(resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta, authorName, projectName) {
  resultsFriedmanPVal <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) + 1 )) 
  colnames(resultsFriedmanPVal) <- c("models", "recall", "pf", "precision", "F1")
  
  for(m in 1:length(metricList)) {
    modelCompMatrix <- as.matrix(cbind(changeNaToZero(resultsPM[, metricList[m]]), 
                                       changeNaToZero(resultsSM[, metricList[m]]), 
                                       changeNaToZero(resultsGM[, metricList[m]]),
                                       changeNaToZero(resultsWSM[, metricList[m]]),
                                       changeNaToZero(resultsMeta[, metricList[m]])))
    colnames(modelCompMatrix) <- c("PM", "SM", "GM", "WSM", "Meta")
    modelCompResults <- friedman.test(modelCompMatrix)
    
    resultsFriedmanPVal[1, "models"] <- paste("Friedman", sep = "")
    
    resultsFriedmanPVal[1, paste(metricList[m], sep = "")] <- modelCompResults$p.value
  }
  return (resultsFriedmanPVal)
}
getNemenyiComparisonResultOfModels <- function(resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta, authorName, projectName) {
  resultsNemenyiPVal <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) + 1 )) 
  colnames(resultsNemenyiPVal) <- c("models", "recall", "pf", "precision", "F1")
  
  for(m in 1:length(metricList)) {
    
    ### OLD WAY: nemenyi test by using two, two and two
    #sadece ikisi ile karsilastirdim, bu sekilde ve yeni sekilde farkli sonuclar veriyor
    
    # PmSmMatrix <- as.matrix(cbind(resultsPM[, metricList[m]], resultsSM[, metricList[m]]))
    # colnames(PmSmMatrix) <- c("PM", "SM")
    # comparisonResultPmSm <- posthoc.friedman.nemenyi.test(PmSmMatrix)
    # 
    # PmGmMatrix <- as.matrix(cbind(resultsPM[, metricList[m]], resultsGM[, metricList[m]]))
    # colnames(PmGmMatrix) <- c("PM", "GM")
    # comparisonResultPmGm <- posthoc.friedman.nemenyi.test(PmGmMatrix)
    # 
    # SmGmMatrix <- as.matrix(cbind(resultsSM[, metricList[m]], resultsGM[, metricList[m]]))
    # colnames(SmGmMatrix) <- c("SM", "GM")
    # comparisonResultSmGm <- posthoc.friedman.nemenyi.test(SmGmMatrix)
    
    ### NEW WAY: nemenyi test by using four together
    modelCompMatrix <- as.matrix(cbind(resultsPM[, metricList[m]], resultsSM[, metricList[m]], resultsGM[, metricList[m]], resultsWSM[, metricList[m]], resultsMeta[, metricList[m]]))
    colnames(modelCompMatrix) <- c("PM", "SM", "GM", "WSM", "Meta")
    modelCompResults <- posthoc.friedman.nemenyi.test(modelCompMatrix)
    
    #modelCompResults matrix pVals:
    ##    PM    SM    GM    WSM   Meta
    #SM   [1,1] NA    NA    NA    NA
    #GM   [2,1] [2,2] NA    NA    NA
    #WSM  [3,1] [3,2] [3,3] NA    NA
    #PM+  [4,1] [4,2] [4,3] [4,4] [4,5]
    
    resultsNemenyiPVal[1, "models"] <- paste("PM / SM", sep = "")
    resultsNemenyiPVal[2, "models"] <- paste("PM / GM", sep = "")
    resultsNemenyiPVal[3, "models"] <- paste("SM / GM", sep = "")
    resultsNemenyiPVal[4, "models"] <- paste("PM / WSM", sep = "")
    resultsNemenyiPVal[5, "models"] <- paste("PM / PM+", sep = "")
    resultsNemenyiPVal[6, "models"] <- paste("SM / WSM", sep = "")
    resultsNemenyiPVal[7, "models"] <- paste("SM / PM+", sep = "")
    resultsNemenyiPVal[8, "models"] <- paste("GM / WSM", sep = "")
    resultsNemenyiPVal[9, "models"] <- paste("GM / PM+", sep = "")
    resultsNemenyiPVal[10, "models"] <- paste("WSM / PM+", sep = "")

    resultsNemenyiPVal[1, paste(metricList[m], sep = "")] <- modelCompResults$p.value[1, 1]
    resultsNemenyiPVal[2, paste(metricList[m], sep = "")] <- modelCompResults$p.value[2, 1]
    resultsNemenyiPVal[3, paste(metricList[m], sep = "")] <- modelCompResults$p.value[2, 2]
    resultsNemenyiPVal[4, paste(metricList[m], sep = "")] <- modelCompResults$p.value[3, 1]
    resultsNemenyiPVal[5, paste(metricList[m], sep = "")] <- modelCompResults$p.value[3, 2]
    resultsNemenyiPVal[6, paste(metricList[m], sep = "")] <- modelCompResults$p.value[3, 3]
    resultsNemenyiPVal[7, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 1]
    resultsNemenyiPVal[8, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 2]
    resultsNemenyiPVal[9, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 3]
    resultsNemenyiPVal[10, paste(metricList[m], sep = "")] <- modelCompResults$p.value[4, 4]
  }
  return (resultsNemenyiPVal)
}
getAuthorColorByMetric <- function(projectName, authorName, metricName, numberOfColors) {
  pVal <- 0.05
  betterCount <- 0
  worseCount <- 0
  if(metricName == 'pf')
    aboveAvgIsGood <- 'F'
  else
    aboveAvgIsGood <- 'T'
  
  resultsPM <- readAuthorTestResultsAllIterations(projectName, authorName, "PM")
  resultsSM <- readAuthorTestResultsAllIterations(projectName, authorName, "SM")
  resultsGM <- readAuthorTestResultsAllIterations(projectName, authorName, "GM")
  resultsWSM <- readAuthorTestResultsAllIterations(projectName, authorName, "WSM")
  resultsMeta <- readAuthorTestResultsAllIterations(projectName, authorName, "Meta")
  
  #returns a table that contains pValues of PM/SM, PM/GM and SM/GM comparisons
  authorComparisonResults <- getNemenyiComparisonResultOfModels(resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta, authorName, projectName)
  
  medianPM <- medianChangeNaToZero(resultsPM[, metricName])
  medianSM <- medianChangeNaToZero(resultsSM[, metricName])
  medianGM <- medianChangeNaToZero(resultsGM[, metricName])
  medianWSM <- medianChangeNaToZero(resultsWSM[, metricName])
  medianMeta <- medianChangeNaToZero(resultsMeta[, metricName])
  
  # dodgerblue1
  # lightskyblue
  # lightskyblue1
  # lightsteelblue1
  
  # firebrick2
  # indianred1
  # salmon
  # sandybrown
  
  # yellowgreen
  
  PM_SMDiff <- authorComparisonResults[1, metricName] < pVal
  PM_GMDiff <- authorComparisonResults[2, metricName] < pVal
  PM_WSMDiff <- authorComparisonResults[4, metricName] < pVal
  PM_MetaDiff <- authorComparisonResults[5, metricName] < pVal
  
  diffVector <- c(PM_SMDiff, PM_GMDiff, PM_WSMDiff, PM_MetaDiff)
  modelCompNames <- c("PM_SM", "PM_GM", "PM_WSM", "PM_Meta")
  howManyDifference <- length(diffVector[diffVector == TRUE])
  
  
  if(howManyDifference == 0) #if PM is not any statistically different from others
    tmpColorString <- "'white'"
  else if(howManyDifference == 1){ #if PM is statistically different from only one other model
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel <- get(paste("median", strsplit(modelCompNames[modelIndex], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel)
      tmpColorString <- "'sandybrown'"
    else if(medianPM > medianOfComparedModel)
      tmpColorString <- "'lightsteelblue1'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else if(howManyDifference == 2){ #if PM is statistically different from two of the other models
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
    medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
  
    if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2)
      tmpColorString <- "'salmon'"
    else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2)
      tmpColorString <- "'lightskyblue1'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else if(howManyDifference == 2){ #if PM is statistically different from three of the other models
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
    medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
    medianOfComparedModel3 <- get(paste("median", strsplit(modelCompNames[modelIndex[3]], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2 && medianPM < medianOfComparedModel3)
      tmpColorString <- "'indianred1'"
    else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2 && medianPM > medianOfComparedModel3)
      tmpColorString <- "'lightskyblue'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else if(howManyDifference == 2){ #if PM is statistically different from all the other models
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
    medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
    medianOfComparedModel3 <- get(paste("median", strsplit(modelCompNames[modelIndex[3]], "_")[[1]][2], sep = ""))
    medianOfComparedModel4 <- get(paste("median", strsplit(modelCompNames[modelIndex[4]], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2 && medianPM < medianOfComparedModel3 && medianPM < medianOfComparedModel4)
      tmpColorString <- "'firebrick2'"
    else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2 && medianPM > medianOfComparedModel3 && medianPM > medianOfComparedModel4)
      tmpColorString <- "'dodgerblue1'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else
    tmpColorString <- "'yellowgreen'"
  
  if(numberOfColors == 2){
    tmpColorString <- gsub("dodgerblue1", "blue", tmpColorString)
    tmpColorString <- gsub("lightskyblue1", "blue", tmpColorString)
    tmpColorString <- gsub("lightskyblue", "blue", tmpColorString)
    tmpColorString <- gsub("lightsteelblue1", "blue", tmpColorString)
    
    tmpColorString <- gsub("firebrick2", "red", tmpColorString)
    tmpColorString <- gsub("indianred1", "red", tmpColorString)
    tmpColorString <- gsub("salmon", "red", tmpColorString)
    tmpColorString <- gsub("sandybrown", "red", tmpColorString)
    
    tmpColorString <- gsub("yellowgreen", "green", tmpColorString)
  }
  
  return (tmpColorString)
}
getAuthorColorByMetricOnlyWPM <- function(projectName, authorName, metricName) {
  pVal <- 0.05
  betterCount <- 0
  worseCount <- 0
  if(metricName == 'pf')
    aboveAvgIsGood <- 'F'
  else
    aboveAvgIsGood <- 'T'
  
  resultsPM <- readAuthorTestResultsAllIterations(projectName, authorName, "PM")
  resultsWSM <- readAuthorTestResultsAllIterations(projectName, authorName, "WSM")
  
  #returns a table that contains pValues of PM/SM, PM/GM and SM/GM comparisons
  authorComparisonResults <- getNemenyiComparisonResultOfModels(resultsPM, resultsWSM, authorName, projectName)
  
  medianPM <- medianChangeNaToZero(resultsPM[, metricName])
  medianWSM <- medianChangeNaToZero(resultsWSM[, metricName])

  PM_WSMDiff <- authorComparisonResults[4, metricName] < pVal
  
  diffVector <- c(PM_SMDiff, PM_GMDiff, PM_WSMDiff)
  modelCompNames <- c("PM_SM", "PM_GM", "PM_WSM", "PM_Meta")
  howManyDifference <- length(diffVector[diffVector == TRUE])
  
  
  if(howManyDifference == 0) #if PM is not any statistically different from others
    tmpColorString <- "'white'"
  else if(howManyDifference == 1){ #if PM is statistically different from only one other model
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel <- get(paste("median", strsplit(modelCompNames[modelIndex], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel)
      tmpColorString <- "'sandybrown'"
    else if(medianPM > medianOfComparedModel)
      tmpColorString <- "'lightsteelblue1'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else if(howManyDifference == 2){ #if PM is statistically different from two of the other models
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
    medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2)
      tmpColorString <- "'salmon'"
    else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2)
      tmpColorString <- "'lightskyblue1'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else if(howManyDifference == 2){ #if PM is statistically different from three of the other models
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
    medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
    medianOfComparedModel3 <- get(paste("median", strsplit(modelCompNames[modelIndex[3]], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2 && medianPM < medianOfComparedModel3)
      tmpColorString <- "'indianred1'"
    else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2 && medianPM > medianOfComparedModel3)
      tmpColorString <- "'lightskyblue'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else if(howManyDifference == 2){ #if PM is statistically different from all the other models
    modelIndex <- which(diffVector == TRUE)
    medianOfComparedModel1 <- get(paste("median", strsplit(modelCompNames[modelIndex[1]], "_")[[1]][2], sep = ""))
    medianOfComparedModel2 <- get(paste("median", strsplit(modelCompNames[modelIndex[2]], "_")[[1]][2], sep = ""))
    medianOfComparedModel3 <- get(paste("median", strsplit(modelCompNames[modelIndex[3]], "_")[[1]][2], sep = ""))
    medianOfComparedModel4 <- get(paste("median", strsplit(modelCompNames[modelIndex[4]], "_")[[1]][2], sep = ""))
    
    if(medianPM < medianOfComparedModel1 && medianPM < medianOfComparedModel2 && medianPM < medianOfComparedModel3 && medianPM < medianOfComparedModel4)
      tmpColorString <- "'firebrick2'"
    else if(medianPM > medianOfComparedModel1 && medianPM > medianOfComparedModel2 && medianPM > medianOfComparedModel3 && medianPM > medianOfComparedModel4)
      tmpColorString <- "'dodgerblue1'"
    else
      tmpColorString <- "'yellowgreen'"
  }
  else
    tmpColorString <- "'yellowgreen'"
  
  if(numberOfColors == 2){
    tmpColorString <- gsub("dodgerblue1", "blue", tmpColorString)
    tmpColorString <- gsub("lightskyblue1", "blue", tmpColorString)
    tmpColorString <- gsub("lightskyblue", "blue", tmpColorString)
    tmpColorString <- gsub("lightsteelblue1", "blue", tmpColorString)
    
    tmpColorString <- gsub("firebrick2", "red", tmpColorString)
    tmpColorString <- gsub("indianred1", "red", tmpColorString)
    tmpColorString <- gsub("salmon", "red", tmpColorString)
    tmpColorString <- gsub("sandybrown", "red", tmpColorString)
    
    tmpColorString <- gsub("yellowgreen", "green", tmpColorString)
  }
  
  return (tmpColorString)
}


plotIndividualResults <- function(resultsPM, resultsSM, resultsGM, authorName, projectName) {
  #https://www.statmethods.net/advgraphs/layout.html
  #With the par() function, you can include the option mfrow=c(nrows, ncols) 
  #to create a matrix of nrows x ncols plots that are filled in by row. 
  #mfcol=c(nrows, ncols) fills in the matrix by columns.
  
  #https://www.statmethods.net/advgraphs/axes.html
  #las	labels are parallel (=0) or perpendicular(=2) to axis
  
  dir.create(paste("plots"), showWarnings = FALSE)
  #jpeg(filename = paste("./plots/", authorName, ".jpg", sep = ""), res = 120, width = 1000, height = 300)
  pdf(file = paste("./plots/", authorName, ".pdf", sep = ""), width = 20, height = 5)
  par(mfrow = c(1, length(metricList)), oma = c(3, 10, 3, 0) + 0.3, mar = c(0, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
  
  boxplot(resultsGM$recall, resultsSM$recall, resultsPM$recall, horizontal = TRUE, names = c("GM", "SM", "PM"), ylim = c(0.0, 1.0), las = 2)
  boxplot(resultsGM$pf, resultsSM$pf, resultsPM$pf, horizontal = TRUE, ylim = c(0.0, 1.0), las = 2)
  boxplot(resultsGM$precision, resultsSM$precision, resultsPM$precision, horizontal = TRUE, ylim = c(0.0, 1.0), las = 2)
  boxplot(resultsGM$F1, resultsSM$F1, resultsPM$F1, horizontal = TRUE, ylim = c(0.0, 1.0), las = 2)
  
  mtext(paste("pd, pf, precision, F1 values (", algorithmName, " applied on ", projectName, ")", sep = ""), outer = TRUE, cex = 1.5)
  
  dev.off()
  
  #info
  print(paste(projectName, " - ", authorName, " individual boxplots", sep = ""))
}
plotIndividualResultsColored <- function(resultsPM, resultsSM, resultsGM, authorName, projectName, colorCount) {
  dir.create(paste("plots"), showWarnings = FALSE)
  pdf(file = paste("./plots/", authorName, "Colored.pdf", sep = ""), width = 20, height = 5)
  par(mfrow = c(1, length(metricList)), oma = c(3, 10, 3, 0) + 0.3, mar = c(0, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
  
  for(m in 1:length(metricList)) {
    tmpColor <- getAuthorColorByMetric(projectName, authorName, metricList[m], colorCount)
    tmpColorString <- paste("'white', 'white',", tmpColor, sep = "")
    
    
    if(m == 1) {
      eval(parse(text = paste("boxplot(resultsGM$", metricList[m], ", ",
                              "resultsSM$", metricList[m], ", ",
                              "resultsPM$", metricList[m], ", ",
                              "las = 2, ylim = c(0.0, 1.0), horizontal = TRUE, names = c('GM', 'SM', 'PM'), col = c(", tmpColorString, "))", sep =  "")))
    }
    else {
      eval(parse(text = paste("boxplot(resultsGM$", metricList[m], ", ",
                              "resultsSM$", metricList[m], ", ",
                              "resultsPM$", metricList[m], ", ",
                              "las = 2, ylim = c(0.0, 1.0), horizontal = TRUE, col = c(", tmpColorString, "))", sep =  "")))
    }
  }
  
  mtext(paste("pd, pf, precision, F1 values (", algorithmName, " applied on ", projectName, ")", sep = ""), outer = TRUE, cex = 1.5)
  dev.off()
  
  #info
  print(paste(projectName, " - ", authorName, " individual boxplots", sep = ""))
}
plotCombinedAuthors <- function(projectName) {
  tmpSelectedAuthorsDf <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  
  adjustedHeight <- 3 + length(tmpSelectedAuthorsDf$Author) * 0.6
  pdf(file = "./plots/AllAuthors.pdf", width = 20, height = adjustedHeight)
  par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
  
  for(m in 1:length(metricList)) {
    tmpObjString <- ""
    tmpColString <- ""
    
    for(a in 1:length(authorNames)) {
      #assign(authorNames[a], readAuthorTestResultsAllIterations(projectName, authorName, "PM"))
      
      tmpObjString <- paste(tmpObjString, "`", authorNames[a], "`", "$", metricList[m], ", ", sep = "")
      
      if(samplingOption == "WITH") {
        tmpColString <- paste(tmpColString, "'", 
                              tail(strsplit(as.character(authorNames[a]), split=" ")[[1]], 1), #take only last name of author
                              " (", min(tmpSelectedAuthorsDf$NumberOfChanges), ")", "', ", sep = "")
      } else if(samplingOption == "WO") {
        tmpColString <- paste(tmpColString, "'", 
                              tail(strsplit(as.character(authorNames[a]), split=" ")[[1]], 1), #take only last name of author
                              " (", tmpSelectedAuthorsDf[which(tmpSelectedAuthorsDf$Author == authorNames[a]), ]$NumberOfChanges, ")", "', ", sep = "")
      }
    }
    
    tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 2)
    tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 2)
    
    if(m == 1) {
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), horizontal = TRUE, names = c(", tmpColString, "))", sep =  "")))
      title(ylab = "Selected authors (sample size)", line = 4)
      title(xlab = metricList[m], mgp=c(4,1,0))
    }
    else {
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), horizontal = TRUE)", sep =  "")))
      title(xlab = metricList[m], mgp=c(4,1,0))
    }
  }
  
  # mtext(paste("pd, pf, precision, F1 values for the models (", algorithmName, " on ", projectName, ")", sep = ""), outer = TRUE, cex = 1.5)
  # text(x=10, y=8, "Selected authors (sample size)", pos = 2)
  dev.off()
  
  #info
  print(paste(projectName, " all authors boxplot", sep = ""))
}
plotCombinedAuthorsColored <- function(projectName, outFileName, numberOfColors, onlyWPM) {
  tmpSelectedAuthorsDf <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  authorAliases <- as.character(tmpSelectedAuthorsDf$Alias)
  
  adjustedHeight <- 3 + length(tmpSelectedAuthorsDf$Author) * 0.4
  pdf(file = paste("./plots/", outFileName, ".pdf", sep = ""), width = 20, height = adjustedHeight)
  par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
  
  for(m in 1:length(metricList)) {
    tmpObjString <- ""
    tmpColString <- ""
    tmpColorString <- ""
    
    for(a in 1:length(authorNames)) {
      #assign(authorNames[a], readAuthorTestResultsAllIterations(projectName, authorName, "PM"))
      currentAuthorsColor <- getAuthorColorByMetric(projectName, authorNames[a], metricList[m], numberOfColors, onlyWPM)
      
      tmpObjString <- paste(tmpObjString, "`", authorNames[a], "`", "$", metricList[m], ", ", sep = "")
      
      if(samplingOption == "WITH") {
        tmpColString <- paste(tmpColString, "'", authorAliases[a],
                              " (", min(tmpSelectedAuthorsDf$NumberOfChanges), ")", "', ", sep = "")
      } else if(samplingOption == "WO") {
        tmpColString <- paste(tmpColString, "'", authorAliases[a],
                              " (", tmpSelectedAuthorsDf[which(tmpSelectedAuthorsDf$Author == authorNames[a]), ]$NumberOfChanges, ")", "', ", sep = "")
      }
      
      tmpColorString <- paste(tmpColorString, currentAuthorsColor, ", ", sep = "")
    }
    
    tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 2)
    tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 2)
    tmpColorString <- substr(tmpColorString, 1, nchar(tmpColorString) - 2)
    
    if(m == 1) {
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, cex.axis = 2, cex.lab = 1.5, ylim = c(0.0, 1.0), horizontal = TRUE, col = c(", tmpColorString, "), names = c(", tmpColString, "))", sep =  "")))
      title(ylab = "Selected authors (sample size)", line = 4)
      title(xlab = metricList[m], mgp=c(4,1,0), cex.lab = 2)
    }
    else {
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, cex.axis = 2, cex.lab = 1.5, ylim = c(0.0, 1.0), horizontal = TRUE, col = c(", tmpColorString, "))", sep =  "")))
      title(xlab = metricList[m], mgp=c(4,1,0), cex.lab = 2)
    }
  }
  
  # mtext(paste("pd, pf, precision, F1 values for the models (", algorithmName, " on ", projectName, ")", sep = ""), outer = TRUE, cex = 1.5)
  
  dev.off()
  
  #info
  print(paste(projectName, " all authors colored boxplot", sep = ""))
}
tableMakeColoredIndividualResults <- function(projectName, colorCount) {
  
  # createCopyFolder(oldDir = paste(baseDir, projectName, "/results/", sep = ""),
  #                  newDir = paste(baseDir, projectName, "/ColoredResults", sep = ""))
  
  tmpSelectedAuthorsDf <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  
  for(a in 1:length(authorNames)) {
    fileNamePM <- paste(baseDir, projectName, "/results/", authorNames[a], "ResultsPM.xlsx", sep = "")
    resultsTable <- read.xlsx(fileNamePM, sheetIndex = 1, header = TRUE)
    
    wb <- loadWorkbook(fileNamePM)
    
    # fg1 <- Fill(foregroundColor="lightskyblue")   # create fill object # 1
    # cs1 <- CellStyle(wb, fill=fg1)        # create cell style # 1
    # fg2 <- Fill(foregroundColor="tomato")    # create fill object # 2
    # cs2 <- CellStyle(wb, fill=fg2)        # create cell style # 2 
    
    sheets <- getSheets(wb)               # get all sheets
    sheet <- sheets[[1]]          # get specific sheet
    rows <- getRows(sheet, rowIndex = 2:(nrow(resultsTable)+1))
    cells <- getCells(rows, colIndex = 5:ncol(resultsTable))
    
    values <- lapply(cells, getCellValue) # extract the cell values
    
    for (m in 1:length(metricList)) {
      tmpColor <- getAuthorColorByMetric(projectName, authorNames[a], metricList[m], colorCount)
      tmpColor <- substr(tmpColor, 2, nchar(tmpColor) - 1)
      
      fg <- Fill(foregroundColor = tmpColor)
      cs <- CellStyle(wb, fill = fg)
      startPointInTable <- m
      
      for (i in seq(from = startPointInTable, by = 4, length.out = 10)) {
        setCellStyle(cells[[i]], cs)
      }
      
      saveWorkbook(wb, fileNamePM)
    }
  }
  
  #info
  print(paste(projectName, " make individual results tables colored", sep = ""))
}
tableAllPMResults <- function(projectName) {
  tmpAllChanges <- get(paste(projectName, "Changes", sep = ""))
  colnames(tmpAllChanges)[1] <- "HASHID"
  allChangesBugInducingChangeCount <- length(which(tmpAllChanges$CLASS == "BUG"))
  
  tmpSelectedAuthorsDf <- read.xlsx(file = "/Users/beken/R/ChangeData/Experiment/selectedAuthors.xlsx", sheetName = paste(projectName), header = TRUE)
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  authorChangeCounts <- as.character(tmpSelectedAuthorsDf$NumberOfChanges)
  authorBugInducingChangeCount <- as.character(tmpSelectedAuthorsDf$NumberOfBugInducingChanges)
  
  resultsTable <- as.data.frame(matrix(nrow = 0, ncol = 21))
  
  #mean, median, min, max values for -pd, -pf, -precision, -F1
  for(a in 1:length(authorNames)) {
    resultsTable[nrow(resultsTable) + 1, ] <- c(authorNames[a], authorChangeCounts[a], authorBugInducingChangeCount[a], (as.numeric(authorBugInducingChangeCount[a])/as.numeric(authorChangeCounts[a])), "",
                                                meanChangeNaToZero(get(authorNames[a])[, paste(metricList[1], sep = "")]),
                                                medianChangeNaToZero(get(authorNames[a])[, paste(metricList[1], sep = "")]),
                                                minChangeNaToZero(get(authorNames[a])[, paste(metricList[1], sep = "")]),
                                                maxChangeNaToZero(get(authorNames[a])[, paste(metricList[1], sep = "")]),
                                                meanChangeNaToZero(get(authorNames[a])[, paste(metricList[2], sep = "")]),
                                                medianChangeNaToZero(get(authorNames[a])[, paste(metricList[2], sep = "")]),
                                                minChangeNaToZero(get(authorNames[a])[, paste(metricList[2], sep = "")]),
                                                maxChangeNaToZero(get(authorNames[a])[, paste(metricList[2], sep = "")]),
                                                meanChangeNaToZero(get(authorNames[a])[, paste(metricList[3], sep = "")]),
                                                medianChangeNaToZero(get(authorNames[a])[, paste(metricList[3], sep = "")]),
                                                minChangeNaToZero(get(authorNames[a])[, paste(metricList[3], sep = "")]),
                                                maxChangeNaToZero(get(authorNames[a])[, paste(metricList[3], sep = "")]),
                                                meanChangeNaToZero(get(authorNames[a])[, paste(metricList[4], sep = "")]),
                                                medianChangeNaToZero(get(authorNames[a])[, paste(metricList[4], sep = "")]),
                                                minChangeNaToZero(get(authorNames[a])[, paste(metricList[4], sep = "")]),
                                                maxChangeNaToZero(get(authorNames[a])[, paste(metricList[4], sep = "")]))
  }
  
  colnames(resultsTable) <- c("Author", "Commits", "Bug-inducing Commits", "Bug-inducing %", "Sample size",
                              "mean-Pd", "median-Pd", "min-Pd", "max-Pd", 
                              "mean-Pf", "median-Pf", "min-Pf", "max-Pf",
                              "mean-Precision", "median-Precision", "min-Precision", "max-Precision",
                              "mean-F1", "median-F1", "min-F1", "max-F1")
  
  resultsFile <- paste(baseDir, "ResultsPM.xlsx", sep = "")
  
  if(file.exists(resultsFile)) {
    write.xlsx(resultsTable, file = resultsFile, sheetName = paste(projectName, sep = ""), row.names = FALSE, append = TRUE)
  }
  else {
    write.xlsx(resultsTable, file = resultsFile, sheetName = paste(projectName, sep = ""), row.names = FALSE)
  }
  
  #info
  print(paste(projectName, " all results table", sep = ""))
}
tableAllResults <- function(projectName) {
  tmpAllChanges <- get(paste(projectName, "Changes", sep = ""))
  colnames(tmpAllChanges)[1] <- "HASHID"
  allChangesBugInducingChangeCount <- length(which(tmpAllChanges$CLASS == "BUG"))
  
  tmpSelectedAuthorsDf <- read.xlsx(file = "/Users/beken/R/ChangeData/Experiment/selectedAuthors.xlsx", sheetName = paste(projectName), header = TRUE)
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  authorChangeCounts <- as.character(tmpSelectedAuthorsDf$NumberOfChanges)
  authorBugInducingChangeCount <- as.character(tmpSelectedAuthorsDf$NumberOfBugInducingChanges)
  
  resultsTable <- as.data.frame(matrix(nrow = 0, ncol = 21))
  
  #mean, median, min, max values for -pd, -pf, -precision, -F1
  for(a in 1:length(authorNames)) {
    resultsPM <- readAuthorTestResultsAllIterations(projectName, authorNames[a], "PM")
    resultsSM <- readAuthorTestResultsAllIterations(projectName, authorNames[a], "SM")
    resultsGM <- readAuthorTestResultsAllIterations(projectName, authorNames[a], "GM")
    resultsWSM <- readAuthorTestResultsAllIterations(projectName, authorNames[a], "WSM")
    resultsMeta <- readAuthorTestResultsAllIterations(projectName, authorNames[a], "Meta")
    
    resultsTable[nrow(resultsTable) + 1, ] <- c(paste(authorNames[a], "PM"), #author and model name
                                                authorChangeCounts[a], #author's commit count
                                                authorBugInducingChangeCount[a], #auhtor's bug-inducing commit count
                                                (as.numeric(authorBugInducingChangeCount[a])/as.numeric(authorChangeCounts[a])), #author's bug-inducing commit ratio
                                                "", #sample size
                                                meanChangeNaToZero(resultsPM[, paste(metricList[1], sep = "")]),
                                                medianChangeNaToZero(resultsPM[, paste(metricList[1], sep = "")]),
                                                minChangeNaToZero(resultsPM[, paste(metricList[1], sep = "")]),
                                                maxChangeNaToZero(resultsPM[, paste(metricList[1], sep = "")]),
                                                meanChangeNaToZero(resultsPM[, paste(metricList[2], sep = "")]),
                                                medianChangeNaToZero(resultsPM[, paste(metricList[2], sep = "")]),
                                                minChangeNaToZero(resultsPM[, paste(metricList[2], sep = "")]),
                                                maxChangeNaToZero(resultsPM[, paste(metricList[2], sep = "")]),
                                                meanChangeNaToZero(resultsPM[, paste(metricList[3], sep = "")]),
                                                medianChangeNaToZero(resultsPM[, paste(metricList[3], sep = "")]),
                                                minChangeNaToZero(resultsPM[, paste(metricList[3], sep = "")]),
                                                maxChangeNaToZero(resultsPM[, paste(metricList[3], sep = "")]),
                                                meanChangeNaToZero(resultsPM[, paste(metricList[4], sep = "")]),
                                                medianChangeNaToZero(resultsPM[, paste(metricList[4], sep = "")]),
                                                minChangeNaToZero(resultsPM[, paste(metricList[4], sep = "")]),
                                                maxChangeNaToZero(resultsPM[, paste(metricList[4], sep = "")]))
    
    #TODO: some information is different for SM - GM
    #bug ratio
    #commit count vs
    resultsTable[nrow(resultsTable) + 1, ] <- c(paste(authorNames[a], "SM"), #author and model name 
                                                authorChangeCounts[a], 
                                                authorBugInducingChangeCount[a], 
                                                (as.numeric(authorBugInducingChangeCount[a])/as.numeric(authorChangeCounts[a])), 
                                                "",
                                                meanChangeNaToZero(resultsSM[, paste(metricList[1], sep = "")]),
                                                medianChangeNaToZero(resultsSM[, paste(metricList[1], sep = "")]),
                                                minChangeNaToZero(resultsSM[, paste(metricList[1], sep = "")]),
                                                maxChangeNaToZero(resultsSM[, paste(metricList[1], sep = "")]),
                                                meanChangeNaToZero(resultsSM[, paste(metricList[2], sep = "")]),
                                                medianChangeNaToZero(resultsSM[, paste(metricList[2], sep = "")]),
                                                minChangeNaToZero(resultsSM[, paste(metricList[2], sep = "")]),
                                                maxChangeNaToZero(resultsSM[, paste(metricList[2], sep = "")]),
                                                meanChangeNaToZero(resultsSM[, paste(metricList[3], sep = "")]),
                                                medianChangeNaToZero(resultsSM[, paste(metricList[3], sep = "")]),
                                                minChangeNaToZero(resultsSM[, paste(metricList[3], sep = "")]),
                                                maxChangeNaToZero(resultsSM[, paste(metricList[3], sep = "")]),
                                                meanChangeNaToZero(resultsSM[, paste(metricList[4], sep = "")]),
                                                medianChangeNaToZero(resultsSM[, paste(metricList[4], sep = "")]),
                                                minChangeNaToZero(resultsSM[, paste(metricList[4], sep = "")]),
                                                maxChangeNaToZero(resultsSM[, paste(metricList[4], sep = "")]))
    
    resultsTable[nrow(resultsTable) + 1, ] <- c(paste(authorNames[a], "GM"), #author and model name
                                                authorChangeCounts[a], 
                                                authorBugInducingChangeCount[a], 
                                                (as.numeric(authorBugInducingChangeCount[a])/as.numeric(authorChangeCounts[a])), 
                                                "",
                                                meanChangeNaToZero(resultsGM[, paste(metricList[1], sep = "")]),
                                                medianChangeNaToZero(resultsGM[, paste(metricList[1], sep = "")]),
                                                minChangeNaToZero(resultsGM[, paste(metricList[1], sep = "")]),
                                                maxChangeNaToZero(resultsGM[, paste(metricList[1], sep = "")]),
                                                meanChangeNaToZero(resultsGM[, paste(metricList[2], sep = "")]),
                                                medianChangeNaToZero(resultsGM[, paste(metricList[2], sep = "")]),
                                                minChangeNaToZero(resultsGM[, paste(metricList[2], sep = "")]),
                                                maxChangeNaToZero(resultsGM[, paste(metricList[2], sep = "")]),
                                                meanChangeNaToZero(resultsGM[, paste(metricList[3], sep = "")]),
                                                medianChangeNaToZero(resultsGM[, paste(metricList[3], sep = "")]),
                                                minChangeNaToZero(resultsGM[, paste(metricList[3], sep = "")]),
                                                maxChangeNaToZero(resultsGM[, paste(metricList[3], sep = "")]),
                                                meanChangeNaToZero(resultsGM[, paste(metricList[4], sep = "")]),
                                                medianChangeNaToZero(resultsGM[, paste(metricList[4], sep = "")]),
                                                minChangeNaToZero(resultsGM[, paste(metricList[4], sep = "")]),
                                                maxChangeNaToZero(resultsGM[, paste(metricList[4], sep = "")]))
    
    resultsTable[nrow(resultsTable) + 1, ] <- c(paste(authorNames[a], "WSM"), #author and model name 
                                                authorChangeCounts[a], 
                                                authorBugInducingChangeCount[a], 
                                                (as.numeric(authorBugInducingChangeCount[a])/as.numeric(authorChangeCounts[a])), 
                                                "",
                                                meanChangeNaToZero(resultsWSM[, paste(metricList[1], sep = "")]),
                                                medianChangeNaToZero(resultsWSM[, paste(metricList[1], sep = "")]),
                                                minChangeNaToZero(resultsWSM[, paste(metricList[1], sep = "")]),
                                                maxChangeNaToZero(resultsWSM[, paste(metricList[1], sep = "")]),
                                                meanChangeNaToZero(resultsWSM[, paste(metricList[2], sep = "")]),
                                                medianChangeNaToZero(resultsWSM[, paste(metricList[2], sep = "")]),
                                                minChangeNaToZero(resultsWSM[, paste(metricList[2], sep = "")]),
                                                maxChangeNaToZero(resultsWSM[, paste(metricList[2], sep = "")]),
                                                meanChangeNaToZero(resultsWSM[, paste(metricList[3], sep = "")]),
                                                medianChangeNaToZero(resultsWSM[, paste(metricList[3], sep = "")]),
                                                minChangeNaToZero(resultsWSM[, paste(metricList[3], sep = "")]),
                                                maxChangeNaToZero(resultsWSM[, paste(metricList[3], sep = "")]),
                                                meanChangeNaToZero(resultsWSM[, paste(metricList[4], sep = "")]),
                                                medianChangeNaToZero(resultsWSM[, paste(metricList[4], sep = "")]),
                                                minChangeNaToZero(resultsWSM[, paste(metricList[4], sep = "")]),
                                                maxChangeNaToZero(resultsWSM[, paste(metricList[4], sep = "")]))
    
    resultsTable[nrow(resultsTable) + 1, ] <- c(paste(authorNames[a], "Meta"), #author and model name 
                                                authorChangeCounts[a], 
                                                authorBugInducingChangeCount[a], 
                                                (as.numeric(authorBugInducingChangeCount[a])/as.numeric(authorChangeCounts[a])), 
                                                "",
                                                meanChangeNaToZero(resultsMeta[, paste(metricList[1], sep = "")]),
                                                medianChangeNaToZero(resultsMeta[, paste(metricList[1], sep = "")]),
                                                minChangeNaToZero(resultsMeta[, paste(metricList[1], sep = "")]),
                                                maxChangeNaToZero(resultsMeta[, paste(metricList[1], sep = "")]),
                                                meanChangeNaToZero(resultsMeta[, paste(metricList[2], sep = "")]),
                                                medianChangeNaToZero(resultsMeta[, paste(metricList[2], sep = "")]),
                                                minChangeNaToZero(resultsMeta[, paste(metricList[2], sep = "")]),
                                                maxChangeNaToZero(resultsMeta[, paste(metricList[2], sep = "")]),
                                                meanChangeNaToZero(resultsMeta[, paste(metricList[3], sep = "")]),
                                                medianChangeNaToZero(resultsMeta[, paste(metricList[3], sep = "")]),
                                                minChangeNaToZero(resultsMeta[, paste(metricList[3], sep = "")]),
                                                maxChangeNaToZero(resultsMeta[, paste(metricList[3], sep = "")]),
                                                meanChangeNaToZero(resultsMeta[, paste(metricList[4], sep = "")]),
                                                medianChangeNaToZero(resultsMeta[, paste(metricList[4], sep = "")]),
                                                minChangeNaToZero(resultsMeta[, paste(metricList[4], sep = "")]),
                                                maxChangeNaToZero(resultsMeta[, paste(metricList[4], sep = "")]))
  }
  
  colnames(resultsTable) <- c("Author", "Commits", "Bug-inducing Commits", "Bug-inducing %", "Sample size",
                              "mean-Pd", "median-Pd", "min-Pd", "max-Pd", 
                              "mean-Pf", "median-Pf", "min-Pf", "max-Pf",
                              "mean-Precision", "median-Precision", "min-Precision", "max-Precision",
                              "mean-F1", "median-F1", "min-F1", "max-F1")
  
  resultsFile <- paste(baseDir, "ResultsAll.xlsx", sep = "")
  
  if(file.exists(resultsFile)) {
    write.xlsx(resultsTable, file = resultsFile, sheetName = paste(projectName, sep = ""), row.names = FALSE, append = TRUE)
  }
  else {
    write.xlsx(resultsTable, file = resultsFile, sheetName = paste(projectName, sep = ""), row.names = FALSE)
  }
  
  #info
  print(paste(projectName, " all results table", sep = ""))
}
tableMakeColoredAllPMResults <- function(projectName, colorCount) {
  tmpSelectedAuthorsDf <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  
  fileNamePM <- paste(baseDir, "Results.xlsx", sep = "")
  resultsTable <- read.xlsx(fileNamePM, sheetIndex = projectName, header = TRUE)
  wb <- loadWorkbook(fileNamePM)
  sheets <- getSheets(wb)
  sheet <- sheets[[projectName]]  
  rows <- getRows(sheet, rowIndex = 2:(nrow(resultsTable)+1))
  cells <- getCells(rows, colIndex = 6:ncol(resultsTable))
  values <- lapply(cells, getCellValue) # extract the cell values
  
  for(a in 1:length(authorNames)) {
    for (m in 1:length(metricList)) {
      tmpColor <- getAuthorColorByMetric(projectName, authorNames[a], metricList[m], colorCount)
      tmpColor <- substr(tmpColor, 2, nchar(tmpColor) - 1)
      
      fg <- Fill(foregroundColor = tmpColor)
      cs <- CellStyle(wb, fill = fg)
      
      cellIndex <- (a - 1) * 16 + (m * 4) - 2
      setCellStyle(cells[[cellIndex]], cs)
      saveWorkbook(wb, fileNamePM)
    }
  }
}
tableMakeColoredAllResults <- function(projectName, numberOfColors) {
  tmpSelectedAuthorsDf <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  
  fileNamePM <- paste(baseDir, "ResultsAll.xlsx", sep = "")
  resultsTable <- read.xlsx(fileNamePM, sheetIndex = projectName, header = TRUE)
  wb <- loadWorkbook(fileNamePM)
  sheets <- getSheets(wb)
  sheet <- sheets[[projectName]]  
  rows <- getRows(sheet, rowIndex = 2:(nrow(resultsTable) + 1))
  cells <- getCells(rows, colIndex = 6:ncol(resultsTable))
  values <- lapply(cells, getCellValue) # extract the cell values
  
  for(a in 1:length(authorNames)) {
    for (m in 1:length(metricList)) {
      tmpColor <- getAuthorColorByMetric(projectName, authorNames[a], metricList[m], numberOfColors)
      tmpColor <- substr(tmpColor, 2, nchar(tmpColor) - 1)
      
      fg <- Fill(foregroundColor = tmpColor)
      cs <- CellStyle(wb, fill = fg)
      
      # cellIndex <- (a - 1) * 48 + (m * 4) - 2 #3models
      cellIndex <- (a - 1) * 80 + (m * 4) - 2 #5models
      setCellStyle(cells[[cellIndex]], cs)
      saveWorkbook(wb, fileNamePM)
    }
  }
}
tableComparisonOfModels <- function(projectName) {
  pVal <- 0.05
  
  tmpSelectedAuthors <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthors$Author)
  
  comparisonCountsTable <- as.data.frame(matrix(nrow = 11, ncol = 3 * length(metricList))) 
  colnames(comparisonCountsTable) <- c("pd", "", "", "pf", "", "", "precision", "", "", "F1", "", "")
  comparisonCountsTable[1, ] <- c(">", "<", "=", ">", "<", "=", ">", "<", "=", ">", "<", "=")
  rownames(comparisonCountsTable) <- c("", 
                                       "PM . SM", 
                                       "PM . GM", 
                                       "SM . GM", 
                                       "PM . WSM", 
                                       "PM . PM+", 
                                       "SM . WSM", 
                                       "SM . PM+", 
                                       "GM . WSM", 
                                       "GM . PM+", 
                                       "WSM . PM+")
  
  allAuthorNemenyiPVals <- as.data.frame(matrix(nrow = 0, ncol = 2 + length(metricList))) 
  colnames(allAuthorNemenyiPVals) <- c("author", "model", "recall", "pf", "precision", "F1")
  allAuthorFriedmanPVals <- as.data.frame(matrix(nrow = 0, ncol = 2 + length(metricList))) 
  colnames(allAuthorFriedmanPVals) <- c("author", "model", "recall", "pf", "precision", "F1")
  
  for(m in 1:length(metricList)) {
    countPMGreaterSM <- 0
    countPMGreaterGM <- 0   
    countSMGreaterGM <- 0
    countPMEqualsSM <- 0
    countPMEqualsGM <- 0   
    countSMEqualsGM <- 0

    countPMGreaterWSM <- 0
    countPMGreaterMeta <- 0   
    countSMGreaterWSM <- 0
    countSMGreaterMeta <- 0
    countGMGreaterWSM <- 0
    countGMGreaterMeta <- 0
    countWSMGreaterMeta <- 0
    countPMEqualsWSM <- 0
    countPMEqualsMeta <- 0   
    countSMEqualsWSM <- 0
    countSMEqualsMeta <- 0
    countGMEqualsWSM <- 0
    countGMEqualsMeta <- 0
    countWSMEqualsMeta <- 0
    
    for(d in 1:length(authorNames)) {
      authorName <- authorNames[d]
      
      resultsPM <- readAuthorTestResultsAllIterations(projectName, authorNames[d], "PM")
      resultsSM <- readAuthorTestResultsAllIterations(projectName, authorNames[d], "SM")
      resultsGM <- readAuthorTestResultsAllIterations(projectName, authorNames[d], "GM")
      resultsWSM <- readAuthorTestResultsAllIterations(projectName, authorNames[d], "WSM")
      resultsMeta <- readAuthorTestResultsAllIterations(projectName, authorNames[d], "Meta")
      
      currentAuthorFriedmanPVals <- cbind(authorNames[d], getFriedmanComparisonResultOfModels(resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta, authorName, projectName))
      currentAuthorNemenyiPVals <- cbind(authorNames[d], getNemenyiComparisonResultOfModels(resultsPM, resultsSM, resultsGM, resultsWSM, resultsMeta, authorName, projectName))
      if(m == 1) { #only bind the pVals once at the beginning of the m loop
        allAuthorFriedmanPVals <- rbind(allAuthorFriedmanPVals, currentAuthorFriedmanPVals)
        allAuthorNemenyiPVals <- rbind(allAuthorNemenyiPVals, currentAuthorNemenyiPVals)
      }
      
      # allAuthorNemenyiPVals[(d - 1) * 3 + 1, m + 2] #PM.SM
      # allAuthorNemenyiPVals[(d - 1) * 3 + 2, m + 2] #PM.GM
      # allAuthorNemenyiPVals[(d - 1) * 3 + 3, m + 2] #SM.GM
      # currentAuthorNemenyiPVals[1, m + 2] #PM.SM
      # currentAuthorNemenyiPVals[2, m + 2] #PM.GM
      # currentAuthorNemenyiPVals[3, m + 2] #SM.GM
      
      if (currentAuthorNemenyiPVals[1, m + 2] < pVal) { #if PM and SM are different
        if(medianChangeNaToZero(resultsPM[, m + 4]) > medianChangeNaToZero(resultsSM[, m + 4]))
          countPMGreaterSM <- countPMGreaterSM + 1
      }
      else { # else PM and SM same
        countPMEqualsSM <- countPMEqualsSM + 1
      }
      if(currentAuthorNemenyiPVals[2, m + 2] < pVal) { #if PM and GM are different
        if(medianChangeNaToZero(resultsPM[, m + 4]) > medianChangeNaToZero(resultsGM[, m + 4]))
          countPMGreaterGM <- countPMGreaterGM + 1
      }
      else { # else PM and GM are same
        countPMEqualsGM <- countPMEqualsGM + 1
      }
      if(currentAuthorNemenyiPVals[3, m + 2] < pVal) {  #if GM and SM are different
        if(medianChangeNaToZero(resultsSM[, m + 4]) > medianChangeNaToZero(resultsGM[, m + 4]))
          countSMGreaterGM <- countSMGreaterGM + 1
      }
      else { # else PM and GM are same
        countSMEqualsGM <- countSMEqualsGM + 1
      }
      if(currentAuthorNemenyiPVals[4, m + 2] < pVal) {  #if PM and WSM are different
        if(medianChangeNaToZero(resultsPM[, m + 4]) > medianChangeNaToZero(resultsWSM[, m + 4]))
          countPMGreaterWSM <- countPMGreaterWSM + 1
      }
      else { # else PM and WSM are same
        countPMEqualsWSM <- countPMEqualsWSM + 1
      }
      if(currentAuthorNemenyiPVals[5, m + 2] < pVal) {  #if PM and Meta are different
        if(medianChangeNaToZero(resultsPM[, m + 4]) > medianChangeNaToZero(resultsMeta[, m + 4]))
          countPMGreaterMeta <- countPMGreaterMeta + 1
      }
      else { # else PM and Meta are same
        countPMEqualsMeta <- countPMEqualsMeta + 1
      }
      if(currentAuthorNemenyiPVals[6, m + 2] < pVal) {  #if SM and WSM are different
        if(medianChangeNaToZero(resultsSM[, m + 4]) > medianChangeNaToZero(resultsWSM[, m + 4]))
          countSMGreaterWSM <- countSMGreaterWSM + 1
      }
      else { # else SM and WSM are same
        countSMEqualsWSM <- countSMEqualsWSM + 1
      }
      if(currentAuthorNemenyiPVals[7, m + 2] < pVal) {  #if SM and Meta are different
        if(medianChangeNaToZero(resultsSM[, m + 4]) > medianChangeNaToZero(resultsMeta[, m + 4]))
          countSMGreaterMeta <- countSMGreaterMeta + 1
      }
      else { # else SM and Meta are same
        countSMEqualsMeta <- countSMEqualsMeta + 1
      }
      if(currentAuthorNemenyiPVals[8, m + 2] < pVal) {  #if GM and WSM are different
        if(medianChangeNaToZero(resultsGM[, m + 4]) > medianChangeNaToZero(resultsWSM[, m + 4]))
          countGMGreaterWSM <- countGMGreaterWSM + 1
      }
      else { # else GM and WSM are same
        countGMEqualsWSM <- countGMEqualsWSM + 1
      }
      if(currentAuthorNemenyiPVals[9, m + 2] < pVal) {  #if GM and Meta are different
        if(medianChangeNaToZero(resultsGM[, m + 4]) > medianChangeNaToZero(resultsMeta[, m + 4]))
          countGMGreaterMeta <- countGMGreaterMeta + 1
      }
      else { # else GM and Meta are same
        countGMEqualsMeta <- countGMEqualsMeta + 1
      }
      if(currentAuthorNemenyiPVals[10, m + 2] < pVal) {  #if WSM and Meta are different
        if(medianChangeNaToZero(resultsWSM[, m + 4]) > medianChangeNaToZero(resultsMeta[, m + 4]))
          countWSMGreaterMeta <- countWSMGreaterMeta + 1
      }
      else { # else WSM and Meta are same
        countWSMEqualsMeta <- countWSMEqualsMeta + 1
      }
    }
    
    # PM . SM
    comparisonCountsTable[2, 3 * (m - 1) + 1] <- paste(countPMGreaterSM, "/", length(authorNames), sep = "")
    comparisonCountsTable[2, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countPMGreaterSM + countPMEqualsSM)), "/", length(authorNames), sep = "")
    comparisonCountsTable[2, 3 * (m - 1) + 3] <- paste(countPMEqualsSM, "/",length(authorNames), sep = "")
    # PM . GM 
    comparisonCountsTable[3, 3 * (m - 1) + 1] <- paste(countPMGreaterGM, "/", length(authorNames), sep = "")
    comparisonCountsTable[3, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countPMGreaterGM + countPMEqualsGM)), "/", length(authorNames), sep = "")
    comparisonCountsTable[3, 3 * (m - 1) + 3] <- paste(countPMEqualsGM, "/", length(authorNames), sep = "")
    # SM . GM
    comparisonCountsTable[4, 3 * (m - 1) + 1] <- paste(countSMGreaterGM, "/", length(authorNames), sep = "") 
    comparisonCountsTable[4, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countSMGreaterGM + countSMEqualsGM)), "/", length(authorNames), sep = "")
    comparisonCountsTable[4, 3 * (m - 1) + 3] <- paste(countSMEqualsGM, "/", length(authorNames), sep = "") 
    # PM . WSM
    comparisonCountsTable[5, 3 * (m - 1) + 1] <- paste(countPMGreaterWSM, "/", length(authorNames), sep = "") 
    comparisonCountsTable[5, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countPMGreaterWSM + countPMEqualsWSM)), "/", length(authorNames), sep = "")
    comparisonCountsTable[5, 3 * (m - 1) + 3] <- paste(countPMEqualsWSM, "/", length(authorNames), sep = "") 
    # PM . PM+
    comparisonCountsTable[6, 3 * (m - 1) + 1] <- paste(countPMGreaterMeta, "/", length(authorNames), sep = "") 
    comparisonCountsTable[6, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countPMGreaterMeta + countPMEqualsMeta)), "/", length(authorNames), sep = "")
    comparisonCountsTable[6, 3 * (m - 1) + 3] <- paste(countPMEqualsMeta, "/", length(authorNames), sep = "") 
    # SM . WSM
    comparisonCountsTable[7, 3 * (m - 1) + 1] <- paste(countSMGreaterWSM, "/", length(authorNames), sep = "") 
    comparisonCountsTable[7, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countSMGreaterWSM + countSMEqualsWSM)), "/", length(authorNames), sep = "")
    comparisonCountsTable[7, 3 * (m - 1) + 3] <- paste(countSMEqualsWSM, "/", length(authorNames), sep = "") 
    # SM . PM+
    comparisonCountsTable[8, 3 * (m - 1) + 1] <- paste(countSMGreaterMeta, "/", length(authorNames), sep = "") 
    comparisonCountsTable[8, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countSMGreaterMeta + countSMEqualsMeta)), "/", length(authorNames), sep = "")
    comparisonCountsTable[8, 3 * (m - 1) + 3] <- paste(countSMEqualsMeta, "/", length(authorNames), sep = "") 
    # GM . WSM
    comparisonCountsTable[9, 3 * (m - 1) + 1] <- paste(countGMGreaterWSM, "/", length(authorNames), sep = "") 
    comparisonCountsTable[9, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countGMGreaterWSM + countGMEqualsWSM)), "/", length(authorNames), sep = "")
    comparisonCountsTable[9, 3 * (m - 1) + 3] <- paste(countGMEqualsWSM, "/", length(authorNames), sep = "") 
    # GM . PM+
    comparisonCountsTable[10, 3 * (m - 1) + 1] <- paste(countGMGreaterMeta, "/", length(authorNames), sep = "") 
    comparisonCountsTable[10, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countGMGreaterMeta + countGMEqualsMeta)), "/", length(authorNames), sep = "")
    comparisonCountsTable[10, 3 * (m - 1) + 3] <- paste(countGMEqualsMeta, "/", length(authorNames), sep = "") 
    # WSM . PM+
    comparisonCountsTable[11, 3 * (m - 1) + 1] <- paste(countWSMGreaterMeta, "/", length(authorNames), sep = "") 
    comparisonCountsTable[11, 3 * (m - 1) + 2] <- paste((length(authorNames) - (countWSMGreaterMeta + countWSMEqualsMeta)), "/", length(authorNames), sep = "")
    comparisonCountsTable[11, 3 * (m - 1) + 3] <- paste(countWSMEqualsMeta, "/", length(authorNames), sep = "") 
  }
  
  resultsFile <- paste(baseDir, "ComparisonCounts.xlsx", sep = "")
  
  if(file.exists(resultsFile)) {
    write.xlsx(allAuthorFriedmanPVals, file = paste(baseDir, "ComparisonCounts.xlsx", sep = ""), sheetName = paste(projectName, "Friedman", sep = ""), row.names = FALSE, append = TRUE)
    write.xlsx(allAuthorNemenyiPVals, file = paste(baseDir, "ComparisonCounts.xlsx", sep = ""), sheetName = paste(projectName, "Nemenyi", sep = ""), row.names = FALSE, append = TRUE)
    write.xlsx(comparisonCountsTable, file = paste(baseDir, "ComparisonCounts.xlsx", sep = ""), sheetName = paste(projectName, "Counts", sep = ""), row.names = TRUE, append = TRUE)
  }
  else{
    write.xlsx(allAuthorFriedmanPVals, file = paste(baseDir, "ComparisonCounts.xlsx", sep = ""), sheetName = paste(projectName, "Friedman", sep = ""), row.names = FALSE)
    write.xlsx(allAuthorNemenyiPVals, file = paste(baseDir, "ComparisonCounts.xlsx", sep = ""), sheetName = paste(projectName, "Nemenyi", sep = ""), row.names = FALSE, append = TRUE)
    write.xlsx(comparisonCountsTable, file = paste(baseDir, "ComparisonCounts.xlsx", sep = ""), sheetName = paste(projectName, "Counts", sep = ""), row.names = TRUE, append = TRUE)
  }
  
  #info
  print(paste(projectName, " PM/SM/GM/WSM/PM+ comparison tables", sep = ""))
}
tableFeatures <- function(projectName) {
  tmpAllChanges <- get(paste(projectName, "Changes", sep = ""))
  
  if(projectName == "Gimp" || projectName == "Perl" || projectName == "Rails" || projectName == "Rhino") {
    tmpAllChanges <- tmpAllChanges[, -c(2)] #delete first author name
  }
  
  tmpAllChanges <- tmpAllChanges[, -c(1, 3, 4, 8)]
  tmpSelectedAuthorsDf <- get(paste(projectName, "SelectedAuthors", sep = ""))
  authorNames <- as.character(tmpSelectedAuthorsDf$Author)
  
  featureTable <- data.frame()
  
  featureTable[nrow(featureTable) + 1, 1] <- projectName
  for(f in 2:(length(tmpAllChanges) - 1)) {
    positionInResults <- (f - 2) * 4 + 2
    featureTable[nrow(featureTable), positionInResults] <- min(tmpAllChanges[, f])
    featureTable[nrow(featureTable), positionInResults + 1] <- max(tmpAllChanges[, f])
    featureTable[nrow(featureTable), positionInResults + 2] <- median(tmpAllChanges[, f])
    featureTable[nrow(featureTable), positionInResults + 3] <- mean(tmpAllChanges[, f])
  }
  
  for(d in 1:length(tmpSelectedAuthorsDf$Author)) {
    featureTable[nrow(featureTable) + 1, 1] <- authorNames[d]
    for(f in 2:(length(tmpAllChanges) - 1)) {
      positionInResults <- (f - 2) * 4 + 2
      featureTable[nrow(featureTable), positionInResults] <- min(tmpAllChanges[which(tmpAllChanges[, 1] == authorNames[d]) , f])
      featureTable[nrow(featureTable), positionInResults + 1] <- max(tmpAllChanges[which(tmpAllChanges[, 1] == authorNames[d]) , f])
      featureTable[nrow(featureTable), positionInResults + 2] <- median(tmpAllChanges[which(tmpAllChanges[, 1] == authorNames[d]) , f])
      featureTable[nrow(featureTable), positionInResults + 3] <- mean(tmpAllChanges[which(tmpAllChanges[, 1] == authorNames[d]) , f])
    }
  }
  
  colnames(featureTable) = c("AuthorName", 
                             "minCHURN", "maxCHURN", "medianCHURN", "meanCHURN", 
                             "minADD",  "maxADD",  "medianADD",  "meanADD", 
                             "minDEL", "maxDEL", "medianDEL", "meanDEL", 
                             "minNS", "maxNS", "medianNS","meanNS", 
                             "minND", "maxND", "medianND", "meanND", 
                             "minNF", "maxNF", "medianNF", "meanNF", 
                             "minENT", "maxENT", "medianENT", "meanENT", 
                             "minNDEV", "maxNDEV", "medianNDEV", "meanNDEV", 
                             "minAGE", "maxAGE", "medianAGE", "meanAGE", 
                             "minNFC", "maxNFC", "medianNFC", "meanNFC", 
                             "minEXP", "maxEXP", "medianEXP", "meanEXP", 
                             "minREXP", "maxREXP", "medianREXP", "meanREXP", 
                             "minSEXP", "maxSEXP", "medianSEXP", "meanSEXP")
  
  featureFile <- paste(baseDir, "Features.xlsx", sep = "")
  
  if(file.exists(featureFile)) {
    write.xlsx(featureTable, file = featureFile, sheetName = paste(projectName, sep = ""), row.names = FALSE, append = TRUE)
  }
  else {
    write.xlsx(featureTable, file = featureFile, sheetName = paste(projectName, sep = ""), row.names = FALSE)
  }
}


for(p in 1:length(projectList)) {
  #some variable names for easy usage
  projectName <- projectList[p]
  selectedAuthorsDfName <- paste(projectName, "SelectedAuthors", sep = "")
  changesDfName <- paste(projectName, "Changes", sep = "")
  
  #set working directory for the project
  workingDir <- paste(baseDir, projectList[p], sep = "")
  setwd(workingDir)
  
  #create folder for plots and results
  dir.create(paste("plots"), showWarnings = FALSE)
  dir.create(paste("results"), showWarnings = FALSE)
  
  #read selected authors and keep them in a table and a list, also keep the number of selected authors
  assign(selectedAuthorsDfName, 
         read.xlsx(file = paste(mainStoreDir, "selectedAuthors.xlsx", sep = ""), sheetName = paste(projectName), header = TRUE))
  authorNames <- as.character(get(paste(projectName, "SelectedAuthors", sep = ""))$Author)
  numberOfSelectedDev <- length(get(selectedAuthorsDfName)$Author)
  
  #read all changes
  assign(changesDfName, read.csv(file = paste(mainStoreDir, projectName, "Changes.csv", sep = ""), header = TRUE, sep = ","))
  
  #because of Rails data has a few buggy change data, we split Rails data into 3 folds
  if (projectName == "Rails")
    numberOfFolds <- 3
  else
    numberOfFolds <- 10
  
  for(a in 1:numberOfSelectedDev) {
    authorName <- as.character(get(selectedAuthorsDfName)[a, "Author"])
    
    #read from previously saved files
    if(savePredictionResultsConfMatrix == "N") {
      resultsPM <- readAuthorTestResultsAllIterations(projectName, authorName, "PM")
      resultsSM <- readAuthorTestResultsAllIterations(projectName, authorName, "SM")
      resultsGM <- readAuthorTestResultsAllIterations(projectName, authorName, "GM")
      resultsWSM <- readAuthorTestResultsAllIterations(projectName, authorName, "WSM")
      resultsMeta <- readAuthorTestResultsAllIterations(projectName, authorName, "Meta")
    }
    else {
      #TODO: bu kisim loop disina gitmeli
      resultsGM <- as.data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors=FALSE)
      colnames(resultsGM) <- c("TP", "FP", "FN", "TN", "recall", "pf", "precision", "F1")
      resultsSM <- as.data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors=FALSE)
      colnames(resultsSM) <- c("TP", "FP", "FN", "TN", "recall", "pf", "precision", "F1")
      resultsPM <- as.data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors=FALSE)
      colnames(resultsPM) <- c("TP", "FP", "FN", "TN", "recall", "pf", "precision", "F1")
      resultsWSM <- as.data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors=FALSE)
      colnames(resultsWSM) <- c("TP", "FP", "FN", "TN", "recall", "pf", "precision", "F1")
      resultsMeta <- as.data.frame(matrix(nrow = 0, ncol = 8), stringsAsFactors=FALSE)
      colnames(resultsMeta) <- c("TP", "FP", "FN", "TN", "recall", "pf", "precision", "F1")
      
      for(r in 1:numberOfRuns) {
        GMPredictions <- data.frame()
        SMPredictions <- data.frame()
        PMPredictions <- data.frame()
        WSMPredictions <- data.frame()
        MetaPredictions <- data.frame()
        
        if(algorithmName == "RF"){ #RF prediction results saved cumulatively by mistake (fold10 contains all folds predictions, for ex. fold9 contains 1 to 9)
          f <- numberOfFolds #3 for Rails 10 for others
          PMPredictions <- rbind(PMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
          SMPredictions <- rbind(SMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
          GMPredictions <- rbind(GMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
          WSMPredictions <- rbind(WSMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
        }
        else{
          for(f in 1:numberOfFolds) {
            PMPredictions <- rbind(PMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredPM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            SMPredictions <- rbind(SMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            GMPredictions <- rbind(GMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredGM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
            WSMPredictions <- rbind(WSMPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredWSM", f, ".csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))
          }
        }
        
        #I saved predictions of the Meta model into a single PredMeta file (do not need a loop for folds)
        MetaPredictions <- rbind(MetaPredictions, read.csv(paste(baseDir, projectName, "/", authorName, "/Run", r, "/", authorName, "PredMeta.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE))

        PMPredictions <- editClassLabels(PMPredictions)
        SMPredictions <- editClassLabels(SMPredictions)
        GMPredictions <- editClassLabels(GMPredictions)
        WSMPredictions <- editClassLabels(WSMPredictions)
        MetaPredictions <- editClassLabels(MetaPredictions)
        
        resultsPM <- rbind(resultsPM, calculateConfusionMatrix(PMPredictions))
        resultsSM <- rbind(resultsSM, calculateConfusionMatrix(SMPredictions))
        resultsGM <- rbind(resultsGM, calculateConfusionMatrix(GMPredictions))
        resultsWSM <- rbind(resultsWSM, calculateConfusionMatrix(WSMPredictions))
        resultsMeta <- rbind(resultsMeta, calculateConfusionMatrix(MetaPredictions))
        
        
        if(savePredictionResultsConfMatrix == "Y") {
          #keep results of all models in xlsx files
          write.xlsx(resultsPM, file = paste("./results/", authorName, "ResultsPM.xlsx", sep = ""), row.names = FALSE)
          write.xlsx(resultsSM, file = paste("./results/", authorName, "ResultsSM.xlsx", sep = ""), row.names = FALSE)
          write.xlsx(resultsGM, file = paste("./results/", authorName, "ResultsGM.xlsx", sep = ""), row.names = FALSE)
          write.xlsx(resultsWSM, file = paste("./results/", authorName, "ResultsWSM.xlsx", sep = ""), row.names = FALSE)
          write.xlsx(resultsMeta, file = paste("./results/", authorName, "ResultsMeta.xlsx", sep = ""), row.names = FALSE)
        }
      }
      
      resultsPM <- changeNaToZero(resultsPM)
      resultsSM <- changeNaToZero(resultsSM)
      resultsGM <- changeNaToZero(resultsGM)
      resultsWSM <- changeNaToZero(resultsWSM)
      resultsMeta <- changeNaToZero(resultsMeta)
    }
    
    #creates individual dataframes for each developer to keep PM results
    assign(paste(authorName, sep = ""), resultsPM)
    # write.csv(file = paste("./results/", authorNames[i], ".csv", sep = ""), x = get(authorNames[i]), row.names = FALSE)
    
    #create plots for each author's three results (PM, SM, GM)
    # plotIndividualResults(tmpResultsPM, tmpResultsSM, tmpResultsGM, authorName, projectName)
    # plotIndividualResultsColored(tmpResultsPM, tmpResultsSM, tmpResultsGM, authorName, projectName, 2)
  }
  
  #create all authors combined plots
  # plotCombinedAuthors(projectName)
  # plotCombinedAuthorsColored(projectName, "AllAuthorsColoredOnlyWPM", 2, 1)
  
  #make colored PM results of each author
  # tableMakeColoredIndividualResults(projectName, 5)
  
  #print out PM results for all authors/projects in an excel
  # tableAllPMResults(projectName)
  # tableAllResults(projectName)
  # tableMakeColoredAllPMResults(projectName)
  tableMakeColoredAllResults(projectName, 5)
  
  #feature analysis
  # tableFeatures(projectName)
  
  #print comparison results of PM,SM,GM,WSM,PM+ to excel
  # tableComparisonOfModels(projectName)
}



