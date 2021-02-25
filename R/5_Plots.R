library(ScottKnottESD)
# library(PMCMRplus)
library(PMCMR)
library(effsize)
library(xtable)
library(robustbase)
library(xlsx)
library(stringr)

rm(list = ls())

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
setwd(mainProjectDir)

experimentType <- "MixTime"
devsDataSizeOption.Mix <- "Neq" #Neq, Eq
dataBalancingOption <- "None" #None, Under, SMOTE
developerSelectionVersion <- "SelectedV2"
preparedTrainTestOrInterval <- "P" #P,I
applyLogFilter <- FALSE

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

baseExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix,
                                 ifelse(dataBalancingOption == "None", "", paste("_", dataBalancingOption, sep = "")), "_",
                                 "DevsV2", #hep V2 olmali, zaten V2 V1 i kapsiyor
                                 # ifelse(developerSelectionVersion == "SelectedV2", "DevsV2", "DevsV1"),
                                 sep = "")

read.results.PMSMGM <- function(projectName, authorAlias){
  resultsPM <- data.frame()
  resultsSM <- data.frame()
  resultsGM <- data.frame()
  
  if(projectName != "")
    projectList <- projectName
  if(authorAlias != "")
    selectedAuthorAliases <- authorAlias
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
    
    for(d in 1:length(selectedAuthorAliases)){
      authorAlias <- selectedAuthorAliases[d]
      
      authorResultsDir <- paste(get(paste(alg, "_Dir", sep = "")), projectName, "/", authorAlias, "/Run10/", authorAlias, "_PredGM10", ".csv", sep = "")
      if(!file.exists(authorResultsDir)){
        print(paste("Author data is not available @", authorResultsDir))
        next;
      }
      
      resultsPM <- rbind(resultsPM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsPM.csv", sep = ""), header = TRUE))
      resultsSM <- rbind(resultsSM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsSM.csv", sep = ""), header = TRUE))
      resultsGM <- rbind(resultsGM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsGM.csv", sep = ""), header = TRUE))
    }
  }
  
  return(list(PM = resultsPM, SM = resultsSM, GM = resultsGM))
}

plot.PMvsTraditional <- function(resultsList, projectName, authorAlias){
  resultsPM <- resultsList$PM
  resultsSM <- resultsList$SM
  resultsGM <- resultsList$GM
  
  if(nrow(resultsPM) > 0){
    if(projectName == ""){
      outputFileName <- paste(plotOutputDir, "PmVsTraditional.pdf", sep = "") 
    }
    else if(authorAlias == ""){
      outputFileName <- paste(plotOutputDir, "ForEachProject/", projectName, "_PmVsTraditional.pdf", sep = "")
      dir.create(path = paste(plotOutputDir, "ForEachProject/", sep = ""), recursive = T, showWarnings = F)
    }
    else{
      outputFileName <- paste(plotOutputDir, "ForEachDeveloper/", projectName, "/", authorAlias, "_PmVsTraditional.pdf", sep = "")
      dir.create(path = paste(plotOutputDir, "ForEachDeveloper/", projectName, "/", sep = ""), recursive = T, showWarnings = F)
    }
    
    pdf(file = outputFileName, width = 25, height = 5)
    par(mfrow = c(1, length(metricList)), mar = c(3, 3, 3, 2), oma = c(1, 3, 2, 1))
    
    for(m in metricList){
      if(m == "MCC")
        boxplot(resultsPM[, m], resultsSM[, m], resultsGM[, m], ylim = c(-1.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
      else
        boxplot(resultsPM[, m], resultsSM[, m], resultsGM[, m], ylim = c(0.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
      title(m)
    }
    
    dev.off()
  }
}

write.winLoss.PMvsTraditional <- function(resultsList, projectName, authorAlias){
  resultsPM <- resultsList$PM
  resultsSM <- resultsList$SM
  resultsGM <- resultsList$GM
  
  if(nrow(resultsPM) > 0){
    if(projectName == "")
      outputFileName <- paste(plotOutputDir, "PMvsTraditional.xlsx", sep = "")
    else if(authorAlias == ""){
      outputFileName <- paste(plotOutputDir, "ForEachProject/", projectName, "_PMvsTraditional.xlsx", sep = "")
      dir.create(path = paste(plotOutputDir, "ForEachProject/", sep = ""), recursive = T, showWarnings = F)
    }
    else{
      outputFileName <- paste(plotOutputDir, "ForEachDeveloper/", projectName, "/", authorAlias, "_PMvsTraditional.xlsx", sep = "")
      dir.create(path = paste(plotOutputDir, "ForEachDeveloper/", projectName, "/", sep = ""), recursive = T, showWarnings = F)
    }
    
    comparisonCount <- 2
    effectSizeTable <- winLossTable <- data.frame(matrix(nrow = 1 + length(metricList), ncol = comparisonCount))
    rownames(effectSizeTable) <- rownames(winLossTable) <- c(" ", metricList)
    colnames(effectSizeTable) <- colnames(winLossTable) <- rep(alg, 1, each = 2)
    
    # offSet <- ifelse(alg == algorithms[1], 1, (1 + comparisonCount))
    offSet <- 1 #cunku kod degisti ve bu kisimda sadece bir adet algoritma sonucunu yazdiriyorum, offSet 1 yapmak yeterli
    
    effectSizeTable[1, offSet] <- winLossTable[1, offSet] <- "PM vs SM"
    effectSizeTable[1, offSet + 1] <- winLossTable[1, offSet + 1] <- "PM vs GM"
    
    for(m in metricList){
      if(m == "pf") 
        higherIsBetter <- 0
      else
        higherIsBetter <- 1
      
      #PM vs SM ##########
      colPM <- resultsPM[, m]
      colSM <- resultsSM[, m]
      
      colSM[is.na(colSM)] <- 0
      colPM[is.na(colPM)] <- 0
      
      friedman.test(cbind(colPM, colSM))
      nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colSM))
      
      effectSizeResult <- cohen.d(c(colPM, colSM), rep(c("PM","SM"), each = length(colPM)), hedges.correction=TRUE)
      effectSizeTable[m, offSet] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
      
      if(nemenyiResult$p.value < pVal)
        if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colSM))
          winLossTable[m, offSet] <- if(higherIsBetter) "PM" else "SM"
      else
        winLossTable[m, offSet] <- if(higherIsBetter) "SM" else "PM"
      else
        winLossTable[m, offSet] <- "-"
      #############
      
      #PM vs GM ##########
      colPM <- resultsPM[, m]
      colGM <- resultsGM[, m]
      
      colPM[is.na(colPM)] <- 0
      colGM[is.na(colGM)] <- 0
      
      friedman.test(cbind(colPM, colGM))
      nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colGM))
      
      effectSizeResult <- cohen.d(c(colPM, colGM), rep(c("PM","GM"), each = length(colPM)), hedges.correction=TRUE)
      effectSizeTable[m, offSet + 1] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
      
      if(nemenyiResult$p.value < pVal)
        if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colGM))
          winLossTable[m, offSet + 1] <- if(higherIsBetter) "PM" else "GM"
      else
        winLossTable[m, offSet + 1] <- if(higherIsBetter) "GM" else "PM"
      else
        winLossTable[m, offSet + 1] <- "-"
      #############
    }
    
    xlsx::write.xlsx(winLossTable, file = outputFileName, sheetName = "win-lose", row.names = T, col.names = T, showNA = FALSE)
    xlsx::write.xlsx(effectSizeTable, file = outputFileName, sheetName = "effect size", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
  }
}

whole.pipeline.PMvsTraditional <- function(projectName, authorAlias){
  resultsList <- read.results.PMSMGM(projectName, authorAlias)
  plot.PMvsTraditional(resultsList, projectName, authorAlias)
  write.winLoss.PMvsTraditional(resultsList, projectName, authorAlias)
}


# single plots & results for each experimentation & algorithm ####
# for(alg in algorithms){

alg <- "NB"

  outExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix,
                                  ifelse(dataBalancingOption == "None", "", paste("_", dataBalancingOption, sep = "")), "_",
                                  ifelse(developerSelectionVersion == "SelectedV2", "DevsV2", "DevsV1"), "_",
                                  alg,
                                  sep = "")
  
  loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature)
  
  # for whole experimentation
  whole.pipeline.PMvsTraditional(projectName = "")
  
  # for each project 
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    whole.pipeline.PMvsTraditional(projectName)
  }
  
  # for each developer
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    
    for(d in 1:numberOfSelectedAuthors){
      authorAlias <- selectedAuthorAliases[d]
      whole.pipeline.PMvsTraditional(projectName, authorAlias)
    }
  }
# }
#################################################################


# plot & write all developers' results ####
# numberOfColours <- 7
numberOfColours <- 3
numberOfModels <- 3

loadAllAuthorsInfo(experimentType, developerSelectionVersion)

selectedAuthorAliases <- allAuthorsInfo[, "Alias"]
selectedAuthorNames <- allAuthorsInfo[, "Author"]
selectedAuthorProjects <- allAuthorsInfo[, "projectName"]

for(alg in algorithms){
# alg <- "NB"

  outExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix,
                                  ifelse(dataBalancingOption == "None", "", paste("_", dataBalancingOption, sep = "")), "_",
                                  ifelse(developerSelectionVersion == "SelectedV2", "DevsV2", "DevsV1"), "_",
                                  alg,
                                  sep = "")
  
  loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature)
  
  plotInputDir <- get(paste(alg, "Dir", sep = "_"))
  
  adjustedHeight <- 3 + length(selectedAuthorAliases) * 0.5
  pdf(file = paste(plotOutputDir, "AllAuthors_", alg, "_", numberOfColours, "Colours_", numberOfModels, "Models.pdf", sep = ""), width = 20, height = adjustedHeight)
  par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
  
  for(m in metricList){
    resultsPM <- list()
    authorColors <- list()
    authorInfo <- list()
    
    tmpObjString <- ""
    tmpColString <- ""
    tmpInfoString <- ""
    
    for(a in 1:length(selectedAuthorAliases)){
      authorResultsDir <- paste(get(paste(alg, "_Dir", sep = "")), selectedAuthorProjects[a], "/", selectedAuthorAliases[a], "/Run10/", selectedAuthorAliases[a], "_PredGM10", ".csv", sep = "")
      if(!file.exists(authorResultsDir)){
        print(paste("Author data is not available @", authorResultsDir))
        next;
      }
      
      buggyCommitRatio <- round(allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "BuggyCommitRatio"], 2)
      totalCommitCount <- allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "CommitCount"]
      
      resultsPM[[length(resultsPM) + 1]] <- getAuthorResults(inputDir = plotInputDir,
                                                             projectName = selectedAuthorProjects[a],
                                                             authorAlias = selectedAuthorAliases[a],
                                                             modelName = "PM")[, m]
      
      authorColors[[length(authorColors) + 1]] <- getAuthorColorByMetric(plotInputDir,  
                                                                         projectName = selectedAuthorProjects[a], 
                                                                         authorAlias = selectedAuthorAliases[a],
                                                                         metricName = m, 
                                                                         numberOfColors = numberOfColours, 
                                                                         numberOfModels = numberOfModels)
      
      authorInfo[[length(authorInfo) + 1]] <- paste(selectedAuthorAliases[a], 
                                                    " / ",
                                                    selectedAuthorProjects[a], 
                                                    "\n",
                                                    totalCommitCount,
                                                    " / ",
                                                    buggyCommitRatio,
                                                    sep = "")
      
      
      tmpObjString <- paste(tmpObjString, "resultsPM[[", length(resultsPM), "]], ", sep = "")
      tmpColString <- paste(tmpColString, authorColors[[length(authorColors)]], ",", sep = "")
      tmpInfoString <- paste(tmpInfoString, "authorInfo[[", length(authorInfo), "]], ", sep = "")
    }
    
    tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 2)
    tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 1)
    tmpInfoString <- substr(tmpInfoString, 1, nchar(tmpInfoString) - 2)
    
    if(m == "pd")
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "), names = c(", tmpInfoString, "))", sep =  "")))
    else if(m == "MCC")
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(-1.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
    else
      eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
  }
  
  dev.off()
}
#################################################################
  





# older codes
plot.PMvsTraditional.NBRF <- FALSE
write.winLoss.PMvsTraditional.NBRF <- FALSE
plot.PMvsWPM.NBRF <- FALSE
write.winLoss.PMvsWPM.NBRF <- FALSE
plot.PMvsMeta.NBRF <- FALSE
write.winLoss.PMvsMeta.NBRF <- FALSE

read.5Models.forEachDev <- FALSE
plot.5Models.forEachDev <- FALSE
plot.3Models.forEachDev <- FALSE
plot.5Models.forEachDev.Coloured <- FALSE
write.5Models.forEachDev <- FALSE
plot.5Models.forEachDev.SK <- FALSE
makeColouredExcel.5Models.forEachDev <- FALSE
write.PMBeatTable <- FALSE

plot.Cluster.5Models <- FALSE
plot.Cluster.5Models.SK <- FALSE
makeColouredExcel.Cluster <- FALSE

plot.Overall5Models <- FALSE
write.Overall5Models <- FALSE
compare.NormalVsUnderSamp.SK <- FALSE

write.NofB20 <- FALSE

write.SeqTimevsMixTime <- FALSE
compare.MixVsSeq.SK <- FALSE

plot.AllSelectedDevelopers <- FALSE
plot.SomeSelectedDevelopers <- FALSE

write.allExp.allAlg.forEachProject <- FALSE

plot.PMvsTraditional <- TRUE
write.winLoss.PMvsTraditional <- TRUE
plot.3Models.forEachProject <- TRUE
write.3Models.forEachProject <- TRUE
write.winLoss.PMvsTraditional.forEachProject <- TRUE


if(plot.PMvsTraditional.NBRF | write.winLoss.PMvsTraditional.NBRF){
  algorithms <- c("NB", "RF")
    
  ### reading results ##############
  resultsNB_PM <- data.frame()
  resultsNB_SM <- data.frame()
  resultsNB_GM <- data.frame()
  
  resultsRF_PM <- data.frame()
  resultsRF_SM <- data.frame()
  resultsRF_GM <- data.frame()
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
    
    for(a in 1:length(authorNames)){
      if(experimentType == "MixTime")
        authorName <- authorNames[a]
      else
        authorName <- authorAliases[a]
      
      nbR <- read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE)
      rfR <- read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE)
      
      if(nrow(nbR) != nrow(rfR))
        cat("project: ", p, " author: ", a)
      
      resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      
      resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
    }
  }
  #######################
  
  # plotOutputDir <- paste(plotOutputDir, "PMvsTraditional", sep = "")
  
  if(plot.PMvsTraditional.NBRF){
    pdf(file = paste(plotOutputDir, "PmVsTraditional.pdf", sep = ""), width = 27, height = 9)
    par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 8, 4, 2), oma = c(1, 6, 2, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(alg in c("NB", "RF")){
      for(m in metricList){
        resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]   
        resultSM <- get(paste("results", alg, "_SM", sep = ""))[, m]   
        resultGM <- get(paste("results", alg, "_GM", sep = ""))[, m]  
        
        if(m == "MCC")
          boxplot(resultPM, resultSM, resultGM, ylim = c(-1.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
        else
          boxplot(resultPM, resultSM, resultGM, ylim = c(0.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
        if(m == "pd")
          title(ylab = ifelse(paste(alg) == "NB", "Naive Bayes", "Random Forest") , cex.lab = 2, line = 5) #, font.lab = 2)
        if(alg == "NB")
          title(m, cex.main = 2, line = 2, font.main = 1)
      }
    }
    dev.off()
  }
  
  if(write.winLoss.PMvsTraditional.NBRF == 1){
    comparisonCount <- 2
    effectSizeTable <- winLossTable <- data.frame(matrix(nrow = 1 + length(metricList), ncol = comparisonCount * length(algorithms)))
    rownames(effectSizeTable) <- rownames(winLossTable) <- c(" ", metricList)
    colnames(effectSizeTable) <- colnames(winLossTable) <- rep(algorithms, 1, each = 2)
    
    effectSizeTable[1, c(1,3)] <- winLossTable[1, c(1,3)] <- "PM vs SM"
    effectSizeTable[1, c(2,4)] <- winLossTable[1, c(2,4)] <- "PM vs GM"
    
    for(alg in algorithms){
      offSet <- ifelse(alg == "NB", 1, (1 + comparisonCount))
      
      for(m in metricList){
        if(m == "pf") 
          higherIsBetter <- 0
        else
          higherIsBetter <- 1
        
        #PM vs SM ##########
        colPM <- get(paste("results", alg, "_PM", sep = ""))[, m]
        colSM <- get(paste("results", alg, "_SM", sep = ""))[, m]
        
        colSM[is.na(colSM)] <- 0
        colPM[is.na(colPM)] <- 0
        
        friedman.test(cbind(colPM, colSM))
        nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colSM))
        
        effectSizeResult <- cohen.d(c(colPM, colSM), rep(c("PM","SM"), each = length(colPM)), hedges.correction=TRUE)
        effectSizeTable[m, offSet] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
        
        if(nemenyiResult$p.value < pVal)
          if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colSM))
            winLossTable[m, offSet] <- if(higherIsBetter) "PM" else "SM"
        else
          winLossTable[m, offSet] <- if(higherIsBetter) "SM" else "PM"
        else
          winLossTable[m, offSet] <- "-"
        #############
        
        #PM vs GM ##########
        colPM <- get(paste("results", alg, "_PM", sep = ""))[, m]
        colGM <- get(paste("results", alg, "_GM", sep = ""))[, m]
        
        colPM[is.na(colPM)] <- 0
        colGM[is.na(colGM)] <- 0
        
        friedman.test(cbind(colPM, colGM))
        nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colGM))
        
        effectSizeResult <- cohen.d(c(colPM, colGM), rep(c("PM","GM"), each = length(colPM)), hedges.correction=TRUE)
        effectSizeTable[m, offSet + 1] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
        
        if(nemenyiResult$p.value < pVal)
          if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colGM))
            winLossTable[m, offSet + 1] <- if(higherIsBetter) "PM" else "GM"
        else
          winLossTable[m, offSet + 1] <- if(higherIsBetter) "GM" else "PM"
        else
          winLossTable[m, offSet + 1] <- "-"
        #############
      }
    }
    
    xlsx::write.xlsx(winLossTable, file = paste(plotOutputDir, "PMvsTraditional.xlsx", sep = ""), sheetName = "win-lose", row.names = T, col.names = T, showNA = FALSE)
    xlsx::write.xlsx(effectSizeTable, file = paste(plotOutputDir, "PMvsTraditional.xlsx", sep = ""), sheetName = "effect size", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
  }
}

if(plot.PMvsWPM.NBRF | write.winLoss.PMvsWPM.NBRF){
  algorithms <- c("NB", "RF")

  ### reading results ##############
  resultsNB_PM <- data.frame()
  resultsNB_WPM <- data.frame()

  resultsRF_PM <- data.frame()
  resultsRF_WPM <- data.frame()

  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
  
    for(a in 1:length(authorNames)){
      if(experimentType == "MixTime")
        authorName <- authorNames[a]
      else
        authorName <- authorAliases[a]

      resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = T))
      resultsNB_WPM <- rbind(resultsNB_WPM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))

      resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_WPM <- rbind(resultsRF_WPM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
    }
  }
  #######################

  if(plot.PMvsWPM.NBRF){
    pdf(file = paste(plotOutputDir, "PmVsWPM.pdf", sep = ""), width = 20, height = 8)
    par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(alg in c("NB", "RF")){
      for(m in metricList){
        resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]
        resultWPM <- get(paste("results", alg, "_WPM", sep = ""))[, m]
        
        if(m == "MCC")
          boxplot(resultPM, resultWPM, ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, las = 0, names = c("PM", "WPM"))
        else
          boxplot(resultPM, resultWPM, ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, las = 0, names = c("PM", "WPM"))
        title(ylab = paste(alg, m), cex.lab = 1.7)
      }
    }
    dev.off()
  }

  if(write.winLoss.PMvsWPM.NBRF == 1){
    comparisonCount <- 1

    effectSizeTable <- winLossTable <- data.frame(matrix(nrow = 1 + length(metricList), ncol = comparisonCount * length(algorithms)))
    rownames(effectSizeTable) <- rownames(winLossTable) <- c(" ", metricList)
    colnames(effectSizeTable) <- colnames(winLossTable) <- rep(algorithms, 1)

    effectSizeTable[1, c(1,2)] <- winLossTable[1, c(1,2)] <- "PM vs WPM"
    
    for(alg in algorithms){
      offSet <- ifelse(alg == "NB", 1, (1 + comparisonCount))

      for(m in metricList){
        if(m == "pf")
          higherIsBetter <- 0
        else
          higherIsBetter <- 1

        #PM vs WPM ##########
        colPM <- get(paste("results", alg, "_PM", sep = ""))[, m]
        colWPM <- get(paste("results", alg, "_WPM", sep = ""))[, m]

        colWPM[is.na(colWPM)] <- 0
        colPM[is.na(colPM)] <- 0
        
        friedman.test(cbind(colPM, colWPM))
        nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colWPM))

        effectSizeResult <- cohen.d(c(colPM, colWPM), rep(c("PM","WPM"), each = length(colPM)), hedges.correction=TRUE)
        effectSizeTable[m, offSet] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
        
        if(nemenyiResult$p.value < pVal)
          if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colWPM))
            winLossTable[m, offSet] <- if(higherIsBetter) "PM" else "WPM"
        else
          winLossTable[m, offSet] <- if(higherIsBetter) "WPM" else "PM"
        else
          winLossTable[m, offSet] <- "-"
        #############
      }
    }
    xlsx::write.xlsx(winLossTable, file = paste(plotOutputDir, "PMvsWPM.xlsx", sep = ""), sheetName = "win-lose", row.names = T, col.names = T, showNA = FALSE)
    xlsx::write.xlsx(effectSizeTable, file = paste(plotOutputDir, "PMvsWPM.xlsx", sep = ""), sheetName = "effect size", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
  }
}

if(plot.PMvsMeta.NBRF | write.winLoss.PMvsMeta.NBRF){
  algorithms <- c("NB", "RF")

  ### reading results ##############
  resultsNB_PM <- data.frame()
  resultsNB_Meta <- data.frame()

  resultsRF_PM <- data.frame()
  resultsRF_Meta <- data.frame()

  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)

    for(a in 1:length(authorNames)){
      if(experimentType == "MixTime")
        authorName <- authorNames[a]
      else
        authorName <- authorAliases[a]
      
      resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta <- rbind(resultsNB_Meta, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))

      resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta <- rbind(resultsRF_Meta, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
    }
  }
  #######################

  if(plot.PMvsMeta.NBRF){
    pdf(file = paste(plotOutputDir, "PmVsMeta.pdf", sep = ""), width = 17, height = 8)
    par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(alg in c("NB", "RF")){
      for(m in metricList){
        resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]
        resultMeta <- get(paste("results", alg, "_Meta", sep = ""))[, m]

        if(m == "MCC")
          boxplot(resultPM, resultMeta, ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "PM+"))
        else
          boxplot(resultPM, resultMeta, ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "PM+"))
        title(ylab = paste(alg, m), cex.lab = 1.7)
      }
    }
    dev.off()
  }

  if(write.winLoss.PMvsMeta.NBRF == 1){
    comparisonCount <- 1
   
    effectSizeTable <- winLossTable <- data.frame(matrix(nrow = 1 + length(metricList), ncol = comparisonCount * length(algorithms)))
    rownames(effectSizeTable) <- rownames(winLossTable) <- c(" ", metricList)
    colnames(effectSizeTable) <- colnames(winLossTable) <- rep(algorithms, 1)

    effectSizeTable[1, c(1,2)] <- winLossTable[1, c(1,2)] <- "PM vs PM+"

    for(alg in algorithms){
      offSet <- ifelse(alg == "NB", 1, (1 + comparisonCount))

      for(m in metricList){
        if(m == "pf")
          higherIsBetter <- 0
        else
          higherIsBetter <- 1

        #PM vs Meta ##########
        colPM <- get(paste("results", alg, "_PM", sep = ""))[, m]
        colMeta <- get(paste("results", alg, "_Meta", sep = ""))[, m]

        colMeta[is.na(colMeta)] <- 0
        colPM[is.na(colPM)] <- 0
        
        friedman.test(cbind(colPM, colMeta))
        nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colMeta))

        effectSizeResult <- cohen.d(c(colPM, colMeta), rep(c("PM","PM+"), each = length(colPM)), hedges.correction=TRUE)
        effectSizeTable[m, offSet] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
        
        if(nemenyiResult$p.value < pVal)
          if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colMeta))
            winLossTable[m, offSet] <- if(higherIsBetter) "PM" else "PM+"
        else
          winLossTable[m, offSet] <- if(higherIsBetter) "PM+" else "PM"
        else
          winLossTable[m, offSet] <- "-"
        #############
      }
    }
    xlsx::write.xlsx(winLossTable, file = paste(plotOutputDir, "PMvsMeta.xlsx", sep = ""), sheetName = "win-loss", row.names = T, col.names = T, showNA = FALSE)
    xlsx::write.xlsx(effectSizeTable, file = paste(plotOutputDir, "PMvsMeta.xlsx", sep = ""), sheetName = "effect size", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
  }

}

if(read.5Models.forEachDev){
  algorithms <- c("NB", "RF")
  
  allProjectsResults_NB <- allProjectsResults_RF <- 
    as.data.frame(matrix(ncol = length(metricList) * 4 + 6, nrow = 0))
  colnames(allProjectsResults_NB) <- colnames(allProjectsResults_RF) <-
    c("Author", "Alias", "Model", "BuggyCommit", "TotalCommit", "BuggyRatio", paste(rep(c("min", "mean", "median", "max"), 7), rep(metricList, 1, each = 4), sep = "-"))
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
    
    dir.create(paste(plotOutputDir, projectName, sep = ""), showWarnings = F)
    
    if(write.5Models.forEachDev){
      allResultsNB <- as.data.frame(matrix(ncol = length(metricList) * 4 + 6, nrow = length(authorNames) * 5))
      colnames(allResultsNB) <- c("Author", "Alias", "Model", "BuggyCommit", "TotalCommit", "BuggyRatio",
                                  paste(rep(c("min", "mean", "median", "max"), 7), rep(metricList, 1, each = 4), sep = "-"))
      allResultsRF <- as.data.frame(matrix(ncol = length(metricList) * 4 + 6, nrow = length(authorNames) * 5))
      colnames(allResultsRF) <- c("Author", "Alias", "Model", "BuggyCommit", "TotalCommit", "BuggyRatio",
                                  paste(rep(c("min", "mean", "median", "max"), 7), rep(metricList, 1, each = 4), sep = "-"))
    }
    
    for(a in 1:length(authorNames)){
      if(experimentType == "MixTime")
        authorName <- authorNames[a]
      else
        authorName <- authorAliases[a]
      
      resultsNB_PM <- read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE)
      resultsNB_SM <- read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE)
      resultsNB_GM <- read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE)
      resultsNB_WPM <- read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE)
      resultsNB_Meta <- read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE)
      
      resultsRF_PM <- read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE)
      resultsRF_SM <- read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE)
      resultsRF_GM <- read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE)
      resultsRF_WPM <- read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE)
      resultsRF_Meta <- read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE)
      
      
      selectedAuthors <- get(paste(projectName, "SelectedAuthors", sep = ""))
      authorCommitCount <- selectedAuthors[selectedAuthors$Author == authorName, ]$CommitCount
      authorBuggyCommitCount <- selectedAuthors[selectedAuthors$Author == authorName, ]$BuggyCommitCount
      authorAlias <- selectedAuthors[selectedAuthors$Author == authorName, ]$Alias
      
      if(plot.5Models.forEachDev){
        pdf(file = paste(plotOutputDir, projectName, "/", authorName, ".pdf", sep = ""), width = 20, height = 8)
        par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 4, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
        for(alg in algorithms){
          for(m in metricList){
            resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]   
            resultSM <- get(paste("results", alg, "_SM", sep = ""))[, m]   
            resultGM <- get(paste("results", alg, "_GM", sep = ""))[, m]  
            resultWPM <- get(paste("results", alg, "_WPM", sep = ""))[, m]  
            resultMeta <- get(paste("results", alg, "_Meta", sep = ""))[, m]  
            
            if(m == "MCC")
              boxplot(resultPM, resultSM, resultGM, resultWPM, resultMeta, las=2,
                      ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
            else
              boxplot(resultPM, resultSM, resultGM, resultWPM, resultMeta, las=2,
                      ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
            title(ylab = paste(alg, m), cex.lab = 1.7)
          }
        }
        mtext(paste(authorAlias, " from ", projectName, 
                    "\n Fix-inducing commit ratio: ", gsub("\\.", ",", round(authorBuggyCommitCount / authorCommitCount, 3)), 
                    " \n Total commits: ", authorCommitCount, sep = ""), outer=TRUE,  cex=1, line=-0.5)
        dev.off()
      }
      
      if(plot.3Models.forEachDev){
        pdf(file = paste(plotOutputDir, projectName, "/", authorName, "_3Models.pdf", sep = ""), width = 15, height = 6)
        par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 4, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
        for(alg in algorithms){
          for(m in metricList){
            resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]   
            resultSM <- get(paste("results", alg, "_SM", sep = ""))[, m]   
            resultGM <- get(paste("results", alg, "_GM", sep = ""))[, m]  
            
            if(m == "MCC")
              boxplot(resultPM, resultSM, resultGM, las=2,
                      ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM"))
            else
              boxplot(resultPM, resultSM, resultGM, las=2,
                      ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM"))
            title(ylab = paste(alg, m), cex.lab = 1.7)
          }
        }
        mtext(paste(authorAlias, " from ", projectName, 
                    "\n Fix-inducing commit ratio: ", gsub("\\.", ",", round(authorBuggyCommitCount / authorCommitCount, 3)), 
                    " \n Total commits: ", authorCommitCount, sep = ""), outer=TRUE,  cex=1, line=-0.5)
        dev.off()
      }
      
      if(plot.5Models.forEachDev.Coloured){
        pdf(file = paste(plotOutputDir, projectName, "/", authorName, "Coloured.pdf", sep = ""), width = 20, height = 8)
        par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 4, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
        for(alg in algorithms){
          for(m in metricList){
            resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]   
            resultSM <- get(paste("results", alg, "_SM", sep = ""))[, m]   
            resultGM <- get(paste("results", alg, "_GM", sep = ""))[, m]  
            resultWPM <- get(paste("results", alg, "_WPM", sep = ""))[, m]  
            resultMeta <- get(paste("results", alg, "_Meta", sep = ""))[, m]  
            
            tmpColor <- getAuthorColorByMetric(plotInputDir =  get(paste(alg, "_Dir", sep = "")),
                                               projectName = projectName,
                                               authorName = authorName,
                                               metricName =  m,
                                               numberOfColors = 5, 
                                               numberOfModels = 5)
            tmpColorString <- paste(tmpColor, ", 'white', 'white', 'white', 'white'", sep = "")

            if(m == "MCC")
              eval(parse(text = paste("boxplot(resultPM, resultSM, resultGM, resultWPM, resultMeta, 
                                      las = 2, ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5,
                                      names = c('PM', 'SM', 'GM', 'WPM', 'PM+'), col = c(", tmpColorString, "))", sep =  "")))
            else
              eval(parse(text = paste("boxplot(resultPM, resultSM, resultGM, resultWPM, resultMeta, 
                                      las = 2, ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5,
                                      names = c('PM', 'SM', 'GM', 'WPM', 'PM+'), col = c(", tmpColorString, "))", sep =  "")))
            title(ylab = paste(alg, m), cex.lab = 1.7)
          }
        }
        mtext(paste(authorAlias, " from ", projectName, 
                    "\n Fix-inducing commit ratio: ", gsub("\\.", ",", round(authorBuggyCommitCount / authorCommitCount, 3)), 
                    " \n Total commits: ", authorCommitCount, sep = ""), outer=TRUE,  cex=1, line=-0.5)
        dev.off()
      }
      
      if(write.5Models.forEachDev){
        allResultsNB[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "Author"] <- authorName
        allResultsNB[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "Alias"] <- as.character(authorAlias)
        allResultsNB[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "Model"] <- c("PM", "SM", "GM", "WPM", "PM+")
        allResultsNB[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "BuggyCommit"] <- authorBuggyCommitCount
        allResultsNB[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "TotalCommit"] <- authorCommitCount
        allResultsNB[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "BuggyRatio"] <- authorBuggyCommitCount / authorCommitCount
        
        allResultsRF[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "Author"] <- authorName
        allResultsRF[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "Alias"] <- as.character(authorAlias)
        allResultsRF[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "Model"] <- c("PM", "SM", "GM", "WPM", "PM+")
        allResultsRF[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "BuggyCommit"] <- authorBuggyCommitCount
        allResultsRF[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "TotalCommit"] <- authorCommitCount
        allResultsRF[((a - 1) * 5 + 1):((a - 1) * 5 + 5), "BuggyRatio"] <- authorBuggyCommitCount / authorCommitCount
        
        for(m in 1:length(metricList)){
          allResultsNB[(a - 1) * 5 + 1, (m - 1) * 4 + 7] <- round(min(resultsNB_PM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 1, (m - 1) * 4 + 8] <- round(mean(resultsNB_PM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 1, (m - 1) * 4 + 9] <- round(median(resultsNB_PM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 1, (m - 1) * 4 + 10] <- round(max(resultsNB_PM[, m + 4], na.rm = T), 3)
          
          allResultsNB[(a - 1) * 5 + 2, (m - 1) * 4 + 7] <- round(min(resultsNB_SM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 2, (m - 1) * 4 + 8] <- round(mean(resultsNB_SM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 2, (m - 1) * 4 + 9] <- round(median(resultsNB_SM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 2, (m - 1) * 4 + 10] <- round(max(resultsNB_SM[, m + 4], na.rm = T), 3)
          
          allResultsNB[(a - 1) * 5 + 3, (m - 1) * 4 + 7] <- round(min(resultsNB_GM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 3, (m - 1) * 4 + 8] <- round(mean(resultsNB_GM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 3, (m - 1) * 4 + 9] <- round(median(resultsNB_GM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 3, (m - 1) * 4 + 10] <- round(max(resultsNB_GM[, m + 4], na.rm = T), 3)
          
          allResultsNB[(a - 1) * 5 + 4, (m - 1) * 4 + 7] <- round(min(resultsNB_WPM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 4, (m - 1) * 4 + 8] <- round(mean(resultsNB_WPM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 4, (m - 1) * 4 + 9] <- round(median(resultsNB_WPM[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 4, (m - 1) * 4 + 10] <- round(max(resultsNB_WPM[, m + 4], na.rm = T), 3)
          
          allResultsNB[(a - 1) * 5 + 5, (m - 1) * 4 + 7] <- round(min(resultsNB_Meta[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 5, (m - 1) * 4 + 8] <- round(mean(resultsNB_Meta[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 5, (m - 1) * 4 + 9] <- round(median(resultsNB_Meta[, m + 4], na.rm = T), 3)
          allResultsNB[(a - 1) * 5 + 5, (m - 1) * 4 + 10] <- round(max(resultsNB_Meta[, m + 4], na.rm = T), 3)
          

          allResultsRF[(a - 1) * 5 + 1, (m - 1) * 4 + 7] <- round(min(resultsRF_PM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 1, (m - 1) * 4 + 8] <- round(mean(resultsRF_PM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 1, (m - 1) * 4 + 9] <- round(median(resultsRF_PM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 1, (m - 1) * 4 + 10] <- round(max(resultsRF_PM[, m + 4], na.rm = T), 3)
          
          allResultsRF[(a - 1) * 5 + 2, (m - 1) * 4 + 7] <- round(min(resultsRF_SM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 2, (m - 1) * 4 + 8] <- round(mean(resultsRF_SM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 2, (m - 1) * 4 + 9] <- round(median(resultsRF_SM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 2, (m - 1) * 4 + 10] <- round(max(resultsRF_SM[, m + 4], na.rm = T), 3)
          
          allResultsRF[(a - 1) * 5 + 3, (m - 1) * 4 + 7] <- round(min(resultsRF_GM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 3, (m - 1) * 4 + 8] <- round(mean(resultsRF_GM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 3, (m - 1) * 4 + 9] <- round(median(resultsRF_GM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 3, (m - 1) * 4 + 10] <- round(max(resultsRF_GM[, m + 4], na.rm = T), 3)
          
          allResultsRF[(a - 1) * 5 + 4, (m - 1) * 4 + 7] <- round(min(resultsRF_WPM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 4, (m - 1) * 4 + 8] <- round(mean(resultsRF_WPM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 4, (m - 1) * 4 + 9] <- round(median(resultsRF_WPM[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 4, (m - 1) * 4 + 10] <- round(max(resultsRF_WPM[, m + 4], na.rm = T), 3)
          
          allResultsRF[(a - 1) * 5 + 5, (m - 1) * 4 + 7] <- round(min(resultsRF_Meta[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 5, (m - 1) * 4 + 8] <- round(mean(resultsRF_Meta[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 5, (m - 1) * 4 + 9] <- round(median(resultsRF_Meta[, m + 4], na.rm = T), 3)
          allResultsRF[(a - 1) * 5 + 5, (m - 1) * 4 + 10] <- round(max(resultsRF_Meta[, m + 4], na.rm = T), 3)
        }
      }
      
      if(plot.5Models.forEachDev.SK){
        pdf(file = paste(plotOutputDir, projectName, "/", authorName, "_SK.pdf", sep = ""), width = 12, height = 15)
        par(mfrow = c(2, 3), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
        for(m in metricList){
          modelCompMatrix <- as.matrix(cbind(resultsNB_PM[, m],
                                             resultsNB_SM[, m],
                                             resultsNB_GM[, m],
                                             resultsNB_WPM[, m],
                                             resultsNB_Meta[, m],
                                             resultsRF_PM[, m],
                                             resultsRF_SM[, m],
                                             resultsRF_GM[, m],
                                             resultsRF_WPM[, m],
                                             resultsRF_Meta[, m]))
          colnames(modelCompMatrix) <- paste(rep(algorithms, 1, each = 5), c("PM", "SM", "GM", "WPM", "PM+"), sep = "-")
          modelCompResults <- sk_esd(modelCompMatrix)
          rownames(modelCompResults$m.inf) <- gsub(pattern = ".", replacement = "+", x = rownames(modelCompResults$m.inf), fixed = TRUE)
          plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")
        }
        dev.off()
      }
    }
    
    if(write.5Models.forEachDev){ 
      allProjectsResults_NB <- rbind(allProjectsResults_NB, allResultsNB)
      allProjectsResults_RF <- rbind(allProjectsResults_RF, allResultsRF)
    }
    
    if(write.5Models.forEachDev){
      if(p == 1){
        xlsx::write.xlsx(x = allResultsNB, file = paste(plotOutputDir, "Results_NB_", samplingOption, ".xlsx", sep = ""), sheetName = projectName, row.names = FALSE)
        xlsx::write.xlsx(x = allResultsRF, file = paste(plotOutputDir, "Results_RF_", samplingOption, ".xlsx", sep = ""), sheetName = projectName, row.names = FALSE)
      }else{
        xlsx::write.xlsx(x = allResultsNB, file = paste(plotOutputDir, "Results_NB_", samplingOption, ".xlsx", sep = ""), sheetName = projectName, row.names = FALSE, append = T)
        xlsx::write.xlsx(x = allResultsRF, file = paste(plotOutputDir, "Results_RF_", samplingOption, ".xlsx", sep = ""), sheetName = projectName, row.names = FALSE, append = T)
      }
    }
  }
  if(write.5Models.forEachDev){
    xlsx::write.xlsx(x = allProjectsResults_NB, file = paste(plotOutputDir, "Results_NB_", samplingOption, ".xlsx", sep = ""), sheetName = "ALL", row.names = FALSE, append = T)
    xlsx::write.xlsx(x = allProjectsResults_RF, file = paste(plotOutputDir, "Results_RF_", samplingOption, ".xlsx", sep = ""), sheetName = "ALL", row.names = FALSE, append = T)
    # openxlsx::write.xlsx(x = allProjectsResults_NB, file = paste(plotOutputDir, "Results_NB_WO_ALL.xlsx", sep = ""), sheetName = "ALL", row.names = FALSE, append = T)
    # openxlsx::write.xlsx(x = allProjectsResults_RF, file = paste(plotOutputDir, "Results_RF_WO_ALL.xlsx", sep = ""), sheetName = "ALL", row.names = FALSE, append = T)
  }
}

if(makeColouredExcel.5Models.forEachDev){
  algorithms <- c("NB", "RF")
  numberOfModelsCompared <- c(3) #c(3, 5)
  numberOfColours <- 5
  
  for(alg in algorithms){
    for(n in numberOfModelsCompared){
      for(p in 1:length(projectList)){
        projectName <- projectList[p]
        loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
        
        resultsFile <- paste(plotOutputDir, "Results_", alg, "_", samplingOption, ".xlsx", sep = "")
        colouredResultsFile <- paste(plotOutputDir, "Results_", numberOfColours, "Coloured_", n , "ModelComp_", alg, "_", samplingOption, ".xlsx", sep = "")
        file.copy(resultsFile, colouredResultsFile)
        
        resultsTable <- xlsx::read.xlsx(colouredResultsFile, sheetIndex = projectName, header = TRUE)
        wb <- loadWorkbook(colouredResultsFile)
        sheets <- getSheets(wb)
        sheet <- sheets[[projectName]]  
        rows <- getRows(sheet, rowIndex = 2:(nrow(resultsTable) + 1))
        cells <- getCells(rows, colIndex = 7:ncol(resultsTable))
        values <- lapply(cells, getCellValue) # extract the cell values
        
        for(a in 1:length(authorNames)){
          if(experimentType == "MixTime")
            authorName <- authorNames[a]
          else
            authorName <- authorAliases[a]
          
          for(m in 1:length(metricList)) {
            tmpColor <- getAuthorColorByMetric(plotInputDir = get(paste(alg, "Dir", sep = "_")), 
                                               projectName, 
                                               authorName, 
                                               metricName = metricList[m], 
                                               numberOfColors = 5, 
                                               numberOfModels = n)
            tmpColor <- substr(tmpColor, 2, nchar(tmpColor) - 1)
            
            fg <- Fill(foregroundColor = tmpColor)
            cs <- CellStyle(wb, fill = fg)
            
            # cellIndex <- (a - 1) * 48 + (m * 4) - 2 #3models
            # cellIndex <- (a - 1) * 80 + (m * 4) - 2 #5models
            cellIndex <- (a - 1) * length(metricList) * 4 * 5 + (m * 4) - 1 #5: model count, 4: min-max-mean-median 
            setCellStyle(cells[[cellIndex]], cs)
            saveWorkbook(wb, colouredResultsFile)
          }
        }
      }
    }
  }
}

if(makeColouredExcel.Cluster){
  algorithms <- c("NB", "RF")
  numberOfModelsCompared <- c(3, 5)
  numberOfColours <- 5
  
  for(alg in algorithms){
    for(n in numberOfModelsCompared){
      allAuthorNames <- character(0)
      allAuthorAliases <- character(0)
      allAuthorsProjectNames <- character(0)
      for(p in 1:length(projectList)){
        projectName <- projectList[p]
        loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
        
        allAuthorNames <- c(allAuthorNames, authorNames)
        allAuthorAliases <- c(allAuthorAliases, authorAliases)
        allAuthorsProjectNames <- c(allAuthorsProjectNames, rep(projectName, length(authorNames)))
      }
      
      resultsFile <- paste(plotOutputDir, "DevClusters/Results_5Clusters_", alg, "_", samplingOption, ".xlsx", sep = "")
      colouredResultsFile <- paste(plotOutputDir, "DevClusters/Results_5Clusters_", numberOfColours, "Coloured_", n , "ModelComp_", alg, "_", samplingOption, ".xlsx", sep = "")
      file.copy(resultsFile, colouredResultsFile)
       
      resultsTable <- xlsx::read.xlsx(colouredResultsFile, sheetIndex = 1, header = TRUE)
      wb <- loadWorkbook(colouredResultsFile)
      sheets <- getSheets(wb)
      sheet <- sheets[[1]]  
      rows <- getRows(sheet, rowIndex = 2:(nrow(resultsTable) + 1))
      cells <- getCells(rows, colIndex = 8:(ncol(resultsTable) - 1))
      values <- lapply(cells, getCellValue) # extract the cell values
        
      for(a in 1:length(allAuthorNames)){
        if(experimentType == "MixTime")
          authorName <- allAuthorNames[a]
        else
          authorName <- allAuthorAliases[a]
        
        for(m in 1:length(metricList)) {
          tmpColor <- getAuthorColorByMetric(plotInputDir = get(paste(alg, "Dir", sep = "_")), 
                                             projectName = allAuthorsProjectNames[a], 
                                             authorName = authorName, 
                                             metricName = metricList[m], 
                                             numberOfColors = numberOfColours, 
                                             numberOfModels = n)
          tmpColor <- substr(tmpColor, 2, nchar(tmpColor) - 1)
          
          fg <- Fill(foregroundColor = tmpColor)
          cs <- CellStyle(wb, fill = fg)
          
          # cellIndex <- (a - 1) * 48 + (m * 4) - 2 #3models
          # cellIndex <- (a - 1) * 80 + (m * 4) - 2 #5models
          cellIndex <- (a - 1) * length(metricList) * 4 * 5 + (m * 4) - 1 #5: model count, 4: min-max-mean-median 
          setCellStyle(cells[[cellIndex]], cs)
          saveWorkbook(wb, colouredResultsFile)
        }
      }
    }
  }
}

if(plot.Cluster.5Models){
  algorithms <- c("NB", "RF")
  clusters <- c(5,3,2,1,4)
  modelTypes <- c("PM", "SM", "GM", "WPM", "PM+")
  
  for(alg in algorithms){
    allResults <- read.xlsx(file = paste(plotOutputDir, "DevClusters/Results_5Clusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetIndex = 1)

    pdf(file = paste(plotOutputDir, "DevClusters/ClusterPerformances_", alg, ".pdf", sep = ""), width = 22, height = 20)
    par(mfrow = c(length(clusters), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    for(cls in clusters){
      projectListOfCluster <- allResults[which(allResults$Cluster == cls & allResults$Model == "PM"), "projectName"]
      developerListOfCluster <- allResults[which(allResults$Cluster == cls & allResults$Model == "PM"), "Author"]
      
      resultPM <- data.frame()
      resultSM <- data.frame()
      resultGM <- data.frame()
      resultWPM <- data.frame()
      resultMeta <- data.frame()
      
      for(d in 1:length(developerListOfCluster)){
        resultPM <- rbind(resultPM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                     projectName = projectListOfCluster[d],
                                                     authorName = developerListOfCluster[d],
                                                     modelName = "PM"))
        resultSM <- rbind(resultSM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                     projectName = projectListOfCluster[d],
                                                     authorName = developerListOfCluster[d],
                                                     modelName = "SM"))
        
        resultGM <- rbind(resultGM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                     projectName = projectListOfCluster[d],
                                                     authorName = developerListOfCluster[d],
                                                     modelName = "GM"))
        
        resultWPM <- rbind(resultWPM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                      projectName = projectListOfCluster[d],
                                                      authorName = developerListOfCluster[d],
                                                      modelName = "WPM"))
        
        resultMeta <- rbind(resultMeta, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                      projectName = projectListOfCluster[d],
                                                      authorName = developerListOfCluster[d],
                                                      modelName = "Meta"))
      }
      
      for(m in metricList){
        if(m == "MCC")
          boxplot(resultPM[, m], resultSM[, m], resultGM[, m], resultWPM[, m], resultMeta[, m], 
                  ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
        else
          boxplot(resultPM[, m], resultSM[, m], resultGM[, m], resultWPM[, m], resultMeta[, m], 
                  ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
        title(ylab = paste("Cluster-", cls, " (", length(developerListOfCluster), " author) ", m, sep = ""), cex.lab = 1.7)
      }
    }
    dev.off()
  
  }

}

#does not completed
if(plot.Cluster.5Models.SK){
  algorithms <- c("NB", "RF")
  clusters <- c(5,3,2,1,4)
  modelTypes <- c("PM", "SM", "GM", "WPM", "PM+")
  
  for(alg in algorithms){
    allResults <- read.xlsx(file = paste(plotOutputDir, "DevClusters/Results_5Clusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetIndex = 1)
    
    pdf(file = paste(plotOutputDir, "DevClusters/Cluster_SK_", alg, ".pdf", sep = ""), width = 22, height = 20)
    par(mfrow = c(3, 3), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
    
    resultPM_List <- list()
    resultSM_List <- list()
    resultGM_List <- list()
    resultWPM_List <- list()
    resultMeta_List <- list()
    
    for(cls in clusters){
      projectListOfCluster <- allResults[which(allResults$Cluster == cls & allResults$Model == "PM"), "projectName"]
      developerListOfCluster <- allResults[which(allResults$Cluster == cls & allResults$Model == "PM"), "Author"]
      
      resultPM <- data.frame()
      resultSM <- data.frame()
      resultGM <- data.frame()
      resultWPM <- data.frame()
      resultMeta <- data.frame()
      
      for(d in 1:length(developerListOfCluster)){
        resultPM <- rbind(resultPM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                     projectName = projectListOfCluster[d],
                                                     authorName = developerListOfCluster[d],
                                                     modelName = "PM"))
        resultSM <- rbind(resultSM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                     projectName = projectListOfCluster[d],
                                                     authorName = developerListOfCluster[d],
                                                     modelName = "SM"))
        
        resultGM <- rbind(resultGM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                     projectName = projectListOfCluster[d],
                                                     authorName = developerListOfCluster[d],
                                                     modelName = "GM"))
        
        resultWPM <- rbind(resultWPM, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                       projectName = projectListOfCluster[d],
                                                       authorName = developerListOfCluster[d],
                                                       modelName = "WPM"))
        
        resultMeta <- rbind(resultMeta, getAuthorResults(inputDir = get(paste(alg, "_Dir", sep = "")),
                                                         projectName = projectListOfCluster[d],
                                                         authorName = developerListOfCluster[d],
                                                         modelName = "Meta"))
      }
      
      resultPM_List[[length(resultPM_List) + 1]] <- resultPM
      resultSM_List[[length(resultSM_List) + 1]] <- resultSM
      resultGM_List[[length(resultGM_List) + 1]] <- resultGM
      resultWPM_List[[length(resultWPM_List) + 1]] <- resultWPM
      resultMeta_List[[length(resultMeta_List) + 1]] <- resultMeta
    } 
    
    for(m in metricList){
      modelCompMatrix <- as.matrix(cbind(resultPM_List[[1]][, m],
                                         resultSM_List[[1]][, m],
                                         resultGM_List[[1]][, m],
                                         resultWPM_List[[1]][, m],
                                         resultMeta_List[[1]][, m],
                                         resultPM_List[[2]][, m],
                                         resultSM_List[[2]][, m],
                                         resultGM_List[[2]][, m],
                                         resultWPM_List[[2]][, m],
                                         resultMeta_List[[2]][, m],
                                         resultPM_List[[3]][, m],
                                         resultSM_List[[3]][, m],
                                         resultGM_List[[3]][, m],
                                         resultWPM_List[[3]][, m],
                                         resultMeta_List[[3]][, m],
                                         resultPM_List[[4]][, m],
                                         resultSM_List[[4]][, m],
                                         resultGM_List[[4]][, m],
                                         resultWPM_List[[4]][, m],
                                         resultMeta_List[[4]][, m],
                                         resultPM_List[[5]][, m],
                                         resultSM_List[[5]][, m],
                                         resultGM_List[[5]][, m],
                                         resultWPM_List[[5]][, m],
                                         resultMeta_List[[5]][, m]))
      colnames(modelCompMatrix) <- paste(rep(algorithms, 1, each = 5), c("PM", "SM", "GM", "WPM", "PM+"), sep = "-")
      modelCompResults <- sk_esd(modelCompMatrix)
      rownames(modelCompResults$m.inf) <- gsub(pattern = ".", replacement = "+", x = rownames(modelCompResults$m.inf), fixed = TRUE)
      plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")
      
      
        if(m == "MCC")
          boxplot(resultPM[, m], resultSM[, m], resultGM[, m], resultWPM[, m], resultMeta[, m], 
                  ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
        else
          boxplot(resultPM[, m], resultSM[, m], resultGM[, m], resultWPM[, m], resultMeta[, m], 
                  ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
        title(ylab = paste("Cluster-", cls, " (", length(developerListOfCluster), " author) ", m, sep = ""), cex.lab = 1.7)
      }

    dev.off()
    
  }
  
}

if(write.PMBeatTable){
  algorithms <- c("NB", "RF")
  numberOfModelsCompared <- c(3)
  
  for(alg in algorithms){
    clusterResults <- read.xlsx(paste(plotOutputDir, "/Results_5Clusters_", alg, "_", samplingOption, ".xlsx", sep = ""), sheetIndex = 1)
    
    for(n in numberOfModelsCompared){
      allBeatTable <- as.data.frame(matrix(nrow = 2, ncol = (n - 1) * 3 * length(metricList) + 3))
      clusteredBeats <- as.data.frame(matrix(nrow = 2, ncol = (n - 1) * 3 * length(metricList) + 2))
      if(n == 3){
        allBeatTable[1, 1:7 * 6 - 2] <- metricList
        allBeatTable[2, 4:(6 * length(metricList) + 3)] <- c("PM>SM", "PM<SM", "PM=SM", "PM>GM", "PM<GM", "PM=GM")
        
        clusteredBeats[1, 1:7 * 6 - 3] <- metricList
        clusteredBeats[2, 3:(6 * length(metricList) + 2)] <- c("PM>SM", "PM<SM", "PM=SM", "PM>GM", "PM<GM", "PM=GM") 
        clusteredBeats[2, 1:2] <- c("Cluster", "AuthorCount")
      }
      
      for(p in 1:length(projectList)){
        projectName <- projectList[p]
        loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
        
        plotInputDir <- get(paste(alg, "Dir", sep = "_"))
        beatFile <- paste(plotOutputDir, "Beat", n , "_", alg, "_", samplingOption, ".xlsx", sep = "")
        
        projectBeatTable <- as.data.frame(matrix(nrow = length(authorNames) + 2, ncol = (n - 1) * 3 * length(metricList) + 3))
        projectBeatTable[3:(length(authorNames) + 2), 1] <- authorNames
        projectBeatTable[3:(length(authorNames) + 2), 2] <- authorAliases
        if(n == 3){
          projectBeatTable[1, 1:7 * 6 - 2] <- metricList
          projectBeatTable[2, 4:(6 * length(metricList) + 3)] <- c("PM>SM", "PM<SM", "PM=SM", "PM>GM", "PM<GM", "PM=GM")
        }
        else if(n == 5){
          # projectBeatTable[1, c(3,7,11,15,19,23,27)] <- metricList
          # projectBeatTable[2, 3:((n - 1) * length(metricList) + 2)] <- c("PM>SM", "PM<SM", "PM=SM", "PM>GM", "PM<GM", "PM=GM", "PM>WPM", "PM<WPM", "PM=WPM", "PM>PM+", "PM<PM+", "PM=PM+")
        }
        
        for(a in 1:length(authorNames)){
          if(experimentType == "MixTime")
            authorName <- authorNames[a]
          else
            authorName <- authorAliases[a]
          
          clusterIdOfAuthor <- clusterResults[which(clusterResults[, "Author"] == authorName), ]$Cluster[1]
          projectBeatTable[a + 2, 3] <- clusterIdOfAuthor
          
          if(n == 3){
            for(m in 1:length(metricList)){
              metricName <- metricList[m]
              
              resultsPM <- getAuthorResults(plotInputDir, projectName, authorName, "PM")
              resultsSM <- getAuthorResults(plotInputDir, projectName, authorName, "SM")
              resultsGM <- getAuthorResults(plotInputDir, projectName, authorName, "GM")
              
              #returns a table that contains pValues of PM/SM, PM/GM and SM/GM comparisons
              authorComparisonResults <- getNemenyiComparisonResultOfModels(authorName, projectName, n, resultsPM, resultsSM, resultsGM)
              
              medianPM <- medianChangeNaToZero(resultsPM[, metricName])
              medianSM <- medianChangeNaToZero(resultsSM[, metricName])
              medianGM <- medianChangeNaToZero(resultsGM[, metricName])
            
              PM_SMDiff <- authorComparisonResults[1, metricName] < pVal
              PM_GMDiff <- authorComparisonResults[2, metricName] < pVal
          
              if(PM_SMDiff){
                if(medianPM > medianSM){
                  projectBeatTable[a + 2, (m - 1) * 6 + 4] <- ifelse(metricName != "pf", 1, 0)
                  projectBeatTable[a + 2, (m - 1) * 6 + 5] <- ifelse(metricName != "pf", 0, 1)
                  projectBeatTable[a + 2, (m - 1) * 6 + 6] <- 0
                }
                else{
                  projectBeatTable[a + 2, (m - 1) * 6 + 4] <- ifelse(metricName != "pf", 0, 1)
                  projectBeatTable[a + 2, (m - 1) * 6 + 5] <- ifelse(metricName != "pf", 1, 0)
                  projectBeatTable[a + 2, (m - 1) * 6 + 6] <- 0
                }
              }
              else{      
                projectBeatTable[a + 2, (m - 1) * 6 + 4] <- 0
                projectBeatTable[a + 2, (m - 1) * 6 + 5] <- 0
                projectBeatTable[a + 2, (m - 1) * 6 + 6] <- 1
              }
              if(PM_GMDiff){
                if(medianPM > medianGM){
                  projectBeatTable[a + 2, (m - 1) * 6 + 7] <- ifelse(metricName != "pf", 1, 0)
                  projectBeatTable[a + 2, (m - 1) * 6 + 8] <- ifelse(metricName != "pf", 0, 1)
                  projectBeatTable[a + 2, (m - 1) * 6 + 9] <- 0
                }
                else{
                  projectBeatTable[a + 2, (m - 1) * 6 + 7] <- ifelse(metricName != "pf", 0, 1)
                  projectBeatTable[a + 2, (m - 1) * 6 + 8] <- ifelse(metricName != "pf", 1, 0)
                  projectBeatTable[a + 2, (m - 1) * 6 + 9] <- 0
                }
              }
              else{      
                projectBeatTable[a + 2, (m - 1) * 6 + 7] <- 0
                projectBeatTable[a + 2, (m - 1) * 6 + 8] <- 0
                projectBeatTable[a + 2, (m - 1) * 6 + 9] <- 1
              }
            }
          }
          else if(n == 5){}
        }
        
        dat <- data.frame(lapply(projectBeatTable[3:nrow(projectBeatTable), 4:ncol(projectBeatTable)], as.numeric))
        projectBeatTable[nrow(projectBeatTable) + 1, 4:ncol(projectBeatTable)] <- colSums(dat)
        projectBeatTable[nrow(projectBeatTable), 1] <- "Total"
        
        allBeatTable <- rbind(allBeatTable, projectBeatTable[3:(nrow(projectBeatTable) - 1), ])
        
        if(p == 1)
          write.xlsx(projectBeatTable, beatFile, sheetName = projectName, row.names = F, col.names = F)
        else
          write.xlsx(projectBeatTable, beatFile, sheetName = projectName, row.names = F, col.names = F, append = T)
      }
      
      dat <- data.frame(lapply(allBeatTable[3:nrow(allBeatTable), 3:ncol(allBeatTable)], as.numeric))
      allBeatTable[nrow(allBeatTable) + 1, 4:ncol(allBeatTable)] <- colSums(dat[, 2:ncol(dat)])
      allBeatTable[nrow(allBeatTable), 1] <- "Total"
      
      aggregatedSums <- aggregate(dat, by = list(dat[, 1]), FUN = sum)
      colnames(aggregatedSums) <- colnames(clusteredBeats)
      clusteredBeats <- rbind(clusteredBeats, aggregatedSums)
      clusteredBeats[3:7, 2] <- as.numeric(clusteredBeats[3:7, 2]) / 1:5
      
      dat <- data.frame(lapply(clusteredBeats[3:nrow(clusteredBeats), 2:ncol(clusteredBeats)], as.numeric))
      clusteredBeats[nrow(clusteredBeats) + 1, 2:ncol(clusteredBeats)] <- colSums(dat)
      clusteredBeats[nrow(clusteredBeats), 1] <- "Total"
      
      write.xlsx(allBeatTable, beatFile, sheetName = "all", row.names = F, col.names = F, append = T) 
      write.xlsx(clusteredBeats, beatFile, sheetName = "clusters", row.names = F, col.names = F, append = T) 
    }
  }
}
 
if(plot.Overall5Models){
  algorithms <- c("NB", "RF")
  
  ### reading results ##############
  resultsNB_PM <- data.frame()
  resultsNB_SM <- data.frame()
  resultsNB_GM <- data.frame()
  resultsNB_WPM <- data.frame()
  resultsNB_Meta <- data.frame()
  
  resultsRF_PM <- data.frame()
  resultsRF_SM <- data.frame()
  resultsRF_GM <- data.frame()
  resultsRF_WPM <- data.frame()
  resultsRF_Meta <- data.frame()
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
    
    for(a in 1:length(authorNames)){
      if(experimentType == "MixTime")
        authorName <- authorNames[a]
      else
        authorName <- authorAliases[a]
      
      resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsNB_WPM <- rbind(resultsNB_WPM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta <- rbind(resultsNB_Meta, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsRF_WPM <- rbind(resultsRF_WPM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta <- rbind(resultsRF_Meta, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
    }
  }
  #######################
  
  pdf(file = paste(plotOutputDir, "5models.pdf", sep = ""), width = 27, height = 10)
  par(mfrow = c(length(algorithms), length(metricList)), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5) 
  
  for(alg in c("NB", "RF")){
    for(m in metricList){
      resultPM <- get(paste("results", alg, "_PM", sep = ""))[, m]   
      resultSM <- get(paste("results", alg, "_SM", sep = ""))[, m]   
      resultGM <- get(paste("results", alg, "_GM", sep = ""))[, m]  
      resultWPM <- get(paste("results", alg, "_WPM", sep = ""))[, m]  
      resultMeta <- get(paste("results", alg, "_Meta", sep = ""))[, m]  
      
      if(m == "MCC")
        boxplot(resultPM, resultSM, resultGM, resultWPM, resultMeta, ylim = c(-1.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
      else
        boxplot(resultPM, resultSM, resultGM, resultWPM, resultMeta, ylim = c(0.0, 1.0), cex.axis = 1.5, cex.lab = 1.5, names = c("PM", "SM", "GM", "WPM", "PM+"))
      title(ylab = paste(alg, m), cex.lab = 1.7)
    }
  }
  dev.off()

  pdf(file = paste(plotOutputDir, "5models_SK.pdf", sep = ""), width = 10, height = 20)
  par(mfrow = c(4, 2), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)

  for(m in metricList){
    modelCompMatrix <- as.matrix(cbind(resultsNB_PM[, m],
                                       resultsNB_SM[, m],
                                       resultsNB_GM[, m],
                                       resultsNB_WPM[, m],
                                       resultsNB_Meta[, m],
                                       resultsRF_PM[, m],
                                       resultsRF_SM[, m],
                                       resultsRF_GM[, m],
                                       resultsRF_WPM[, m],
                                       resultsRF_Meta[, m]))
    colnames(modelCompMatrix) <- paste(rep(algorithms, 1, each = 5), c("PM", "SM", "GM", "WPM", "PM+"), sep = "_")
    modelCompResults <- sk_esd(modelCompMatrix)
    rownames(modelCompResults$m.inf) <- gsub(pattern = ".", replacement = "+", x = rownames(modelCompResults$m.inf), fixed = TRUE)
    plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "", cex.lab = 1.5)
  }

  dev.off()
}

if(write.SeqTimevsMixTime){
  samplingOption = "WO"
  testSetMonths = "6"
  
  NB_Dir_Mix <- paste(mainProjectDir, "output/", "MixTime", "_NB/", samplingOption, "/", sep = "")
  RF_Dir_Mix <- paste(mainProjectDir, "output/", "MixTime", "_RF/", samplingOption, "/", sep = "")
 
  NB_Dir_Seq <- paste(mainProjectDir, "output/", "SeqTime", "_NB/", testSetMonths, "Months/", sep = "")
  RF_Dir_Seq <- paste(mainProjectDir, "output/", "SeqTime", "_RF/", testSetMonths, "Months/", sep = "")
  
  metricList <- c("pd", "pf", "F1", "AUC")
  algorithms <- c("NB", "RF")
  experimentationList <- c("Mix", "Seq")
  
  compTable <- as.data.frame(matrix(nrow = 17, ncol = 8))
  compTable[1, 4:8] <- c("PM", "SM", "GM", "WPM", "PM+")
  compTable[2:17, 3] <- metricList
  compTable[c(2,6,10,14), 2] <- algorithms
  compTable[c(2,10), 1] <- experimentationList
  
  
  for(i in 1:length(experimentationList)){
    resultsNB_PM <- data.frame()
    resultsNB_SM <- data.frame()
    resultsNB_GM <- data.frame()
    resultsNB_WPM <- data.frame()
    resultsNB_Meta <- data.frame()
    
    resultsRF_PM <- data.frame()
    resultsRF_SM <- data.frame()
    resultsRF_GM <- data.frame()
    resultsRF_WPM <- data.frame()
    resultsRF_Meta <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]

      if(experimentationList[i] == "Mix")
        loadProjectDataOnlyAuthorInfo(experimentType = "MixTime", testSetMonths = "", samplingOption = "WO", projectName)
      else{
        loadProjectDataOnlyAuthorInfo(experimentType = "SeqTime", testSetMonths = "6", samplingOption = "", projectName)
        authorNames <- authorAliases
      }
        
      for(a in 1:length(authorNames)){
        authorName <- authorNames[a]
        
        resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = T))
        resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        resultsNB_WPM <- rbind(resultsNB_WPM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
        resultsNB_Meta <- rbind(resultsNB_Meta, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
        
        resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        resultsRF_WPM <- rbind(resultsRF_WPM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
        resultsRF_Meta <- rbind(resultsRF_Meta, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      }
    }
  
    # assign(paste("resultsNB_PM", experimentationList[i], sep = "_"), resultsNB_PM)
    # assign(paste("resultsNB_SM", experimentationList[i], sep = "_"), resultsNB_SM)
    # assign(paste("resultsNB_GM", experimentationList[i], sep = "_"), resultsNB_GM)
    # assign(paste("resultsNB_WPM", experimentationList[i], sep = "_"), resultsNB_WPM)
    # assign(paste("resultsNB_Meta", experimentationList[i], sep = "_"), resultsNB_Meta)
    # 
    # assign(paste("resultsRF_PM", experimentationList[i], sep = "_"), resultsRF_PM)
    # assign(paste("resultsRF_SM", experimentationList[i], sep = "_"), resultsRF_SM)
    # assign(paste("resultsRF_GM", experimentationList[i], sep = "_"), resultsRF_GM)
    # assign(paste("resultsRF_WPM", experimentationList[i], sep = "_"), resultsRF_WPM)
    # assign(paste("resultsRF_Meta", experimentationList[i], sep = "_"), resultsRF_Meta)
    
    if(experimentationList[i] == "Mix"){
      rowOffset <- 1
      
      for(m in 1:length(metricList)){
        compTable[rowOffset + m, 4] <- round(median(resultsNB_PM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 5] <- round(median(resultsNB_SM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m, 6] <- round(median(resultsNB_GM[, metricList[m]], na.rm = TRUE), 2)  
        compTable[rowOffset + m, 7] <- round(median(resultsNB_WPM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m, 8] <- round(median(resultsNB_Meta[, metricList[m]], na.rm = TRUE), 2) 
        
        compTable[rowOffset + m + 4, 4] <- round(median(resultsRF_PM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 5] <- round(median(resultsRF_SM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 6] <- round(median(resultsRF_GM[, metricList[m]], na.rm = TRUE), 2)  
        compTable[rowOffset + m + 4, 7] <- round(median(resultsRF_WPM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 8] <- round(median(resultsRF_Meta[, metricList[m]], na.rm = TRUE), 2) 
      }
    }
    if(experimentationList[i] == "Seq"){
      rowOffset <- 9
      
      for(m in 1:length(metricList)){
        compTable[rowOffset + m, 4] <- round(median(resultsNB_PM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 5] <- round(median(resultsNB_SM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 6] <- round(median(resultsNB_GM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 7] <- round(median(resultsNB_WPM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 8] <- round(median(resultsNB_Meta[, metricList[m]], na.rm = TRUE), 2) 
        
        compTable[rowOffset + m + 4, 4] <- round(median(resultsRF_PM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 5] <- round(median(resultsRF_SM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 6] <- round(median(resultsRF_GM[, metricList[m]], na.rm = TRUE), 2)  
        compTable[rowOffset + m + 4, 7] <- round(median(resultsRF_WPM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 8] <- round(median(resultsRF_Meta[, metricList[m]], na.rm = TRUE), 2) 
      }
    }
  }
  
  write.xlsx(compTable, file = paste(plotOutputDir, "MixVsSeq.xlsx", sep = ""), sheetName = "MixVsSeq")
 
  latexTable <- xtable(compTable, 
                       caption= "Comparison of MixTime and SeqTime experimentations")
   
}

if(compare.MixVsSeq.SK){
  samplingOption = "WO"
  testSetMonths = "6"
  
  NB_Dir_Mix <- paste(mainProjectDir, "output/", "MixTime", "_NB/", samplingOption, "/", sep = "")
  RF_Dir_Mix <- paste(mainProjectDir, "output/", "MixTime", "_RF/", samplingOption, "/", sep = "")
  
  NB_Dir_Seq <- paste(mainProjectDir, "output/", "SeqTime", "_NB/", testSetMonths, "Months/", sep = "")
  RF_Dir_Seq <- paste(mainProjectDir, "output/", "SeqTime", "_RF/", testSetMonths, "Months/", sep = "")
  
  algorithms <- c("NB", "RF")
  experimentationList <- c("Mix", "Seq")
  
  resultsNB_PM_Mix <- data.frame()
  resultsNB_SM_Mix <- data.frame()
  resultsNB_GM_Mix <- data.frame()
  resultsNB_WPM_Mix <- data.frame()
  resultsNB_Meta_Mix <- data.frame()
  
  resultsRF_PM_Mix <- data.frame()
  resultsRF_SM_Mix <- data.frame()
  resultsRF_GM_Mix <- data.frame()
  resultsRF_WPM_Mix <- data.frame()
  resultsRF_Meta_Mix <- data.frame()
  
  resultsNB_PM_Seq <- data.frame()
  resultsNB_SM_Seq <- data.frame()
  resultsNB_GM_Seq <- data.frame()
  resultsNB_WPM_Seq <- data.frame()
  resultsNB_Meta_Seq <- data.frame()
  
  resultsRF_PM_Seq <- data.frame()
  resultsRF_SM_Seq <- data.frame()
  resultsRF_GM_Seq <- data.frame()
  resultsRF_WPM_Seq <- data.frame()
  resultsRF_Meta_Seq <- data.frame()
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType = "MixTime", testSetMonths = "", samplingOption = "WO", projectName)
    
    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]
      
      resultsNB_PM_Mix <- rbind(resultsNB_PM_Mix, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = T))
      resultsNB_SM_Mix <- rbind(resultsNB_SM_Mix, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM_Mix <- rbind(resultsNB_GM_Mix, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsNB_WPM_Mix <- rbind(resultsNB_WPM_Mix, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta_Mix <- rbind(resultsNB_Meta_Mix, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsRF_PM_Mix <- rbind(resultsRF_PM_Mix, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM_Mix <- rbind(resultsRF_SM_Mix, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM_Mix <- rbind(resultsRF_GM_Mix, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsRF_WPM_Mix <- rbind(resultsRF_WPM_Mix, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta_Mix <- rbind(resultsRF_Meta_Mix, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
    }
  }
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType = "SeqTime", testSetMonths = testSetMonths, samplingOption = "", projectName)
    
    for(a in 1:length(authorNames)){
      authorAlias <- authorAliases[a]

      resultsNB_PM_Seq <- rbind(resultsNB_PM_Seq, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsPM.csv", sep = ""), header = T))
      resultsNB_SM_Seq <- rbind(resultsNB_SM_Seq, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM_Seq <- rbind(resultsNB_GM_Seq, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsNB_WPM_Seq <- rbind(resultsNB_WPM_Seq, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta_Seq <- rbind(resultsNB_Meta_Seq, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsRF_PM_Seq <- rbind(resultsRF_PM_Seq, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM_Seq <- rbind(resultsRF_SM_Seq, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM_Seq <- rbind(resultsRF_GM_Seq, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsRF_WPM_Seq <- rbind(resultsRF_WPM_Seq, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta_Seq <- rbind(resultsRF_Meta_Seq, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorAlias, "ResultsMeta.csv", sep = ""), header = TRUE))
    }
  }
  
  pdf(file = paste(plotOutputDir, "MixVsSeq_SK.pdf", sep = ""), width = 25, height = 35)
  par(mfrow = c(5, 2), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
  for(m in metricList){
    modelCompMatrix <- as.matrix(cbind(resultsNB_PM_Mix[, m],
                                       resultsNB_SM_Mix[, m],
                                       resultsNB_GM_Mix[, m],
                                       resultsNB_WPM_Mix[, m],
                                       resultsNB_Meta_Mix[, m],
                                       resultsRF_PM_Mix[, m],
                                       resultsRF_SM_Mix[, m],
                                       resultsRF_GM_Mix[, m],
                                       resultsRF_WPM_Mix[, m],
                                       resultsRF_Meta_Mix[, m],
                                       resultsNB_PM_Seq[, m],
                                       resultsNB_SM_Seq[, m],
                                       resultsNB_GM_Seq[, m],
                                       resultsNB_WPM_Seq[, m],
                                       resultsNB_Meta_Seq[, m],
                                       resultsRF_PM_Seq[, m],
                                       resultsRF_SM_Seq[, m],
                                       resultsRF_GM_Seq[, m],
                                       resultsRF_WPM_Seq[, m],
                                       resultsRF_Meta_Seq[, m]))
    colnames(modelCompMatrix) <- c("NB_PM_Mix", "NB_SM_Mix", "NB_GM_Mix", "NB_WPM_Mix", "NB_PM+_Mix",
                                   "RF_PM_Mix", "RF_SM_Mix", "RF_GM_Mix", "RF_WPM_Mix", "RF_PM+_Mix",
                                   "NB_PM_Seq", "NB_SM_Seq", "NB_GM_Seq", "NB_WPM_Seq", "NB_PM+_Seq",
                                   "RF_PM_Seq", "RF_SM_Seq", "RF_GM_Seq", "RF_WPM_Seq", "RF_PM+_Seq")
    modelCompResults <- sk_esd(modelCompMatrix)
    rownames(modelCompResults$m.inf) <- gsub(pattern = ".", replacement = "+", x = rownames(modelCompResults$m.inf), fixed = TRUE)
    plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")
  }
  dev.off()
}

if(write.NormalVsUnderSamp){
  NB_Dir_NonSamp <- paste(mainProjectDir, "output/", "MixTime", "_NB/", samplingOption, "/", sep = "")
  RF_Dir_NonSamp <- paste(mainProjectDir, "output/", "MixTime", "_RF/", samplingOption, "/", sep = "")
  
  NB_Dir_UnderSamp <- paste(mainProjectDir, "output/", "MixTime_UnderSamp", "_NB/", samplingOption, "/", sep = "")
  RF_Dir_UnderSamp <- paste(mainProjectDir, "output/", "MixTime_UnderSamp", "_RF/", samplingOption, "/", sep = "")
  
  # metricList <- c("pd", "pf", "F1", "AUC")
  # algorithms <- c("NB", "RF")
  experimentationList <- c("NonSamp", "UnderSamp")
  
  compTable <- as.data.frame(matrix(nrow = 29, ncol = 8))
  compTable[1, 4:8] <- c("PM", "SM", "GM", "WPM", "PM+")
  compTable[2:29, 3] <- metricList
  compTable[c(2,9,16,21), 2] <- algorithms
  compTable[c(2,16), 1] <- experimentationList
  # compTable[1, 4:8] <- c("PM", "SM", "GM", "WPM", "PM+")
  # compTable[2:17, 3] <- metricList
  # compTable[c(2,6,10,14), 2] <- algorithms
  # compTable[c(2,10), 1] <- experimentationList
  
  for(i in 1:length(experimentationList)){
    resultsNB_PM <- data.frame()
    resultsNB_SM <- data.frame()
    resultsNB_GM <- data.frame()
    resultsNB_WPM <- data.frame()
    resultsNB_Meta <- data.frame()
    
    resultsRF_PM <- data.frame()
    resultsRF_SM <- data.frame()
    resultsRF_GM <- data.frame()
    resultsRF_WPM <- data.frame()
    resultsRF_Meta <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      
      # if(experimentationList[i] == "NonSamp")
        loadProjectDataOnlyAuthorInfo(experimentType = "MixTime", testSetMonths = "", samplingOption = "WO", projectName)
      # else{
      #   loadProjectDataOnlyAuthorInfo(experimentType = "SeqTime", testSetMonths = "6", samplingOption = "", projectName)
      #   authorNames <- authorAliases
      # }
      
      for(a in 1:length(authorNames)){
        authorName <- authorNames[a]
        
        resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = T))
        resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        resultsNB_WPM <- rbind(resultsNB_WPM, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
        resultsNB_Meta <- rbind(resultsNB_Meta, read.csv(file = paste(get(paste("NB_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
        
        resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        resultsRF_WPM <- rbind(resultsRF_WPM, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
        resultsRF_Meta <- rbind(resultsRF_Meta, read.csv(file = paste(get(paste("RF_Dir", experimentationList[i], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      }
    }
    
    # assign(paste("resultsNB_PM", experimentationList[i], sep = "_"), resultsNB_PM)
    # assign(paste("resultsNB_SM", experimentationList[i], sep = "_"), resultsNB_SM)
    # assign(paste("resultsNB_GM", experimentationList[i], sep = "_"), resultsNB_GM)
    # assign(paste("resultsNB_WPM", experimentationList[i], sep = "_"), resultsNB_WPM)
    # assign(paste("resultsNB_Meta", experimentationList[i], sep = "_"), resultsNB_Meta)
    # 
    # assign(paste("resultsRF_PM", experimentationList[i], sep = "_"), resultsRF_PM)
    # assign(paste("resultsRF_SM", experimentationList[i], sep = "_"), resultsRF_SM)
    # assign(paste("resultsRF_GM", experimentationList[i], sep = "_"), resultsRF_GM)
    # assign(paste("resultsRF_WPM", experimentationList[i], sep = "_"), resultsRF_WPM)
    # assign(paste("resultsRF_Meta", experimentationList[i], sep = "_"), resultsRF_Meta)
    
    if(experimentationList[i] == "NonSamp"){
      rowOffset <- 1
      
      for(m in 1:length(metricList)){
        compTable[rowOffset + m, 4] <- round(median(resultsNB_PM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 5] <- round(median(resultsNB_SM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m, 6] <- round(median(resultsNB_GM[, metricList[m]], na.rm = TRUE), 2)  
        compTable[rowOffset + m, 7] <- round(median(resultsNB_WPM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m, 8] <- round(median(resultsNB_Meta[, metricList[m]], na.rm = TRUE), 2) 
        
        compTable[rowOffset + m + 4, 4] <- round(median(resultsRF_PM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 5] <- round(median(resultsRF_SM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 6] <- round(median(resultsRF_GM[, metricList[m]], na.rm = TRUE), 2)  
        compTable[rowOffset + m + 4, 7] <- round(median(resultsRF_WPM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 8] <- round(median(resultsRF_Meta[, metricList[m]], na.rm = TRUE), 2) 
      }
    }
    if(experimentationList[i] == "UnderSamp"){
      rowOffset <- 15
      
      for(m in 1:length(metricList)){
        compTable[rowOffset + m, 4] <- round(median(resultsNB_PM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 5] <- round(median(resultsNB_SM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 6] <- round(median(resultsNB_GM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 7] <- round(median(resultsNB_WPM[, metricList[m]], na.rm = TRUE), 2)
        compTable[rowOffset + m, 8] <- round(median(resultsNB_Meta[, metricList[m]], na.rm = TRUE), 2) 
        
        compTable[rowOffset + m + 4, 4] <- round(median(resultsRF_PM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 5] <- round(median(resultsRF_SM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 6] <- round(median(resultsRF_GM[, metricList[m]], na.rm = TRUE), 2)  
        compTable[rowOffset + m + 4, 7] <- round(median(resultsRF_WPM[, metricList[m]], na.rm = TRUE), 2) 
        compTable[rowOffset + m + 4, 8] <- round(median(resultsRF_Meta[, metricList[m]], na.rm = TRUE), 2) 
      }
    }
  }
  
  write.xlsx(compTable, file = paste(plotOutputDir, "NonVsUnderSamp.xlsx", sep = ""), sheetName = "NonVsUnderSamp")
  
  # latexTable <- xtable(compTable, 
                       # caption= "Comparison of MixTime and SeqTime experimentations")
  
}

if(compare.NormalVsUnderSamp.SK){
  NB_Dir_NonSamp <- paste(mainProjectDir, "output/", "MixTime", "_NB/", samplingOption, "/", sep = "")
  RF_Dir_NonSamp <- paste(mainProjectDir, "output/", "MixTime", "_RF/", samplingOption, "/", sep = "")
  
  NB_Dir_UnderSamp <- paste(mainProjectDir, "output/", "MixTime_UnderSamp", "_NB/", samplingOption, "/", sep = "")
  RF_Dir_UnderSamp <- paste(mainProjectDir, "output/", "MixTime_UnderSamp", "_RF/", samplingOption, "/", sep = "")
  
  experimentationList <- c("NonSamp", "UnderSamp")
  
  resultsNB_PM_NonSamp <- data.frame()
  resultsNB_SM_NonSamp <- data.frame()
  resultsNB_GM_NonSamp <- data.frame()
  resultsNB_WPM_NonSamp <- data.frame()
  resultsNB_Meta_NonSamp <- data.frame()
  
  resultsRF_PM_NonSamp <- data.frame()
  resultsRF_SM_NonSamp <- data.frame()
  resultsRF_GM_NonSamp <- data.frame()
  resultsRF_WPM_NonSamp <- data.frame()
  resultsRF_Meta_NonSamp <- data.frame()
  
  resultsNB_PM_UnderSamp <- data.frame()
  resultsNB_SM_UnderSamp <- data.frame()
  resultsNB_GM_UnderSamp <- data.frame()
  resultsNB_WPM_UnderSamp <- data.frame()
  resultsNB_Meta_UnderSamp <- data.frame()
    
  resultsRF_PM_UnderSamp <- data.frame()
  resultsRF_SM_UnderSamp <- data.frame()
  resultsRF_GM_UnderSamp <- data.frame()
  resultsRF_WPM_UnderSamp <- data.frame()
  resultsRF_Meta_UnderSamp <- data.frame()
   
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType = "MixTime", testSetMonths = "", samplingOption = "WO", projectName)
    
    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]
      
      resultsNB_PM_NonSamp <- rbind(resultsNB_PM_NonSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = T))
      resultsNB_SM_NonSamp <- rbind(resultsNB_SM_NonSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM_NonSamp <- rbind(resultsNB_GM_NonSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsNB_WPM_NonSamp <- rbind(resultsNB_WPM_NonSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta_NonSamp <- rbind(resultsNB_Meta_NonSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsRF_PM_NonSamp <- rbind(resultsRF_PM_NonSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM_NonSamp <- rbind(resultsRF_SM_NonSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM_NonSamp <- rbind(resultsRF_GM_NonSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsRF_WPM_NonSamp <- rbind(resultsRF_WPM_NonSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta_NonSamp <- rbind(resultsRF_Meta_NonSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[1], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsNB_PM_UnderSamp <- rbind(resultsNB_PM_UnderSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = T))
      resultsNB_SM_UnderSamp <- rbind(resultsNB_SM_UnderSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM_UnderSamp <- rbind(resultsNB_GM_UnderSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsNB_WPM_UnderSamp <- rbind(resultsNB_WPM_UnderSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta_UnderSamp <- rbind(resultsNB_Meta_UnderSamp, read.csv(file = paste(get(paste("NB_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsRF_PM_UnderSamp <- rbind(resultsRF_PM_UnderSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM_UnderSamp <- rbind(resultsRF_SM_UnderSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM_UnderSamp <- rbind(resultsRF_GM_UnderSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsRF_WPM_UnderSamp <- rbind(resultsRF_WPM_UnderSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta_UnderSamp <- rbind(resultsRF_Meta_UnderSamp, read.csv(file = paste(get(paste("RF_Dir", experimentationList[2], sep = "_")), projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
    }
  }
  
  pdf(file = paste(plotOutputDir, "NonVsUnderSamp_SK.pdf", sep = ""), width = 25, height = 35)
  par(mfrow = c(5, 2), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
  for(m in metricList){
    modelCompMatrix <- as.matrix(cbind(resultsNB_PM_NonSamp[, m],
                                       resultsNB_SM_NonSamp[, m],
                                       resultsNB_GM_NonSamp[, m],
                                       resultsNB_WPM_NonSamp[, m],
                                       resultsNB_Meta_NonSamp[, m],
                                       resultsRF_PM_NonSamp[, m],
                                       resultsRF_SM_NonSamp[, m],
                                       resultsRF_GM_NonSamp[, m],
                                       resultsRF_WPM_NonSamp[, m],
                                       resultsRF_Meta_NonSamp[, m],
                                       resultsNB_PM_UnderSamp[, m],
                                       resultsNB_SM_UnderSamp[, m],
                                       resultsNB_GM_UnderSamp[, m],
                                       resultsNB_WPM_UnderSamp[, m],
                                       resultsNB_Meta_UnderSamp[, m],
                                       resultsRF_PM_UnderSamp[, m],
                                       resultsRF_SM_UnderSamp[, m],
                                       resultsRF_GM_UnderSamp[, m],
                                       resultsRF_WPM_UnderSamp[, m],
                                       resultsRF_Meta_UnderSamp[, m]))
    colnames(modelCompMatrix) <- c("NB_PM_NonSamp", "NB_SM_NonSamp", "NB_GM_NonSamp", "NB_WPM_NonSamp", "NB_PM+_NonSamp",
                                   "RF_PM_NonSamp", "RF_SM_NonSamp", "RF_GM_NonSamp", "RF_WPM_NonSamp", "RF_PM+_NonSamp",
                                   "NB_PM_UnderSamp", "NB_SM_UnderSamp", "NB_GM_UnderSamp", "NB_WPM_UnderSamp", "NB_PM+_UnderSamp",
                                   "RF_PM_UnderSamp", "RF_SM_UnderSamp", "RF_GM_UnderSamp", "RF_WPM_UnderSamp", "RF_PM+_UnderSamp")
    modelCompResults <- sk_esd(modelCompMatrix)
    rownames(modelCompResults$m.inf) <- gsub(pattern = ".", replacement = "+", x = rownames(modelCompResults$m.inf), fixed = TRUE)
    plot(modelCompResults, ylab = m, las = 2, xlab = "", title = "")
  }
  dev.off()
}

if(write.Overall5Models){
  algorithms <- c("NB", "RF")
  
  ### reading results ##############
  resultsNB_PM <- data.frame()
  resultsNB_SM <- data.frame()
  resultsNB_GM <- data.frame()
  resultsNB_WPM <- data.frame()
  resultsNB_Meta <- data.frame()
  
  resultsRF_PM <- data.frame()
  resultsRF_SM <- data.frame()
  resultsRF_GM <- data.frame()
  resultsRF_WPM <- data.frame()
  resultsRF_Meta <- data.frame()
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
    
    for(a in 1:length(authorNames)){
      if(experimentType == "MixTime")
        authorName <- authorNames[a]
      else
        authorName <- authorAliases[a]
      
      resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsNB_WPM <- rbind(resultsNB_WPM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsNB_Meta <- rbind(resultsNB_Meta, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
      resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
      resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      resultsRF_WPM <- rbind(resultsRF_WPM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsWPM.csv", sep = ""), header = TRUE))
      resultsRF_Meta <- rbind(resultsRF_Meta, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsMeta.csv", sep = ""), header = TRUE))
      
      # authorsResultsNB_NofB20 <- colMedians(as.matrix(read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsNofB20.csv", sep = ""), header = TRUE)))
      # authorsResultsRF_NofB20 <- colMedians(as.matrix(read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsNofB20.csv", sep = ""), header = TRUE)))
    }
    # resultsNB_NofB20 <- rbind(resultsNB_NofB20, authorsResultsNB_NofB20)
    # resultsRF_NofB20 <- rbind(resultsRF_NofB20, authorsResultsNB_NofB20)
  }
  #######################
  
  
  tableAll <- as.data.frame(matrix(nrow = 7, ncol = 5))
  tableAll[3:7, 1] <-  c("PM", "SM", "GM", "WPM", "PM+")
  # tableAll[2, 2:17] <- c(metricList, "NofB20")
  tableAll[2, 2:15] <- c(metricList)
  tableAll[1, c(2, 10)] <- c("NB", "RF")
  
  for(alg in c("NB", "RF")){
    if(alg == "NB")
      offset <- 1
    else
      offset <- 8
    
    for(m in 1:length(metricList)){
      resultPM <- get(paste("results", alg, "_PM", sep = ""))[, metricList[m]]   
      resultSM <- get(paste("results", alg, "_SM", sep = ""))[, metricList[m]]   
      resultGM <- get(paste("results", alg, "_GM", sep = ""))[, metricList[m]]   
      resultWPM <- get(paste("results", alg, "_WPM", sep = ""))[, metricList[m]]   
      resultMeta <- get(paste("results", alg, "_Meta", sep = ""))[, metricList[m]] 
      
      tableAll[3, offset + m] <- round(median(resultPM, na.rm = T), 2)
      tableAll[4, offset + m] <- round(median(resultSM, na.rm = T), 2)
      tableAll[5, offset + m] <- round(median(resultGM, na.rm = T), 2)
      tableAll[6, offset + m] <- round(median(resultWPM, na.rm = T), 2)
      tableAll[7, offset + m] <- round(median(resultMeta, na.rm = T), 2)
    }
    
    xlsx::write.xlsx(x = tableAll, file = paste(plotOutputDir, "5ModelsOverallResults.xlsx", sep = ""), 
                     sheetName = "5models.SepAlg", col.names = FALSE, row.names = FALSE)
    
  }
  
  
  tableAll <- as.data.frame(matrix(nrow = 7, ncol = 5))
  tableAll[3:7, 1] <-  c("PM", "SM", "GM", "WPM", "PM+")
  # tableAll[2, 2:17] <- c(metricList, "NofB20")
  tableAll[2, 2:8] <- c(metricList)
  # tableAll[1, c(2, 10)] <- c("NB", "RF")
  
  for(m in 1:length(metricList)){
    tableAll[3, 1 + m] <- round(median(c(resultsNB_PM[, metricList[m]], resultsRF_PM[, metricList[m]]), na.rm = T), 2)
    tableAll[4, 1 + m] <- round(median(c(resultsNB_SM[, metricList[m]], resultsRF_SM[, metricList[m]]), na.rm = T), 2)
    tableAll[5, 1 + m] <- round(median(c(resultsNB_GM[, metricList[m]], resultsRF_GM[, metricList[m]]), na.rm = T), 2)
    tableAll[6, 1 + m] <- round(median(c(resultsNB_WPM[, metricList[m]], resultsRF_WPM[, metricList[m]]), na.rm = T), 2)
    tableAll[7, 1 + m] <- round(median(c(resultsNB_Meta[, metricList[m]], resultsRF_Meta[, metricList[m]]), na.rm = T), 2)
  }
  
  xlsx::write.xlsx(x = tableAll, file = paste(plotOutputDir, "5ModelsOverallResults.xlsx", sep = ""), 
                   sheetName = "5models", col.names = FALSE, row.names = FALSE, append = T)
}

if(write.NofB20){
  # algorithms <- c("NB", "RF")
  
  tableNofB20 <- as.data.frame(matrix(nrow = 8, ncol = 11))
  tableNofB20[2, 2:11] <-  c("PM", "SM", "GM", "WPM", "PM+")
  tableNofB20[1, c(2, 7)] <- c("NB", "RF")
  tableNofB20[3:8, 1] <- projectList
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
    
    resultsNB_NofB20 <- data.frame()
    resultsRF_NofB20 <- data.frame()
    
    for(a in 1:length(authorNames)){
      authorName <- authorNames[a]
     
      authorsResultsNB <- colMedians(as.matrix(read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsNofB20.csv", sep = ""), header = TRUE)))
      authorsResultsRF <- colMedians(as.matrix(read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsNofB20.csv", sep = ""), header = TRUE)))
      
      resultsNB_NofB20 <- rbind(resultsNB_NofB20, authorsResultsNB)
      resultsRF_NofB20 <- rbind(resultsRF_NofB20, authorsResultsRF)
    }
    
    offset <- 2
    tableNofB20[2 + p, offset] <- sum(resultsNB_NofB20[, 1])
    tableNofB20[2 + p, offset + 1] <- sum(resultsNB_NofB20[, 2])
    tableNofB20[2 + p, offset + 2] <- sum(resultsNB_NofB20[, 3])
    tableNofB20[2 + p, offset + 3] <- sum(resultsNB_NofB20[, 4])
    tableNofB20[2 + p, offset + 4] <- sum(resultsNB_NofB20[, 5])
    
    offset <- 7 
    tableNofB20[2 + p, offset] <- sum(resultsRF_NofB20[, 1])
    tableNofB20[2 + p, offset + 1] <- sum(resultsRF_NofB20[, 2])
    tableNofB20[2 + p, offset + 2] <- sum(resultsRF_NofB20[, 3])
    tableNofB20[2 + p, offset + 3] <- sum(resultsRF_NofB20[, 4])
    tableNofB20[2 + p, offset + 4] <- sum(resultsRF_NofB20[, 5])
  }
  
  xlsx::write.xlsx(x = tableNofB20, file = paste(plotOutputDir, "NofB20Results.xlsx", sep = ""), 
                     sheetName = "NofB20", col.names = FALSE, row.names = FALSE)
}

if(plot.AllSelectedDevelopers.PM){
  algorithms <- c("NB", "RF")
  # numberOfColours <- 7
  numberOfColours <- 3
  numberOfModels <- 3
  
  loadAllAuthorsInfo(experimentType)
  
  selectedAuthorAliases <- allAuthorsInfo[which(allAuthorsInfo$Selected == "T"), "Alias"]
  selectedAuthorNames <- allAuthorsInfo[which(allAuthorsInfo$Selected == "T"), "Author"]
  selectedAuthorProjects <- allAuthorsInfo[which(allAuthorsInfo$Selected == "T"), "projectName"]
  
  for(alg in algorithms){
    plotInputDir <- get(paste(alg, "Dir", sep = "_"))
    
    adjustedHeight <- 3 + length(selectedAuthorNames) * 0.5
    pdf(file = paste(plotOutputDir, "AllAuthors_", alg, "_", numberOfColours, "Colours_", numberOfModels, "Models.pdf", sep = ""), width = 20, height = adjustedHeight)
    par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
    
    for(m in metricList){
      resultsPM <- list()
      authorColors <- list()
      authorInfo <- list()
      
      tmpObjString <- ""
      tmpColString <- ""
      tmpInfoString <- ""
      
      for(a in 1:length(selectedAuthorNames)){
        buggyCommitRatio <- round(allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "BuggyCommitRatio"], 2)
        totalCommitCount <- allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "CommitCount"]
        
        
        resultsPM[[length(resultsPM) + 1]] <- getAuthorResults(inputDir = plotInputDir,
                                                               projectName = selectedAuthorProjects[a],
                                                               authorName = selectedAuthorNames[a],
                                                               modelName = "PM")[, m]
        
        authorColors[[length(authorColors) + 1]] <- getAuthorColorByMetric(plotInputDir,  
                                                                           projectName = selectedAuthorProjects[a], 
                                                                           authorName = selectedAuthorNames[a],  
                                                                           metricName = m, 
                                                                           numberOfColors = numberOfColours, 
                                                                           numberOfModels = numberOfModels)
        
        authorInfo[[length(authorInfo) + 1]] <- paste(selectedAuthorAliases[a], 
                                                      " / ",
                                                      selectedAuthorProjects[a], 
                                                      "\n",
                                                      totalCommitCount,
                                                      " / ",
                                                      buggyCommitRatio,
                                                      sep = "")
        
        
        tmpObjString <- paste(tmpObjString, "resultsPM[[", a, "]], ", sep = "")
        tmpColString <- paste(tmpColString, authorColors[[a]], ",", sep = "")
        tmpInfoString <- paste(tmpInfoString, "authorInfo[[", a, "]], ", sep = "")
      }
      
      tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 2)
      tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 1)
      tmpInfoString <- substr(tmpInfoString, 1, nchar(tmpInfoString) - 2)
      
      if(m == "pd")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "), names = c(", tmpInfoString, "))", sep =  "")))
      else if(m == "MCC")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(-1.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      else
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      
      # title(xlab = paste(m), cex.lab = 1.7)
      
    }
    dev.off()
  }
}

if(plot.AllSelectedDevelopers.PMSMGM){
  algorithms <- c("NB", "RF")
  numberOfColours <- 2 #2,7
  numberOfModels <- 3 #number of compared models
  metricList <- c("pd", "pf", "precision", "F1", "AUC", "MCC", "BScore")
  
  loadAllAuthorsInfo(experimentType)
  
  selectedAuthorAliases <- allAuthorsInfo[which(allAuthorsInfo$Selected == "T"), "Alias"]
  selectedAuthorNames <- allAuthorsInfo[which(allAuthorsInfo$Selected == "T"), "Author"]
  selectedAuthorProjects <- allAuthorsInfo[which(allAuthorsInfo$Selected == "T"), "projectName"]
  
  for(alg in algorithms){
    plotInputDir <- get(paste(alg, "Dir", sep = "_"))
    
    adjustedHeight <- length(selectedAuthorNames) * 1.9
    pdf(file = paste(plotOutputDir, "AllAuthors_", alg, "_", numberOfColours, "Colours_", numberOfModels, "Models_PMSMGM.pdf", sep = ""), width = 20, height = adjustedHeight)
    par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
    
    for(m in metricList){
      resultsPM <- vector("list", length = length(selectedAuthorNames))
      resultsSM <- vector("list", length = length(selectedAuthorNames))
      resultsGM <- vector("list", length = length(selectedAuthorNames))

      tmpObjString <- ""
      tmpColString <- ""
      tmpInfoString <- ""
      
      for(a in length(selectedAuthorNames):1){
        buggyCommitRatio <- round(allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "BuggyCommitRatio"], 2)
        totalCommitCount <- allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "CommitCount"]
        
        # PM
        resultsPM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                                               projectName = selectedAuthorProjects[a],
                                                               authorName = selectedAuthorNames[a],
                                                               modelName = "PM")[, m]
        
        authorColor <- getAuthorColorByMetric(plotInputDir,  
                                              projectName = selectedAuthorProjects[a], 
                                              authorName = selectedAuthorNames[a],  
                                              metricName = m, 
                                              numberOfColors = numberOfColours, 
                                              numberOfModels = numberOfModels)
        
        authorInfo <- paste(selectedAuthorAliases[a], 
                            " / ",
                            selectedAuthorProjects[a], 
                            "\n",
                            totalCommitCount,
                            " / ",
                            buggyCommitRatio,
                            sep = "")
        
        # SM
        resultsSM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                                               projectName = selectedAuthorProjects[a],
                                                               authorName = selectedAuthorNames[a],
                                                               modelName = "SM")[, m]
        
        # GM
        resultsGM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                                               projectName = selectedAuthorProjects[a],
                                                               authorName = selectedAuthorNames[a],
                                                               modelName = "GM")[, m]
        
        
        tmpObjString <- paste(tmpObjString, "resultsGM[[", a, "]],resultsSM[[", a, "]],resultsPM[[", a, "]],resultsPM[[", a, "]],", sep = "") 
        tmpColString <- paste(tmpColString, "'cornsilk3','cornsilk3',", authorColor, ",'white',", sep = "")
        tmpInfoString <- paste(tmpInfoString, "'GM','SM','PM','", authorInfo, "',", sep = "")
      }
      
      tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 1)
      tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 1)
      tmpInfoString <- substr(tmpInfoString, 1, nchar(tmpInfoString) - 1)
      
      # write.table(tmpObjString, "tmpObjString.txt")
      # write.table(tmpColString, "tmpColString.txt")
      # write.table(tmpInfoString, "tmpInfoString.txt")
      
      if(m == "pd")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "), names = c(", tmpInfoString, "))", sep =  "")))
      else if(m == "MCC")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(-1.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      else
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      
      # title(xlab = paste(m), cex.lab = 1.7)
    }
    dev.off()
  }
}

if(plot.SomeSelectedDevelopers){
  algorithms <- c("NB", "RF")
  numberOfColours <- 2 #2,7
  numberOfModels <- 3
  metricList <- c("pd", "pf", "F1")
  # metricList <- c("pd", "pf", "precision", "F1", "AUC", "MCC", "BScore")
  
  # selectedAuthorAliases <- c("Dev_2", "Dev_1", "Dev_4", "Dev_18", "Dev_14", "Dev_10", "Dev_18", "Dev_7", "Dev_29", "Dev_4", "Dev_26", "Dev_50")
  # selectedAuthorNames <- c("Tom Lane", "Brett Leslie Porter", "José Valim", "Jesse Vincent", "Jakub Friedl", "William Skaggs", "Kjartan Maraas", "Nick Ing-Simmons", "Shlomi Fish", "Marc G. Fournier", "Andrew White", "Jaime Iniesta")
  # selectedAuthorProjects <- c("PostgreSql", "Maven2", "Rails", "Perl", "Gimp", "Gimp", "Gimp", "Perl", "Perl", "PostgreSql", "Rails", "Rails")
  
  # chosen for RF undersampling results
  # selectedAuthorAliases <- c("Dev_2", "Dev_3", "Dev_10", "Dev_24", "Dev_1", "Dev_7", "Dev_10", "Dev_4", "Dev_8", "Dev_18", "Dev_56", "Dev_78", "Dev_1", "Dev_3")
  # selectedAuthorNames <- c("Michael Natterer", "Martin Nordholts", "William Skaggs", "Adrian Likins", "Brett Leslie Porter", "Nick Ing-Simmons",
  #                          "Steve Hay", "Marc G. Fournier", "Jon Leighton", "Emilio Tagua", "Gonçalo Silva", "Mislav Marohnić", "Igor Bukanov", "Hannes Wallnoefer")
  # selectedAuthorProjects <- c("Gimp", "Gimp", "Gimp", "Gimp", "Maven2", "Perl", "Perl", "PostgreSql", "Rails", "Rails", "Rails", "Rails", "Rhino", "Rhino")
  
  # chosen for NB undersampling results
  selectedAuthorAliases <- c("Dev_6", "Dev_13", "Dev_3", "Dev_6", "Dev_34", "Dev_4", "Dev_4", "Dev_73", "Dev_2")
  selectedAuthorNames <- c("Simon Budig", "Kevin Cozens", "John Dennis Casey", "Gurusamy Sarathy", "Malcolm Beattie", "Marc G. Fournier", "José Valim", "Andreas Scherer", "Norris Boyd")
  selectedAuthorProjects <- c("Gimp", "Gimp", "Maven2", "Perl", "Perl", "PostgreSql", "Rails", "Rails", "Rhino")

  loadAllAuthorsInfo(experimentType)
  
  for(alg in algorithms){
    plotInputDir <- get(paste(alg, "Dir", sep = "_"))
    
    adjustedHeight <- 3 + length(selectedAuthorNames) * 0.5
    pdf(file = paste(plotOutputDir, "SelectedAuthors_", alg, "_", numberOfColours, "Colours_", numberOfModels, "Models_", length(metricList), "Metrics.pdf", sep = ""), width = length(metricList)*3, height = adjustedHeight)
    par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 2, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
    #par(mfrow = c(2, 4), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
    
    for(m in metricList){
      resultsPM <- vector("list", length = length(selectedAuthorNames))
      
      tmpObjString <- ""
      tmpColString <- ""
      tmpInfoString <- ""
      
      for(a in length(selectedAuthorNames):1){
        buggyCommitRatio <- round(allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "BuggyCommitRatio"], 2)
        totalCommitCount <- allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "CommitCount"]
        
        resultsPM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                           projectName = selectedAuthorProjects[a],
                                           authorName = selectedAuthorNames[a],
                                           modelName = "PM")[, m]
        
        authorColor <- getAuthorColorByMetric(plotInputDir,  
                                              projectName = selectedAuthorProjects[a], 
                                              authorName = selectedAuthorNames[a],  
                                              metricName = m, 
                                              numberOfColors = numberOfColours, 
                                              numberOfModels = numberOfModels)
        
        authorInfo <- paste(selectedAuthorAliases[a], 
                            " / ",
                            selectedAuthorProjects[a], 
                            "\n",
                            totalCommitCount,
                            " / ",
                            buggyCommitRatio,
                            sep = "")
        
        tmpObjString <- paste(tmpObjString, "resultsPM[[", a, "]],", sep = "")
        tmpColString <- paste(tmpColString, authorColor, ",", sep = "")
        tmpInfoString <- paste(tmpInfoString, "'", authorInfo, "',", sep = "")
      }
      
      tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 1)
      tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 1)
      tmpInfoString <- substr(tmpInfoString, 1, nchar(tmpInfoString) - 1)
      
      if(m == "pd")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "), names = c(", tmpInfoString, "))", sep =  "")))
      else if(m == "MCC")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(-1.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      else
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      
      # title(xlab = paste(m), cex.lab = 1.7)
      
    }
    dev.off()
  }
}

if(plot.SomeSelectedDevelopers.PMSMGM){
  # algorithms <- c("NB", "RF")
  algorithms <- c("NB")
  numberOfColours <- 2 #2,7
  numberOfModels <- 3
  metricList <- c("pd", "pf", "F1")
  
  # selectedAuthorAliases <- c("Dev_2", "Dev_1", "Dev_4", "Dev_18", "Dev_14", "Dev_10", "Dev_18", "Dev_7", "Dev_29", "Dev_4", "Dev_26", "Dev_50")
  # selectedAuthorNames <- c("Tom Lane", "Brett Leslie Porter", "José Valim", "Jesse Vincent", "Jakub Friedl", "William Skaggs", "Kjartan Maraas", "Nick Ing-Simmons", "Shlomi Fish", "Marc G. Fournier", "Andrew White", "Jaime Iniesta")
  # selectedAuthorProjects <- c("PostgreSql", "Maven2", "Rails", "Perl", "Gimp", "Gimp", "Gimp", "Perl", "Perl", "PostgreSql", "Rails", "Rails")
  
  # chosen for RF undersampling results
  # selectedAuthorAliases <- c("Dev_2", "Dev_3", "Dev_10", "Dev_24", "Dev_1", "Dev_7", "Dev_10", "Dev_4", "Dev_8", "Dev_18", "Dev_56", "Dev_78", "Dev_1", "Dev_3")
  # selectedAuthorNames <- c("Michael Natterer", "Martin Nordholts", "William Skaggs", "Adrian Likins", "Brett Leslie Porter", "Nick Ing-Simmons", 
  #                          "Steve Hay", "Marc G. Fournier", "Jon Leighton", "Emilio Tagua", "Gonçalo Silva", "Mislav Marohnić", "Igor Bukanov", "Hannes Wallnoefer")
  # selectedAuthorProjects <- c("Gimp", "Gimp", "Gimp", "Gimp", "Maven2", "Perl", "Perl", "PostgreSql", "Rails", "Rails", "Rails", "Rails", "Rhino", "Rhino")
  
  # chosen for NB undersampling results
  selectedAuthorAliases <- c("Dev_6", "Dev_13", "Dev_3", "Dev_6", "Dev_34", "Dev_4", "Dev_4", "Dev_73", "Dev_2")
  selectedAuthorNames <- c("Simon Budig", "Kevin Cozens", "John Dennis Casey", "Gurusamy Sarathy", "Malcolm Beattie", "Marc G. Fournier", "José Valim", "Andreas Scherer", "Norris Boyd")
  selectedAuthorProjects <- c("Gimp", "Gimp", "Maven2", "Perl", "Perl", "PostgreSql", "Rails", "Rails", "Rhino")
  
  loadAllAuthorsInfo(experimentType)
  
  for(alg in algorithms){
    plotInputDir <- get(paste(alg, "Dir", sep = "_"))
    
    adjustedHeight <- length(selectedAuthorNames) * 1.7
    pdf(file = paste(plotOutputDir, "SelectedAuthors_", alg, "_", numberOfColours, "Colours_", numberOfModels, "Models_", length(metricList), "Metrics_PMSMGM_v2(NB).pdf", sep = ""), width = length(metricList)*3, height = adjustedHeight)
    par(mfrow = c(1, length(metricList)), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 2, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
    #par(mfrow = c(2, 4), oma = c(5, 10, 3, 0) + 0.3, mar = c(5, 1, 3, 0) + 0.3, cex.lab = 1.5, cex.axis = 1.5)
    
    for(m in metricList){
      resultsPM <- vector("list", length = length(selectedAuthorNames))
      resultsSM <- vector("list", length = length(selectedAuthorNames))
      resultsGM <- vector("list", length = length(selectedAuthorNames))
      
      tmpObjString <- ""
      tmpColString <- ""
      tmpInfoString <- ""
      
      for(a in length(selectedAuthorNames):1){
        buggyCommitRatio <- round(allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "BuggyCommitRatio"], 2)
        totalCommitCount <- allAuthorsInfo[which(allAuthorsInfo$Alias == selectedAuthorAliases[a] & allAuthorsInfo$projectName == selectedAuthorProjects[a]), "CommitCount"]
        
        # PM
        resultsPM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                                               projectName = selectedAuthorProjects[a],
                                                               authorName = selectedAuthorNames[a],
                                                               modelName = "PM")[, m]
        
        authorColor <- getAuthorColorByMetric(plotInputDir,  
                                               projectName = selectedAuthorProjects[a], 
                                               authorName = selectedAuthorNames[a],  
                                               metricName = m, 
                                               numberOfColors = numberOfColours, 
                                               numberOfModels = numberOfModels)
        
        authorInfo <- paste(selectedAuthorAliases[a], 
                            " / ",
                            selectedAuthorProjects[a], 
                            "\n",
                            totalCommitCount,
                            " / ",
                            buggyCommitRatio,
                            sep = "")
        
        # SM
        resultsSM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                           projectName = selectedAuthorProjects[a],
                                           authorName = selectedAuthorNames[a],
                                           modelName = "SM")[, m]
        
        # GM
        resultsGM[[a]] <- getAuthorResults(inputDir = plotInputDir,
                                           projectName = selectedAuthorProjects[a],
                                           authorName = selectedAuthorNames[a],
                                           modelName = "GM")[, m]
        
        
        tmpObjString <- paste(tmpObjString, "resultsGM[[", a, "]],resultsSM[[", a, "]],resultsPM[[", a, "]],resultsPM[[", a, "]],", sep = "")
        tmpColString <- paste(tmpColString, "'cornsilk3','cornsilk3',", authorColor, ",'white',", sep = "")
        tmpInfoString <- paste(tmpInfoString, "'GM','SM','PM','", authorInfo, "',", sep = "")
      }
      
      tmpObjString <- substr(tmpObjString, 1, nchar(tmpObjString) - 1)
      tmpColString <- substr(tmpColString, 1, nchar(tmpColString) - 1)
      tmpInfoString <- substr(tmpInfoString, 1, nchar(tmpInfoString) - 1)
      
      if(m == "pd")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "), names = c(", tmpInfoString, "))", sep =  "")))
      else if(m == "MCC")
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(-1.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      else
        eval(parse(text = paste("boxplot(", tmpObjString, ", las = 2, ylim = c(0.0, 1.0), main = '", m, "', horizontal = TRUE, col = adjustcolor(c(", tmpColString, "), alpha.f = 0.5), staplecol = c(", tmpColString, "), medcol = c(", tmpColString, "), whiskcol = c(", tmpColString, "), boxcol = c(", tmpColString, "), outcol = c(", tmpColString, "))", sep =  "")))
      
      # title(xlab = paste(m), cex.lab = 1.7)
      
    }
    dev.off()
  }
}

if(write.allExp.allAlg.forEachProject){
  algorithms <- c("NB", "RF")
  
  for(functionName in c("median", "mean")){
    performanceValues.Mix.WITH <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) * 3 + 1))
    performanceValues.Mix.WITH[nrow(performanceValues.Mix.WITH) + 1, c(2, 9, 16)] <- c("PM", "SM", "GM")
    performanceValues.Mix.WITH[nrow(performanceValues.Mix.WITH) + 1, ] <- c("Project", rep(metricList, 3))
    
    performanceValues.Mix.WO <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) * 3 + 1))
    performanceValues.Mix.WO[nrow(performanceValues.Mix.WO) + 1, c(2, 9, 16)] <- c("PM", "SM", "GM")
    performanceValues.Mix.WO[nrow(performanceValues.Mix.WO) + 1, ] <- c("Project", rep(metricList, 3))
    
    performanceValues.Seq <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) * 3 + 1))
    performanceValues.Seq[nrow(performanceValues.Seq) + 1, c(2, 9, 16)] <- c("PM", "SM", "GM")
    performanceValues.Seq[nrow(performanceValues.Seq) + 1, ] <- c("Project", rep(metricList, 3))
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectDataOnlyAuthorInfo(experimentType, testSetMonths, samplingOption, projectName)
      
      NB_Dir <- paste(mainProjectDir, "output/", "MixTime_NB/WITH/", sep = "")
      RF_Dir <- paste(mainProjectDir, "output/", "MixTime_RF/WITH/", sep = "")
      ### reading results ##############
      resultsNB_PM <- data.frame()
      resultsNB_SM <- data.frame()
      resultsNB_GM <- data.frame()
      
      resultsRF_PM <- data.frame()
      resultsRF_SM <- data.frame()
      resultsRF_GM <- data.frame()
      
      for(a in 1:length(authorNames)){
        if(experimentType == "MixTime")
          authorName <- authorNames[a]
        else
          authorName <- authorAliases[a]
        
        resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        
        resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      }
      #######################
      performanceValues.Mix.WITH[nrow(performanceValues.Mix.WITH) + 1, 1] <- paste(projectName)
      performanceValues.Mix.WITH[nrow(performanceValues.Mix.WITH), c(2:8)] <- round(apply(rbind(resultsNB_PM[, metricList], resultsRF_PM[, metricList]), 2, functionName, na.rm = T), 3)
      performanceValues.Mix.WITH[nrow(performanceValues.Mix.WITH), c(9:15)] <- round(apply(rbind(resultsNB_SM[, metricList], resultsRF_SM[, metricList]), 2, functionName, na.rm = T), 3)
      performanceValues.Mix.WITH[nrow(performanceValues.Mix.WITH), c(16:22)] <- round(apply(rbind(resultsNB_GM[, metricList], resultsRF_GM[, metricList]), 2, functionName, na.rm = T), 3)
      
      NB_Dir <- paste(mainProjectDir, "output/", "MixTime_NB/WO/", sep = "")
      RF_Dir <- paste(mainProjectDir, "output/", "MixTime_RF/WO/", sep = "")
      ### reading results ##############
      resultsNB_PM <- data.frame()
      resultsNB_SM <- data.frame()
      resultsNB_GM <- data.frame()
      
      resultsRF_PM <- data.frame()
      resultsRF_SM <- data.frame()
      resultsRF_GM <- data.frame()
      
      for(a in 1:length(authorNames)){
        if(experimentType == "MixTime")
          authorName <- authorNames[a]
        else
          authorName <- authorAliases[a]
        
        resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        
        resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      }
      #######################
      performanceValues.Mix.WO[nrow(performanceValues.Mix.WO) + 1, 1] <- paste(projectName)
      performanceValues.Mix.WO[nrow(performanceValues.Mix.WO), c(2:8)] <- round(apply(rbind(resultsNB_PM[, metricList], resultsRF_PM[, metricList]), 2, functionName, na.rm = T), 3)
      performanceValues.Mix.WO[nrow(performanceValues.Mix.WO), c(9:15)] <- round(apply(rbind(resultsNB_SM[, metricList], resultsRF_SM[, metricList]), 2, functionName, na.rm = T), 3)
      performanceValues.Mix.WO[nrow(performanceValues.Mix.WO), c(16:22)] <- round(apply(rbind(resultsNB_GM[, metricList], resultsRF_GM[, metricList]), 2, functionName, na.rm = T), 3)
      
      
      loadProjectDataOnlyAuthorInfo("SeqTime", "6", "", projectName)
      
      NB_Dir <- paste(mainProjectDir, "output/", "SeqTime_NB/", "6Months/", "MaxTrainCommit", maxCommitCountInTrainSet, "/", sep = "")
      RF_Dir <- paste(mainProjectDir, "output/", "SeqTime_RF/", "6Months/", "MaxTrainCommit", maxCommitCountInTrainSet, "/", sep = "")
      ### reading results ##############
      resultsNB_PM <- data.frame()
      resultsNB_SM <- data.frame()
      resultsNB_GM <- data.frame()
      
      resultsRF_PM <- data.frame()
      resultsRF_SM <- data.frame()
      resultsRF_GM <- data.frame()
      
      for(a in 1:length(authorNames)){
        authorName <- authorAliases[a]
        
        resultsNB_PM <- rbind(resultsNB_PM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsNB_SM <- rbind(resultsNB_SM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsNB_GM <- rbind(resultsNB_GM, read.csv(file = paste(NB_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
        
        resultsRF_PM <- rbind(resultsRF_PM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsPM.csv", sep = ""), header = TRUE))
        resultsRF_SM <- rbind(resultsRF_SM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsSM.csv", sep = ""), header = TRUE))
        resultsRF_GM <- rbind(resultsRF_GM, read.csv(file = paste(RF_Dir, projectName, "/results/", authorName, "ResultsGM.csv", sep = ""), header = TRUE))
      }
      #######################
      performanceValues.Seq[nrow(performanceValues.Seq) + 1, 1] <- paste(projectName)
      performanceValues.Seq[nrow(performanceValues.Seq), c(2:8)] <- round(apply(rbind(resultsNB_PM[, metricList], resultsRF_PM[, metricList]), 2, functionName, na.rm = T), 3)
      performanceValues.Seq[nrow(performanceValues.Seq), c(9:15)] <- round(apply(rbind(resultsNB_SM[, metricList], resultsRF_SM[, metricList]), 2, functionName, na.rm = T), 3)
      performanceValues.Seq[nrow(performanceValues.Seq), c(16:22)] <- round(apply(rbind(resultsNB_GM[, metricList], resultsRF_GM[, metricList]), 2, functionName, na.rm = T), 3)
    }
    
    xlsx::write.xlsx(performanceValues.Mix.WITH, file = paste(plotOutputDir, "PerformanceResultsForEachProjectAllExp_", functionName, ".xlsx", sep = ""), sheetName = "MixTimeWITH", row.names = F, col.names = F, showNA = FALSE)
    xlsx::write.xlsx(performanceValues.Mix.WO, file = paste(plotOutputDir, "PerformanceResultsForEachProjectAllExp_", functionName, ".xlsx", sep = ""), sheetName = "MixTimeWO", row.names = F, col.names = F, showNA = FALSE, append = TRUE)
    xlsx::write.xlsx(performanceValues.Seq, file = paste(plotOutputDir, "PerformanceResultsForEachProjectAllExp_", functionName, ".xlsx", sep = ""), sheetName = "SeqTime", row.names = F, col.names = F, showNA = FALSE, append = TRUE)
  }
}

if(plot.PMvsTraditional | write.winLoss.PMvsTraditional){
  for(alg in algorithms){
    outExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix,
                                    ifelse(dataBalancingOption == "None", "", paste("_", dataBalancingOption, sep = "")), "_",
                                    ifelse(developerSelectionVersion == "SelectedV2", "DevsV2", "DevsV1"), "_",
                                    alg,
                                    sep = "")
    
    loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature)
    
    ### reading results ##############
    resultsPM <- data.frame()
    resultsSM <- data.frame()
    resultsGM <- data.frame()
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
      
      for(d in 1:numberOfSelectedAuthors){
        authorName <- selectedAuthorNames[d]
        authorAlias <- selectedAuthorAliases[d]
        
        resultsPM <- rbind(resultsPM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsPM.csv", sep = ""), header = TRUE))
        resultsSM <- rbind(resultsSM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsSM.csv", sep = ""), header = TRUE))
        resultsGM <- rbind(resultsGM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsGM.csv", sep = ""), header = TRUE))
      }
    }
    #######################
    
    if(plot.PMvsTraditional){
      pdf(file = paste(plotOutputDir, "PmVsTraditional.pdf", sep = ""), width = 25, height = 5)
      par(mfrow = c(1, length(metricList)), mar = c(3, 3, 3, 2), oma = c(1, 3, 2, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
      
      for(m in metricList){
        if(m == "MCC")
          boxplot(resultsPM[, m], resultsSM[, m], resultsGM[, m], ylim = c(-1.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
        else
          boxplot(resultsPM[, m], resultsSM[, m], resultsGM[, m], ylim = c(0.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
        title(m)
      }
      
      dev.off()
    }
    
    if(write.winLoss.PMvsTraditional){
      comparisonCount <- 2
      effectSizeTable <- winLossTable <- data.frame(matrix(nrow = 1 + length(metricList), ncol = comparisonCount))
      rownames(effectSizeTable) <- rownames(winLossTable) <- c(" ", metricList)
      colnames(effectSizeTable) <- colnames(winLossTable) <- rep(alg, 1, each = 2)
      
      # offSet <- ifelse(alg == algorithms[1], 1, (1 + comparisonCount))
      offSet <- 1 #cunku kod degisti ve bu kisimda sadece bir adet algoritma sonucunu yazdiriyorum, offSet 1 yapmak yeterli
      
      effectSizeTable[1, offSet] <- winLossTable[1, offSet] <- "PM vs SM"
      effectSizeTable[1, offSet + 1] <- winLossTable[1, offSet + 1] <- "PM vs GM"
      
      for(m in metricList){
        if(m == "pf") 
          higherIsBetter <- 0
        else
          higherIsBetter <- 1
        
        #PM vs SM ##########
        colPM <- resultsPM[, m]
        colSM <- resultsSM[, m]
        
        colSM[is.na(colSM)] <- 0
        colPM[is.na(colPM)] <- 0
        
        friedman.test(cbind(colPM, colSM))
        nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colSM))
        
        effectSizeResult <- cohen.d(c(colPM, colSM), rep(c("PM","SM"), each = length(colPM)), hedges.correction=TRUE)
        effectSizeTable[m, offSet] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
        
        if(nemenyiResult$p.value < pVal)
          if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colSM))
            winLossTable[m, offSet] <- if(higherIsBetter) "PM" else "SM"
        else
          winLossTable[m, offSet] <- if(higherIsBetter) "SM" else "PM"
        else
          winLossTable[m, offSet] <- "-"
        #############
        
        #PM vs GM ##########
        colPM <- resultsPM[, m]
        colGM <- resultsGM[, m]
        
        colPM[is.na(colPM)] <- 0
        colGM[is.na(colGM)] <- 0
        
        friedman.test(cbind(colPM, colGM))
        nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colGM))
        
        effectSizeResult <- cohen.d(c(colPM, colGM), rep(c("PM","GM"), each = length(colPM)), hedges.correction=TRUE)
        effectSizeTable[m, offSet + 1] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
        
        if(nemenyiResult$p.value < pVal)
          if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colGM))
            winLossTable[m, offSet + 1] <- if(higherIsBetter) "PM" else "GM"
        else
          winLossTable[m, offSet + 1] <- if(higherIsBetter) "GM" else "PM"
        else
          winLossTable[m, offSet + 1] <- "-"
        #############
      }
      xlsx::write.xlsx(winLossTable, file = paste(plotOutputDir, "PMvsTraditional.xlsx", sep = ""), sheetName = "win-lose", row.names = T, col.names = T, showNA = FALSE)
      xlsx::write.xlsx(effectSizeTable, file = paste(plotOutputDir, "PMvsTraditional.xlsx", sep = ""), sheetName = "effect size", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
    }
  }
}

if(plot.3Models.forEachProject || write.3Models.forEachProject || write.winLoss.PMvsTraditional.forEachProject){
  for(alg in algorithms){
    outExperimentSignature <- paste(experimentType, "_", devsDataSizeOption.Mix,
                                    ifelse(dataBalancingOption == "None", "", paste("_", dataBalancingOption, sep = "")), "_",
                                    ifelse(developerSelectionVersion == "SelectedV2", "DevsV2", "DevsV1"), "_",
                                    alg,
                                    sep = "")
    
    loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature)
    
    dir.create(paste(plotOutputDir, "ForEachProject/", sep = ""), showWarnings = F, recursive = T)
    
    if(write.3Models.forEachProject){
      performanceValuesMedian <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) * 3 + 1))
      performanceValuesMedian[nrow(performanceValuesMedian) + 1, c(2, 9, 16)] <- c("PM", "SM", "GM")
      performanceValuesMedian[nrow(performanceValuesMedian) + 1, ] <- c("Project", rep(metricList, 3))
      
      performanceValuesMean <- as.data.frame(matrix(nrow = 0, ncol = length(metricList) * 3 + 1))
      performanceValuesMean[nrow(performanceValuesMean) + 1, c(2, 9, 16)] <- c("PM", "SM", "GM")
      performanceValuesMean[nrow(performanceValuesMean) + 1, ] <- c("Project", rep(metricList, 3))
    }
    
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectAuthorsData(experimentType, projectName, developerSelectionVersion)
      
      ### reading results ##############
      resultsPM <- data.frame()
      resultsSM <- data.frame()
      resultsGM <- data.frame()
      
      for(d in 1:numberOfSelectedAuthors){
        authorName <- selectedAuthorNames[d]
        authorAlias <- selectedAuthorAliases[d]
        
        resultsPM <- rbind(resultsPM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsPM.csv", sep = ""), header = TRUE))
        resultsSM <- rbind(resultsSM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsSM.csv", sep = ""), header = TRUE))
        resultsGM <- rbind(resultsGM, read.csv(file = paste(get(paste(alg, "_Dir", sep = "")), projectName, "/results/", authorAlias, "_ResultsGM.csv", sep = ""), header = TRUE))
      }
      #######################
      
      if(write.3Models.forEachProject){
        performanceValuesMedian[nrow(performanceValuesMedian) + 1, 1] <- paste(projectName, alg)
        performanceValuesMedian[nrow(performanceValuesMedian), c(2:8)] <- round(apply(resultsPM[, metricList], 2, median, na.rm = T), 3)
        performanceValuesMedian[nrow(performanceValuesMedian), c(9:15)] <- round(apply(resultsSM[, metricList], 2, median, na.rm = T), 3)
        performanceValuesMedian[nrow(performanceValuesMedian), c(16:22)] <- round(apply(resultsGM[, metricList], 2, median, na.rm = T), 3)
        
        performanceValuesMean[nrow(performanceValuesMean) + 1, 1] <- paste(projectName, alg)
        performanceValuesMean[nrow(performanceValuesMean), c(2:8)] <- round(apply(resultsPM[, metricList], 2, mean, na.rm = T), 3)
        performanceValuesMean[nrow(performanceValuesMean), c(9:15)] <- round(apply(resultsSM[, metricList], 2, mean, na.rm = T), 3)
        performanceValuesMean[nrow(performanceValuesMean), c(16:22)] <- round(apply(resultsGM[, metricList], 2, mean, na.rm = T), 3)
      }
      
      if(plot.3Models.forEachProject){
        pdf(file = paste(plotOutputDir, "ForEachProject/", projectName, "_PmVsTraditional.pdf", sep = ""), width = 25, height = 5)
        par(mfrow = c(1, length(metricList)), mar = c(3, 3, 3, 2), oma = c(1, 3, 2, 1))
        
        for(m in metricList){
          if(m == "MCC")
            boxplot(resultsPM[, m], resultsSM[, m], resultsGM[, m], ylim = c(-1.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
          else
            boxplot(resultsPM[, m], resultsSM[, m], resultsGM[, m], ylim = c(0.0, 1.0), cex.axis = 2, cex.lab = 2, names = c("PM", "SM", "GM"))
          title(m)
        }
        
        dev.off()
      }
      
      if(write.winLoss.PMvsTraditional.forEachProject){
        comparisonCount <- 2
        effectSizeTable <- winLossTable <- data.frame(matrix(nrow = 1 + length(metricList), ncol = comparisonCount))
        rownames(effectSizeTable) <- rownames(winLossTable) <- c(" ", metricList)
        colnames(effectSizeTable) <- colnames(winLossTable) <- rep(alg , 1, each = 2)
        
        # offSet <- ifelse(alg == algorithms[1], 1, (1 + comparisonCount))
        offSet <- 1
        
        effectSizeTable[1, offSet] <- winLossTable[1, offSet] <- "PM vs SM"
        effectSizeTable[1, offSet + 1] <- winLossTable[1, offSet + 1] <- "PM vs GM"
        
        for(m in metricList){
          if(m == "pf") 
            higherIsBetter <- 0
          else
            higherIsBetter <- 1
          
          #PM vs SM ##########
          colPM <- resultsPM[, m]
          colSM <- resultsSM[, m]
          
          colSM[is.na(colSM)] <- 0
          colPM[is.na(colPM)] <- 0
          
          friedman.test(cbind(colPM, colSM))
          nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colSM))
          
          effectSizeResult <- cohen.d(c(colPM, colSM), rep(c("PM","SM"), each = length(colPM)), hedges.correction=TRUE)
          effectSizeTable[m, offSet] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
          
          if(nemenyiResult$p.value < pVal)
            if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colSM))
              winLossTable[m, offSet] <- if(higherIsBetter) "PM" else "SM"
          else
            winLossTable[m, offSet] <- if(higherIsBetter) "SM" else "PM"
          else
            winLossTable[m, offSet] <- "-"
          #############
          
          #PM vs GM ##########
          colPM <- resultsPM[, m]
          colGM <- resultsGM[, m]
          
          colPM[is.na(colPM)] <- 0
          colGM[is.na(colGM)] <- 0
          
          friedman.test(cbind(colPM, colGM))
          nemenyiResult <- posthoc.friedman.nemenyi.test(cbind(colPM, colGM))
          
          effectSizeResult <- cohen.d(c(colPM, colGM), rep(c("PM","GM"), each = length(colPM)), hedges.correction=TRUE)
          effectSizeTable[m, offSet + 1] <- paste(round(effectSizeResult$estimate, 3), effectSizeResult$magnitude)
          
          if(nemenyiResult$p.value < pVal)
            if(medianChangeNaToZero(colPM) > medianChangeNaToZero(colGM))
              winLossTable[m, offSet + 1] <- if(higherIsBetter) "PM" else "GM"
          else
            winLossTable[m, offSet + 1] <- if(higherIsBetter) "GM" else "PM"
          else
            winLossTable[m, offSet + 1] <- "-"
          #############
        }
        
        xlsx::write.xlsx(winLossTable, file = paste(plotOutputDir, "ForEachProject/", projectName, "_PMvsTraditional.xlsx", sep = ""), sheetName = "win-lose", row.names = T, col.names = T, showNA = FALSE)
        xlsx::write.xlsx(effectSizeTable, file = paste(plotOutputDir, "ForEachProject/", projectName, "_PMvsTraditional.xlsx", sep = ""), sheetName = "effect size", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
      }
    }
    
    if(write.3Models.forEachProject){
      xlsx::write.xlsx(performanceValuesMedian, file = paste(plotOutputDir, "ForEachProject/", "PerformanceValuesMedian.xlsx", sep = ""), sheetName = "median", row.names = T, col.names = T, showNA = FALSE)
      xlsx::write.xlsx(performanceValuesMean, file = paste(plotOutputDir, "ForEachProject/", "PerformanceValuesMean.xlsx", sep = ""), sheetName = "mean", row.names = T, col.names = T, showNA = FALSE, append = TRUE)
    }
  }
}

