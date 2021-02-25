library(xlsx)
library(Hmisc)
library(ScottKnottESD)

# library(openxlsx)
# detach("package:openxlsx", unload = TRUE)

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime" #SequentialTime, MixTime
baseExperimentSignature <- "MixTime"
experimentSignature <- "MixTime_CorrelationAnalysis"
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

correlation <- FALSE
variableClustering <- FALSE

columnsToBeRemoved <- c(1,2,3,4,5,9)


if(correlation){
  dir.create(path = paste(plotOutputDir, samplingOption, "/correlation/", sep = ""), showWarnings = FALSE, recursive = TRUE)
  
  if(experimentType == "MixTime"){
    for(p in 1:length(projectList)){
      projectName <- projectList[p]
      loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
      
      gmChanges <- get(paste(projectName, "Changes", sep = ""))
      
      # gmMetricValues <- gmChanges[, processMetrics]
      # cormat <- cor(gmMetricValues)
      # melted_cormat <- melt(cormat)
      # 
      # pdf(file = paste(plotOutputDir, samplingOption, "/correlation/", projectName, "_GM.pdf", sep = ""), width = 10, height = 10)
      # par(mfrow = c(1, 1), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
      # 
      # ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
      #   geom_tile()
      # 
      # dev.off()
      
      for(a in 1:numberOfSelectedDev){
        authorName <- authorNames[a]
        pmChanges <- gmChanges[gmChanges$AUTHOR_NAME == authorName, ]
        
        pmMetricValues <- pmChanges[, processMetrics]
        cormat <- cor(pmMetricValues)
        melted_cormat <- melt(cormat)
        
        pdf(file = paste(plotOutputDir, samplingOption, "/correlation/", projectName, "_", authorName, ".pdf", sep = ""), width = 10, height = 10)
        par(mfrow = c(1, 1), mar = c(6, 5, 2, 2), oma = c(1, 1, 1, 1)) #, cex.lab = 1.5, cex.axis = 1.5)
        
        ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
          geom_tile()
        
        dev.off()
      }
    }
  }
}

if(variableClustering){
  dir.create(paste(plotOutputDir, samplingOption, sep = ""), showWarnings = F, recursive = T)
  
  for(p in 1:length(projectList)){
    projectName <- projectList[p]
    loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
    
    gmChanges <- get(paste(projectName, "Changes", sep = ""))
    smChanges <- data.frame()
    
    for(a in 1:numberOfSelectedDev){
      authorName <- authorNames[a]
      
      pmChanges <- gmChanges[gmChanges$AUTHOR_NAME == authorName, ]
      smChanges <- rbind(smChanges, pmChanges)
      
      # PM
      pmMetrics <- pmChanges[, processMetrics]
      
      pdf(file = paste(plotOutputDir, samplingOption, "/", projectName, "_", authorName, ".pdf", sep = ""), width = 11)     
      par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
      v <- varclus(as.matrix(pmMetrics), similarity = "spear") # spearman is the default anyway
      print(round(v$sim, 2))
      plot(v)
      dev.off()
      
      
      
      # df2 = cor(pmMetrics)
      # hc = findCorrelation(df2, cutoff=0.7) # putt any value as a "cutoff" 
      # hc = sort(hc)
      # reduced_Data = pmMetrics[,-c(hc)]
      # print (reduced_Data)
    }
    
    # SM
    smMetrics <- smChanges[, processMetrics] 
    
    pdf(file = paste(plotOutputDir, samplingOption, "/", projectName, "_", "SM", ".pdf", sep = ""), width = 11)     
    par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
    v <- varclus(as.matrix(smMetrics), similarity = "spear") # spearman is the default anyway
    print(round(v$sim,2))
    plot(v)
    dev.off()
    
    # GM
    gmMetrics <- gmChanges[, processMetrics] 
    
    pdf(file = paste(plotOutputDir, samplingOption, "/", projectName, "_", "GM", ".pdf", sep = ""), width = 11)     
    par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
    v <- varclus(as.matrix(gmMetrics), similarity = "spear") # spearman is the default anyway
    print(round(v$sim,2))
    plot(v)
    dev.off()
    
  }
  
}

