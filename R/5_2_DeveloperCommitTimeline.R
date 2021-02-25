library(ggplot2)

rm(list = ls()) #clean the environment

mainProjectDir <- "/Users/beken/Projects/DP_OS/"
experimentType <- "MixTime"
baseExperimentSignature <- "MixTime" #"MixTime_UnderSamp", "MixTime"
samplingOption <- "WO"
testSetMonths <- ""

setwd(mainProjectDir)

source("./R/0_Load.R")
source("./R/0_GeneralFunctions.R")

outExperimentSignature <- "DevCommitTimeline"

loadPlotEnvironment(mainProjectDir, experimentType, baseExperimentSignature, outExperimentSignature, samplingOption)


shortMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
returnAsDate <- function(date){
  # date <- "Wed Feb 1 12:48:12 2017 +0300"
  year <- unlist(strsplit(date, " ", fixed = TRUE))[5]
  month <- which(shortMonths %in% unlist(strsplit(date, " ", fixed = TRUE))[2])
  dayOfMonth <- unlist(strsplit(date, " ", fixed = TRUE))[3]
  time <- unlist(strsplit(date, " ", fixed = TRUE))[4]
  hour <- strsplit(time, ":", fixed = TRUE)[[1]][1]
  min <- strsplit(time, ":", fixed = TRUE)[[1]][2]
  sec <- strsplit(time, ":", fixed = TRUE)[[1]][3]
  
  #ISOdatetime(Year, Month, Day, Hour, Min, Sec)
  dateTime <- ISOdatetime(year, month, dayOfMonth, hour, min, sec, tz="UTC")
  
  return (dateTime)
}

plotPdf <- 1

for(p in 1:length(projectList)){
  projectName <- projectList[p]
  loadProjectData(experimentType, testSetMonths, samplingOption, projectName)
  
  allAuthors <- xlsx::read.xlsx(file = paste(finalDataDir, selectedAuthorsMixTimeName, sep = ""), sheetName = projectName)
  
  shortenedCommits <- get(paste(projectName, "Changes", sep = ""))[, c("AUTHOR_NAME", "AUTHOR_DATE", "CHURN", "NDEV", "Ent", "CLASS")]
  shortenedCommits <- shortenedCommits[shortenedCommits$AUTHOR_NAME %in% authorNames, ]
  shortenedCommits <- shortenedCommits[rev(order(match(shortenedCommits$AUTHOR_NAME, orderedAuthorList))),]
  colnames(shortenedCommits)[ncol(shortenedCommits)] <- "Buggy"
  shortenedCommits[, "NEW_DATE"] <- 0
  class(shortenedCommits[, "NEW_DATE"]) <- "Date"
  
  for(t in 1:nrow(shortenedCommits)){
    shortenedCommits[t , "NEW_DATE"] <- returnAsDate(as.character(shortenedCommits[t, "AUTHOR_DATE"]))
    # shortenedCommits[t, "SELECTED"] <- allAuthors[which(allAuthors$Author == shortenedCommits[t, "AUTHOR_NAME"]), "Selected"]
    shortenedCommits[t, "AUTHOR_INFO"] <- paste(shortenedCommits[t, "AUTHOR_NAME"], 
                                                "(",
                                                allAuthors[which(allAuthors$Author == as.character(shortenedCommits[t, "AUTHOR_NAME"])), "CommitCount"], 
                                                "/",
                                                round(allAuthors[which(allAuthors$Author == as.character(shortenedCommits[t, "AUTHOR_NAME"])), "BuggyCommitRatio"], 2), 
                                                ")",
                                                sep = "")
    shortenedCommits[t, "COMMIT_COUNT"] <- allAuthors[which(allAuthors$Author == as.character(shortenedCommits[t, "AUTHOR_NAME"])), "CommitCount"]
  }
  

  pdf(file = paste(plotOutputDir, projectName, ".pdf", sep = ""), width = 40, height = length(unique(shortenedCommits$AUTHOR_NAME)))
  fill = c("steelblue", "yellowgreen")
  
  # ggplot(shortenedCommits, aes(y = AUTHOR_INFO, x = NEW_DATE, size = m, color = Buggy)) + geom_point() + ggtitle("Developer commit counts by time") + labs(x = "Date", y = "Author (commit / buggy commit)") + scale_fill_manual(values = fill) 
  ggplot(shortenedCommits, aes(y = AUTHOR_INFO, x = NEW_DATE, color = Buggy)) + geom_point() + ggtitle("Developer commit counts by time") + labs(x = "Date", y = "Author (commit / buggy commit)") + scale_fill_manual(values = fill) 
  #+ geom_point(data=shortenedCommits[shortenedCommits$SELECTED == "T",], color="red")
  #+ theme(axis.text.y=element_text(colour=ifelse(levels(shortenedCommits$SELECTED) == 'yellow', 'red', 'blue')))
  
  dev.off() 
}

