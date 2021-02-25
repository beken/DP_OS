# Directory & file name structure
# experimentType_devsDataSizeOption.Mix_dataBalancingOption_developerSelectionVersion_algorithm
# Example: MixTime_Neq_UnderSamp_DevV1_NB

# parameters: 
# devsDataSizeOption.Mix <- "Neq" #Neq, Eq
# dataBalancingOption <- "None" # Under, SMOTE

# TrainTest data folder & file name structure examples
# MixTime_Neq_TrainTest/Gimp/Dev_1/Run1/Dev_1_Test1.csv
# MixTime_Neq_TrainTest/Gimp/Dev_1/Run1/Dev_1_TrainGM10.csv


# Directory variables
dataDir <- paste(mainProjectDir, "data/", sep = "")
finalDataDir <- paste(mainProjectDir, "data/final/", sep = "")
resultDataDir <- paste(mainProjectDir, "data/result/", sep = "")
changeDataDir <- paste(finalDataDir, "ChangeData/", sep = "")
processingDataDir <- paste(mainProjectDir, "data/processing/", sep = "")
outputDir <- paste(mainProjectDir, "output/", sep = "")
plotDir <- paste(mainProjectDir, "plots/", sep = "")

# Common variables
dataIsNotExistError <- "dataIsNotExist"

projectList <- c("Gimp", "Maven2", "Perl", "PostgreSql", "Rails", "Rhino")
algorithms <- c("NB", "HyperSMURF") #c("NB", "RF", "HyperSMURF", "XGBoost")
metricList <- c("pd", "pf", "precision", "F1", "AUC", "MCC", "BScore")
processMetrics <- c("ADD", "DEL", "CHANGE_TYPE", "Ent", "ND", "NS","NF", "NDEV", "AGE", "NFC", "EXP", "REXP", "SEXP")
modelTypes <- c("PM", "SM", "GM") #c("PM", "SM", "GM", "WPM")
shortMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# columnsToBeRemoved.Mix <- c(1,2,3,4,5,9)
# columnsToBeRemoved.Seq <- c(1,2,3,4,5,9)
selectedAuthorsFileName.Mix <- "selectedAuthorsMixTime.xlsx" 
selectedAuthorsFileName.Seq <- "selectedAuthorsSeqTime.xlsx"

# MixTime experiment variables
numberOfRuns <- 10
numberOfFolds <- 10

# SeqTime experiment variables
maxCommitCountInTrainSet.Seq <- 55000 #500, 55000
minCommitCountToSelectInterval.Seq <- 50
minBuggyCommitCountToSelectInterval.Seq <- 5






