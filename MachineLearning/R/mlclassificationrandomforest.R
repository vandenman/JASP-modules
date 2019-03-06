#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

MLClassificationRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Random Forest Classification"
  
  # Read dataset
  if (options$target == "") options$target <- NULL
  dataset <- .readDataSetToEnd(columns.as.factor = options$target, columns = options$predictors,
                               columns.as.nommnomm = options$indicator)
  
  # Check if results can be computed
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .classRanForErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) classRanForResults <- .classRanForComputeResults(jaspResults, dataset, options)
  
  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .classRanForTable(                jaspResults, options, classRanForResults, ready)
  .classRanForApplyTable(           jaspResults, options, classRanForResults, ready)
  # .classRanForClassificationTable(  jaspResults, options, classRanForResults, ready)
  .classRanForTableVarImportance(   jaspResults, options, classRanForResults, ready)
  .classRanForContainerPlots(       jaspResults, options, classRanForResults, ready)
  .classRanForPlotVarImp(           jaspResults, options, classRanForResults, ready)
  .classRanForPlotTreesVsModelError(jaspResults, options, classRanForResults, ready)
  
  return()
}

# Check for errors
.classRanForErrorHandling <- function(dataset, options) {
  
  # Error Check 1: 0 observations for the target variable
  .hasErrors(
    dataset = dataset, 
    perform = "run", 
    type = c('observations', 'variance', 'infinity'),
    all.target = options$target,
    observations.amount = '< 1',
    exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: Apply indicator should not have any missing values (consist of 0s and 1s)
  .hasErrors(
    dataset = dataset, 
    perform = "run", 
    type = c('observations', 'variance', 'infinity'),
    all.target = options$indicator,
    observations.amount = nrow(dataset),
    exitAnalysisIfErrors = TRUE)
  
}

# Compute results
.classRanForComputeResults <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["stateRegRanForResults"]])) return (jaspResults[["stateRegRanForResults"]]$object)
  
  # Create results object
  results <- list()
  
  results[["spec"]] <- .classRanForCalcSpecs(dataset, options)
  results[["res"]] <- list()
  
  # Set seed	
  if (options$seedBox == "manual") set.seed(options$seed) else set.seed(Sys.time())
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs
  if (sum(is.na(dataset)) > 0) {
    
    # If a predictor column consists of only NAs, exclude that column entirely
    for (predictor in preds) {
      if(sum(is.na(dataset[, predictor])) == nrow(dataset)) {
        preds <- preds[-predictor]
      } 
    }
    
    # Option: apply na.roughfix or otherwise apply the default: na.omit (removes all rows that contain NA values)
    if (options$missingValues == "roughfix") {
      dataset <- randomForest::na.roughfix(dataset)
    } else {
      dataset <- na.omit(dataset)
    }
    
  }
  
  # Splitting the data into training set, test set, and application set
  if (options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1) # apply indicator doesn't work yet
    idxModel <- which(dataset[, indicator] == 0) # something goes wrong with making these indices
    
    applyData <- dataset[idxApply, preds, drop = FALSE]
    modelData <- dataset[idxModel, -indicator]
    
  } else {
    
    modelData <- dataset
    
  }
  
  idxTrain <- sample(1:nrow(modelData), floor(results$spec$dataTrainingModel * nrow(modelData)))
  idxTest <- (1:nrow(modelData))[-idxTrain]
  
  xTrain <- modelData[idxTrain, preds, drop = FALSE]
  yTrain <- modelData[idxTrain, target]
  xTest <- modelData[idxTest, preds, drop = FALSE]
  yTest <- modelData[idxTest, target]
  
  # Run Random Forest
  results[["res"]] <- randomForest::randomForest(x = xTrain, y = yTrain, xtest = xTest, ytest = yTest,
                                                 ntree = results$spec$noOfTrees, mtry = results$spec$noOfPredictors,
                                                 sampsize = results$spec$dataBootstrapModel, importance = TRUE,
                                                 keep.forest = TRUE)
  
  results[["data"]] <- list(xTrain = xTrain, yTrain = yTrain, xTest = xTest, yTest = yTest)
  
  # Making a variable importance table
  results[["varImp"]] <- plyr::arrange(data.frame(
    Variable = .unv(as.factor(names(results$res$importance[,1]))),
    MeanIncrMSE = results$res$importance[, 1],
    TotalDecrNodeImp = results$res$importance[, 2],
    Stan = apply(results$res$importance, 2, scale)[, 1] + apply(results$res$importance, 2, scale)[,2]
  ), -Stan)
  
  if(options$indicator != "") results[["apply"]] <- predict(results$res, applyData, type = "class")
  
  # Save results to state
  jaspResults[["stateclassRanForResults"]] <- createJaspState(results)
  jaspResults[["stateclassRanForResults"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees",
                                                           "numberOfTrees", "noOfPredictors", "numberOfPredictors",
                                                           "dataTrainingModel", "percentageDataTraining",
                                                           "dataBootstrapModel", "percentageDataBootstrap",
                                                           "seedBox", "seed", "missingValues"))
  
  return(results)
}

.classRanForCalcSpecs <- function(dataset, options) {
  specs <- list()
  
  # Setting the number of trees
  if (options$noOfTrees == "manual") {
    specs$noOfTrees <- as.integer(options$numberOfTrees)
  } else {
    specs$noOfTrees <- 500
  }
  
  # Setting the number of variables considered at each split
  if (options$noOfPredictors == "manual") {
    specs$noOfPredictors <- as.integer(options$numberOfPredictors)
  } else {
    specs$noOfPredictors <- if (!is.null(options$target) && !is.factor(options$target)) 
      max(floor(length(.v(options$predictors))/3), 1) else floor(sqrt(length(.v(options$predictors))))
  }
  
  # What percentage of the data should be used for training?
  if (options$dataTrain == "manual") {
    specs$dataTrainingModel <- options$percentageDataTraining
  } else {
    specs$dataTrainingModel <- .8
  }		
  
  # What percentage of the training data should be used for bootstrapping?
  if (options$dataBootstrapModel == "manual") {
    specs$dataBootstrapModel <- options$percentageDataBootstrap
  } else {
    specs$dataBootstrapModel <- ceiling(.5 * nrow(dataset))
  }
  
  return(specs)
}

# Output functions
.classRanForTable <- function(jaspResults, options, classRanForResults, ready) {
  if (!is.null(jaspResults[["classRanForTable"]])) return()
  
  # Create table and bind to jaspResults
  classRanForTable <- createJaspTable(title = "Random Forest Classification Model Summary")
  jaspResults[["classRanForTable"]] <- classRanForTable
  jaspResults[["classRanForTable"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees",
                                                      "numberOfTrees", "noOfPredictors", "numberOfPredictors",
                                                      "dataTrainingModel", "percentageDataTraining",
                                                      "dataBootstrapModel", "percentageDataBootstrap", "seedBox",
                                                      "seed", "missingValues", "tableVariableImportance"))
  
  # Add column info
  if(options$dataTrainingModel == "auto" || options$percentageDataTraining < 1){
    classRanForTable$addColumnInfo(name = "testError",  title = "Test Set Error", type = "number", format = "sf:4")
  }
  classRanForTable$addColumnInfo(name = "oobError",  title = "OOB Error", type = "number", format = "sf:4")
  classRanForTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  classRanForTable$addColumnInfo(name = "mtry",  title = "Variables tried", type = "integer")
  
  # Add data per column
  classRanForTable[["testError"]]  <- if (ready)
    mean(classRanForResults$res$test$predicted == classRanForResults$data$yTest) else "."
  classRanForTable[["oobError"]]  <- if (ready)
    classRanForResults$res$err.rate[length(classRanForResults$res$err.rate)] else "."
  classRanForTable[["ntrees"]]  <- if (ready) classRanForResults$res$ntree else "."
  classRanForTable[["mtry"]]  <- if (ready) classRanForResults$res$mtry else "."
  
}

.classRanForApplyTable <- function(jaspResults, options, classRanForResults, ready) {
  if (!is.null(jaspResults[["classRanForApplyTable"]])) return()
  if (options$indicator == "") return()
  
  # Create table and bind to jaspResults
  classRanForApplyTable <- createJaspTable(title = "Random Forest Model Predictions")
  jaspResults[["classRanForApplyTable"]] <- classRanForApplyTable
  jaspResults[["classRanForApplyTable"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees", 
                                                         "numberOfTrees", "noOfPredictors", "numberOfPredictors",
                                                         "dataTrainingModel", "percentageDataTraining",
                                                         "dataBootstrapModel", "percentageDataBootstrap", "seedBox",
                                                         "seed", "missingValues", "tableVariableImportance"))
  
  # Add column info
  classRanForApplyTable$addColumnInfo(name = "row",  title = "Row", type = "integer")
  classRanForApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  classRanForApplyTable[["row"]]  <- if (ready) as.numeric(rownames(as.data.frame(classRanForResults$apply))) else "."
  classRanForApplyTable[["pred"]]  <- if (ready) as.numeric(classRanForResults$apply) else "."
  
}

.classRanForTableVarImportance <- function(jaspResults, options, classRanForResults, ready) {
  if (!is.null(jaspResults[["tableVarImp"]])) return()
  if (!options$tableVariableImportance) return()
  
  # Create table
  classRanForTableVarImp <- createJaspTable(title = "Variable Importance")
  jaspResults[["tableVarImp"]] <- classRanForTableVarImp
  jaspResults[["tableVarImp"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees", "numberOfTrees",
                                                 "noOfPredictors", "numberOfPredictors", "dataTrainingModel",
                                                 "percentageDataTraining", "dataBootstrapModel",
                                                 "percentageDataBootstrap", "seedBox", "seed", "missingValues",
                                                 "tableVariableImportance"))
  
  # Add column info
  classRanForTableVarImp$addColumnInfo(name = "predictor",  title = " ", type = "string")
  classRanForTableVarImp$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number",
                                     format = "sf:4")
  classRanForTableVarImp$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity", type = "number",
                                     format = "sf:4")
  
  # Ordering the variables according to their mean decrease in accuracy
  if(ready) varImpOrder <- sort(classRanForResults$res$importance[,1], decr = TRUE, index.return = TRUE)$ix
  
  # Add data per column
  classRanForTableVarImp[["predictor"]]  <- if(ready) 
    .unv(names(classRanForResults$res$importance[varImpOrder, 1])) else "."
  classRanForTableVarImp[["MDiA"]]  <- if(ready) classRanForResults$res$importance[varImpOrder, 1] else "."
  classRanForTableVarImp[["MDiNI"]]  <- if(ready) classRanForResults$res$importance[varImpOrder, 2] else "."
  
}

# .classRanForClassificationTable <- function(jaspResults, options, classRanForResults, ready) {
#   if (!is.null(jaspResults[["tableClassification"]])) return()
#   if (!options$tableClassification) return()
#   
#   # Create table
#   classRanForTableClassification <- createJaspTable(title = "Classification")
#   jaspResults[["tableClassification"]] <- classRanForTableClassification
#   jaspResults[["tableClassification"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees", 
#                                                          "numberOfTrees", "noOfPredictors", "numberOfPredictors",
#                                                          "dataTrainingModel", "percentageDataTraining",
#                                                          "dataBootstrapModel", "percentageDataBootstrap", "seedBox",
#                                                          "seed", "missingValues", "tableVariableImportance"))
#   
#   # Add column info
#   classRanForTableClassification$addColumnInfo(name = "predictor",  title = " ", type = "string")
#   classRanForTableClassification$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number",
#                                                format = "sf:4")
#   classRanForTableClassification$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity",
#                                                type = "number", format = "sf:4")
#   
#   # Ordering the variables according to their mean decrease in accuracy
#   if(ready) varImpOrder <- sort(classRanForResults$res$importance[,1], decr = TRUE, index.return = TRUE)$ix
#   
#   # Add data per column
#   classRanForTableClassification[["predictor"]]  <- if(ready) 
#     .unv(names(classRanForResults$res$importance[varImpOrder, 1])) else "."
#   classRanForTableClassification[["MDiA"]]  <- if(ready) classRanForResults$res$importance[varImpOrder, 1] else "."
#   classRanForTableClassification[["MDiNI"]]  <- if(ready) classRanForResults$res$importance[varImpOrder, 2] else "."
#   
# }

.classRanForContainerPlots <- function(jaspResults, options, classRanForResults, ready) {
  if (!any(options$plotVarImp, options$plotTreesVsModelError, options$plotPredPerformance)) return()
  if (!ready) return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Random Forest Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotVarImp", "plotTreesVsModelError", "plotPredPerformance"))
  }
}

.classRanForPlotVarImp <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotVarImp) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  varImpOrder <- sort(classRanForResults$res$importance[,1], decr = FALSE, index.return = TRUE)$ix
  
  varImp <- dplyr::tibble(
    Variable = .unv(as.factor(names(classRanForResults$res$importance[varImpOrder, 1]))),
    MeanIncrMSE = classRanForResults$res$importance[varImpOrder, 1],
    TotalDecrNodeImp = classRanForResults$res$importance[varImpOrder, 2]
  )
  
  varImpPlot1 <- .classRanForVarImpPlot1Helper(options, classRanForResults, varImp)
  varImpPlot2 <- .classRanForVarImpPlot2Helper(options, classRanForResults, varImp)
  pct[['varImpPlot1']] <- createJaspPlot(plot = varImpPlot1, title = "Mean Decrease in Accuracy per Variable",
                                         width = 400, height = 20 * nrow(varImp) + 60)
  pct[['varImpPlot2']] <- createJaspPlot(plot = varImpPlot2, title = "Total Increase in Node Purity per Variable",
                                         width = 400, height = 20 * nrow(varImp) + 60)
}

.classRanForVarImpPlot1Helper <- function(options, classRanForResults, varImp) {
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImp, ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(
        x = "",
        y = "Mean Decrease in Accuracy"
      ),
    horizontal = TRUE
  )
  
  return(varImpPlot1)
}

.classRanForVarImpPlot2Helper <- function(options, classRanForResults, varImp) {
  
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImp, ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(
        x = "",
        y = "Total Increase in Node Purity"
      ),
    horizontal = TRUE
  )
  
  return(varImpPlot2)
}

.classRanForPlotTreesVsModelError <- function(jaspResults, options, classRanForResults, ready) {
  if (!options$plotTreesVsModelError) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  treesMSE <- dplyr::tibble(
    trees = 1:length(classRanForResults$res$err.rate[,1]),
    error = classRanForResults$res$err.rate[,1]
  )
  
  plotTreesVsModelError <- .classRanForPlotTreesVsModelErrorHelper(options, classRanForResults, treesMSE)
  pct[['plotTreesVsModelError']] <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs MSE",
                                                   width = 400, height = 400)
}

.classRanForPlotTreesVsModelErrorHelper <- function(options, classRanForResults, treesMSE) {
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error)) +
      ggplot2::geom_line(size = 1) +
      # ggplot2::scale_y_continuous(limits = c(0, round(max(ceiling(treesMSE$MSE)), -1))) +
      ggplot2::xlab("Trees") +
      ggplot2::ylab("Error Rate")
  )
  
  return(plotTreesVsModelError)
}
