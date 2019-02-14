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

MLRegressionRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Random Forest Regression"
  
  # Read dataset
  if (options$target == "") options$target <- NULL
  dataset <- .readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors,
                               columns.as.factor = options$indicator)
  
  # Check if results can be computed
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regRanForErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regRanForResults <- .regRanForComputeResults(jaspResults, dataset, options)
  
  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .regRanForTable(                jaspResults, options, regRanForResults, ready)
  .regRanForApplyTable(           jaspResults, options, regRanForResults, ready)
  .regRanForTableVarImportance(   jaspResults, options, regRanForResults, ready)
  .regRanForContainerPlots(       jaspResults, options, regRanForResults, ready)
  .regRanForPlotVarImp(           jaspResults, options, regRanForResults, ready)
  .regRanForPlotTreesVsModelError(jaspResults, options, regRanForResults, ready)
  .regRanForPlotPredPerformance(  jaspResults, options, regRanForResults, ready)
  
  return()
}

# Check for errors
.regRanForErrorHandling <- function(dataset, options) {
  
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
.regRanForComputeResults <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["stateRegRanForResults"]])) return (jaspResults[["stateRegRanForResults"]]$object)
  
  # Create results object
  results <- list()
  
  results[["spec"]] <- .regRanForCalcSpecs(dataset, options)
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
    
    indicatorDotV <- .v(indicator)

    applyData <- dataset[as.logical(dataset[, indicatorDotV]), preds, drop = FALSE]
    modelData <- dataset[-as.logical(dataset[, indicatorDotV]), -indicator, drop = FALSE]

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
  
  if(options$indicator != "") {
    results[["apply"]] <- randomForest::predict.randomForest(results$res, applyData, type = "response")
  } else {
    results[["apply"]] <- NULL
  }
  
  # Save results to state
  jaspResults[["stateregRanForResults"]] <- createJaspState(results)
  jaspResults[["stateregRanForResults"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees",
                                                           "numberOfTrees", "noOfPredictors", "numberOfPredictors",
                                                           "dataTrainingModel", "percentageDataTraining",
                                                           "dataBootstrapModel", "percentageDataBootstrap",
                                                           "seedBox", "seed", "missingValues"))
  
  return(results)
}

.regRanForCalcSpecs <- function(modelData, options) {
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
  if (options$dataTrainingModel == "manual") {
    specs$dataTrainingModel <- options$percentageDataTraining
  } else {
    specs$dataTrainingModel <- .8
  }		
  
  # What percentage of the training data should be used for bootstrapping?
  if (options$dataBootstrapModel == "manual") {
    specs$dataBootstrapModel <- options$percentageDataBootstrap
  } else {
    specs$dataBootstrapModel <- ceiling(.632*nrow(modelData)*specs$dataTrainingModel)
  }
  
  return(specs)
}

# Output functions
.regRanForTable <- function(jaspResults, options, regRanForResults, ready) {
  if (!is.null(jaspResults[["regRanForTable"]])) return()
  
  # Create table and bind to jaspResults
  regRanForTable <- createJaspTable(title = "Random Forest Regression Model Summary")
  jaspResults[["regRanForTable"]] <- regRanForTable
  jaspResults[["regRanForTable"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees", "numberOfTrees", 
                                                    "noOfPredictors", "numberOfPredictors", "dataTrainingModel",
                                                    "percentageDataTraining", "dataBootstrapModel",
                                                    "percentageDataBootstrap", "seedBox", "seed", "missingValues",
                                                    "tableVariableImportance"))
  
  # Add column info
  if(options$dataTrainingModel == "auto" || options$percentageDataTraining < 1){
    regRanForTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", format = "sf:4")
  }
  regRanForTable$addColumnInfo(name = "oobMSE",  title = "OOB MSE", type = "number", format = "sf:4")
  regRanForTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  regRanForTable$addColumnInfo(name = "mtry",  title = "m", type = "integer")
  
  # Add data per column
  regRanForTable[["testMSE"]]  <- if (ready) 
    mean((regRanForResults$res$test$predicted - regRanForResults$data$yTest)^2) else "."
  regRanForTable[["oobMSE"]]  <- if (ready) regRanForResults$res$mse[length(regRanForResults$res$mse)] else "."
  regRanForTable[["ntrees"]]  <- if (ready) regRanForResults$res$ntree else "."
  regRanForTable[["mtry"]]  <- if (ready) regRanForResults$res$mtry else "."
  
}

.regRanForApplyTable <- function(jaspResults, options, regRanForResults, ready) {
  if (!is.null(jaspResults[["regRanForApplyTable"]])) return()
  if (options$indicator == "") return()
  
  # Create table and bind to jaspResults
  regRanForApplyTable <- createJaspTable(title = "Random Forest Model Predictions")
  jaspResults[["regRanForApplyTable"]] <- regRanForApplyTable
  jaspResults[["regRanForApplyTable"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees", 
                                                         "numberOfTrees", "noOfPredictors", "numberOfPredictors",
                                                         "dataTrainingModel", "percentageDataTraining",
                                                         "dataBootstrapModel", "percentageDataBootstrap", "seedBox",
                                                         "seed", "missingValues", "tableVariableImportance"))
  
  # Add column info
  regRanForApplyTable$addColumnInfo(name = "row",  title = "Row", type = "integer")
  regRanForApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  regRanForApplyTable[["row"]]  <- if (ready) as.numeric(rownames(as.data.frame(regRanForResults$apply))) else "."
  regRanForApplyTable[["pred"]]  <- if (ready) as.numeric(regRanForResults$apply) else "."
  
}

.regRanForTableVarImportance <- function(jaspResults, options, regRanForResults, ready) {
  if (!is.null(jaspResults[["tableVarImp"]])) return()
  if (!options$tableVariableImportance) return()
  
  # Create table
  regRanForTableVarImp <- createJaspTable(title = "Variable Importance")
  jaspResults[["tableVarImp"]] <- regRanForTableVarImp
  jaspResults[["tableVarImp"]]$dependOnOptions(c("target", "predictors", "indicator", "noOfTrees", "numberOfTrees",
                                                 "noOfPredictors", "numberOfPredictors", "dataTrainingModel",
                                                 "percentageDataTraining", "dataBootstrapModel",
                                                 "percentageDataBootstrap", "seedBox", "seed", "missingValues",
                                                 "tableVariableImportance"))
  
  # Add column info
  regRanForTableVarImp$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regRanForTableVarImp$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number",
                                     format = "sf:4")
  regRanForTableVarImp$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity", type = "number",
                                     format = "sf:4")
  
  # Ordering the variables according to their mean decrease in accuracy
  if(ready) varImpOrder <- sort(regRanForResults$res$importance[,1], decr = TRUE, index.return = TRUE)$ix
  
  # Add data per column
  regRanForTableVarImp[["predictor"]]  <- if(ready) 
    .unv(names(regRanForResults$res$importance[varImpOrder, 1])) else "."
  regRanForTableVarImp[["MDiA"]]  <- if(ready) regRanForResults$res$importance[varImpOrder, 1] else "."
  regRanForTableVarImp[["MDiNI"]]  <- if(ready) regRanForResults$res$importance[varImpOrder, 2] else "."
  
}

.regRanForContainerPlots <- function(jaspResults, options, regRanForResults, ready) {
  if (!any(options$plotVarImp, options$plotTreesVsModelError, options$plotPredPerformance)) return()
  if (!ready) return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Random Forest Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotVarImp", "plotTreesVsModelError", "plotPredPerformance"))
  }
}

.regRanForPlotVarImp <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotVarImp) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  varImpOrder <- sort(regRanForResults$res$importance[,1], decr = FALSE, index.return = TRUE)$ix
  
  varImp <- dplyr::tibble(
    Variable = .unv(as.factor(names(regRanForResults$res$importance[varImpOrder, 1]))),
    MeanIncrMSE = regRanForResults$res$importance[varImpOrder, 1],
    TotalDecrNodeImp = regRanForResults$res$importance[varImpOrder, 2]
  )
  
  varImpPlot1 <- .regRanForVarImpPlot1Helper(options, regRanForResults, varImp)
  varImpPlot2 <- .regRanForVarImpPlot2Helper(options, regRanForResults, varImp)
  pct[['varImpPlot1']] <- createJaspPlot(plot = varImpPlot1, title = "Mean Decrease in Accuracy per Variable",
                                         width = 400, height = 20 * nrow(varImp) + 60)
  pct[['varImpPlot2']] <- createJaspPlot(plot = varImpPlot2, title = "Total Increase in Node Purity per Variable",
                                         width = 400, height = 20 * nrow(varImp) + 60)
}

.regRanForVarImpPlot1Helper <- function(options, regRanForResults, varImp) {
  
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

.regRanForVarImpPlot2Helper <- function(options, regRanForResults, varImp) {
  
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

.regRanForPlotTreesVsModelError <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotTreesVsModelError) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  treesMSE <- dplyr::tibble(
    trees = 1:length(regRanForResults$res$mse),
    MSE = regRanForResults$res$mse
  )
  
  plotTreesVsModelError <- .regRanForPlotTreesVsModelErrorHelper(options, regRanForResults, treesMSE)
  pct[['plotTreesVsModelError']] <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs MSE",
                                                   width = 400, height = 400)
}

.regRanForPlotTreesVsModelErrorHelper <- function(options, regRanForResults, treesMSE) {
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = MSE)) +
      ggplot2::geom_line(size = 1) +
      # ggplot2::scale_y_continuous(limits = c(0, round(max(ceiling(treesMSE$MSE)), -1))) +
      ggplot2::xlab("Trees")
  )
  
  return(plotTreesVsModelError)
}

.regRanForPlotPredPerformance <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotPredPerformance) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  if(regRanForResults$spec$dataTrainingModel == 1){
    predPerformance <- data.frame(true = regRanForResults$res$predicted, predicted = regRanForResults$data$yTrain) 
  } else {
    predPerformance <- data.frame(true = regRanForResults$res$test$predicted, predicted = regRanForResults$data$yTest) 
  }
  
  plotPredPerformance <- .regRanForPlotPredPerformanceHelper(options, regRanForResults, predPerformance)
  pct[['plotPredPerformance']] <- createJaspPlot(plot = plotPredPerformance, title = "Predictive Performance",
                                                 width = 400, height = 400)
}

.regRanForPlotPredPerformanceHelper <- function(options, regRanForResults, predPerformance) {
  
  limits <- c(0, round(max(c(max(floor(predPerformance$true)), max(floor(predPerformance$predicted)))), -1))
  
  plotPredPerformance <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = predPerformance, mapping = ggplot2::aes(x = true, y = predicted)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_line(data = data.frame(x = limits, y = limits), mapping = ggplot2::aes(x = x, y = y), 
                         col = "darkred", size = 1) +
      ggplot2::scale_x_continuous("True", limits = limits) +
      ggplot2::scale_y_continuous("Predicted", limits = limits)
  )
  
  return(plotPredPerformance)
}
