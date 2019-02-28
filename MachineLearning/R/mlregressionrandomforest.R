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
  dataset <- .regRegReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regRanForErrorHandling(dataset, options)
  
  # Save analysis options in an object so that they don't have to be listed every time
  analysisOptions <- c("target", "predictors", "indicator", "applyModel", "noOfTrees", "numberOfTrees",
                       "noOfPredictors", "numberOfPredictors", "dataTrain", "percentageDataTraining",
                       "dataBootstrapModel", "percentageDataBootstrap", "seedBox", "seed", "missingValues")
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regRanForResults <- .regRanForComputeResults(jaspResults, dataset, options, analysisOptions)
  
  # Output tables
  .regRanForTable(      jaspResults, options, regRanForResults, ready, analysisOptions)
  .regRanForApplyTable( jaspResults, options, regRanForResults, ready, analysisOptions)
  .regRanForVarImpTable(jaspResults, options, regRanForResults, ready, analysisOptions)
  
  # Output plots
  if (ready) .regRanForPlotVarImp(           jaspResults, options, regRanForResults)
  if (ready) .regRanForPlotTreesVsModelError(jaspResults, options, regRanForResults)
  if (ready) .regRanForPlotPredPerformance(  jaspResults, options, regRanForResults)
  
  return()
}

# Read dataset
.regRegReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors,
                            columns.as.factor = options$indicator)
  
  return(data)
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
  
  # Error Check 3: There should be least 2 predictors, otherwise randomForest() complains
  if (length(.v(options$predictors)) < 2L) {
    JASP:::.quitAnalysis("Please provide at least 2 predictors.")
  }
  
}

# Compute results
.regRanForComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateRegRanForResults"]])) return (jaspResults[["stateRegRanForResults"]]$object)
  
  results <- list()
  results[["spec"]] <- .regRanForCalcSpecs(dataset, options)
  
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
  
  idxTrain <- sample(1:nrow(modelData), floor(results$spec$dataTrain * nrow(modelData)))
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
    Stan = apply(results$res$importance, 2, scale)[,1] + apply(results$res$importance, 2, scale)[,2]
  ), -Stan)
  
  # Applying the model
  if(options$indicator != "") {
    results[["apply"]] <- randomForest::predict.randomForest(results$res, applyData, type = "response")
  } else {
    results[["apply"]] <- NULL
  }
  
  # Save results to state
  jaspResults[["stateregRanForResults"]] <- createJaspState(results)
  jaspResults[["stateregRanForResults"]]$dependOnOptions(analysisOptions)
  
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
  if (options$dataTrain == "manual") {
    specs$dataTrain <- options$percentageDataTraining
  } else {
    specs$dataTrain <- .8
  }		
  
  # What percentage of the training data should be used for bootstrapping?
  if (options$dataBootstrapModel == "manual") {
    specs$dataBootstrapModel <- options$percentageDataBootstrap
  } else {
    specs$dataBootstrapModel <- .5
  }
  
  return(specs)
}

.regRanForTable <- function(jaspResults, options, regRanForResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRanForTable"]])) return()
  
  # Create table and bind to jaspResults
  regRanForTable <- createJaspTable(title = "Random Forest Regression Model Summary")
  jaspResults[["regRanForTable"]] <- regRanForTable
  jaspResults[["regRanForTable"]]$dependOnOptions(analysisOptions)
  
  # Add column info
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regRanForTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", format = "sf:4")
  }
  regRanForTable$addColumnInfo(name = "oobMSE",  title = "OOB MSE", type = "number", format = "sf:4")
  regRanForTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  regRanForTable$addColumnInfo(name = "mtry",  title = "m", type = "integer")
  
  # Add data per column
  regRanForTable[["testMSE"]] <- if (ready) 
    mean((regRanForResults$res$test$predicted - regRanForResults$data$yTest)^2) else "."
  regRanForTable[["oobMSE"]]  <- if (ready) regRanForResults$res$mse[length(regRanForResults$res$mse)] else "."
  regRanForTable[["ntrees"]]  <- if (ready) regRanForResults$res$ntree else "."
  regRanForTable[["mtry"]]    <- if (ready) regRanForResults$res$mtry else "."
  
}

.regRanForApplyTable <- function(jaspResults, options, regRanForResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRanForApplyTable"]])) return()
  if (options$indicator == "") return()
  
  # Create table and bind to jaspResults
  regRanForApplyTable <- createJaspTable(title = "Random Forest Model Predictions")
  jaspResults[["regRanForApplyTable"]] <- regRanForApplyTable
  jaspResults[["regRanForApplyTable"]]$dependOnOptions(analysisOptions)
  
  # Add column info
  regRanForApplyTable$addColumnInfo(name = "row",  title = "Row", type = "integer")
  regRanForApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  regRanForApplyTable[["row"]]  <- if (ready) as.numeric(rownames(as.data.frame(regRanForResults$apply))) else "."
  regRanForApplyTable[["pred"]]  <- if (ready) as.numeric(regRanForResults$apply) else "."
  
}

.regRanForVarImpTable <- function(jaspResults, options, regRanForResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRanForVarImpTable"]])) return()
  if (!options$regRanForVarImpTable) return()
  
  # Create table
  regRanForVarImpTable <- createJaspTable(title = "Variable Importance")
  jaspResults[["regRanForVarImpTable"]] <- regRanForVarImpTable
  jaspResults[["regRanForVarImpTable"]]$dependOnOptions(c(analysisOptions, "regRanForVarImpTable"))
  
  # Add column info
  regRanForVarImpTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regRanForVarImpTable$addColumnInfo(name = "acc",  title = "Mean decr. in accuracy", type = "number", format = "sf:4")
  regRanForVarImpTable$addColumnInfo(name = "pur",  title = "Total incr. in node purity", type = "number",
                                     format = "sf:4")
  regRanForVarImpTable$addColumnInfo(name = "stan",  title = "Sum of stand. values", type = "number", format = "sf:4")
  
  # Add data per column
  regRanForVarImpTable[["predictor"]] <- if(ready) as.character(regRanForResults$varImp$Variable)       else "."
  regRanForVarImpTable[["acc"]]       <- if(ready) as.numeric(regRanForResults$varImp$MeanIncrMSE)      else "."
  regRanForVarImpTable[["pur"]]       <- if(ready) as.numeric(regRanForResults$varImp$TotalDecrNodeImp) else "."
  regRanForVarImpTable[["stan"]]      <- if(ready) as.numeric(regRanForResults$varImp$Stan)             else "."
    
}

.regRanForPlotVarImp <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotVarImp) return()
  
  varImpOrder <- sort(regRanForResults$res$importance[,1], decr = FALSE, index.return = TRUE)$ix
  
  varImp <- dplyr::tibble(
    Variable = .unv(as.factor(names(regRanForResults$res$importance[varImpOrder, 1]))),
    MeanIncrMSE = regRanForResults$res$importance[varImpOrder, 1],
    TotalDecrNodeImp = regRanForResults$res$importance[varImpOrder, 2]
  )
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImp, ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(
        x = "",
        y = "Mean Decrease in Accuracy"
      ),
    horizontal = TRUE
  )
  
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImp, ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(
        x = "",
        y = "Total Increase in Node Purity"
      ),
    horizontal = TRUE
  )
  
  varImpPlot1 <- createJaspPlot(plot = varImpPlot1, title = "Mean Decrease in Accuracy per Variable",
                                         width = 400, height = 20 * nrow(varImp) + 60)
  
  varImpPlot2 <- createJaspPlot(plot = varImpPlot2, title = "Total Increase in Node Purity per Variable",
                                         width = 400, height = 20 * nrow(varImp) + 60)
  
  jaspResults[["plotVarImp"]] <- c(varImpPlot1, varImpPlot2)
  jaspResults[["plotVarImp"]]$dependOnOptions(c(analysisOptions, "plotVarImp"))
}

.regRanForPlotTreesVsModelError <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotTreesVsModelError) return()
  
  treesMSE <- dplyr::tibble(
    trees = 1:length(regRanForResults$res$mse),
    MSE = regRanForResults$res$mse
  )
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = MSE)) +
      ggplot2::geom_line(size = 1) +
      # ggplot2::scale_y_continuous(limits = c(0, round(max(ceiling(treesMSE$MSE)), -1))) +
      ggplot2::xlab("Trees") +
      ggplot2::ylab("Mean Squared Error")
  )
  
  plotTreesVsModelError <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs MSE",
                                          width = 400, height = 400)
  
  jaspResults[["plotTreesVsModelError"]] <- plotTreesVsModelError
  jaspResults[["plotTreesVsModelError"]]$dependOnOptions("plotTreesVsModelError")
}

.regRanForPlotPredPerformance <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotPredPerformance) return()
  regRanForResults$data$yTrain
  if(regRanForResults$spec$dataTrain == 1){
    predPerformance <- data.frame(true = regRanForResults$data$yTrain, predicted = regRanForResults$res$predicted) 
  } else {
    predPerformance <- data.frame(true = regRanForResults$data$yTest, predicted = regRanForResults$res$test$predicted) 
  }
  
  limits <- c(0, round(max(c(max(floor(predPerformance$true)), max(floor(predPerformance$predicted)))), -1))
  
  plotPredPerformance <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = predPerformance, mapping = ggplot2::aes(x = true, y = predicted)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_line(data = data.frame(x = limits, y = limits), mapping = ggplot2::aes(x = x, y = y), 
                         col = "darkred", size = 1) +
      ggplot2::scale_x_continuous("True", limits = limits) +
      ggplot2::scale_y_continuous("Predicted", limits = limits)
  )
  
  plotPredPerformance <- createJaspPlot(plot = plotPredPerformance, title = "Predictive Performance",
                                                 width = 400, height = 400)
  
  jaspResults[["plotPredPerformance"]] <- plotPredPerformance
  jaspResults[["plotPredPerformance"]]$dependOnOptions("plotPredPerformance")
}
