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

MLRegressionRegularized <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Regularized Linear Regression"
  
  # Read dataset
  dataset <- .regRegReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .regRegErrorHandling(dataset, options)
  
  # Save analysis options in an object so that they don't have to be listed every time
  analysisOptions <- c("target", "predictors", "indicator", "penalty", "applyModel", "shrinkage", "lambda",
                       "elasticSpec", "alphaElastic", "dataTrain", "percentageDataTraining", "seedBox", "seed", "NAs")
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regRegResults <- .regRegComputeResults(jaspResults, dataset, options, analysisOptions)
  
  # Output containers, tables, and plots based on the results
  .regRegTable(                jaspResults, options, regRegResults, ready, analysisOptions)
  # .regRegRelInfTable(          jaspResults, options, regRegResults, ready, analysisOptions)
  # .regRegApplyTable(           jaspResults, options, regRegResults, ready, analysisOptions)
  # .regRegRelInfPlot(           jaspResults, options, regRegResults, ready, analysisOptions)
  
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

# Error checking
.regRegErrorHandling <- function(dataset, options) {
  
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: Apply indicator should not have any missing values (consist of 0s and 1s)
  if (options$indicator != "") {
    .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
               all.target = options$indicator, observations.amount = nrow(dataset), exitAnalysisIfErrors = TRUE)
  }
  
}

# Compute results
.regRegComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateRegRegResults"]])) return (jaspResults[["stateRegRegResults"]]$object)
  
  # Create results object and add options
  results <- list()
  results[["spec"]] <- .regRegCalcSpecs(dataset, options)
  
  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  if(options$indicator != "") indicator <- which(colnames(dataset) == .v(options$indicator))
  
  # Deal with NAs: apply roughfix or omit NA rows
  if (sum(is.na(dataset)) > 0) {
    
    if (options$applyModel == "applyImpute") {

      idxApply <- which(is.na(dataset[, target]))

      if (options$NAs == "roughfix") {
        predImpute <- randomForest::na.roughfix(dataset[idxApply, preds])
      } else {
        predImpute <- na.omit(dataset[idxApply, preds])
      }

    }
    
    if (options$NAs == "roughfix") dataset <- randomForest::na.roughfix(dataset) else dataset <- na.omit(dataset)
    
  }
  
  # Splitting the data into training set, test set, and application set
  if (options$applyModel == "applyIndicator" && options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1)
    idxModel <- which(dataset[, indicator] == 0)
    
    applyData <- dataset[idxApply, preds, drop = FALSE]
    modelData <- dataset[idxModel, ]
    
    } else {
    
      modelData <- dataset
    
  }
  
  # Set seed	
  if (options$seedBox == "manual") set.seed(options$seed) else set.seed(Sys.time())
  
  # Compile training and test data
  idxTrain <- sample(1:nrow(modelData), floor(results$spec$dataTrain * nrow(modelData)))
  idxTest <- (1:nrow(modelData))[-idxTrain]
  
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  trainPreds <- model.matrix(formula, modelData[idxTrain, c(preds, target), drop = FALSE])[, -1]
  trainTarget <- as.numeric(modelData[idxTrain, target])
  
  testPreds <- model.matrix(formula, modelData[idxTest, c(preds, target), drop = FALSE])[, -1]
  testTarget <- as.numeric(modelData[idxTest, target])
  
  # Run regularized regression
  results[["res"]] <- glmnet::glmnet(trainPreds, trainTarget, family = "gaussian", alpha = 0) # results$spec$alpha
  
  results[["data"]] <- list(trainPreds = trainPreds, trainTarget = trainTarget, 
                            testPreds = testPreds, testTarget = testTarget)
  # results[["relInf"]] <- summary(results$res, plot = FALSE)
  
  # Derive test set predictions and calculate test error rate
  # prob <- gbm::predict.gbm(results$res, newdata = testData, n.trees = results$optTrees, type = "response")
    
  # results[["testError"]] <- mean(testTarget != as.character(results$preds))
  results[["testError"]] <- 0.01
  results[["shrinkage"]] <- results$spec$shrinkage
  
  # Apply model to new data if requested
  if(options$applyModel == "applyIndicator" && options$indicator != "") {
    
    applyProb <- gbm::predict.gbm(results$res, newdata = applyData, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- data.frame(case = idxApply, pred = colnames(applyProb)[apply(applyProb, 1, which.max)])
    
  } else if (options$applyModel == "applyImpute") {

    applyProb <- gbm::predict.gbm(results$res, newdata = predImpute, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- data.frame(case = idxApply, pred = colnames(applyProb)[apply(applyProb, 1, which.max)])
    
  }
  
  # Save results to state
  jaspResults[["stateRegRegResults"]] <- createJaspState(results)
  jaspResults[["stateRegRegResults"]]$dependOnOptions(analysisOptions)
  
  return(results)
}

.regRegCalcSpecs <- function(modelData, options) {
  specs <- list()
  
  # Setting the shrinkage parameter lambda
  if (options$shrinkage == "manual") specs$shrinkage <- options$lambda else specs$shrinkage <- 0.2
  
  # Setting the alpha parameter (0 = Ridge, 1 = Lasso, inbetween = Elastic Net)
  if (options$penalty != "ridge" && options$penalty != "lasso") {
    
    if (options$elasticSpec == "Optimize") {
      specs$alphaElastic <- NULL # tk add optimization for alpha
    } else if (options$elasticSpec == "Manual") {
      specs$alphaElastic <- options$alphaElastic
    } else {
      specs$alphaElastic <- 0.5
    }
    
  }
  
  # Choosing the regularization method
  if (options$penalty == "ridge") {
    specs$alpha <- 0
    specs$method <- "Ridge"
  } else if (options$penalty == "lasso") {
    specs$alpha <- 1
    specs$method <- "Lasso"
  } else {
    specs$alpha <- specs$alphaElastic
    specs$method <- "Elastic Net"
  }
  
  # What percentage of the data should be used for training?
  if (options$dataTrain == "manual") specs$dataTrain <- options$percentageDataTraining else specs$dataTrain <- .8
  
  return(specs)
}

# Output functions
.regRegTable <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRegTable"]])) return()
  
  # Create table and bind to jaspResults
  regRegTable <- createJaspTable(title = "Regularized Linear Regression Model Summary")
  jaspResults[["regRegTable"]] <- regRegTable
  jaspResults[["regRegTable"]]$dependOnOptions(analysisOptions)
  
  regRegTable$position <- 1
  
  # Add column info
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regRegTable$addColumnInfo(name = "testError",  title = "Test Set Error", type = "number", format = "sf:4")
  }
  regRegTable$addColumnInfo(name = "method"   ,  title = "Method"   , type = "string")
  regRegTable$addColumnInfo(name = "shrinkage",  title = "Shrinkage", type = "number", format = "sf:4")
  
  # Add data per column
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regRegTable[["testError"]]  <- if (ready) regRegResults$testError else "."
  }
  regRegTable[["method"]]  <- if (ready) regRegResults$spec$method else "."
  regRegTable[["shrinkage"]]  <- if (ready) regRegResults$shrinkage else "."

}

.regRegRelInfTable <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["tableVarImp"]])) return()
  if (!options$regRegRelInfTable) return()
  
  # Create table
  regRegRelInfTable <- createJaspTable(title = "Relative Influence")
  jaspResults[["regRegRelInfTable"]] <- regRegRelInfTable
  jaspResults[["regRegRelInfTable"]]$dependOnOptions(c(analysisOptions, "regRegRelInfTable"))
  
  regRegRelInfTable$position <- 3
  
  # Add column info
  regRegRelInfTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regRegRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number", format = "sf:4")
  
  # Add data per column
  regRegRelInfTable[["predictor"]]  <- if(ready) .unv(regRegResults$relInf$var) else "."
  regRegRelInfTable[["relIn"]]  <- if(ready) regRegResults$relInf$rel.inf else "."
  
}

.regRegApplyTable <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["applyModel"]])) return()
  if (options$applyModel == "noApp") return()
  
  # Create table and bind to jaspResults
  regRegApplyTable <- createJaspTable(title = "Boosting Model Predictions")
  jaspResults[["regRegApplyTable"]] <- regRegApplyTable
  jaspResults[["regRegApplyTable"]]$dependOnOptions(c(analysisOptions, "applyModel"))
  
  regRegApplyTable$position <- 4
  
  # Add column info
  regRegApplyTable$addColumnInfo(name = "case",  title = "Case", type = "integer")
  regRegApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "string")
  
  # Add data per column
  regRegApplyTable[["case"]]  <- if (ready) as.integer(regRegResults$apply$case)   else "."
  regRegApplyTable[["pred"]]  <- if (ready) as.character(regRegResults$apply$pred) else "."
  
}

.regRegRelInfPlot <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!options$plotRelInf) return()
  if (!ready) return()
  
  relInfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(regRegResults$relInf, ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Relative Influence"),
    horizontal = TRUE
  )
  
  # Create plot and bind to jaspResults
  regRegRelInfPlot <- createJaspPlot(plot = relInfPlot, title = "Relative Influence Plot",
                                         width = 500, height = 20 * nrow(regRegResults$relInf) + 60)
  jaspResults[["regRegRelInfPlot"]] <- regRegRelInfPlot
  jaspResults[["regRegRelInfPlot"]]$dependOnOptions(c(analysisOptions, "plotRelInf"))
}

.regRegPlotDeviance <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!options$plotDeviance) return()
  if (!ready) return()
  
  if (regRegResults$method == "OOB") {
    
    deviance <- data.frame(trees = 1:regRegResults$res$n.trees, 
                           trainError = regRegResults$res$train.error) 
    
    plotDeviance <- JASPgraphs::themeJasp(
      ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::xlab("Trees") +
        ggplot2::ylab("Deviance") +
      ggplot2::geom_vline(xintercept = regRegResults$optTrees, color = "lightgray", linetype = "dashed")
    )
    
  } else {
    
    deviance <- data.frame(trees = 1:regRegResults$res$n.trees,
                           trainError = regRegResults$res$train.error,
                           cvError = regRegResults$res$cv.error) 
    
    plotDeviance <- JASPgraphs::themeJasp(
      ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_line(mapping = ggplot2::aes(x = trees, y = cvError), size = 1, colour = "aquamarine4") +
        ggplot2::xlab("Trees") +
        ggplot2::ylab("Deviance") +
        ggplot2::geom_vline(xintercept = regRegResults$optTrees, color = "lightgray", linetype = "dashed")
    )
    
  }

  # Create plot and bind to jaspResults
  plotDeviance <- createJaspPlot(plot = plotDeviance, title = "Deviance Plot", width = 500, height = 400)
  jaspResults[["plotDeviance"]] <- plotDeviance
  jaspResults[["plotDeviance"]]$dependOnOptions(c(analysisOptions, "plotDeviance"))
}

.regRegPlotOOBChangeDev <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!options$plotOOBChangeDev) return()
  if (!ready) return()
    
  oobDev <- data.frame(trees = 1:regRegResults$res$n.trees, oobImprove = regRegResults$res$oobag.improve)
  
  plotOOBChangeDev <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
      ggplot2::xlab("Trees") +
      ggplot2::ylab("OOB Change in Deviance") +
      ggplot2::geom_vline(xintercept = regRegResults$optTrees, color = "lightgray", linetype = "dashed")
    )
  
  # Create plot and bind to jaspResults
  regRegPlotOOBChangeDev <- createJaspPlot(plot = plotOOBChangeDev,title = "OOB Improvement Plot",
                                               width = 500, height = 400)
  jaspResults[["regRegPlotOOBChangeDev"]] <- regRegPlotOOBChangeDev
  jaspResults[["regRegPlotOOBChangeDev"]]$dependOnOptions(c(analysisOptions, "regRegPlotOOBChangeDev"))
}
