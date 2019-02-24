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
                       "elasticSpec", "alphaElastic", "standardize", "intercept", "pmax", "pmaxSpec", "thresh",
                       "threshSpec", "dataTrain", "percentageDataTraining", "seedBox", "seed", "NAs")
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) regRegResults <- .regRegComputeResults(jaspResults, dataset, options, analysisOptions)
  
  # Output containers, tables, and plots based on the results
  .regRegTable(       jaspResults, options, regRegResults, ready, analysisOptions)
  .regRegCoefTable(   jaspResults, options, regRegResults, ready, analysisOptions)
  .regRegApplyTable(  jaspResults, options, regRegResults, ready, analysisOptions)
  .regRegPredPerfPlot(jaspResults, options, regRegResults, ready, analysisOptions)
  .regRegCVLambdaPlot(jaspResults, options, regRegResults, ready, analysisOptions)
  
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
        predImpute <- randomForest::na.roughfix(dataset[idxApply, c(preds, target), drop = FALSE])
      } else {
        predImpute <- na.omit(dataset[idxApply, c(preds, target), drop = FALSE])
      }

    } else if (options$NAs == "roughfix") {
      
      dataset <- randomForest::na.roughfix(dataset)
      
      } else {
        
        dataset <- na.omit(dataset)
    
        } 
    
  }
  
  # Splitting the data into training set, test set, and application set
  if (options$applyModel == "applyIndicator" && options$indicator != "") {
    
    idxApply <- which(dataset[, indicator] == 1)
    idxModel <- which(dataset[, indicator] == 0)
    
    applyData <- dataset[idxApply, c(preds, target), drop = FALSE]
    modelData <- dataset[idxModel, ]
    
    } else if (options$applyModel == "applyImpute") {
      
      applyData <- predImpute
      modelData <- dataset[-idxApply, ]
      
    } else {
    
      applyData <- NULL
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
  
  if (!is.null(applyData)) applyData <- model.matrix(formula, applyData)[, -1]
  
  # Run regularized regression
  results[["res"]] <- glmnet::cv.glmnet(x = trainPreds, y = trainTarget, nfolds = 10,
                                        type.measure = "deviance", family = "gaussian",
                                        alpha = results$spec$alpha, standardize = results$spec$standardize,
                                        intercept = results$spec$intercept, thresh = results$spec$thresh,
                                        if(options$penalty != "ridge") pmax = results$spec$pmax,
                                        weights = rep(1, nrow(trainPreds)))
  
  results[["cvMSE"]] <- results$res$cvm[results$res$lambda == results$res$lambda.min]
  results[["cvMSELambda"]] <- data.frame(lambda = results$res$lambda, MSE = results$res$cvm, sd = results$res$cvsd)
  
  results[["data"]] <- list(trainPreds = trainPreds, trainTarget = trainTarget, 
                            testPreds = testPreds, testTarget = testTarget)
  
  # Derive test set predictions and calculate test error rate
  if (options$shrinkage == "manual") {
    results[["lambda"]] <- results$spec$lambda
  } else {
    results[["lambda"]] <- results$res$lambda.min
  }
  
  modPred <- predict(results$res, newx = testPreds, s = results$lambda, type = "link", exact = TRUE,
                     x = trainPreds, y = trainTarget, family = "gaussian", alpha = results$spec$alpha,
                     standardize = results$spec$standardize, intercept = results$spec$intercept,
                     thresh = results$spec$thresh, if(options$penalty != "ridge") pmax = results$spec$pmax)
  
  # Predictive performance
  results[["predPerf"]] <- data.frame(pred = as.numeric(modPred), obs = as.numeric(testTarget))
  results[["testMSE"]] <- mean((modPred - testTarget)^2)
  
  # Coefficients table
  results[["coefs"]] <- coef(results$res, s = results$lambda)
  
  # Apply model to new data if requested
  if((options$applyModel == "applyIndicator" && options$indicator != "") || options$applyModel == "applyImpute") {
    
    applyPred <- predict(results$res, newx = applyData, s = results$lambda, type = "link", exact = TRUE,
                         x = trainPreds, y = trainTarget, family = "gaussian", alpha = results$spec$alpha,
                         standardize = results$spec$standardize, intercept = results$spec$intercept,
                         thresh = results$spec$thresh, if(options$penalty != "ridge") pmax = results$spec$pmax)
    
    results[["apply"]] <- data.frame(case = idxApply, pred = as.numeric(applyPred))
    
  }
  
  # Save results to state
  jaspResults[["stateRegRegResults"]] <- createJaspState(results)
  jaspResults[["stateRegRegResults"]]$dependOnOptions(analysisOptions)
  
  return(results)
}

.regRegCalcSpecs <- function(dataset, options) {
  
  specs <- list()
  
  # Specifying a lambda sequence to build the model on when manual
  specs$lambdaSeq <- 10^seq(10, -2, length = 100)
  
  # Setting the shrinkage parameter lambda
  if (options$shrinkage == "manual") specs$lambda <- options$lambda else specs$lambda <- NULL
  
  # Setting the alpha parameter (0 = Ridge, 1 = Lasso, inbetween = Elastic Net)
  if (options$penalty == "elasticNet") {
    
    if (options$elasticSpec == "optimize") {
      specs$alphaElastic <- NULL # tk add optimization for alpha
    } else if (options$elasticSpec == "manual") {
      specs$alphaElastic <- options$alphaElastic
    } else {
      specs$alphaElastic <- 0.5
    }
    
  }
  
  # Choosing the regularization method
  if (options$penalty == "ridge") {
    specs$alpha <- 0
    specs$penalty <- "L2 (Ridge)"
  } else if (options$penalty == "lasso") {
    specs$alpha <- 1
    specs$penalty <- "L1 (Lasso)"
  } else {
    specs$alpha <- specs$alphaElastic
    specs$penalty <- "Elastic Net"
  }
  
  # Should the data be standardized?
  if (options$standardize == "on") specs$standardize <- TRUE else specs$standardize <- FALSE
  
  # Should the intercept be fitted?
  if (options$intercept == "on") specs$intercept <- TRUE else specs$intercept <- FALSE
  
  # Setting a convergence threshold for coordinate descent
  if (options$thresh == "manual") specs$thresh <- options$threshSpec else specs$thresh <- 1e-7
  
  # Setting the maximum number of nonzero coefficients
  if (options$pmax == "manual") specs$pmax <- options$pmaxSpec
  
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
  if (options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regRegTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", format = "sf:4")
  }
  
  if (options$shrinkage == "auto") {
    regRegTable$addColumnInfo(name = "cvMSE",  title = "CV MSE", type = "number", format = "sf:4") 
  }
  
  regRegTable$addColumnInfo(name = "penalty"  ,  title = "Penalty"  , type = "string")
  
  if (options$penalty == "elasticNet") {
    regRegTable$addColumnInfo(name = "alpha",  title = "α", type = "number", format = "sf:4") 
  }
  
  regRegTable$addColumnInfo(name = "lambda",  title = "λ", type = "number", format = "sf:4")
  
  # Add data per column
  if (options$dataTrain == "auto" || options$percentageDataTraining < 1){
    regRegTable[["testMSE"]]  <- if (ready) regRegResults$testMSE    else "."
  }
  
  if (options$shrinkage == "auto") {
    regRegTable[["cvMSE"]]   <- if (ready) regRegResults$cvMSE else "." 
  }
  
  regRegTable[["penalty"]] <- if (ready) regRegResults$spec$penalty else "."
  
  if (options$penalty == "elasticNet") regRegTable[["alpha"]] <- if (ready) regRegResults$spec$alphaElastic else "."
  
  if (options$shrinkage == "auto") {
    regRegTable[["lambda"]]  <- if (ready) regRegResults$res$lambda.min  else "." 
  } else {
    regRegTable[["lambda"]]  <- if (ready) regRegResults$spec$lambda  else "." 
  }

}

.regRegCoefTable <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["regRegCoefTable"]])) return()
  if (!options$regRegCoefTable) return()
  
  # Create table
  regRegRelInfTable <- createJaspTable(title = "Regression Coefficients")
  jaspResults[["regRegCoefTable"]] <- regRegRelInfTable
  jaspResults[["regRegCoefTable"]]$dependOnOptions(c(analysisOptions, "regRegCoefTable"))
  
  regRegRelInfTable$position <- 3
  
  # Add column info
  regRegRelInfTable$addColumnInfo(name = "var",  title = " ", type = "string")
  regRegRelInfTable$addColumnInfo(name = "coefs",  title = "Coefficient", type = "number", format = "sf:4")
  
  # Add data per column
  regRegRelInfTable[["var"]]   <- if(ready) .unv(rownames(regRegResults$coefs)) else "."
  regRegRelInfTable[["coefs"]] <- if(ready) as.numeric(regRegResults$coefs) else "."
  
}

.regRegApplyTable <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["applyModel"]])) return()
  if (options$applyModel == "noApp") return()
  
  # Create table and bind to jaspResults
  regRegApplyTable <- createJaspTable(title = "Regularized Regression Model Predictions")
  jaspResults[["regRegApplyTable"]] <- regRegApplyTable
  jaspResults[["regRegApplyTable"]]$dependOnOptions(c(analysisOptions, "applyModel"))
  
  regRegApplyTable$position <- 4
  
  # Add column info
  regRegApplyTable$addColumnInfo(name = "case",  title = "Case", type = "integer")
  regRegApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  regRegApplyTable[["case"]]  <- if (ready) as.integer(regRegResults$apply$case)   else "."
  regRegApplyTable[["pred"]]  <- if (ready) as.numeric(regRegResults$apply$pred) else "."
  
}

.regRegPredPerfPlot <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!options$plotPredPerf) return()
  if (!ready) return()
  
  limits <- c(0, round(max(c(max(floor(regRegResults$predPerf$pred)), max(floor(regRegResults$predPerf$obs))))))
  
  regRegPredPerfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = regRegResults$predPerf, mapping = ggplot2::aes(x = obs, y = pred)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_line(data = data.frame(x = limits, y = limits), mapping = ggplot2::aes(x = x, y = y),
                         col = "darkred", size = 1) +
      ggplot2::xlab("Observed") +
      ggplot2::ylab("Predicted")
  )
  
  regRegPredPerfPlot <- createJaspPlot(plot = regRegPredPerfPlot, title = "Predictive Performance",
                                       width = 400, height = 400)
  
  jaspResults[["regRegPredPerfPlot"]] <- regRegPredPerfPlot
  jaspResults[["regRegPredPerfPlot"]]$dependOnOptions(c(analysisOptions, "plotPredPerf"))
}

.regRegCVLambdaPlot <- function(jaspResults, options, regRegResults, ready, analysisOptions) {
  if (!options$plotCVLambda) return()
  if (!ready) return()
  
  regRegCVLambdaPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = regRegResults$cvMSELambda, mapping = ggplot2::aes(x = lambda, y = MSE)) +
      ggplot2::geom_ribbon(data = regRegResults$cvMSELambda, mapping = ggplot2::aes(ymin = MSE - sd, ymax = MSE + sd),
                           fill = "grey70") +
      ggplot2::geom_line(size = 1, colour = "black") +
      ggplot2::scale_x_continuous("λ") +
      ggplot2::ylab("Mean Squared Error") +
      ggplot2::geom_vline(xintercept = regRegResults$res$lambda.min, color = "lightgray", linetype = "dashed")
    )
  
  # Create plot and bind to jaspResults
  regRegCVLambdaPlot <- createJaspPlot(plot = regRegCVLambdaPlot, title = "Lambda Evaluation",
                                       width = 500, height = 400)
  
  jaspResults[["regRegCVLambdaPlot"]] <- regRegCVLambdaPlot
  jaspResults[["regRegCVLambdaPlot"]]$dependOnOptions(c(analysisOptions, "plotCVLambda"))
}
