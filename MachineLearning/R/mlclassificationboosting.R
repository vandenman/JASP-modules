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

MLClassificationBoosting <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Boosting Classification"
  
  # Read dataset
  dataset <- .classBoostReadData(dataset, options)
  
  # Check if results can be computed
  ready <- (options$target != "" && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .classBoostErrorHandling(dataset, options)
  
  # Save analysis options in an object so that they don't have to be listed every time
  analysisOptions <- c("target", "predictors", "indicator", "applyModel", "noOfTrees", "numberOfTrees", "shrinkage", 
                       "shrinkPar", "int.depth", "int.depth.parameter", "modelOptimization", "dataTrain",
                       "percentageDataTraining", "bag.fraction", "bag.fraction.spec", "seedBox", "seed", "NAs")
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) classBoostResults <- .classBoostComputeResults(jaspResults, dataset, options, analysisOptions)
  
  # Output containers, tables, and plots based on the results
  .classBoostTable(                jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostConfTable(            jaspResults, options, classBoostResults, ready, analysisOptions, dataset)
  .classBoostRelInfTable(          jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostApplyTable(           jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostRelInfPlot(           jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostPlotDeviance(         jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostPlotOOBChangeDev(     jaspResults, options, classBoostResults, ready, analysisOptions)
  
  return()
}

# Read dataset
.classBoostReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  data <- .readDataSetToEnd(columns.as.factor = c(options$target, options$indicator), columns = options$predictors)
  
  return(data)
}

# Error checking
.classBoostErrorHandling <- function(dataset, options) {
  
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: Apply indicator should not have any missing values (consist of 0s and 1s)
  if (options$indicator != "") {
    .hasErrors(dataset = dataset, perform = "run", type = c('observations', 'variance', 'infinity'),
               all.target = options$indicator, observations.amount = nrow(dataset), exitAnalysisIfErrors = TRUE)
  }
  
  # Error Check 3: There should be at least 2 predictors (otherwise gbm() complains)
  if (options$target != "" && ncol(dataset) < 3L){
    JASP:::.quitAnalysis("Please provide at least 2 predictors.")
  }
  
  # Error Check 4: The target variable should have at least 2 classes
  if (nlevels(dataset[, .v(options$target)]) < 2){
    JASP:::.quitAnalysis("The target variable should have at least 2 classes.")
  }
  
  # Error Check 5: If target values should be imputed, there have to be missing values in the target
  # if (options$applyModel == "applyImpute" && sum(is.na(dataset[, .v(options$target)])) < 1) {
  #   JASP:::.quitAnalysis("To apply model to missing values in target, please provide observations that have missing 
  #   values in the target variable.")
  # }
  
}

# Compute results
.classBoostComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateClassBoostResults"]])) return (jaspResults[["stateClassBoostResults"]]$object)
  
  # Create results object and add options
  results <- list()
  results[["spec"]] <- .classBoostCalcSpecs(dataset, options)
  
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
  
  trainData <- modelData[idxTrain, c(preds, target), drop = FALSE]
  testData <- modelData[idxTest, preds, drop = FALSE]
  testTarget <- as.character(modelData[idxTest, target])
  
  # Prepare Boosting
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  if (nlevels(dataset[, target]) == 2) {
    
    distribution <- "multinomial" # should be bernoulli but that somehow screws up the function
    # does this even matter since multinomial distribution IS the bernoulli distribution when k = 2 and n = 1?
    # see Wikipedia multinomial distribution
    
    } else {
      
      distribution <- "multinomial"
      
    }
  
  # Run Boosting
  results[["res"]] <- gbm::gbm(formula = formula, data = trainData, n.trees = results$spec$noOfTrees,
                               shrinkage = results$spec$shrinkage, interaction.depth = results$spec$int.depth,
                               cv.folds = results$spec$modelOptimization, bag.fraction = results$spec$bag.fraction,
                               n.minobsinnode = results$spec$nNode, distribution = distribution)
  
  results[["data"]] <- list(trainData = trainData, testData = testData, testTarget = testTarget)
  results[["relInf"]] <- summary(results$res, plot = FALSE)
  
  if(options$modelOptimization == "cv") results[["method"]] <- "cv" else results[["method"]] <- "OOB"
  
  if (options$modelOptimization != "noOpt") {
    results[["optTrees"]] <- gbm::gbm.perf(results$res, plot.it = FALSE, method = results$method)[1]
  } else {
    results[["optTrees"]] <- results$spec$noOfTrees
  }
  
  # Derive test set predictions and calculate test error rate
  prob <- gbm::predict.gbm(results$res, newdata = testData, n.trees = results$optTrees, type = "response")
  
  if (distribution == "bernoulli") {
    results[["preds"]] <- round(prob, 0)
  } else {
    results[["preds"]] <- colnames(prob)[apply(prob, 1, which.max)]
  }
    
  results[["testError"]] <- mean(testTarget != as.character(results$preds))
  results[["confTable"]] <- table("Pred" = results$preds, "True" = results$data$testTarget)
  
  # Apply model to new data if requested
  if(options$applyModel == "applyIndicator" && options$indicator != "") {
    
    applyProb <- gbm::predict.gbm(results$res, newdata = applyData, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- data.frame(case = idxApply, pred = colnames(applyProb)[apply(applyProb, 1, which.max)])
    
  } else if (options$applyModel == "applyImpute") {

    applyProb <- gbm::predict.gbm(results$res, newdata = predImpute, n.trees = results$optTrees, type = "response")
    results[["apply"]] <- data.frame(case = idxApply, pred = colnames(applyProb)[apply(applyProb, 1, which.max)])
    
  }
  
  # Save results to state
  jaspResults[["stateClassBoostResults"]] <- createJaspState(results)
  jaspResults[["stateClassBoostResults"]]$dependOnOptions(analysisOptions)
  
  return(results)
}

.classBoostCalcSpecs <- function(modelData, options) {
  specs <- list()
  
  # Setting the number of trees
  if (options$noOfTrees == "manual") specs$noOfTrees <- as.integer(options$numberOfTrees) else specs$noOfTrees <- 100
  
  # Setting the number of variables considered at each split
  if (options$shrinkage == "manual") specs$shrinkage <- options$shrinkPar else specs$shrinkage <- .1
  
  # What percentage of the data should be used for training?
  if (options$int.depth == "manual") specs$int.depth <- options$int.depth.parameter else specs$int.depth <- 1
  
  # What percentage of the data should be used for training?
  if (options$dataTrain == "manual") specs$dataTrain <- options$percentageDataTraining else specs$dataTrain <- .8
  
  # What percentage of the training data should be used per tree?
  if (options$bag.fraction == "manual") specs$bag.fraction <- options$bag.fraction.spec else specs$bag.fraction <- .5
  
  # Minimum number of observations in the terminal nodes of every tree
  if (options$nNode == "manual") specs$nNode <- options$nNodeSpec else specs$nNode <- 10
  
  # Should cross-validation be performed?
  if (options$modelOptimization == "cv") specs$modelOptimization <- 10 else specs$modelOptimization <- 0
  
  return(specs)
}

# Output functions
.classBoostTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["classBoostTable"]])) return()
  
  # Create table and bind to jaspResults
  classBoostTable <- createJaspTable(title = "Boosting Classification Model Summary")
  jaspResults[["classBoostTable"]] <- classBoostTable
  jaspResults[["classBoostTable"]]$dependOnOptions(analysisOptions)
  
  classBoostTable$position <- 1
  
  # Add column info
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    classBoostTable$addColumnInfo(name = "testError",  title = "Test Set Error", type = "number", format = "sf:4")
  }
  classBoostTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  classBoostTable$addColumnInfo(name = "shrinkage",  title = "Shrinkage", type = "number", format = "sf:4")
  classBoostTable$addColumnInfo(name = "intDepth",  title = "Interaction Depth", type = "integer")
  classBoostTable$addColumnInfo(name = "ntrain",  title = "n (Train)", type = "integer")
  classBoostTable$addColumnInfo(name = "ntest",  title = "n (Test)", type = "integer")
  
  # Add data per column
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    classBoostTable[["testError"]]  <- if (ready) classBoostResults$testError else "."
  }
  classBoostTable[["ntrees"]]  <- if (ready) classBoostResults$optTrees else "."
  classBoostTable[["shrinkage"]]  <- if (ready) classBoostResults$res$shrinkage else "."
  classBoostTable[["intDepth"]]  <- if (ready) classBoostResults$res$interaction.depth else "."
  classBoostTable[["ntrain"]]  <- if (ready) classBoostResults$res$nTrain else "."
  classBoostTable[["ntest"]]  <- if (ready) length(classBoostResults$data$testTarget) else "."

}

.classBoostConfTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions, dataset) {
  if (!is.null(jaspResults[["classBoostConfTable"]])) return()
  if (!options$classBoostConfTable) return()
  
  # Create table and bind to jaspResults
  classBoostConfTable <- createJaspTable(title = "Confusion Table")
  jaspResults[["classBoostConfTable"]] <- classBoostConfTable
  jaspResults[["classBoostConfTable"]]$dependOnOptions(c(analysisOptions, "classBoostConfTable"))
  
  classBoostConfTable$position <- 2
  target <- .v(options$target)
  title_observed <- "Observed"
  
  if(ready){
    
    classBoostConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    for( i in 1:length(rownames(classBoostResults$confTable))){
      classBoostConfTable$addColumnInfo(name = paste("varname_real", i, sep = ""), 
                                        title = as.character(rownames(classBoostResults$confTable)[i]), 
                                        type = "integer")
    }
    
    for( i in 1:length(rownames(classBoostResults$confTable))){
      row <- list("varname_pred" = as.character(rownames(classBoostResults$confTable))[i])
      for(j in 1:length(rownames(classBoostResults$confTable))){
        row[[paste("varname_real", j, sep="")]] <- classBoostResults$confTable[i,j]
      }
      if(i == 1){
        row[["pred_name"]] <- "Predicted"
      } else {
        row[["pred_name"]] <- ""
      }
      classBoostConfTable$addRows(row)
    }
    
  } else if (options$target != "" && !ready){
    
    classBoostConfTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    for( i in 1:length(unique(dataset[ ,target]))){
      classBoostConfTable$addColumnInfo(name = paste("varname_real",i, sep = ""), 
                                        title = as.character(sort(unique(dataset[, target]), decreasing = FALSE))[i], 
                                        type = "integer")
    }
    
    for( i in 1:length(unique(dataset[, target]))){
      row <- list("varname_pred" = as.character(sort(unique(dataset[, target]), decreasing = FALSE))[i])
      for(j in 1:length(unique(dataset[, target]))){
        row[[paste("varname_real",j,sep="")]] <- "."
      }
      if(i == 1){
        row[["pred_name"]] <- "Predicted"
      } else {
        row[["pred_name"]] <- ""
      }
      classBoostConfTable$addRows(row)
    }
  } else {
    
    classBoostConfTable$addColumnInfo(name = "pred_name"    , title = "" , type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_pred" , title = "" , type = "string")
    classBoostConfTable$addColumnInfo(name = "varname_real1", title = ".", type = "integer")
    classBoostConfTable$addColumnInfo(name = "varname_real2", title = ".", type = 'integer')
    
    row <- list(pred_name = "Predicted", varname_pred = ".", varname_real1 = "", varname_real2= "")
    classBoostConfTable$addRows(row)
    row <- list(pred_name = "", varname_pred = ".", varname_real1= "", varname_real2= "")
    classBoostConfTable$addRows(row)
    
  }
  
}

.classBoostRelInfTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["tableVarImp"]])) return()
  if (!options$classBoostRelInfTable) return()
  
  # Create table
  classBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  jaspResults[["classBoostRelInfTable"]] <- classBoostRelInfTable
  jaspResults[["classBoostRelInfTable"]]$dependOnOptions(c(analysisOptions, "classBoostRelInfTable"))
  
  classBoostRelInfTable$position <- 3
  
  # Add column info
  classBoostRelInfTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  classBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number", format = "sf:4")
  
  # Add data per column
  classBoostRelInfTable[["predictor"]]  <- if(ready) .unv(classBoostResults$relInf$var) else "."
  classBoostRelInfTable[["relIn"]]  <- if(ready) classBoostResults$relInf$rel.inf else "."
  
}

.classBoostApplyTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["applyModel"]])) return()
  if (options$applyModel == "noApp") return()
  
  # Create table and bind to jaspResults
  classBoostApplyTable <- createJaspTable(title = "Boosting Model Predictions")
  jaspResults[["classBoostApplyTable"]] <- classBoostApplyTable
  jaspResults[["classBoostApplyTable"]]$dependOnOptions(c(analysisOptions, "applyModel"))
  
  classBoostApplyTable$position <- 4
  
  # Add column info
  classBoostApplyTable$addColumnInfo(name = "case",  title = "Case", type = "integer")
  classBoostApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "string")
  
  # Add data per column
  classBoostApplyTable[["case"]]  <- if (ready) as.integer(classBoostResults$apply$case)   else "."
  classBoostApplyTable[["pred"]]  <- if (ready) as.character(classBoostResults$apply$pred) else "."
  
}

.classBoostRelInfPlot <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!options$plotRelInf) return()
  if (!ready) return()
  
  relInfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classBoostResults$relInf, ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Relative Influence"),
    horizontal = TRUE
  )
  
  # Create plot and bind to jaspResults
  classBoostRelInfPlot <- createJaspPlot(plot = relInfPlot, title = "Relative Influence Plot",
                                         width = 500, height = 20 * nrow(classBoostResults$relInf) + 60)
  jaspResults[["classBoostRelInfPlot"]] <- classBoostRelInfPlot
  jaspResults[["classBoostRelInfPlot"]]$dependOnOptions(c(analysisOptions, "plotRelInf"))
}

.classBoostPlotDeviance <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!options$plotDeviance) return()
  if (!ready) return()
  
  if (classBoostResults$method == "OOB") {
    
    deviance <- data.frame(trees = 1:classBoostResults$res$n.trees, 
                           trainError = classBoostResults$res$train.error) 
    
    plotDeviance <- JASPgraphs::themeJasp(
      ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::xlab("Trees") +
        ggplot2::ylab("Deviance") +
      ggplot2::geom_vline(xintercept = classBoostResults$optTrees, color = "lightgray", linetype = "dashed")
    )
    
  } else {
    
    deviance <- data.frame(trees = 1:classBoostResults$res$n.trees,
                           trainError = classBoostResults$res$train.error,
                           cvError = classBoostResults$res$cv.error) 
    
    plotDeviance <- JASPgraphs::themeJasp(
      ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_line(mapping = ggplot2::aes(x = trees, y = cvError), size = 1, colour = "aquamarine4") +
        ggplot2::xlab("Trees") +
        ggplot2::ylab("Deviance") +
        ggplot2::geom_vline(xintercept = classBoostResults$optTrees, color = "lightgray", linetype = "dashed")
    )
    
  }

  # Create plot and bind to jaspResults
  plotDeviance <- createJaspPlot(plot = plotDeviance, title = "Deviance Plot", width = 500, height = 400)
  jaspResults[["plotDeviance"]] <- plotDeviance
  jaspResults[["plotDeviance"]]$dependOnOptions(c(analysisOptions, "plotDeviance"))
}

.classBoostPlotOOBChangeDev <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!options$plotOOBChangeDev) return()
  if (!ready) return()
    
  oobDev <- data.frame(trees = 1:classBoostResults$res$n.trees, oobImprove = classBoostResults$res$oobag.improve)
  
  plotOOBChangeDev <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
      ggplot2::xlab("Trees") +
      ggplot2::ylab("OOB Change in Deviance") +
      ggplot2::geom_vline(xintercept = classBoostResults$optTrees, color = "lightgray", linetype = "dashed")
    )
  
  # Create plot and bind to jaspResults
  classBoostPlotOOBChangeDev <- createJaspPlot(plot = plotOOBChangeDev,title = "OOB Improvement Plot",
                                               width = 500, height = 400)
  jaspResults[["classBoostPlotOOBChangeDev"]] <- classBoostPlotOOBChangeDev
  jaspResults[["classBoostPlotOOBChangeDev"]]$dependOnOptions(c(analysisOptions, "classBoostPlotOOBChangeDev"))
}
