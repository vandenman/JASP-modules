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
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if (ready) errors <- .classBoostErrorHandling(dataset, options)
  
  # Save analysis options in an object so that they don't have to be listed every time
  analysisOptions <- c("target", "predictors", "indicator", "noOfTrees", "numberOfTrees", "shrinkage",
                       "shrinkage.parameter", "int.depth", "int.depth.parameter", "cv.folds", "cv.folds.spec",
                       "dataTrain", "percentageDataTraining", "bag.fraction", "bag.fraction.spec", "seedBox", "seed",
                       "missingValues")
  
  # Compute (a list of) results from which tables and plots can be created
  if (ready) classBoostResults <- .classBoostComputeResults(jaspResults, dataset, options, analysisOptions)
  
  # Output containers, tables, and plots based on the results
  .classBoostTable(                jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostApplyTable(           jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostConfMat(              jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostRelInfTable(          jaspResults, options, classBoostResults, ready, analysisOptions)
  .classBoostContainerPlots(       jaspResults, options, classBoostResults, ready)
  .classBoostRelInfPlot(           jaspResults, options, classBoostResults, ready)
  .classBoostPlotTreesVsModelError(jaspResults, options, classBoostResults, ready)
  
  return()
}

# Read dataset
.classBoostReadData <- function(dataset, options) {
  
  if (options$target == "")    options$target <- NULL
  if (options$indicator == "") options$indicator <- NULL
  
  dataset <- .readDataSetToEnd(columns.as.factor = c(options$target, options$indicator), columns = options$predictors)
  
  return(dataset)
}

# Error checking
.classBoostErrorHandling <- function(dataset, options) {
  
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
  
  # # Error Check 3: At least 2 predictors should be provided (otherwise gbm() function breaks)
  # .hasErrors(
  #   dataset = dataset, 
  #   perform = "run", 
  #   type = c('observations', 'variance', 'infinity'),
  #   all.target = options$predictors,
  #   observations.amount = nrow(dataset), # how can we check for the numbers of predictors?
  #   exitAnalysisIfErrors = TRUE)
  
}

# Compute results
.classBoostComputeResults <- function(jaspResults, dataset, options, analysisOptions) {
  
  if (!is.null(jaspResults[["stateRegBoostResults"]])) return (jaspResults[["stateRegBoostResults"]]$object)
  
  # Create results object
  results <- list()
  
  results[["spec"]] <- .classBoostCalcSpecs(dataset, options)
  results[["res"]] <- list()
  
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
  # testTarget <- factor(testData[, target], levels = levels(.v(dataset[, target])))
  
  # Run Boosting
  formula <- as.formula(paste(.v(options$target), "~", paste(.v(options$predictors), collapse = " + ")))
  
  results[["res"]] <- gbm::gbm(formula = formula, data = trainData, n.trees = results$spec$noOfTrees,
                               shrinkage = results$spec$shrinkage, interaction.depth = results$spec$int.depth,
                               cv.folds = results$spec$cv.folds, bag.fraction = results$spec$bag.fraction,
                               distribution = "multinomial")
  
  results[["data"]] <- list(trainData = trainData, testData = testData, testTarget = testTarget)
  
  results[["relInf"]] <- summary(results$res, plot = FALSE)
  
  # Derive test set predictions and calculate test error rate
  prob <- gbm::predict.gbm(results$res, newdata = testData, n.trees = results$spec$noOfTrees, type = "response")
  results[["preds"]] <- colnames(prob)[apply(prob, 1, which.max)]
  # results[["preds"]] <- factor(colnames(prob)[apply(prob, 1, which.max)], levels = levels(.v(dataset[, target])))
  
  results[["testError"]] <- mean(testTarget != results$preds) # classification error
  
  # Apply model to new data
  if(options$indicator != "") {
    results[["apply"]] <- gbm::predict.gbm(results$res, newdata = applyData, n.trees = results$spec$noOfTrees)
  }
  
  # Save results to state
  jaspResults[["stateclassBoostResults"]] <- createJaspState(results)
  jaspResults[["stateclassBoostResults"]]$dependOnOptions(analysisOptions)
  
  return(results)
}

.classBoostCalcSpecs <- function(modelData, options) {
  specs <- list()
  
  # Setting the number of trees
  if (options$noOfTrees == "manual") specs$noOfTrees <- as.integer(options$numberOfTrees) else specs$noOfTrees <- 100
  
  # Setting the number of variables considered at each split
  if (options$shrinkage == "manual") specs$shrinkage <- options$shrinkage.parameter else specs$shrinkage <- .1
  
  # What percentage of the data should be used for training?
  if (options$int.depth == "manual") specs$int.depth <- options$int.depth.parameter else specs$int.depth <- 1
  
  # How many cv-folds should be used?
  if (options$cv.folds == "manual") specs$cv.folds <- options$cv.folds.spec else specs$cv.folds <- 0
  
  # What percentage of the data should be used for training?
  if (options$dataTrain == "manual") specs$dataTrain <- options$percentageDataTraining else specs$dataTrain <- .8
  
  # What percentage of the training data should be used per tree?
  if (options$bag.fraction == "manual") specs$bag.fraction <- options$bag.fraction.spec else specs$bag.fraction <- .5
  
  return(specs)
}

# Output functions
.classBoostTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["classBoostTable"]])) return()
  
  # Create table and bind to jaspResults
  classBoostTable <- createJaspTable(title = "Boosting Classification Model Summary")
  jaspResults[["classBoostTable"]] <- classBoostTable
  jaspResults[["classBoostTable"]]$dependOnOptions(analysisOptions)
  
  # Add column info
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    classBoostTable$addColumnInfo(name = "testError",  title = "Test Set Error", type = "number", format = "sf:4")
  }
  classBoostTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  classBoostTable$addColumnInfo(name = "shrinkage",  title = "Shrinkage", type = "number", format = "sf:4")
  classBoostTable$addColumnInfo(name = "intDepth",  title = "Interaction Depth", type = "integer")
  classBoostTable$addColumnInfo(name = "ntrain",  title = "Training Observations", type = "integer")
  
  # Add data per column
  if(options$dataTrain == "auto" || options$percentageDataTraining < 1){
    classBoostTable[["testError"]]  <- if (ready) classBoostResults$testError else "."
  }
  classBoostTable[["ntrees"]]  <- if (ready) classBoostResults$res$n.trees else "."
  classBoostTable[["shrinkage"]]  <- if (ready) classBoostResults$res$shrinkage else "."
  classBoostTable[["intDepth"]]  <- if (ready) classBoostResults$res$interaction.depth else "."
  classBoostTable[["ntrain"]]  <- if (ready) classBoostResults$res$nTrain else "."

}

.classBoostApplyTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["classBoostApplyTable"]])) return()
  if (options$indicator == "") return()
  
  # Create table and bind to jaspResults
  classBoostApplyTable <- createJaspTable(title = "Boosting Model Predictions")
  jaspResults[["classBoostApplyTable"]] <- classBoostApplyTable
  jaspResults[["classBoostApplyTable"]]$dependOnOptions(c(analysisOptions, "classBoostApplyTable"))
  
  # Add column info
  classBoostApplyTable$addColumnInfo(name = "row",  title = "Row", type = "integer")
  classBoostApplyTable$addColumnInfo(name = "pred",  title = "Prediction", type = "number", format = "sf:4")
  
  # Add data per column
  classBoostApplyTable[["row"]]  <- if (ready) as.numeric(rownames(as.data.frame(classBoostResults$apply))) else "."
  classBoostApplyTable[["pred"]]  <- if (ready) as.numeric(classBoostResults$apply) else "."
  
}

.classBoostConfMat <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["classBoostConfMat"]])) return()
  if (!options$classBoostConfMat) return()
  
  # Create table and bind to jaspResults
  classBoostConfMat <- createJaspTable(title = "Confusion Matrix")
  jaspResults[["classBoostConfMat"]] <- classBoostConfMat
  jaspResults[["classBoostConfMat"]]$dependOnOptions(c(analysisOptions, "classBoostConfMat"))
  
  # Fill table with confusion matrix
  if (ready) { 
    classBoostConfMat$addColumnInfo(name = "col", title = "Predictor", type = "string")
    for (level in seq_len(nlevels(.v(options$target)))) {
      classBoostConfMat$addColumnInfo(name = "cols", title = , type = "integer", overtitle = "Observed")
    }
    
    # classBoostConfMat[["col"]] <- levels(.v(options$target))
    # classBoostConfMat[["cols"]] <- classBoostConfMat$setData(table(classBoostResults$preds, classBoostResults$data$testTarget))
    
    classBoostConfMat$setData(table(classBoostResults$preds, classBoostResults$data$testTarget))
    
    } else {
      
      classBoostConfMat$setExpectedRows(1)
      
    }
  
}

.classBoostRelInfTable <- function(jaspResults, options, classBoostResults, ready, analysisOptions) {
  if (!is.null(jaspResults[["tableVarImp"]])) return()
  if (!options$classBoostRelInfTable) return()
  
  # Create table
  classBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  jaspResults[["classBoostRelInfTable"]] <- classBoostRelInfTable
  jaspResults[["classBoostRelInfTable"]]$dependOnOptions(c(analysisOptions, "classBoostRelInfTable"))
  
  # Add column info
  classBoostRelInfTable$addColumnInfo(name = "predictor",  title = " ", type = "string")
  classBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number", format = "sf:4")
  
  # Add data per column
  classBoostRelInfTable[["predictor"]]  <- if(ready) .unv(classBoostResults$relInf$var) else "."
  classBoostRelInfTable[["relIn"]]  <- if(ready) classBoostResults$relInf$rel.inf else "."
  
}

.classBoostContainerPlots <- function(jaspResults, options, classBoostResults, ready) {
  if (!any(options$plotRelInf, options$plotTreesVsModelError)) return()
  if (!ready) return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Boosting Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotRelInf", "plotTreesVsModelError"))
  }
}

.classBoostRelInfPlot <- function(jaspResults, options, classBoostResults, ready) {
  if (!options$plotRelInf) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  relInfPlot <- JASPgraphs::themeJasp(
    ggplot2::ggplot(classBoostResults$relInf, ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Relative Influence"),
    horizontal = TRUE
  )
  
  pct[['relInfPlot']] <- createJaspPlot(plot = relInfPlot, title = "Relative Influence per Variable",
                                        width = 500, height = 20 * nrow(classBoostResults$relInf) + 60)
}

.classBoostPlotTreesVsModelError <- function(jaspResults, options, classBoostResults, ready) {
  if (!options$plotTreesVsModelError) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  treesOOBImprove <- dplyr::tibble(
    trees = 1:classBoostResults$res$n.trees,
    oob.improve = classBoostResults$res$oobag.improve
  )
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    ggplot2::ggplot(data = treesOOBImprove, mapping = ggplot2::aes(x = trees, y = oob.improve)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::xlab("Trees") +
      ggplot2::ylab("OOB Change in \n Multinomial Deviance") +
      ggplot2::geom_vline(
        xintercept = which.min(abs(classBoostResults$res$oobag.improve)), color = "lightgray", linetype = "dashed") +
      ggplot2::geom_hline(
        yintercept = classBoostResults$res$oobag.improve[which.min(abs(classBoostResults$res$oobag.improve))],
        color = "lightgray", linetype = "dashed")
  )
  
  pct[['plotTreesVsModelError']] <- createJaspPlot(plot = plotTreesVsModelError, title = "OOB Improvement",
                                                   width = 500, height = 400)
}
