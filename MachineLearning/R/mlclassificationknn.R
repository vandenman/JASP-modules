#
# Copyright (C) 2017 University of Amsterdam
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


MLClassificationKNN <- function(jaspResults, dataset, options, state=NULL) {

    # TODO: Overtitle in confusion table
    # TODO: Make plot work
    # TODO: Work out accuracy loocv and kfold
    # TODO: Make confidence estimates work
    # TODO: Optimzation plot

    # read variables ##
    dataset                      <- .readDataMlKNNclassification(dataset, options)
    # error handling ##
    .mlKnnClassificationErrorHandling(dataset, options)
    # Null state ##
    if(is.null(state))
      state 							       <- list()
    # set the seed so that every time the same set is chosen (to prevent random results) ##
    set.seed(1)
    jaspResults$title 					 <- 'K-Nearest Neighbors Classification'
    # Set the right options for the analysis ##
    opt                          <- .setOptionsClassification(dataset, options)
    formula                      <- .makeformulaClassification(options)
    # Run the analysis
    res <- .doKNNClassification(dataset, options, opt, formula, jaspResults)
    res                         <- jaspResults[["res"]]$object
    # create the evaluation table ##
    .evaluationTableClassification(dataset, options, opt, formula, res, jaspResults)
    # create the optimization table ##
    if (options[['optimizeModel']])
      {
        if(is.null(jaspResults[["optimizationTable"]]))
          .optimizationTable(dataset, options, res, formula, jaspResults)
      }
    # Create the confusion table ##
    if (options[['confusionTable']])
    	{
    		if(is.null(jaspResults[["confusionTable"]]))
    			.confusionTableClassifiaction(dataset, options, res, jaspResults)
    	}
    # # Create the predictions table ##
    if (options[['tablePredictions']])
      {
        if(is.null(jaspResults[["predictionsTable"]]))
          .predictionsTableClassification(options, opt, res, jaspResults)
      }
    # Create the distances table ##
    if (options[['tableDistances']])
      {
        if(is.null(jaspResults[["distancesTable"]]))
          .distancesTableClassification(options, opt, res, jaspResults)
      }
    # Create the weights table ##
    if(options[['tableWeights']])
    {
      if(is.null(jaspResults[["weightsTable"]]))
        .weightsTableClassification(options, opt, res, jaspResults)
    }
    # Create the Error vs K plot ##
    if(options[['plotErrorVsK']] && !is.null(res))
    {
       if(is.null(jaspResults[["plotErrorVsK"]]))
       {
       jaspResults[["plotErrorVsK"]] 		<- .plotErrorVsKClassification(dataset, options, opt, formula, res, jaspResults)
       jaspResults[["plotErrorVsK"]]		$copyDependenciesFromJaspObject(res)
       jaspResults[["plotErrorVsK"]] 		$position <- 3
       }
    }
    # # create the optimization plot ##
    # if(options[["optimizeModel"]] && !is.null(res)){
    #         .plotFunc <- function(){
    #             .plotOptimization(formula,dataset,options)
    #         }
    #         imgObj <- .writeImage(width = options$plotWidth,
    #                               height = options$plotHeight,
    #                               plot = .plotFunc)
    #
    #         plot <- list()
    #
    #         plot[["title"]] <- "Optimization plot"
    #         plot[["data"]] <- imgObj[["png"]]
    #         plot[["obj"]] <- imgObj[["obj"]]
    #         plot[["convertible"]] <- TRUE
    #         plot[["status"]] <- "complete"
    #
    #         results[["optimizationPlot"]] <- plot
    #         state[["optimizationPlot"]] <- results[["optimizationPlot"]]
    #     }
}

.readDataMlKNNclassification <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  if (is.null(dataset)) {
          dataset <- .readDataSetToEnd(columns.as.numeric=variables.to.read,exclude.na.listwise=variables.to.read)
  }
    return(dataset)
}

.mlKnnClassificationErrorHandling <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(!(options[["target"]] == ""))
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  for(i in 1:length(variables.to.read)){
      errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                           all.target = variables.to.read[i],
                           observations.amount = "< 2",
                           exitAnalysisIfErrors = TRUE)

  }
}

.setOptionsClassification <- function(dataset, options){
    opt <- list()
    # set K
    ifelse(test = options[['noOfNearestNeighbours']] == 'auto' & nrow(dataset) <= 1000,
           yes = opt[['NN']] <- 1,
           no = ifelse(test = options[['noOfNearestNeighbours']] == 'auto' & nrow(dataset) < 20000,
                       yes = opt[['NN']] <- 2*round(((nrow(dataset)*0.001)+1)/2)-1,
                       no = ifelse(test = options[['noOfNearestNeighbours']] == 'auto' & nrow(dataset) > 21000,
                                   yes = opt[['NN']] <- 21,
                                   no = opt[['NN']] <- 0)))
    if (options[['noOfNearestNeighbours']] == 'manual'){
        opt[['NN']] <- options[['nearestNeighboursCount']]
    } else if (options[['noOfNearestNeighbours']] == 'optimized'){
        opt[['NN']] <- options[['optimizedFrom']]:options[['optimizedTo']]
    }
    # set training data
    if(options[['percentageTrainingData']] == 'auto'){
        opt[['ntrain']] <- 80
    } else if(options[['percentageTrainingData']] == 'manual'){
        opt[['ntrain']] <- options[['trainingDataManual']]
    }
    # set distance parameter
    if(options[['distanceParameter']] == 'auto'){
        opt[['distance']] <- 2
    } else if (options[['distanceParameter']] == 'manual'){
        opt[['distance']] <- options[['distanceParameterManual']]
    } else if (options[['distanceParameter']] == 'optimized'){
        opt[['distance']] <- 2
    }
    # set weights
    if(options[['weights']]=='unweighted'){
        opt[['weights']] <- 'rectangular'
    } else {
        opt[['weights']] <- options[['weights']]
    }
    # set NA action
    if(options[['naAction']] == 'deleteListwise'){
        opt[['NA']] <- na.omit
    } else if (options[['naAction']] == 'predict'){
        opt[['NA']] <- napredict 							# still has to be looked at
    }
    return(opt)
}

.makeformulaClassification <- function(options){
    predictors <- .v(options[["predictors"]])
    target <- .v(options[["target"]])
    formula <- NULL
    if( options[["target"]] != "" && length(options[["predictors"]]) > 0 && !("" %in% options[["predictors"]]) ){
      formula <- paste(target, "~", paste(predictors, collapse=" + "))
    }
    return(formula)
}

.doKNNClassification <- function(dataset, options, opt, formula, jaspResults){

  dataset                 <- na.omit(dataset)
  train.index             <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(opt[['ntrain']]*0.01,1-(opt[['ntrain']]*0.01)))
  train                   <- dataset[which(train.index == TRUE), ]
  test                    <- dataset[which(train.index == FALSE), ]
  target                  <- .v(options[["target"]])

  ready                   <- ( options[["target"]] != "" && length(options[["predictors"]]) > 0 && !("" %in% options[["predictors"]]) )

  if(ready){
    if(options[['noOfNearestNeighbours']] == 'auto' | options[['noOfNearestNeighbours']] == 'manual'){
      res <- .oneKClassification(dataset, options, opt, train, test, train.index, formula, target)
    } else if (options[['noOfNearestNeighbours']] == 'optimized'){
      res <- .optimizeKClassification(dataset, options, opt, train, test, train.index, formula, target)
    }
  } else {
    res                   <- NULL
  }

  jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                          "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                          "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD", "validationLeaveOneOut",
                                          "validationKFold", "target", "predictors"))

}

.oneKClassification <- function(dataset, options, opt, train, test, train.index, formula, target){

    knn.fit <- kknn::kknn(formula = formula,
                          train = train,
                          test = test,
                          k = opt[['NN']],
                          distance = opt[['distance']],
                          kernel = opt[['weights']],
                          na.action = opt[['NA']],
                          scale = options[['scaleEqualSD']])
    res <- list()
    if(is.numeric(knn.fit$fitted.values)){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = round(knn.fit$fitted.values,0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,target])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = as.character(knn.fit$fitted.values))
        res[['confusion.table']] <- table('Pred'=knn.fit$fitted.values,'Real'=test[,target])
    }
    #res[['confidence']] <- as.matrix(knn.fit$prob)
    res[['model.error']]    <- 1 - sum(diag(prop.table(res[['confusion.table']])))
    res[['Optimal.K']]      <- opt[['NN']]
    res[['Weights']]        <- as.matrix(knn.fit$W)
    res[['Distances']]      <- as.matrix(knn.fit$D)
    res[['predictions']]    <- as.matrix(res[['predictions']])
    return(res)
}

.optimizeKClassification <- function(dataset, options, opt, train, test, train.index, formula, target){

    error <- seq_along(opt[['NN']])
    count <- 1
    for( i in opt[['NN']]){
        knn.fit <- kknn::kknn(formula = formula,
                              train = train,
                              test = test,
                              k = i,
                              distance = opt[['distance']],
                              kernel = opt[['weights']],
                              na.action = opt[['NA']],
                              scale = options[['scaleEqualSD']])
        if(is.numeric(knn.fit$fitted.values)){
            confusion_table <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,target])
        } else {
            confusion_table <- table('Pred'=knn.fit$fit,'Real'=test[,target])
        }
        error[count] <- 1 - sum(diag(prop.table(confusion_table)))
        count <- count + 1
    }
    knn.fit <- kknn::kknn(formula = formula,
                          train = train,
                          test = test,
                          k = opt[['NN']][which.min(error)],
                          distance = opt[['distance']],
                          kernel = opt[['weights']],
                          na.action = opt[['NA']],
                          scale = options[['scaleEqualSD']])
    res <- list()
    if(is.numeric(knn.fit$fitted.values)){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = round(knn.fit$fitted.values,0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,target])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = knn.fit$fitted.values)
        res[['confusion.table']] <- table('Pred'=knn.fit$fitted.values,'Real'=test[,target])
    }
    res[['confidence']] <- knn.fit$prob
    res[['Model.error']] <- error
    res[['Minimal.error']] <- min(error)
    res[['Optimal.K']] <- opt[['NN']][which.min(error)]
    res[['Weights']] <- as.matrix(knn.fit$W)
    res[['Distances']] <- as.matrix(knn.fit$D)
    res[['range']] <- opt[['NN']]
    return(res)
}

.evaluationTableClassification <- function(dataset, options, opt, formula, res, jaspResults){

  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  evaluationTable                       <- createJaspTable("Evaluation Table")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  evaluationTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD", "validationLeaveOneOut",
                                    "validationKFold", "noOfFolds", "predictors", "target"))
  evaluationTable$position <- 1

  evaluationTable$addColumnInfo(name = 'model', title = '', type = 'string')
  evaluationTable$addColumnInfo(name = 'nn', title = 'No. nearest neighbors', type = 'integer')
  evaluationTable$addColumnInfo(name = 'rmse', title = 'Accuracy', type = 'number', format = 'dp:3')

  if(!is.null(res)){
      if(options[['noOfNearestNeighbours']] == 'auto'){
          row <- list(model = 'K-nn model', nn = opt[['NN']], rmse = 1-res[['model.error']])
          evaluationTable$addRows(row)
      } else if (options[['noOfNearestNeighbours']] == 'manual'){
          row <- list(model = 'K-nn model', nn = opt[['NN']], rmse = 1-res[['model.error']])
          evaluationTable$addRows(row)
      } else if (options[['noOfNearestNeighbours']] == 'optimized'){
          row <- list(model = 'K-nn model', nn = res[['Optimal.K']], rmse = 1-res[['Minimal.error']])
          evaluationTable$addRows(row)
      }
      if(options[['validationLeaveOneOut']] & !is.null(res)){
          result <- .looCvClassification(dataset, options, opt, formula)
          row <- list(model = "Leave-One-Out cross validaded model", nn = result[['Optimal.K']], rmse = 1 - sqrt(result[['minimal.error']]))
          evaluationTable$addRows(row)
      }
      if(options[['validationKFold']] & !is.null(res)){
          result_fold <- .kFoldClassification(dataset, options, opt, formula, res)
          row <- list(model = "K-fold cross validated model", nn = result_fold[['Optimal.K']], rmse = 1 - result_fold[['minimal.error']])
          evaluationTable$addRows(row)
      }
      message <- paste0('The model is tested on ',nrow(res[["predictions"]]), " data points.")
      evaluationTable$addFootnote(message, symbol="<i>Note.</i>")
  } else {
      row <- list(model = 'K-nn model', nn = ".", rmse = ".")
      evaluationTable$addRows(row)
      message <- "The model has not been applied to any data yet."
      evaluationTable$addFootnote(message, symbol="<i>Note.</i>")
  }
}

.confusionTableClassifiaction <- function(dataset, options, res, jaspResults){

  if(!is.null(jaspResults[["confusionTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  confusionTable                       <- createJaspTable("Confusion table")
  jaspResults[["confusionTable"]]      <- confusionTable
  confusionTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD", "validationLeaveOneOut",
                                    "validationKFold", "noOfFolds", "predictors", "target", "confusionTable"))
  confusionTable$position <- 3
  target <- .v(options[["target"]])
  title_observed <- "Observed"

  if(!is.null(res)){

    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    for( i in 1:length(rownames(res[["confusion.table"]]))){
        confusionTable$addColumnInfo(name = paste("varname_real",i, sep = ""), title = as.character(rownames(res[["confusion.table"]])[i]), type = "integer")
    }

    for( i in 1:length(rownames(res[["confusion.table"]]))){
        row <- list("varname_pred" = as.character(rownames(res[["confusion.table"]]))[i])
        for(j in 1:length(rownames(res[["confusion.table"]]))){
            row[[paste("varname_real",j,sep="")]] <- res[["confusion.table"]][i,j]
        }
        if(i == 1){
            row[["pred_name"]] <- "Predicted"
        } else {
            row[["pred_name"]] <- ""
        }
        confusionTable$addRows(row)
    }

  } else if (options[["target"]] != "" && is.null(res)){

    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    for( i in 1:length(unique(dataset[ ,target]))){
        confusionTable$addColumnInfo(name = paste("varname_real",i, sep = ""), title = as.character(sort(unique(dataset[,target]), decreasing = FALSE))[i], type = "integer")
    }

    for( i in 1:length(unique(dataset[,target]))){
        row <- list("varname_pred" = as.character(sort(unique(dataset[,target]), decreasing = FALSE))[i])
        for(j in 1:length(unique(dataset[,target]))){
            row[[paste("varname_real",j,sep="")]] <- "."
        }
        if(i == 1){
            row[["pred_name"]] <- "Predicted"
        } else {
            row[["pred_name"]] <- ""
        }
        confusionTable$addRows(row)
    }
  } else {

    confusionTable$addColumnInfo(name = "pred_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_pred", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_real1", title = ".", type = "integer")
    confusionTable$addColumnInfo(name = "varname_real2", title = ".", type = 'integer')

    row <- list(pred_name = "Predicted", varname_pred = ".", varname_real1 = "", varname_real2= "")
    confusionTable$addRows(row)
    row <- list(pred_name = "", varname_pred = ".", varname_real1= "", varname_real2= "")
    confusionTable$addRows(row)

    }
}

.optimizationTable <- function(dataset, options, res, formula, jaspResults){

  if(!is.null(jaspResults[["optimizationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  optimizationTable                       <- createJaspTable("Model optimization")
  jaspResults[["optimizationTable"]]      <- optimizationTable
  optimizationTable$dependOnOptions(c("optimizeModel", "optimizeModelMaxK"))
  optimizationTable$position <- 2
  target <- .v(options[["target"]])
  title_observed <- "Observed"

  optimizationTable$addColumnInfo(name = "model", title = "", type = "string")
  optimizationTable$addColumnInfo(name = "RMSE", title = "Accuracy", type = "number", format = "sf:4;dp:3")
  optimizationTable$addColumnInfo(name = "k", title = "No. nearest neighbors", type = "integer")
  optimizationTable$addColumnInfo(name = "weights", title = "Weights", type = "string")
  optimizationTable$addColumnInfo(name = "distance", title = "Distance parameter", type = "number", format = "dp:1")

  if(!is.null(res)){
    result <- .optimizerKNN(dataset, formula, kmax = options[["optimizeModelMaxK"]], distance_from = 0.1, distance_to = 10)
    row <- list(model = "Optimal parameters", RMSE = 1-result[["MinStatistic"]], k = result[["OptK"]], weights = result[["OptWeights"]], distance = result[["OptDistance"]])
  } else {
    row <- list(model = "Optimal parameters", RMSE = ".", k = ".", weights = ".", distance = ".")
  }
  optimizationTable$addRows(row)
}

.plotErrorVsKClassification <- function(dataset, options, opt, formula, res, jaspResults){

  if( !(options[["noOfNearestNeighbours"]] == "optimized"))
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No optimization has been selected."))
  if(is.null(res))
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))

    if(options[['noOfNearestNeighbours']] == 'optimized'){
      p <- .plotKoptimizedClassification(opt[['NN']], 1- res[['Model.error']], type = "optimized")
    }
    return(createJaspPlot(plot = p, title = "Error vs. K"))
}

.plotKoptimizedClassification <- function(xVar, yVar, type){

  isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
  isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
  bothNumeric <- isNumericX && isNumericY
  d <- data.frame(x = xVar, y = yVar)
  d <- na.omit(d)
  if (!isNumericX)
    d$x <- as.factor(d$x)
  if (!isNumericY)
    d$y <- as.factor(d$y)

  if(type == "optimized")
    y.title <- "Accuracy"
  if(type == "crossvalidated")
    y.title = "Model error"

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
    JASPgraphs::geom_point()

  if (isNumericX) {
    p <- p + ggplot2::scale_x_continuous(name = "Nearest neighbors", breaks = xBreaks, limits = range(xBreaks))
  } else {
    p <- p + ggplot2::scale_x_discrete(name = "Nearest neighbors")
  }
  if (isNumericY) {
    p <- p + ggplot2::scale_y_continuous(name = y.title, breaks = yBreaks, limits = range(yBreaks))
  } else {
    p <- p + ggplot2::scale_y_discrete(name = y.title)
  }

 p <- JASPgraphs::themeJasp(p)
 return(p)
}

.predictionsTableClassification <- function(options, opt, res, jaspResults){

  if(!is.null(jaspResults[["predictionsTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  predictionsTable                       <- createJaspTable("Predictions Table")
  jaspResults[["predictionsTable"]]      <- predictionsTable
  predictionsTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo",
                                    "scaleEqualSD", "predictors", "target", "tablePredictions"))
  predictionsTable$position <- 4

  from <- options[["predictionsFrom"]]
  to <- options[['predictionsTo']]
  predictors <- options[["predictors"]]
  target <- options[["target"]]

  predictionsTable$addColumnInfo(name="number", title="Obs. number", type="integer")
  predictionsTable$addColumnInfo(name="real", title="Observed", type="string")
  predictionsTable$addColumnInfo(name='predicted',title = 'Predicted', type = 'string')
  if(options[['tablePredictionsConfidence']])
      predictionsTable$addColumnInfo(name ='confidence',title = 'Confidence', type = 'number', format = 'dp:2')

  if(is.null(res) & !options[['tablePredictionsConfidence']]){
        row <- list(number = ".", real = ".", predicted = ".")
        predictionsTable$addRows(row)
    } else if (is.null(res) & options[["tablePredictionsConfidence"]]) {
        row <- list(number = ".", real = ".", predicted = ".", confidence = ".")
        predictionsTable$addRows(row)
    } else {
        if(options[['tablePredictionsConfidence']]){
            for(i in from:to){
                row <- list(number = as.numeric(res[['predictions']][i,1]),
                            real = as.character(res[['predictions']][i,2]),
                            predicted = as.character(res[['predictions']][i,3]),
                            confidence = as.numeric(max(res[["confidence"]][i,])))
                predictionsTable$addRows(row)
            }
        } else {
            for(i in from:to){
                row <- list(number = as.numeric(res[['predictions']][i,1]),
                            real = as.character(res[['predictions']][i,2]),
                           predicted = as.character(res[['predictions']][i,3]))
                predictionsTable$addRows(row)
            }
        }
    }
}

.distancesTableClassification <- function(options, opt, res, jaspResults){

  if(!is.null(jaspResults[["distancesTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  distancesTable                       <- createJaspTable("Distances Table")
  jaspResults[["distancesTable"]]      <- distancesTable
  distancesTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo",
                                    "scaleEqualSD", "predictors", "target", "tableDistances"))
  distancesTable$position <- 5

  from <- options[["predictionsFrom"]]
  to <- options[['predictionsTo']]
  predictors <- options[["predictors"]]
  target <- options[["target"]]

  distancesTable$addColumnInfo(name = "number", title = "Obs. number", type = "integer")
    if(!is.null(res)){
        for(i in 1:res[['Optimal.K']]){
            distancesTable$addColumnInfo(name =paste0('distance', i),title = paste0('Distance ',i), type = 'number', format = 'dp:2')
        }
    } else {
        distancesTable$addColumnInfo(name = 'distance1', title = "Distance", type = 'integer')
    }

    if(is.null(res)){
        row <- list(number = ".", distance = ".")
        distancesTable$addRows(row)
    } else {
      data <- as.data.frame(res[["Distances"]][from:to, ])
      number <- 1:nrow(data)
      data <- cbind(number, data)
      for(i in 2:ncol(data)){
        colnames(data)[i] <- paste0('distance', i-1)
      }
      distancesTable$setData(data)
    }

    if(opt[['distance']]==1)
        message <- 'Distances shown are the Manhattan distances'
    if (opt[['distance']] == 2)
      message <-'Distances shown are the Euclidian distances'
    distancesTable$addFootnote(message, symbol="<i>Note.</i>")

}

.weightsTableClassification <- function(options, opt, res, jaspResults){

  if(!is.null(jaspResults[["weightsTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  weightsTable                       <- createJaspTable("Weights Table")
  jaspResults[["weightsTable"]]      <- weightsTable
  weightsTable$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                    "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                    "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo",
                                    "scaleEqualSD", "predictors", "target", "tableWeights"))
  weightsTable$position <- 6

  from <- options[["predictionsFrom"]]
  to <- options[['predictionsTo']]
  predictors <- options[["predictors"]]
  target <- options[["target"]]

  weightsTable$addColumnInfo(name = "number", title = "Obs. number", type = "integer")
  if(!is.null(res)){
    for(i in 1:res[['Optimal.K']]){
      weightsTable$addColumnInfo(name = paste0('weight',i), title = paste('Weight',i,sep = ' '), type = 'number', format = 'dp:2')
    }
  } else {
    weightsTable$addColumnInfo(name = 'weight1', title = 'Weights', type = "integer")
  }

  if(is.null(res)){
    row <- list(number = ".", weights1 = ".")
    weightsTable$addRows(row)
  } else {
    data <- as.data.frame(res[["Weights"]][from:to, ])
    number <- 1:nrow(data)
    data <- cbind(number, data)
    for(i in 2:ncol(data)){
      colnames(data)[i] <- paste0('weight', i-1)
    }
    weightsTable$setData(data)
  }

  message <- paste0('Weights are calculated using the ', opt[['weights']], ' weighting scheme.')
  weightsTable$addFootnote(message, symbol="<i>Note.</i>")
}

.looCvClassification <- function(dataset, options, opt, formula){

  if(options[["noOfNearestNeighbours"]] =="auto" || options[["noOfNearestNeighbours"]] == "manual"){
    ks <- opt[["NN"]]
  } else {
    ks <- options[["optimizedFrom"]]:options[["optimizedTo"]]
  }

    knn.fit <- kknn::train.kknn(formula = formula,
                                data = dataset,
                                ks = ks,
                                distance = opt[['distance']],
                                kernel = opt[['weights']],
                                na.action = opt[['NA']],
                                scale = options[["scaleEqualSD"]])

    if(options[["noOfNearestNeighbours"]] =="auto" || options[["noOfNearestNeighbours"]] == "manual"){
      res <- list()
      res[['error']] <- as.numeric(knn.fit$MEAN.ABS)
      res[['Optimal.K']] <- knn.fit$best.parameters$k
      res[['optimal.weights']] <- knn.fit$best.parameters$kernel
      res[["minimal.error"]] <- min(res[["error"]])
    } else {
      res <- list()
      res[['error']] <- as.numeric(knn.fit$MEAN.ABS)
      res[['Optimal.K']] <- knn.fit$best.parameters$k
      res[['optimal.weights']] <- knn.fit$best.parameters$kernel
      res[["minimal.error"]] <- min(res[["error"]])
    }

    return(res)
}

.kFoldClassification <- function(dataset,options,opt,formula,res){

    knn.fit <- kknn::cv.kknn(formula = formula,
                             data = dataset,
                             distance = opt[['distance']],
                             kernel = opt[['weights']],
                             na.action = opt[['NA']],
                             kcv = options[['noOfFolds']],
                             k = res[['Optimal.K']])
    error <- 1 - length(which(knn.fit[[1]][,1] == knn.fit[[1]][,2]))/nrow(dataset)
    result <- list()
    result[['error']] <- error
    result[['Optimal.K']] <- res[['Optimal.K']]
    result[["minimal.error"]] <- min(error)
    return(result)
}

.optimizerKNN <- function(dataset, formula, kmax, distance_from, distance_to){
    dist <- seq(distance_from,distance_to,by = 0.1)
    # make empty vector to save which distance parameters are used
    d <- numeric()
    # make empty vector to save which values are created
    value <- numeric()
    # same for k
    k<-numeric()
    # and kernel
    kernel <- numeric()
    for(i in 1:length(dist)){
        d <- dist[i]
        kfit <- kknn::train.kknn(formula,dataset,kmax,distance = d)
        if(kfit$response == "continuous"){
            value[i] <- min(sqrt(kfit$MEAN.SQU))
        } else {
            value[i] <- min(kfit$MISCLASS)
        }
        k[i] <- kfit$best.parameters$k
        kernel[i] <- kfit$best.parameters$kernel
    }
    if(kfit$response=="continuous"){
        lab = "RMSE"
    } else {
        lab = "Misclassification"
    }
    index <- which.min(value)
    optimal.k <- k[index]
    optimal.weights <- kernel[index]
    optimal.distance <- dist[index]
    statistic <- min(value)
    return(list("OptK" = optimal.k,
                "OptWeights" = optimal.weights,
                "OptDistance" = optimal.distance,
                "MinStatistic" = statistic,
                "value" = value,
                "dist" = dist,
                "lab" = lab,
                "k" = k,
                "kmax" = kmax))

}

.plotOptimization <- function(formula,dataset,options){

    res <- .optimizerKNN(dataset,formula, kmax = options[["optimizeModelMaxK"]], distance_from = 0.1, distance_to = 10)

    plot3D::scatter2D(y=res[["value"]],
              x = res[["dist"]],
              type = "l",
              bty = "n",
              xlab = "Distance parameter",
              ylab = res[["lab"]],
              las = 1,
              lwd = 4,
              colvar = res[["k"]],
              clim = switch(EXPR = length(unique(res[["k"]]))<3,
                            clim = c(1,res[["kmax"]]),
                            clim = range(res[["k"]])),
              clab = "No. nearest neighbors",
              main = "",
              xlim = range(res[["dist"]]),
              NAcol = "white")

}
