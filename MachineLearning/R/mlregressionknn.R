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

MLRegressionKNN <- function(jaspResults, dataset, options, state=NULL) {

		# read variables ##
	  dataset              	<- .readDataMlKNNclassification(dataset, options)
	  # error handling ##
	  .mlKnnClassificationErrorHandling(dataset, options)
		# set the seed so that every time the same set is chosen (to prevent random results) ##
	  set.seed(1)
	  jaspResults$title 		<- 'K-Nearest Neighbors Regression'
		# Set the right options for the analysis ##
		ready <- length(options[["predictors"]][options[["predictors"]] != ""] > 0) && options[["target"]] != ""

		if(!ready){
			.evaluationTableRegression(dataset, options, opt, res = NULL, jaspResults)
		} else {
			opt 								<- .setOptions(options,dataset)
			# Run the analysis
			res <- .DoKNNregression(dataset, options, opt, jaspResults)
			# create the evaluation table
			.evaluationTableRegression(dataset, options, opt, res, jaspResults)
			# Create the predictions table
	    .predictionsTableClassification(options, opt, res, jaspResults)
	    # Create the distances table
	    .distancesTableClassification(options, opt, res, jaspResults)
	    # Create the weights table ##
	    .weightsTableClassification(options, opt, res, jaspResults)
			# Create the Error vs K plot ##
			.ErrorVsKplotRegression(options, res, jaspResults)
		}
}

.setOptions <- function(options,dataset){
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

.makeformula <- function(predictors,target){
	formula <- paste(target, "~", paste(predictors, collapse=" + "))
	return(formula)
}

.DoKNNregression <- function(dataset, options, opt, jaspResults){

	formula            <- .makeformulaClassification(options)

	train.index <- base::sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(opt[['ntrain']] * 0.01, 1 - (opt[['ntrain']] * 0.01)))
	train <- dataset[which(train.index == TRUE), ]
	test <- dataset[which(train.index == FALSE), ]

	if(options[['noOfNearestNeighbours']] == 'auto' || options[['noOfNearestNeighbours']] == 'manual'){
		res <- .OneKregression(dataset, options, opt, train, test, train.index, formula, .v(options[["target"]]))
	} else if (options[['noOfNearestNeighbours']] == 'optimized'){
		res <- .OptimizeKregression(dataset, options, opt, train, test, train.index, formula, .v(options[["target"]]))
	}

	res[["formula"]] <- formula

	jaspResults[["res"]] <- createJaspState(res)
  jaspResults[["res"]]$dependOnOptions(c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData",
                                          "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights",
                                          "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD", "validationLeaveOneOut",
                                          "validationKFold", "target", "predictors", "seed"))

	return(jaspResults[["res"]]$object)
}

.OneKregression <- function(dataset,options,opt,train,test,train.index,formula,target){

	knn.fit <- kknn::kknn(formula = formula,
						  train = train,
						  test = test,
						  k = opt[['NN']],
						  distance = opt[['distance']],
						  kernel = opt[['weights']],
						  na.action = opt[['NA']],
						  scale = options[['scaleEqualSD']])
	res <- list(
		'predictions'=NULL,
		'RMSE'=NULL,
		'K' = NULL
	)
	y <- dataset[which(train.index == FALSE),target]
	res[['predictions']] <- data.frame(
		'Observation' = 1:nrow(test),
		'Real' = y,
		'Prediction'= knn.fit$fitted.values)
	res[['RMSE']] <- mean((knn.fit$fitted.values - y)^2)^0.5
	res[['Optimal.K']] <- opt[['NN']]
	res[['Weights']] <- as.matrix(knn.fit$W)
	res[['Distances']] <- as.matrix(knn.fit$D)
	return(res)
}

.OptimizeKregression <- function(dataset,options,opt,train,test,train.index,formula,target){

	RMSE <- seq_along(opt[['NN']])
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
		y <- dataset[which(train.index == FALSE),target]
		RMSE[count] <- mean((knn.fit$fitted.values - y)^2)^0.5
		count <- count +1
	}
	knn.fit <- kknn::kknn(formula = formula,
						  train = train,
						  test = test,
						  k = opt[['NN']][which.min(RMSE)],
						  distance = opt[['distance']],
						  kernel = opt[['weights']],
						  na.action = opt[['NA']],
						  scale = options[['scaleEqualSD']])
	res <- list(
		'predictions' = NULL,
		'RMSE' = NULL,
		'Optimal.K' = NULL,
		'Minimal.RMSE' = NULL
	)
	res[['predictions']] <- data.frame(
		'Observation' = 1:nrow(test),
		'Real' = y,
		'Prediction'= knn.fit$fitted.values)
	res[['Weights']] <- as.matrix(knn.fit$W)
	res[['Distances']] <- as.matrix(knn.fit$D)
	res[['RMSE']] <- RMSE
	res[['Minimal.RMSE']] <- min(RMSE)
	res[['Optimal.K']] <- opt[['NN']][which.min(RMSE)]
	res[['range']] <- opt[['NN']]
	return(res)
}

.evaluationTableRegression <- function(dataset, options, opt, res, jaspResults){

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
  evaluationTable$addColumnInfo(name = 'rmse', title = 'RMSE', type = 'number', format = 'dp:3')

  if(!is.null(res)){
      if(options[['noOfNearestNeighbours']] == 'auto'){
          row <- list(model = 'K-nn model', nn = opt[['NN']], rmse = res[['RMSE']])
          evaluationTable$addRows(row)
      } else if (options[['noOfNearestNeighbours']] == 'manual'){
          row <- list(model = 'K-nn model', nn = opt[['NN']], rmse = res[['RMSE']])
          evaluationTable$addRows(row)
      } else if (options[['noOfNearestNeighbours']] == 'optimized'){
          row <- list(model = 'K-nn model', nn = res[['Optimal.K']], rmse = res[['Minimal.RMSE']])
          evaluationTable$addRows(row)
      }
      if(options[['validationLeaveOneOut']] & !is.null(res)){
          result <- .LOOCVregression(dataset, options, opt, res[["formula"]], res)
          row <- list(model = "Leave-One-Out cross validated model", nn = result[['OptimalK']], rmse = sqrt(result[["Minimal.MSE"]]))
          evaluationTable$addRows(row)
      }
      if(options[['validationKFold']] & !is.null(res)){
          result_fold <- .Kfoldregression(dataset, options, opt, res[["formula"]], res)
          row <- list(model = "K-fold cross validated model", nn = result_fold[['OptimalK']], rmse = result_fold[['RMSE']])
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

.PlotErrorVsK <- function(options, res){
	if( !(options[["noOfNearestNeighbours"]] == "optimized"))
		return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No optimization has been selected."))
	p <- .plotKoptimizedClassification(res[['range']], res[['RMSE']], type = "optimized", title = "RMSE")
	return(createJaspPlot(plot = p, title = "Error vs. K", height = 300, width = 400))
}

.LOOCVregression <- function(dataset, options, opt, formula, res){

	knn.fit <- kknn::train.kknn(formula = formula,
															data = dataset,
															ks = res[['Optimal.K']],
															distance = opt[['distance']],
															kernel = opt[['weights']],
															na.action = opt[['NA']],
															scale = options[["scaleEqualSD"]])
	result <- list(
		'MSE' = NULL,
		'OptimalK' = NULL,
		'optimal.weights' = NULL,
		'Minimal.MSE' = NULL
	)
	result[['MSE']] <- as.numeric(knn.fit$MEAN.SQU)
	result[['OptimalK']] <- res[['Optimal.K']]
	result[['optimal.weights']] <- knn.fit$best.parameters[['kernel']]
	result[['Minimal.MSE']] <- min(result[['MSE']])
	return(result)
}

.Kfoldregression <- function(dataset, options, opt, formula, res){
	knn.fit <- kknn::cv.kknn(formula = formula,
							 data = dataset,
							 distance = opt[['distance']],
							 kernel = opt[['weights']],
							 na.action = opt[['NA']],
							 kcv = options[['noOfFolds']],
							 k = res[['Optimal.K']])
	RMSE <- mean((knn.fit[[1]][,1] - knn.fit[[1]][,2])^2)^0.5
	result <- list(
		'Predictions' = NULL,
		'RMSE' = NULL
	)
	result[['Predictions']] <- data.frame(
		'Observation' = 1:nrow(dataset),
		'True' = as.numeric(knn.fit[[1]][,1]),
		'Prediction' = as.numeric(knn.fit[[1]][,2]))
	result[['RMSE']] <- RMSE
	result[['OptimalK']] <- res[['Optimal.K']]
	return(result)
}

.ErrorVsKplotRegression <- function(options, res, jaspResults){
	if(options[['plotErrorVsK']])
	{
		 if(is.null(jaspResults[["plotErrorVsK"]]))
		 {
		 jaspResults[["plotErrorVsK"]] 		<- .PlotErrorVsK(options, res)
		 jaspResults[["plotErrorVsK"]]		$dependOnOptions(c("plotErrorVsK"))
		 jaspResults[["plotErrorVsK"]] 		$position <- 3
		 }
	}
}
