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

MLClusteringKMeans <- function(jaspResults, dataset, options, ...) {

    # set the seed so that every time the same set is chosen (to prevent random results) ##
    set.seed(1)
    jaspResults$title 					<- "K-Means clustering"
    # read variables ##
    dataset                     <- .readDataMlKmeans(dataset, options)
    # error handling & code variable names in base64
    .MlKmeansErrorHandling(dataset, options)

    ready <- length(options[["predictors"]][options[["predictors"]] != ""] > 0)
    if(!ready){
      .EvaluationTableKmeans(res = NULL, options, jaspResults)
    } else {
    # Set the right options for the analysis
    opt                         <- .OptionsSet(dataset, options)
    predictors                  <- .v(options[["predictors"]])
    # Run the analysis and save the results
    res                         <- .DoKmeansAnalysis(dataset, options, opt, predictors, jaspResults, ready)
    # create the evaluation table
    .EvaluationTableKmeans(res, options, jaspResults)
    # create the cluster information table
    .clusterInfoTable(options, res, predictors, jaspResults)
    # Create the predictions table
    .PredictionsTableKmeans(res, options, jaspResults)
     # Create the 2-d clusterplot
    .twoDClusterPlot(dataset, res, opt, options, predictors, jaspResults)
    # Create the within ss vs cluster plot
    .withinssPlot(options, res, jaspResults)
    # Create the criterion vs cluster plot ##
    .criterionPlot(options, res, jaspResults)
  }
}

.readDataMlKmeans <- function(dataset, options){
  predictors <- unlist(options['predictors'])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)) {
          dataset <- .readDataSetToEnd(columns.as.numeric = predictors, exclude.na.listwise = predictors)
  }
  return(dataset)
}

.MlKmeansErrorHandling <- function(dataset, options){
  predictors <- unlist(options$predictors)
  if(length(predictors[predictors != '']) > 0){
      for(i in 1:length(predictors)){
          errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                               all.target = predictors[i],
                               observations.amount = "< 2",
                               exitAnalysisIfErrors = TRUE)
      }
  }
}

.OptionsSet <- function(dataset,options){
    opt <- list()
    # set iterations
    if(options[['noOfIterations']] == 'auto'){
        opt[['iter']] <- 15
    } else if (options[['noOfIterations']] == 'manual'){
        opt[['iter']] <- options[['iterationsCount']]
    }
    # set random sets
    if(options[['noOfRandomSets']] == 'auto'){
        opt[['sets']] <- 20
    } else if (options[['noOfRandomSets']] == 'manual'){
        opt[['sets']] <- options[['randomSetCount']]
    }
    # set algorithm
    if(options[['algorithm']] == 'hartiganWong'){
        opt[['algorithm']] <- 'Hartigan-Wong'
    } else if (options[['algorithm']] == 'lloyd'){
        opt[['algorithm']] <- 'Lloyd'
    } else if (options[['algorithm']] == 'macQueen'){
        opt[['algorithm']] <- 'MacQueen'
    }
    # set clusters
    if(options[['noOfClusters']] == 'auto'){

        ifelse(test = options[['noOfClusters']] == 'auto' & nrow(dataset) <= 1000,
               yes = opt[['clusters']] <- 2,
               no = ifelse(test = options[['noOfClusters']] == 'auto' & nrow(dataset) <= 20000,
                           yes = opt[['clusters']] <- 2*round(((nrow(dataset)*0.001)+1)/2),
                           no = ifelse(test = options[['noOfClusters']] == 'auto' & nrow(dataset) > 21000,
                                       yes = opt[['clusters']] <- 21,
                                       no = opt[['clusters']] <- 0)))
    }
    if(options[['noOfClusters']] == 'manual'){
        opt[['clusters']] <- options[['clusterSize']]
    }
    if(options[['noOfClusters']] == 'optimized'){
        opt[['clusters']] <- options[['optimizedFrom']]:options[['optimizedTo']]
    }
    if(options[['noOfClusters']] == 'robust'){
        opt[['clusters']] <- options[['robustFrom']]:options[['robustTo']]
        if (nrow(dataset)>2000){
            opt[['Pam']] <- FALSE
        } else {
            opt[['Pam']] <- TRUE
        }
        if(options[['criterion']] == 'silhoutteLength'){
            opt[['criterion']] <- 'asw'
        } else if (options[['criterion']] == 'Multiasw'){
            opt[['criterion']] <- 'multiasw'
        } else if (options[['criterion']] == 'Calinski-Harabasz'){
            opt[['criterion']] <- 'ch'
        }
    }
    return(opt)
}

.DoKmeansAnalysis <- function(dataset, options, opt, predictors, jaspResults, ready){
    if(ready){
        if(options[['noOfClusters']] == 'auto' | options[['noOfClusters']] == 'manual'){
            res <- .Kmeansclusterspecified(dataset, options, opt, predictors)
        } else if (options[["noOfClusters"]] == 'robust'){
            res <- .robustclustering(dataset, options, opt, predictors)
        } else if (options[['noOfClusters']] == 'optimized'){
            res <- .Kmeansoptimized(dataset, options, opt, predictors)
        }
    } else {
        res <- NULL
    }
    jaspResults[["res"]] <- createJaspState(res)
    jaspResults[["res"]]$dependOnOptions(c("predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                            "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo", "criterion"))

    return(jaspResults[["res"]]$object)
}

.Kmeansclusterspecified <- function(dataset,options,opt,predictors){
    kfit <- kmeans(dataset[,predictors],
                   centers = opt[['clusters']],
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    res <- list(
        'Predictions' = NULL,
        'clusters' = NULL,
        'centroids' = NULL,
        'size' = NULL,
        'WSS' = NULL,
        'BSS' = NULL,
        'TSS' = NULL
    )
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['clusters']] <- opt[['clusters']]
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    m = ncol(kfit$centers)
    n = length(kfit$cluster)
    k = nrow(kfit$centers)
    D = kfit$tot.withinss
    res[['AIC']] <- D + 2*m*k
    res[['BIC']] <- D + log(n)*m*k
    return(res)
}

.robustclustering <- function(dataset,options,opt,predictors){
    # library(fpc)
    fit <- fpc::pamk(dataset[,predictors],
                     krange = opt[['clusters']],
                     criterion = opt[['criterion']],
                     usepam = opt[['Pam']],
                     scaling = FALSE,
                     alpha = 0.001,
                     critout = FALSE,
                     ns = 10)
    res <- list()
    res[['clusters']] <- fit$nc
    res[['criterion.krange']] <- fit$crit[which(fit$crit!=0)]
    res[['clusterrange']] <- opt[['clusters']]
    for(i in 1:length(opt[["clusters"]])){
        kfit_tmp <- kmeans(dataset[,predictors],
                           centers = opt[['clusters']][[i]],
                           iter.max = opt[['iter']],
                           nstart = opt[['sets']],
                           algorithm = opt[['algorithm']])
        res[['TSS_tmp']][i] <- kfit_tmp$totss
        res[['BSS_tmp']][i] <- kfit_tmp$betweenss
        m = ncol(kfit_tmp$centers)
        n = length(kfit_tmp$cluster)
        k = nrow(kfit_tmp$centers)
        D = kfit_tmp$tot.withinss
        res[['AIC']][i] <- D + 2*m*k
        res[['BIC']][i] <- D + log(n)*m*k
    }
    kfit <- kmeans(dataset[,predictors],
                   centers = fit$nc,
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    dAIC <- res[["AIC"]]-min(res[['AIC']])
    res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
    dBIC <- res[['BIC']] - min(res[['BIC']])
    res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
    return(res)
}

.Kmeansoptimized <- function(dataset,options,opt, predictors){
    WSS <- numeric(options[['optimizedTo']] - options[['optimizedFrom']])
    clusters <- opt[['clusters']]
    res<-list()
    res[['clusterrange']] <- clusters
    for(i in seq_along(clusters)){
        index <- clusters[i]
        kfit_tmp <- kmeans(dataset[,predictors],
                           centers = index,
                           iter.max = opt[['iter']],
                           nstart = opt[['sets']],
                           algorithm = opt[['algorithm']])
        res[['WithinSumSquares']][i] <- kfit_tmp$tot.withinss
        m = ncol(kfit_tmp$centers)
        n = length(kfit_tmp$cluster)
        k = nrow(kfit_tmp$centers)
        D = kfit_tmp$tot.withinss
        res[['AIC']][i] <- D + 2*m*k
        res[['BIC']][i] <- D + log(n)*m*k
        res[['TSS_tmp']][i] <- kfit_tmp$totss
        res[['BSS_tmp']][i] <- kfit_tmp$betweenss

    }
    res[['clusters']] <- .determineoptimum(res, by = "AIC")
    # predictions for best model.
    kfit <- kmeans(dataset[,predictors],
                   centers = res[['clusters']],
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    dAIC <- res[["AIC"]]-min(res[['AIC']])
    res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
    dBIC <- res[['BIC']] - min(res[['BIC']])
    res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
    return(res)
}

.determineoptimum <- function(res,by = "AIC"){
    if(by == "AIC"){
        optimum <- res[["clusterrange"]][which.min(res[['AIC']])]
    } else if (by == "BIC"){
        optimum <- res[["clusterrange"]][which.min(res[['BIC']])]
    }
    return(optimum)
}

.EvaluationTableKmeans <- function(res, options, jaspResults){

  if(!is.null(jaspResults[["evaluationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  evaluationTable                       <- createJaspTable("Evaluation Table")
  jaspResults[["evaluationTable"]]      <- evaluationTable
  evaluationTable$dependOnOptions(c("predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                      "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo",
                                      "criterion", "plotCriterionVsClusters", "plotErrorVsK", "plot2dCluster", "ready"))

    if(options[["noOfClusters"]] == "auto" || options[["noOfClusters"]] == 'manual'){

      evaluationTable$addColumnInfo(name = 'title', title = "", type = 'string')
      evaluationTable$addColumnInfo(name = 'clusters', title = 'Model Clusters', type = 'integer')
      evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
      evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1')
      evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1')

    } else {

      evaluationTable$addColumnInfo(name = 'title', title = "", type = 'string')
      evaluationTable$addColumnInfo(name = 'clusters', title = 'Model Clusters', type = 'integer')
      evaluationTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
      evaluationTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1')
      evaluationTable$addColumnInfo(name = "aicweights", title = "AIC Weights", type = "number", format = "dp:2")
      evaluationTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1')
      evaluationTable$addColumnInfo(name = "bicweights", title = "BIC Weights", type = "number", format = "dp:2")

    }

    evaluationTable$position <- 1

    if(is.null(res)){

        message <- "The model has not been applied to any data yet."
        evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")

        row <- list()
        evaluationTable$addRows(row)
        return()

    } else {

        if(options[['noOfClusters']] == "auto" | options[['noOfClusters']] == "manual"){

            row <- list(title = 'K-means model', clusters = res[['clusters']], measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']], bic = res[['BIC']])
            evaluationTable$addRows(row)

        } else if (options[["noOfClusters"]]=="optimized" | options[['noOfClusters']] == "robust"){

            row <- list()
            index_best_model <- which(res[['clusterrange']] == res[['clusters']])
            for(i in 1:length(res[['clusterrange']])){
                if(i == index_best_model){
                    row[[1]] <- list(title = "Best model", clusters = res[['clusterrange']][[i]], measure = res[["BSS_tmp"]][[i]]/res[["TSS_tmp"]][[i]], aic = res[["AIC"]][[i]], aicweights = res[["AICweights"]][[i]], bic = res[["BIC"]][[i]], bicweights = res[['BICweights']][[i]])
                } else if (i > index_best_model){
                    row[[i]] <- list(title = "", clusters = res[['clusterrange']][[i]], measure = res[["BSS_tmp"]][[i]]/res[["TSS_tmp"]][[i]], aic = res[["AIC"]][[i]], aicweights = res[["AICweights"]][[i]], bic = res[["BIC"]][[i]], bicweights = res[['BICweights']][[i]])
                } else if (i < index_best_model){
                    row[[i+1]] <- list(title = "", clusters = res[['clusterrange']][[i]], measure = res[["BSS_tmp"]][[i]]/res[["TSS_tmp"]][[i]], aic = res[["AIC"]][[i]], aicweights = res[["AICweights"]][[i]], bic = res[["BIC"]][[i]], bicweights = res[['BICweights']][[i]])
                }
            }
            evaluationTable$addRows(row)
        }
        message <- paste0('The model is applied to ',nrow(res[["Predictions"]]), " data points.")
        evaluationTable$addFootnote(message=message, symbol="<i>Note.</i>")
    }
    evaluationTable$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")
    evaluationTable$addCitation("Wagenmakers, E. J., & Farrell, S. (2004). AIC model selection using Akaike weights. Psychonomic bulletin & review, 11(1), 192-196.")
}

.clusterInfoTable <- function(options, res, predictors, jaspResults){

  if(!is.null(jaspResults[["clusterInfoTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if (options[['tableClusterInformation']]){
      if(is.null(jaspResults[["tableClusterInformation"]])){

  clusterInfoTable                        <- createJaspTable("Cluster Information")
  jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
  clusterInfoTable$dependOnOptions(c("tableClusterInformation","predictors", "target",
                                      "noOfClusters","noOfRandomSets", "noOfIterations",
                                      "algorithm", "clusterSize", "optimizedFrom", "optimizedTo",
                                      "robustFrom", "robustTo", "tableClusterInfoSize",
                                      "tableClusterInfoSumSquares", "tableClusterInfoCentroids",
                                      "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares"))
  clusterInfoTable$position               <- 2
  clusterInfoTable$transpose              <- TRUE

  if(!is.null(res)){
    clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
    cluster <- 1:res[["clusters"]]
    data <- as.data.frame(cluster)
  }

  if (options[['tableClusterInfoSize']]){
    clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
    if(!is.null(res)){
      size <- res[["size"]]
      data <- cbind(data, size)
    }
  }
  if (options[['tableClusterInfoSumSquares']]){
    clusterInfoTable$addColumnInfo(name = 'withinss', title = 'Within Sum of Squares', type = 'number', format = 'dp:2')
    if(!is.null(res)){
      withinss <- res[["WSS"]]
      data <- cbind(data, withinss)
    }
  }
  if(options[['tableClusterInfoCentroids']] & length(predictors) > 0){
      for( i in 1:length(predictors)){
          clusterInfoTable$addColumnInfo(name = paste0('centroid', i), title = paste0('Centroid ', .unv(predictors)[i]), type = 'number', format = 'dp:3')
          data <- cbind(data, res[['centroids']][ ,i])
          colnames(data)[length(colnames(data))] <- paste0("centroid", i)
      }
  } else if(options[['tableClusterInfoCentroids']] & length(predictors) == 0){
      clusterInfoTable$addColumnInfo(name = "centroid", title = "Centroid", type = 'number', format = 'dp:3')
  }
  if(!is.null(res)){
      if(options[['tableClusterInfoBetweenSumSquares']]){
          message <- paste0('The Between Sum of Squares of the ', res[["clusters"]], ' cluster model is ', round(res[['BSS']],2))
          clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
      }
      if(options[['tableClusterInfoTotalSumSquares']]){
          message <- paste0('The Total Sum of Squares of the ', res[["clusters"]], ' cluster model is ', round(res[['TSS']],2))
          clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
      }
      clusterInfoTable$setData(data)
  }
  }
  }
}

.PredictionsTableKmeans <- function(res, options, jaspResults){

  if(!is.null(jaspResults[["tablePredictions"]])) return() #The options for this table didn't change so we don't need to rebuild it

  if (options[['tablePredictions']]){
      if(is.null(jaspResults[["tablePredictions"]])){

  tablePredictions                       <- createJaspTable("Predictions Table")
  jaspResults[["tablePredictions"]]      <- tablePredictions
  tablePredictions$dependOnOptions(c("tablePredictions", "predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm",
                                    "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo", "predictionsFrom", "predictionsTo"))
  tablePredictions$position <- 3

  from <- options[['predictionsFrom']]
  to <- options[["predictionsTo"]]

  tablePredictions$addColumnInfo(name = 'number', title = "Obs. number", type = 'integer')
  tablePredictions$addColumnInfo(name = 'prediction', title = 'Predicted cluster', type = 'integer')

  if(!is.null(res)){
      for(i in from:to){
          row <- list(number = i, prediction = round(res[['Predictions']][[i,2]],2))
          tablePredictions$addRows(row)
      }
    }
  }
  }
}

.plot2d <- function(dataset, res, opt, options, predictors, jaspResults){
  if(is.null(res))
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))
  if(length(predictors)!= 2)
    return(createJaspPlot(error="badData", errorMessage='Two variables must be specified'))

    if(options[['noOfClusters']] == 'auto' | options[['noOfClusters']] == 'manual'){
        clusters <- res[['clusters']]
    }  else if (options[['noOfClusters']] == 'optimized' | options[['noOfClusters']] == 'robust'){
        clusters <- res[['clusters']]
    }
    kfit <- kmeans(dataset[,predictors],
                   centers = clusters,
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])

    xVar <- dataset[,predictors[1]]
    yVar <- dataset[,predictors[2]]

    isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
    isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
    bothNumeric <- isNumericX && isNumericY
    d <- data.frame(x = xVar, y = yVar, group = kfit$cluster)
    d <- na.omit(d)

     if (!isNumericX)
       d$x <- as.factor(d$x)
     if (!isNumericY)
       d$y <- as.factor(d$y)

     xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
     yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)

     p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
       JASPgraphs::geom_point(ggplot2::aes(colour = factor(group)))

     if (isNumericX) {
     	p <- p + ggplot2::scale_x_continuous(name = .unv(predictors[1]), breaks = xBreaks, limits = range(xBreaks))
     } else {
     	p <- p + ggplot2::scale_x_discrete(name = .unv(predictors[1]))
     }
     if (isNumericY) {
     	p <- p + ggplot2::scale_y_continuous(name = .unv(predictors[2]), breaks = yBreaks, limits = range(yBreaks))
     } else {
     	p <- p + ggplot2::scale_y_discrete(name = .unv(predictors[2]))
     }

    p <- JASPgraphs::themeJasp(p)

   return(createJaspPlot(plot = p, title= "2-D cluster plot", height = 300, width = 400))
}

.optimPlot <- function(options, res, jaspResults, type){

  if(is.null(res))
    return(createJaspPlot(error="badData", errorMessage="Plotting is not possible: No analysis has been run."))

    xVar <- res[['clusterrange']]
    if(type == "wss"){
      yVar <- res[['WithinSumSquares']]
      plotTitle <- "Within Sum of Squares"
    } else if (type == "criterion"){
      yVar <- res[['criterion.krange']]
      plotTitle <- "Criterion"
    }

    isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
    isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
    bothNumeric <- isNumericX && isNumericY
    d <- data.frame(x = xVar, y = yVar)
    d <- na.omit(d)

    if (!isNumericX)
      d$x <- as.factor(d$x)
    if (!isNumericY)
      d$y <- as.factor(d$y)

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y)

    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
      JASPgraphs::geom_point()

    if (isNumericX) {
    	p <- p + ggplot2::scale_x_continuous(name = "Clusters", breaks = xBreaks, limits = range(xBreaks))
    } else {
    	p <- p + ggplot2::scale_x_discrete(name = "Clusters")
    }
    if (isNumericY) {
    	p <- p + ggplot2::scale_y_continuous(name = plotTitle, breaks = yBreaks, limits = range(yBreaks))
    } else {
    	p <- p + ggplot2::scale_y_discrete(name = plotTitle)
    }

   p <- JASPgraphs::themeJasp(p)

  return(createJaspPlot(plot = p, title= plotTitle, height = 300, width = 400))
}

.twoDClusterPlot <- function(dataset, res, opt, options, predictors, jaspResults){
  if(options[['plot2dCluster']])
  {
     if(is.null(jaspResults[["plot2dCluster"]]))
     {
     jaspResults[["plot2dCluster"]] 		<- .plot2d(dataset, res, opt, options, predictors, jaspResults)
     jaspResults[["plot2dCluster"]]		$copyDependenciesFromJaspObject(jaspResults[["evaluationTable"]])
     jaspResults[["plot2dCluster"]] 		$position <- 4
     }
  }
}

.withinssPlot <- function(options, res, jaspResults){
  if(options[['plotPCAClusterSquares']] && options[['noOfClusters']] == 'optimized')
  {
     if(is.null(jaspResults[["optimPlot"]]))
     {
     jaspResults[["optimPlot"]] 		 <- .optimPlot(options, res, jaspResults, type = "wss")
     jaspResults[["optimPlot"]]		   $copyDependenciesFromJaspObject(jaspResults[["evaluationTable"]])
     jaspResults[["optimPlot"]] 		 $position <- 5
     }
  }
}

.criterionPlot <- function(options, res, jaspResults){
  if(options[['plotCriterionVsClusters']] && options[['noOfClusters']] == 'robust')
  {
    if(is.null(jaspResults[["optimPlot"]]))
    {
    jaspResults[["optimPlot"]] 		 <- .optimPlot(options, res, jaspResults, type = "criterion")
    jaspResults[["optimPlot"]]		 $copyDependenciesFromJaspObject(jaspResults[["evaluationTable"]])
    jaspResults[["optimPlot"]] 		 $position <- 5
    }
  }
}
