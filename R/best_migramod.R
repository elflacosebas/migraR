#'Select best migramod model.
#'
#'One solution to initial values problem based on the generaton of a random sample with size equals one from uniform apriori
#'distributions with values between 0 and 1 for the initial parameters.
#'
#'@param dataIn Standarized migration data set for optimization.
#'@param profile Number of parameters of a Roger and Castro model.
#'@param maxite Maximum number of iterations for model optimization.
#'@param epsilon Tolerance in which the difference between the Mean Squared Error that will finish the algorithm.
#'@param datasimul Table with the different simulations and values of the parameters estimated.
#'@return a list with named parameters
#'@usage best_migramod(dataIn, profile = "eleven",
#'       maxite = 100, epsilon = 1e-05, datasimul = TRUE)
#'@importFrom dplyr mutate
#'@importFrom dplyr mutate_if
#'@importFrom dplyr %>%
#'@importFrom utils txtProgressBar
#'@importFrom utils setTxtProgressBar
#'@export
#'
#'@examples
#'\dontrun{
#' library(migraR)
#' library(dplyr)
#' data("es_asmr")
#' data1 <- es_asmr[-c(1,2),c(1,6)]
#' colnames(data1) <- c("x","y")
#'
#'
#'
#' fitted.val.7 <- best_migramod(dataIn = data1,
#'                               maxite = 10,
#'                               profile = "seven")
#' fitted.val.9 <- best_migramod(dataIn = data1,
#'                               maxite = 10,
#'                              profile = "nine")
#' fitted.val.11 <- best_migramod(dataIn = data1,
#'                                maxite = 10,
#'                                profile = "eleven")
#' fitted.val.13 <- best_migramod(dataIn = data1,
#'                                maxite = 10,
#'                                profile = "thirteen")
#'
#' x11()
#' plot(data1, cex=0.1, xlab = 'Age',
#'     ylab = 'Standarized Migration Rate')
#' lines(data1[,1],
#'       fitted.val.7$modelClass$value(fitted.val.7$bestParam,data1),
#'       col="blue")
#' lines(data1[,1],
#'       fitted.val.9$modelClass$value(fitted.val.9$bestParam,data1),
#'       col="orange")
#' lines(data1[,1],
#'       fitted.val.11$modelClass$value(fitted.val.11$bestParam,data1),
#'       col="blue", lty=3)
#' lines(data1[,1],
#'       fitted.val.13$modelClass$value(fitted.val.13$bestParam,data1),
#'       col="green")
#' legend('topright',
#'        legend = c(paste("(7)", "MAPE:", round(as.numeric(fitted.val.7$bestMAPE),2),
#'                         "R²:", round(as.numeric(fitted.val.7$bestRcuad),3)),
#'                   paste("(9)", "MAPE:", round(as.numeric(fitted.val.9$bestMAPE),2),
#'                         "R²:", round(as.numeric(fitted.val.9$bestRcuad),3)),
#'                   paste("(11)", "MAPE:", round(as.numeric(fitted.val.11$bestMAPE),2),
#'                         "R²:", round(as.numeric(fitted.val.11$bestRcuad),3)),
#'                   paste("(13)", "MAPE:", round(as.numeric(fitted.val.13$bestMAPE),2),
#'                         "R²:", round(as.numeric(fitted.val.13$bestRcuad),3))),
#'        col = c("red",'orange',"blue","darkgreen"), lty = c(2,6,3,5))
#'        }


best_migramod <- function(dataIn, profile="eleven",maxite=100, epsilon = 1E-5, datasimul=TRUE){

  param_0 <- genRandomPar(profile = profile)
  colnames(dataIn) <- c("x","y")
  x <- dataIn[,1]
  y <- dataIn[,2]

  #avoiding the step of creation outside best_migramod
  model.rc = MigraModel(
    name = 'Parametric_model',
    expr = rc_expression(profile = profile) ,
    dataIn = dataIn
  )

  #attach(dataIn)

  valSim <- fit_migramod(dataIn, parameters_0 = param_0, model.rc  = model.rc)$values
  values.names <- names(valSim)

  opti.pos <- switch (profile,
                      seven = 15
                      ,nine = 19
                      ,eleven = 23
                      ,thirteen = 27
                      ,nuptial = 11
  )

  opti <- unlist(valSim[opti.pos])

  counter <-  1
  pb <- txtProgressBar(counter, maxite,style = 3)

  for(i in 2: maxite){ # SR: this is one solution, we lost opti epsilon as a parameter
    #while (opti > epsilon  &&  counter < maxite){
    param_0 <- genRandomPar(profile=profile)
    if(datasimul){
      valSim <- rbind(valSim, fit_migramod(dataIn, parameters_0=param_0, model.rc = model.rc)$values)
      opti <- unlist(valSim[nrow(valSim),opti.pos])
      counter =counter + 1
      setTxtProgressBar(pb, counter)

    }else{

      valSim_b  <- fit_migramod(dataIn, parameters_0=param_0, model.rc)$values
      opti <- unlist(valSim_b[opti.pos])
      if(counter == 0){
        valSim <- valSim_b

      }else{ valSim <- valSim}
      if(opti < unlist(valSim[opti.pos])){
        valSim <- valSim_b
      }else {
        valSim <- valSim
      }
      counter =counter + 1
      setTxtProgressBar(pb, counter)
    }
  }
  close(pb)
  if(datasimul){
    rownames(valSim)<- 1:nrow(valSim)

  }else{
    valSim <- rbind(valSim,valSim)
  }
  params.n <- switch (profile,
                      seven = list(subzero=1:7,hat=8:14)
                      ,nine = list(subzero=1:9,hat=10:18)
                      ,eleven = list(subzero=1:11,hat=12:22)
                      ,thirteen = list(subzero=1:13,hat=14:26)
                      ,nuptial = list(subzero=1:5,hat=6:10)
  )


  colnames(valSim) <- c(paste(values.names[params.n$subzero],'_0',sep =''), paste(values.names[params.n$hat],'_hat',sep ='')
                        ,'optimResult',"message", 'MAPE', 'RCuad' )
  valSim <- as.data.frame(valSim)
  dataSimul <- valSim %>% mutate(message=as.character(message)) %>%
    mutate_if( is.factor, .funs = function(x)as.numeric(as.character(x)))

  bestPar <- dataSimul[which.min(dataSimul$optimResult),params.n$hat]
  bestParam_0 <- dataSimul[which.min(dataSimul$optimResult),params.n$subzero]

  bestPar.mape <- dataSimul[which.min(dataSimul$MAPE),params.n$hat]
  bestOptimRes <- dataSimul[which.min(dataSimul$optimResult), "optimResult"]
  bestMAPE <- dataSimul[which.min(dataSimul$MAPE),"MAPE"]
  bestRcuad <- dataSimul[which.min(dataSimul$MAPE),"RCuad"]

  names(bestPar) <- names(param_0)
  names(bestPar.mape) <- names(param_0)
  bestPar <- sapply(bestPar,as.list)
  names(bestPar) <- names(param_0)

  return(list(bestParam=bestPar
              ,bestParam_0= bestParam_0
              ,bestOptimRes=bestOptimRes
              ,bestMAPE=bestMAPE
              ,bestRcuad=bestRcuad
              ,dataSimul=dataSimul
              , modelClass = model.rc))


  #detach(dataIn)
}



