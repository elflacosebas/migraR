#'select best migramod model
#'
#'One solution to initial values problems
#'
#'
#'
#'@param dataIn Set data for fit
#'@param model.rc a Object of class MigraMod
#'@param maxiter Maximum number of iterations for model optimization
#'@param profile According parameter size of Roger Castro models
#'
#'@return a list with named parameters
#'@examples
#' require(migraR)
#'require(dplyr)
#' data("es_asmr")
#' data1 <- es_asmr[-c(1,2),c(1,5)]
#' colnames(data1) <- c("x","y")
#' attach(data1)
#' model.rc.13 = MigraModel(
#'   name = 'castro_13',
#'   expr = rc_expression(profile = "thirteen")
#' )
#'
#' model.rc.7 = MigraModel(
#'   name = 'castro_7',
#'   expr = rc_expression(profile = "seven")
#' )
#' model.rc.11 = MigraModel(
#'   name = 'castro_11',
#'   expr = rc_expression(profile = "eleven")
#' )
#'
#' plot(data1, cex=0.1, xlab = 'Age', ylab = 'Standarized Migration Rate')
#' fitted.val.11 <- best_migramod(dataIn = data1, model.rc =model.rc.11, maxite = 5E2, profile = "eleven")
#' lines(data1[,1], model.rc.11$value(fitted.val.11$bestParam,data1), col="blue", lty=3)
#' fitted.val.7 <- best_migramod(dataIn = data1, model.rc =model.rc.7, maxite = 5E2, profile = "seven")
#' lines(data1[,1], model.rc.7$value(fitted.val.7$bestParam,data1), col="blue")
#' fitted.val.13 <- best_migramod(dataIn = data1, model.rc =model.rc.13, maxite = 5E2, profile = "thirteen")
#' lines(data1[,1], model.rc.13$value(fitted.val.13$bestParam,data1), col="green")
#' legend("topright",legend = c("seven","eleven","thirteen"),fill = c("red","blue","green"))


best_migramod <- function(dataIn=dataIn, model.rc, profile="eleven",maxite=1E1){
  epsilon <- 0.0001

   param_0 <- genRandomPar(profile=profile)
  colnames(dataIn) <- c("x","y")
  x <- dataIn[,1]
  y <- dataIn[,2]
  valSim <- fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc=model.rc )$values
  values.names <- names(valSim)

  opti.pos <- switch (profile,
                  seven = 15
                  ,eleven = 23
                  ,thirteen = 27
    )

  opti <- unlist(valSim[opti.pos])
  counter <-  1
  pb <- txtProgressBar(counter, maxite,style = 3)
  while ((abs(opti - epsilon) > 1E-06)  &&  counter < maxite ){
 # for(i in 1:iter){
    param_0 <- genRandomPar(profile=profile)
    valSim <- rbind(valSim,fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc )$values)
    opti <- unlist(valSim[nrow(valSim),opti.pos])
    counter =counter + 1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  rownames(valSim)<- 1:nrow(valSim)

  params.n <- switch (profile,
    seven = list(subzero=1:7,hat=8:14)
    ,eleven = list(subzero=1:11,hat=12:22)
    ,thirteen = list(subzero=1:13,hat=14:26)
  )


  colnames(valSim) <- c(paste(values.names[params.n$subzero],'_0',sep =''), paste(values.names[params.n$hat],'_hat',sep ='')
                        ,'optimResult',"message", 'MAPE' )
    valSim <- as.data.frame(valSim)
    dataSimul <- valSim %>% mutate(message=as.character(message)) %>%
         mutate_if( is.factor, .funs = function(x)as.numeric(as.character(x)))

   bestPar <- dataSimul[which.min(dataSimul$optimResult),params.n$hat]
   bestParam_0 <- dataSimul[which.min(dataSimul$optimResult),params.n$subzero]

        bestPar.mape <- dataSimul[which.min(dataSimul$MAPE),params.n$hat]
    bestOptimRes <- dataSimul[which.min(dataSimul$optimResult), "optimResult"]
    bestMAPE <- dataSimul[which.min(dataSimul$MAPE),"MAPE"]
        names(bestPar) <- names(param_0)
        names(bestPar.mape) <- names(param_0)
        bestPar <- sapply(bestPar,as.list)
        names(bestPar) <- names(param_0)

        return(list(bestParam=bestPar
                    ,bestParam_0= bestParam_0
                    ,bestOptimRes=bestOptimRes
                    ,bestMAPE=bestMAPE
                    ,dataSimul=dataSimul))

        }




