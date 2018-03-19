#'select best migramod model
#'
#'One solution to initial values problems
#'
#'
#'
#'@param dataIn Set data for fit
#'@param model.rc a Object of class MigraMod
#'@param iter number of iterations for model
#'
#'@return a list with named parameters
#'@examples
#' data("es_asmr")
#' data1 <- es_asmr[,c(1,3)]
#' colnames(data1) <- c("x","y")
#' model1 = MigraModel(
#'  name = 'castro_11',
#'  expr = rc_expression(profile = 11)$model.exp
#' )
#'
#' fitted.val <- best_migramod(dataIn = data1, model.rc =model1)
#' plot(data1, cex=0.1, xlab = 'Age', ylab = 'Standarized Migration Rate')
#' lines(data1$x, model1$value(fitted.val$bestParam,data1), col="blue")
#'
#'
#'

best_migramod <- function(dataIn=dataIn, model.rc, iter= 100 ){
   param_0 <- genRandomPar(11)
  colnames(dataIn) <- c("x","y")
  x <- dataIn[,1]
  y <- dataIn[,2]
  valSim <- fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc=model.rc )$values
  values.names <- names(valSim)
  for(i in 1:iter){
    param_0 <- genRandomPar(11)
    valSim <- rbind(valSim,fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc )$values)

  }
  rownames(valSim)<- 1:nrow(valSim)
  colnames(valSim) <- c(paste(values.names[1:11],'_0',sep =''), paste(values.names[12:22],'_hat',sep ='')
                        ,'optimResult',"message", 'MAPE' )
    valSim <- as.data.frame(valSim)
    dataSimul <- valSim %>% mutate(message=as.character(message)) %>%
         mutate_if( is.factor, .funs = function(x)as.numeric(as.character(x)))
        bestPar <- dataSimul[which.min(dataSimul$optimResult),12:22]
        bestPar.mape <- dataSimul[which.min(dataSimul$MAPE),12:22]

        names(bestPar) <- names(param_0)
        names(bestPar.mape) <- names(param_0)

        bestPar <- sapply(bestPar,as.list)
        names(bestPar) <- names(param_0)

        return(list(bestParam=bestPar, dataSimul=dataSimul))

        }




