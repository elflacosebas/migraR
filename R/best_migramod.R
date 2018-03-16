#'select best migramod model
#'
#'One solution to initial values problems
#'
#'
#'
#'
#'@example
#'model1 = migramod(
#'name = 'castro_11',
#'expr = rc_expression(model.name="RC.11",profile = 11)
#')
#'data1 <- es_asmr[,c(1,3)]
#'colnames(data1) <- c("x","y")
#'model1$gradient(p= genRandomPar(11), data = data1)
#'best_migramod(dataIn = dataIn2[,c(1,3)], model.rc =model1
#'              fit_migramod(dataIn = es_asmr[,c(1,3)], parameters_0=genRandomPar(11), model.rc =model1)
#'
#'
#'

best_migramod <- function(dataIn=dataIn, model.rc  ){
  param_0 <- genRandomPar(11)
  valSim <- fit_migramod(dataIn = dataIn, paramaters_0=param_0, model.rc=model.rc )
  colnames(dataIn) <- c("x","y")
  for(i in 1:50){
    paramaters_0 <- genRandomPar(11)
    valSim <- rbind(valSim,fit_migramod(dataIn = dataIn, paramaters_0=param_0, model.rc ))
  }
    dataSimul <- valSim %>% mutate(message=as.character(message)) %>%
         mutate_if( is.factor, .funs = function(x)as.numeric(as.character(x)))

         bestPar <- dataSimul[which.min(dataSimul$optimResult),12:22]
        bestPar.mape <- dataSimul[which.min(dataSimul$MAPE),12:22]

        names(bestPar) <- names(parameters_0)
        names(bestPar.mape) <- names(parameters_0)

        return(bestParam=bestPar, model.rc$value(dataIn=dataIn,paramaters_0 = bestPar))

        }

