#' Estimate a migramod model
#'
#' Estimate a migramod model via no linear estimation
#'
#'
#'
#'
#'
#'@param dataIn Empiral for estimate
#'@param paramaters_0 set of Paramater for Rogers castro function
#'@param model.rc Object of class migramod with roger Castro expression usually

fit_migramod <- function(dataIn=dataIn, parameters_0, model.rc  ){
  colnames(dataIn) <- c("x","y")
  x <- dataIn$x
  y <- dataIn$y
      graProof <- model.rc$gradient(p= parameters_0, data = dataIn)
      while((any(is.na(graProof)) | any(is.nan(graProof)))){
      parameters_0 <-  genRandomPar()
      graProof <- model.rc$gradient(p= parameters_0, data = dataIn)
    }

    fit1 <-  nlminb(parameters_0 , function(p, data){
      #r = -((100/dim(dataIn)[1]) * sum(abs(data$y - mo$value(p,data))/(mo$value(p,data))))
      # r = -((data$y - mo$value(p,data))^2)
      r = -(data$y - model.rc$value(p,data))
      return(r %*% r)

    }, gradient = model.rc$gradient, hessian = model.rc$hessian, data=dataIn)

    #while(!(any(is.na(fit1$evaluations[2])) & any(is.nan(fit1$evaluations[2])))){
    #lines(dataIn$x, mo$value(fit1$par,dataIn), col="blue")
    #valSim <- rbind(valSim,c(parameters_0, fit1$par, fit1$objective, fit1$message))
    #}

    mape <- (100/dim(dataIn)[1]) * (sum(abs(dataIn$y - model.rc$value(fit1$par,dataIn))/(model.rc$value(fit1$par,dataIn))))
    values <- c(parameters_0, fit1$par, fit1$objective, fit1$message, mape)


   # names(values)[23:25]<- c("objetive","message","mape")
  return(list(values=values, model.rc$value(p=fit1$par,data) ))

}
