
# Estimate a migramod model via linear optimization
# dataIn Empirical (out/in) migration rates for estimation.
# paramaters_0 Set of initial parameters for selected Rogers and Castro model equation.
# model.rc Object of class migramod with usually a Rogers and Castro model expression.


fit_migramod <- function(data_In = dataIn, parameters_0, model.rc){
  colnames(data_In) <- c("x","y")
  x <- data_In$x
  y <- data_In$y

  graProof <- model.rc$gradient(p= parameters_0, data = data_In)
  constrains <- matrix(rep(c(0,0.7),nrow(graProof)),c(nrow(graProof),2),byrow = T )
  mu.rows <- grepl("mu",row.names(graProof))
  constrains[mu.rows,] <- sort(rep(c(1,90),sum(mu.rows)))
  while((any(is.na(graProof)) || any(is.nan(graProof)))){
    parameters_0 <-  genRandomPar()
    graProof <- model.rc$gradient(p= parameters_0, data = data_In)
  }

  fit1 <-  try(nlminb(parameters_0 , function(p, data){
    r = -(data$y - model.rc$value(p,x))
    return(r %*% r)
  }, gradient = model.rc$gradient
  ,hessian = model.rc$hessian, data=data_In, lower = constrains[,1]
  , upper = constrains[,2]),silent = T)

  if(any(grepl("Error?", fit1))){
    mape <- "-1"
    values <- c(parameters_0, fit1, fit1, fit1, mape)
    return(list(values=values, model.rc$value(p=parameters_0, data)))

  }else{
    mape <- (100/dim(data_In)[1]) * (sum(abs( model.rc$value(fit1$par,data_In) -data_In$y)/(data_In$y)))
    rcuad <- sum((model.rc$value(fit1$par,data_In)-mean(data_In$y))^2)/sum((data_In$y-mean(data_In$y))^2)
    values <- c(parameters_0, fit1$par, fit1$objective, fit1$message, mape, rcuad)
    return(list(values=values, model.rc$value(p=fit1$par,data) ))
  }


}

