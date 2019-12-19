#' Estimate a migramod model via linear optimization
#'
#' @param dataIn Empirical (out/in) migration rates for estimation.
#' @param parameters_0 Set of initial parameters for selected Rogers and Castro model equation.
#' @param model.rc Object of class migramod with usually a Rogers and Castro model expression.
#' @usage fit_migramod (dataIn, parameters_0, model.rc)
#' @return A list of values caluculated in the minimization as OptimResult, R squared or MAPE.
#' @importFrom stats nlminb
#' @importFrom utils data
#' @importFrom utils globalVariables
#'
#'@export
#'
#' @examples
#' \dontrun{
#' param0 <- genRandomPar(profile='seven')
#' model.rc = MigraModel(
#'   name = 'castro_7',
#'   expr = rc_expression(profile = "seven")
#' )
#' fit_migramod(dataIn, param0, model.rc)
#' }

fit_migramod <- function(dataIn, parameters_0, model.rc){
  colnames(dataIn) <- c("x","y")
  x <- dataIn$x
  y <- dataIn$y

  graProof <- model.rc$gradient(p= parameters_0, dataIn = dataIn)
  constrains <- matrix(rep(c(0,0.7),nrow(graProof)),c(nrow(graProof),2),byrow = T )
  mu.rows <- grepl("mu",row.names(graProof))
  constrains[mu.rows,] <- sort(rep(c(1,90),sum(mu.rows)))
  while((any(is.na(graProof)) || any(is.nan(graProof)))){
    parameters_0 <-  genRandomPar()
    graProof <- model.rc$gradient(p= parameters_0, dataIn = dataIn)
  }

  fit1 <-  try(nlminb(parameters_0 , function(p, dataIn){
    r = -(dataIn$y - model.rc$value(p,x))
    return(r %*% r)
  }, gradient = model.rc$gradient
  ,hessian = model.rc$hessian, dataIn=dataIn, lower = constrains[,1]
  , upper = constrains[,2]),silent = T)

  if(any(grepl("Error?", fit1))){
    mape   <- "-1"
    values <- c(parameters_0, fit1, fit1, fit1, mape)
    return(list(values=values, model.rc$value(p=parameters_0, dataIn)))

  }else{
    mape   <- (100/dim(dataIn)[1]) * (sum(abs( model.rc$value(fit1$par,dataIn) -dataIn$y)/(dataIn$y)))
    rcuad  <- sum((model.rc$value(fit1$par,dataIn)-mean(dataIn$y))^2)/sum((dataIn$y-mean(dataIn$y))^2)
    values <- c(parameters_0, fit1$par, fit1$objective, fit1$message, mape, rcuad)
    return(list(values=values, model.rc$value(p=fit1$par,dataIn) ))
  }

}
