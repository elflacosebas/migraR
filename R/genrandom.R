#' Generator function for initial values in rogers castro models
#'
#' Generate the 7 , 9 or 11 random values for Roger Castro migration models
#' @param mu2 first value for maximum in model
#' @param mu3 second value maximum in model
#' @param ... Aditional argument passed to function
#' @return A list with random values for parameters
#' @examples
#' genRandomPar(7)
#' genRandomPar(11)


genRandomPar <- function(n.param =11, mu2=c(1,50),mu3=c(1,90), ...){
  param <- list(mu2=mu2,mu3=mu3,...)

  #primera exponencial
  a1 <- runif(1, 0, 0.999999)
  alpha1 <- runif(1, 0, 0.999999)

  #segunda exponencial
  a2 <- runif(1, 0, 0.999999)
  alpha2 <- runif(1, 0, 0.999999)
  mu2 <- runif(1, param$mu3[1], param$mu3[2]) #customize for user
  lambda2 <- runif(1, 0, 0.999999)

  #tercera exponencial
  a3 <- runif(1, 0, 0.999999)
  alpha3 <- runif(1, 0, 0.999999)
  mu3 <- runif(1,   param$mu3[1],   param$mu3[2])  #customize for user
  lambda3 <- runif(1, 0, 0.999999)

  #constante
  c1 <- runif(1,0, 0.999999)



  parameters_0 <- list(a1=a1,alpha1=alpha1
                       ,a2=a2,mu2=mu2,alpha2=alpha2
                       ,lambda2=lambda2
                       ,a3=a3,mu3=mu3,alpha3=alpha3
                       ,lambda3=lambda3
                       ,c1=c1)
if(n.param == 11 ){
  return(parameters_0)
} else{

  return(parameters_0[-c(7:10)])
}

}



