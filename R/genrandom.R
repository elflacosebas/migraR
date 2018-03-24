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


genRandomPar <- function(profile = "seven",  ...){
  param1 <- list(a1=c(0,0.9),alpha1=c(0,0.9), a2=c(0,0.9)
                , alpha2=c(0,0.9), mu2=c(1,90), lambda2=c(0,0.9)
                , a3=c(0,0.9), alpha3=c(0,0.9), mu3=c(1,90), lambda3=c(0,0.9)
                ,a4=c(0,0.9), lambda4=c(0,0.9)
                ,c1=c(0,0.9))
  param2 <- list(...)
 param <- c(param1[setdiff(names(param1), names(param2))]
             ,param2)

  #primera exponencial
  a1 <- runif(1, param$a1[1], param$a1[2])
  alpha1 <- runif(1,param$alpha1[1], param$alpha1[2])

  #segunda exponencial
  a2 <- runif(1, param$a2[1], param$a2[2])
  alpha2 <- runif(1, param$alpha2[1], param$alpha2[2])
  mu2 <- floor(runif(1, param$mu2[1], param$mu2[2])) #customize for user
  lambda2 <- runif(1, param$lambda2[1], param$lambda2[2])

  #tercera exponencial
  a3 <- runif(1, param$a3[1], param$a3[2])
  alpha3 <- runif(1, param$alpha3[1], param$alpha3[2])
  mu3 <- floor(runif(1,   param$mu3[1],   param$mu3[2]))  #customize for user
  lambda3 <- runif(1, param$lambda3[1], param$lambda3[2])
 #
  a4 <- runif(1, param$a4[1], param$a4[2])
  lambda4 <- runif(1, param$lambda4[1], param$lambda4[2])
  #constante
  c1 <- runif(1,0, 0.999999)



  parameters_0 <- list(a1=a1,alpha1=alpha1
                       ,a2=a2,mu2=mu2,alpha2=alpha2
                       ,lambda2=lambda2
                       ,a3=a3,mu3=mu3,alpha3=alpha3
                       ,lambda3=lambda3,a4=a4,lambda4=lambda4
                       ,c1=c1)


switch(profile,
       seven= parameters_0[c(1:6,13)]
       ,eleven =parameters_0[c(1:10,13)]
      , thirteen = parameters_0[])
}



