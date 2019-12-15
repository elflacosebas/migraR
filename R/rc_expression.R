#' Rogers and Castro expressions for migration schedules(Rogers and Castro, 1981)
#' The schedule specification, 'profile' could be seven, nine, eleven or thirteen.
#' The function can be used to be modified to build the own profile.
#'
#' @param profile Expression that fix the number of parameters has to have the equation to optimize
#' @usage rc_expression(profile = 'seven')
#' @return a list of simulated initial parameters
#'
rc_expression <-   function(profile = "seven"){
      switch(profile,
        "seven" = return(expression(log((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2))))  + c1)))
        ,"nine" =  return(expression(log((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2)))) +
                                             (a3 * exp(alpha3 * x)) + c1)))
        ,"eleven" =  return(expression(log((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2)))) +
                              (a3 * exp(-alpha3 * (x-mu3) - exp(-lambda3 * (x - mu3)))) + c1)))
       ,"thirteen" =  return(expression(log((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2)))) +
                              (a3 * exp(-alpha3 * (x-mu3) - exp(-lambda3 * (x - mu3)))) + (a4 * exp(lambda4 * x)) + c1)))
      )
 }
