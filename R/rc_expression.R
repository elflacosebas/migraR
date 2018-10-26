#' Rogers and Castro expressions (Rogers and Castro, 1981)
#'
#' Rogers and Castro Expressions for migration schedules.
#' @param model.name The schedule specification, 'profile' could be seven, nine, eleven or thirteen

rc_expression <-   function(profile = "eleven"){
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
