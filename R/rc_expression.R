#' Rogers and Castro expressions
#'
#' Are the Rogers and Castro expressions for  migration by individual or grouped ages with methods
#' @param model.name character with the the of migration model
#' @param profile a integer with the number of parameter for the model


rc_expression <-   function(model.name="RC.11",profile = 11, ...){

      if(profile == 11){

        model.exp <- expression((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2)))) +
        return(list(model.name=model.name, model.exp=model.exp))                          (a3 * exp(-alpha3 * (x-mu3) - exp(-lambda3 * (x - mu3)))) + c1)
      } else{
        model.exp <- expression((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2))))  + c1)
        return(list(model.name=model.name, model.exp=model.exp))
      }

    }



