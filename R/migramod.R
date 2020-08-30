# for getting the roxygen right for ref classes, some tips here:
# http://r-pkgs.had.co.nz/namespace.html

#'A Reference Class for fitting MigraModels
#'@title MigraModel Reference Class
#'@description The class incorporates the methods as the calculation of the Jacobian, the gradient and the Hessian matrix. This class is used to evaluate a given rc_expression with an initial tupla of values.
#'@import methods
#' @field name A character name
#' @field expr An rc_expression
#'@export MigraModel
#'
#'
MigraModel <- setRefClass('MigraModel',
                          fields = list(
                            name = 'character',
                            expr = 'expression',
                            dataIn = 'data.frame'
                          ),
                          methods = list(
                            value = function(p, dataIn){
                              x <<- .self$dataIn$x
                              eval(expr, c(as.list(p), as.list(dataIn)))
                            },
                            jacobian = function(p, dataIn){
                              J = t(sapply(all.vars(expr), function(v, p, dataIn){

                                eval(D(expr, v), c(as.list(p), as.list(dataIn)))

                              }, p=p, data=dataIn))

                              return(J[names(p),,drop=FALSE])
                            },
                            gradient = function(p, dataIn){
                              r = dataIn$y - value(p, dataIn)
                              return(-jacobian(p, dataIn) %*% r)
                            },
                            hessian = function(p, dataIn){
                              J = jacobian(p, dataIn)
                              return(J %*% t(J))
                            }
                          )
)
