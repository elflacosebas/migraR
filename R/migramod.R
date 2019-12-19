#'A Reference Class for fitting MigraModels
#'@title MigraModel Reference Class
#'@description The class incorpores the methods as the calculation of the Jacobian, the gradient and the Hessian matrix. This class is used to evaluate a given rc_expression with an initial tupla of values.
#'@import methods
#'@export
#'@exportClass MigraModel
#' @field name A character name
#' @field expr An rc_expression
#'
MigraModel <- setRefClass('MigraModel',
                          fields = list(
                            name = 'character',
                            expr = 'expression'
                          ),
                          methods = list(
                            value = function(p, dataIn){
                              eval(.self$expr, c(as.list(p), as.list(dataIn)))
                            },
                            jacobian = function(p, dataIn){
                              J = t(sapply(all.vars(.self$expr), function(v, p, dataIn){

                                eval(D(.self$expr, v), c(as.list(p), as.list(dataIn)))

                              }, p=p, data=dataIn))

                              return(J[names(p),,drop=F])
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


