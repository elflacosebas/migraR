# for getting the roxygen right for ref classes, some tips here:
# http://r-pkgs.had.co.nz/namespace.html

#'A Reference Class for fitting MigraModels
#'@title MigraModel Reference Class
#'@description The class incorpores the methods as the calculation of the Jacobian, the gradient and the Hessian matrix. This class is used to evaluate a given rc_expression with an initial tupla of values.
#'@import methods
#' @field name A character name
#' @field expr An rc_expression
#'@export MigraModel
#'
#'
MigraModel <- setRefClass('MigraModel',
                          fields = list(
                            name = 'character',
                            expr = 'expression'
                          ),
                          methods = list(
                            value = function(p, data){

                              eval(.self$expr, c(as.list(p), as.list(data)))
                            },

                            jacobian = function(p, data){

                              J = t(sapply(all.vars(.self$expr), function(v, p, data){
                                eval(D(.self$expr, v), c(as.list(p), as.list(data)))

                              }, p=p, data=data))
                              return(J[names(p),,drop=F])
                            },

                            gradient = function(p, data){
                              r = data$y - value(p, data)
                              return(-jacobian(p, data) %*% r)
                            },

                            hessian = function(p, data){
                              J = jacobian(p, data)
                              return(J %*% t(J))
                            }
                          )
)
