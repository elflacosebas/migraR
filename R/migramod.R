#' Migration Rate Models Class.
#'
#'Class built based on Rogers and castro Models.
#'
#'The migramodel class is necessary to construct the optimization. The class incorpores
#'the methods as the calculation of the Jacobian, the gradient and the Hessian matrix.
#'This class is used to evaluate a given rc_expression with an initial tupla of values.

MigraModel <- setRefClass('MigraModelObject',
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
                                # if(v != "c1"){
                                eval(D(.self$expr, v), c(as.list(p), as.list(dataIn)))
                                # } else { rep(1, nrow(dataIn))}
                              }, p=p, dataIn=dataIn))

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
