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
                             value = function(p, data){
                               eval(.self$expr, c(as.list(p), as.list(data)))
                             },
                             jacobian = function(p, data){
                               J = t(sapply(all.vars(.self$expr), function(v, p, data){
                                 # if(v != "c1"){
                                 eval(D(.self$expr, v), c(as.list(p), as.list(data)))
                                 # } else { rep(1, nrow(data))}
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



