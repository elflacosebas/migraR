#' Migration Rate Models Class
#'
#'Class with Rogers and castro Models.
#'
#'The Rogesr and Castro models for a migration are so useful


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



