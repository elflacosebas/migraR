#' Migration Rate Models Class.
#'
#'Class built based on Rogers and castro Models.
#'
#'The Rogers and Castro models where thought to analize the behaviour of the schedule and to parametrize it. 
#'The use is very used in population projections and matematical modelling of migration for behaviour clustering
#'with the persistent regularities and the selectivity in age and sex. The model has four main components (Rogers and Castro, 1981): 
#'A single negative curve of the pre-labour force, a left-skewed unimodal curve of the labour force, an almost bell shaped curve of post-labour force.
#'A constant curve c, wich improves the fit of te mathemsatical expression to the observed schedule. 


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



