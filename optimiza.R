setwd('/home/sebastian/Documentos/UnivAutBarcelona/optimización')

dataIn <- read.table("https://raw.githubusercontent.com/elflacosebas/thesis/master/mexico.txt",header = T)

library(data.table)
require(nloptr)

dataIn <- fread('ext_esp.csv',header=T, sep=';', data.table=F, dec='.')
dataIn <- data.frame(dataIn$edad, dataIn$h.extranjero)
dataIn <- dataIn[-c(1,2,3,4),]
dataIn
colnames(dataIn) <- c("x","y")

#dataIn$std.prop <- dataIn$proportion/sum(dataIn$proportion)
#y <- approxfun(c(dataIn$x), c(dataIn$y))
plot(dataIn$y, type ='l')

exp.test <-function(x1,a1,alpha1,a2,mu2,alpha2,lambda2,a3,mu3,alpha3,lambda3,c1){
  x <- x1
  exp1 <- (a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2)))) + (a3 * exp(-alpha3 * (x-mu3) - exp(-lambda3 * (x - mu3)))) + c1
  
  return(exp1)
}

###############################################################################################################################################################
rc.11 <- expression(log((a1 * exp(-alpha1 * x)) + (a2 * exp(-alpha2 * (x-mu2) - exp(-lambda2 * (x - mu2)))) + (a3 * exp(-alpha3 * (x-mu3) - exp(-lambda3 * (x - mu3)))) + c1))

#sapply(all.vars(rc.13), function(v){
 # D(rc.13, v)
#})

####################### Generación de parámetros aleatorios. #############################################################################################

genRandomPar <- function()  {      
  
  #primera exponencial
  a1 <- runif(1, 0.3584, 0.8189)
  alpha1 <- runif(1, 0.09075, 0.16175)
  
  #segunda exponencial
  a2 <- runif(1, 0.7968, 0.8081)
  alpha2 <- runif(1, 0.0413, 0.1886)
  mu2 <- runif(1, 15, 25)
  lambda2 <- runif(1, 0.03069, 0.96632)
  
  #tercera exponencial
  a3 <- runif(1, 0.1310, 0.8189)
  alpha3 <- runif(1, 0.0439, 0.9392)
  mu3 <- runif(1, 60, 90)
  lambda3 <- runif(1, 0.1903, 0.3139)
  
  #constante
  c1 <- runif(1,0.02985,0.88323)
  
  
  
  parameters_0 <- c(a1=a1,alpha1=alpha1,a2=a2,mu2=mu2,alpha2=alpha2,lambda2=lambda2,a3=a3,mu3=mu3,alpha3=alpha3
                    ,lambda3=lambda3,c1=c1)
  
  return(parameters_0)
}

#####################################creación de objeto clase ######################################

ModelObject <- setRefClass('ModelObject', 
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

parameters_0 <- genRandomPar()

mo = ModelObject(
  name = 'castro_11', 
  expr = rc.11
)

#############################################################################################

#
ite = 100
dataIn2 <- fread('ext_esp.csv',header=T, sep=';', data.table=F, dec='.')
names <- names(dataIn2)

#up <- c(0.02, 0.08, 0.04, 19, 0.06, 0.2, 0.000, 75, 0.6, 0.1, 0.002)
#down <- c(0.04, 0.1, 0.7, 22, 0.09, 0.45, 0.01, 77, 0.8, 0.2, 0.004)

#si_mula <- function(ite){
bestIni <- NA
valSim <- NA
mape <- NA
colnames(dataIn) <- c("x","y")

#mypath <- file.path('/home/sebastian/Documentos/UnivAutBarcelona/optimización/gráficos',paste(names[i], ".jpeg", sep = ""))
#jpeg(file=mypath, res = 150, quality = 100, width = 900, height = 900)

for(i in 1:10){

plot(dataIn, cex=0.1, xlab = 'Age', ylab = 'Standarized Migration Rate')
for(i in 1:10){

  parameters_0 <-  genRandomPar()
  graProof <- mo$gradient(p= parameters_0, data = dataIn)
  
  while((any(is.na(graProof)) | any(is.nan(graProof)))){
    parameters_0 <-  genRandomPar()
    graProof <- mo$gradient(p= parameters_0, data = dataIn)
  } 

  fit1 <-  nlminb(parameters_0 , function(p, data){
   #r = -((100/dim(dataIn)[1]) * sum(abs(data$y - mo$value(p,data))/(mo$value(p,data))))
   # r = -((data$y - mo$value(p,data))^2)
    r = -(data$y - mo$value(p,data))
   return(r %*% r)

  }, gradient = mo$gradient, hessian = mo$hessian, data=dataIn)
  fit1
  
  #while(!(any(is.na(fit1$evaluations[2])) & any(is.nan(fit1$evaluations[2])))){
   #lines(dataIn$x, mo$value(fit1$par,dataIn), col="blue")
   #valSim <- rbind(valSim,c(parameters_0, fit1$par, fit1$objective, fit1$message))
  #} 
  
lines(dataIn$x, mo$value(fit1$par,dataIn), col="blue")

mape <- (100/dim(dataIn)[1]) * (sum(abs(dataIn$y - mo$value(fit1$par,dataIn))/(mo$value(fit1$par,dataIn))))

valSim <- rbind(valSim,c(parameters_0, fit1$par, fit1$objective, fit1$message, mape))

}

dataSimul <- data.frame(apply(valSim[,-ncol(valSim)],c(1,2),as.numeric),valSim[,ncol(valSim)])
dataSimul <- dataSimul[-1,]

colnames(dataSimul) <- c(paste(colnames(valSim)[1:11],'_0',sep =''), paste(colnames(valSim)[12:23],'_hat',sep =''),'optimResult',"mesage", 'MAPE' )
bestPar <- dataSimul[which.min(dataSimul$mape),12:22]
names(bestPar) <- names(parameters_0)

lines(dataIn$x, mo$value(bestPar , dataIn), col="red", lwd = 2.5)
points(dataIn$x, dataIn$y, col="orange", lwd = 2.5)

bestIni <- rbind(bestIni, dataSimul[which.min(dataSimul$optimResult),1:11])



#dev.off()
}

bestIni <- bestIni[-1,]
print(bestIni)
summary(bestIni)

tab2 <- 0
for(i in 1:length(bestPar)){
  data <- dplyr::select(dataSimul, 12:22)
  ic.sup <- bestPar[i] + sd(data[,i]) 
  ic.inf <- bestPar[i] - sd(data[,i])
  tab <- c(round(bestPar[i], digits = 3), round(ic.inf, digits = 3), round(ic.sup, digits = 3), round(min(data[,i]), digits = 3), round(max(data[,i]), digits = 3))
  #ic.inf <- ifelse(ic.inf < 0, 0, ic.inf)
  tab2 <- rbind(tab2, tab)
}
tab2 <- as.matrix(tab2[-1,])
rownames(tab2) <- names(bestPar)
colnames(tab2) <- c('Parameters', 'IC.inf', 'IC.sup', 'Min', 'Max')
print(tab2)
cat('\n')
#write.table(tab2, file = 'tabla.csv', sep =';',dec =',')
tab2<-data.frame(tab2)

return(tab2)

#}

###########################################################################

###########################################################################

ini <- Sys.time()

dataIn2 <- fread('ext_esp.csv',header=T, sep=';', data.table=F, dec='.')
names <- names(dataIn2)

par <- vector("list", dim(dataIn2)[2])

for(i in 1:dim(dataIn2)[2]){
  
  ite=10
  dataIn <- data.frame(dataIn2$edad, dataIn2[,i])
  colnames(dataIn) <- c("x","y")
  par[[i]] <- si_mula(ite)
  
}

fin <- Sys.time()
time = fin-ini
time





############################### escogencia de los parámetros iniciales. 

require(rpart)
require(rpart.plot)

model <- optimResult ~ a1_0 +  alpha1_0  +  a2_0 + mu2_0 + alpha2_0 + lambda2_0 + a3_0 + mu3_0 + alpha3_0 + lambda3_0 +  c1_0

#model <- optimResult ~ .

x11()
rpart.plot(rpart(model, data=dataSimul), cex=0.8)

fitSimul <- lm(model, data = dataSimul)
summary(fitSimul)


arbol <- rpart(model, data=dataSimul)
printcp(arbol)
plotcp(arbol)

parbolrpart <- prune(arbol, cp = arbol$cptable[which.min(arbol[,'xerror']), 'cp'])
printcp(arbol)
plotcp(arbol)

testpred <- predict(arbol, data = dataSimul, type = 'class')


#############################################################################################

####### modelo estocolmo
a1 <- runif(1, 0.02, 0.05)
alpha1 <- runif(1, 0.09, 0.099)

#segunda exponencial
a2 <- runif(1, 0.04,0.07)
alpha2 <- runif(1, 0.06, 0.99)
mu2 <- runif(1, 15, 25)
lambda2 <- runif(1, 0.2, 0.4)

#tercera exponencial
a3 <- runif(1, 0, 0.01)
alpha3 <- runif(1, 0.2, 0.9)
mu3 <- runif(1, 80, 90)
lambda3 <- runif(1, 0.05, 0.2)

#constante
c1 <- runif(1,0,0.99)

#############################################################################################
###### modelo abierto

#primera exponencial
a1 <- runif(1, 0, 0.99)
alpha1 <- runif(1, 0, 0.99)

#segunda exponencial
a2 <- runif(1, 0, 0.99)
alpha2 <- runif(1, 0, 0.99)
mu2 <- runif(1, 15, 25)
lambda2 <- runif(1, 0, 0.99)

#tercera exponencial
a3 <- runif(1, 0, 0.99)
alpha3 <- runif(1, 0, 0.99)
mu3 <- runif(1, 80, 90)
lambda3 <- runif(1, 0, 0.99)

#constante
c1 <- runif(1,0,0.99)

#############################
#modelo nuevo

a1 <- runif(1, 0.3584, 0.8189)
alpha1 <- runif(1, 0.09075, 0.16175)

#segunda exponencial
a2 <- runif(1, 0.7968, 0.8081)
alpha2 <- runif(1, 0.0413, 0.1886)
mu2 <- runif(1, 15, 25)
lambda2 <- runif(1, 0.03069, 0.96632)

#tercera exponencial
a3 <- runif(1, 0.1310, 0.8189)
alpha3 <- runif(1, 0.0439, 0.9392)
mu3 <- runif(1, 60, 90)
lambda3 <- runif(1, 0.1903, 0.3139)

#constante
c1 <- runif(1,0.02985,0.88323)



