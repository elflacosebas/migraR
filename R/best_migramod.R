#'select best migramod model
#'
#'One solution to initial values problems
#'
#'
#'
#'
#'
#'
#'
#'
for(i in 1:10){
plot(dataIn, cex=0.1, xlab = 'Age', ylab = 'Standarized Migration Rate')
for(i in 1:5){
  lines(dataIn$x, mo$value(fit1$par,dataIn), col="blue")



  valSim <- rbind(valSim,values)
  names(values)
  rownames(valSim)<- 1:nrow(valSim)
  colnames(valSim) <- c(paste(names(values)[1:11],'_0',sep ='')
                        ,paste(names(values)[12:22],'_hat',sep =''),'optimResult',"message", 'MAPE' )
  valSim <- as.data.frame(valSim)

  dataSimul <- valSim %>% mutate(message=as.character(message)) %>%
    mutate_if( is.factor, .funs = function(x)as.numeric(as.character(x)))
  dataSimul <- dataSimul[-1,]

  #colnames(dataSimul) <- c(paste(colnames(valSim)[1:11],'_0',sep =''), paste(colnames(valSim)[12:22],'_hat',sep =''),'optimResult',"mesage", 'MAPE' )

  bestPar <- dataSimul[which.min(dataSimul$optimResult),12:22]
  bestPar.mape <- dataSimul[which.min(dataSimul$MAPE),12:22]

  names(bestPar) <- names(parameters_0)
  names(bestPar.mape) <- names(parameters_0)

  lines(dataIn$x, mo$value(bestPar , dataIn), col="red", lwd = 2.5)
  lines(dataIn$x, mo$value(bestPar.mape , dataIn), col="gold", lwd = 2.5)

  points(dataIn$x, dataIn$y, col="orange", lwd = 2.5)

  bestIni <- rbind(bestIni, dataSimul[which.min(dataSimul$optimResult),1:11])

  #dev.off()
}
}

