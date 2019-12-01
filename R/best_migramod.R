
best_migramod <- function(dataIn=dataIn, model.rc, profile="eleven",maxite=100, epsilon = 1E-5, datasimul=T){

  param_0 <- genRandomPar(profile=profile)
  colnames(dataIn) <- c("x","y")
  x <- dataIn[,1]
  y <- dataIn[,2]
  valSim <- fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc=model.rc )$values
  values.names <- names(valSim)

  opti.pos <- switch (profile,
                  seven = 15
                  ,nine = 19
                  ,eleven = 23
                  ,thirteen = 27
    )

  opti <- unlist(valSim[opti.pos])
  counter <-  1
  pb <- txtProgressBar(counter, maxite,style = 3)
  while (opti > epsilon  &&  counter < maxite ){
 # for(i in 1:iter){
    param_0 <- genRandomPar(profile=profile)
    if(datasimul){
      valSim <- rbind(valSim,fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc )$values)
      opti <- unlist(valSim[nrow(valSim),opti.pos])
      counter =counter + 1
      setTxtProgressBar(pb, counter)
    }else {

      valSim_b  <- fit_migramod(dataIn = dataIn, parameters_0=param_0, model.rc )$values
      opti <- unlist(valSim_b[opti.pos])
      if(counter == 0){
        valSim <- valSim_b

      }else{ valSim <- valSim}
     if(opti < unlist(valSim[opti.pos])){
       valSim <- valSim_b
     }else {
         valSim <- valSim
         }
      counter =counter + 1
      setTxtProgressBar(pb, counter)

    }

  }
  close(pb)
  if(datasimul){
    rownames(valSim)<- 1:nrow(valSim)

  }else{
    valSim <- rbind(valSim,valSim)
  }


  params.n <- switch (profile,
    seven = list(subzero=1:7,hat=8:14)
    ,nine = list(subzero=1:9,hat=10:18)
    ,eleven = list(subzero=1:11,hat=12:22)
    ,thirteen = list(subzero=1:13,hat=14:26)
  )


  colnames(valSim) <- c(paste(values.names[params.n$subzero],'_0',sep =''), paste(values.names[params.n$hat],'_hat',sep ='')
                        ,'optimResult',"message", 'MAPE', 'RCuad' )
    valSim <- as.data.frame(valSim)
    dataSimul <- valSim %>% mutate(message=as.character(message)) %>%
         mutate_if( is.factor, .funs = function(x)as.numeric(as.character(x)))

   bestPar <- dataSimul[which.min(dataSimul$optimResult),params.n$hat]
   bestParam_0 <- dataSimul[which.min(dataSimul$optimResult),params.n$subzero]

   bestPar.mape <- dataSimul[which.min(dataSimul$MAPE),params.n$hat]
   bestOptimRes <- dataSimul[which.min(dataSimul$optimResult), "optimResult"]
   bestMAPE <- dataSimul[which.min(dataSimul$MAPE),"MAPE"]
   bestRcuad <- dataSimul[which.min(dataSimul$MAPE),"RCuad"]

   names(bestPar) <- names(param_0)
   names(bestPar.mape) <- names(param_0)
   bestPar <- sapply(bestPar,as.list)
   names(bestPar) <- names(param_0)

        return(list(bestParam=bestPar
                    ,bestParam_0= bestParam_0
                    ,bestOptimRes=bestOptimRes
                    ,bestMAPE=bestMAPE
                    ,bestRcuad=bestRcuad
                    ,dataSimul=dataSimul))

        }




