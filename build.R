
library(devtools)

document()

check()

 library(migraR)
 library(dplyr)
 data("es_asmr")
 data1 <- es_asmr[-c(1,2),c(1,5)]
 # colnames(data1) <- c("x","y")
 # attach(data1)

 model.rc.7 = MigraModel(
   name = 'castro_7',
   expr = rc_expression(profile = "seven")
 )

 model.rc.9 = MigraModel(
   name = 'castro_9',
   expr = rc_expression(profile = "nine")
 )

 model.rc.11 = MigraModel(
   name = 'castro_11',
   expr = rc_expression(profile = "eleven")
 )

 model.rc.13 = MigraModel(
   name = 'castro_13',
   expr = rc_expression(profile = "thirteen")
 )

 fitted.val.7 <- best_migramod(dataIn = data1,
                 model.rc =model.rc.7, maxite = 5E2,
                 profile = "seven")
 fitted.val.9 <- best_migramod(dataIn = data1,
                 model.rc =model.rc.9, maxite = 5E2,
                 profile = "nine")
 fitted.val.11 <- best_migramod(dataIn = data1,
                  model.rc =model.rc.11, maxite = 5E2,
                  profile = "eleven")
 fitted.val.13 <- best_migramod(dataIn = data1,
                  model.rc =model.rc.13, maxite = 5E2,
                  profile = "thirteen")

 x11()
 plot(data1, cex=0.1, xlab = 'Age',
      ylab = 'Standarized Migration Rate')
 lines(data1[,1],
       model.rc.7$value(fitted.val.7$bestParam,data1),
       col="blue")
 lines(data1[,1],
       model.rc.9$value(fitted.val.9$bestParam,data1),
       col="orange")
 lines(data1[,1],
       model.rc.11$value(fitted.val.11$bestParam,data1),
       col="blue", lty=3)
 lines(data1[,1],
       model.rc.13$value(fitted.val.13$bestParam,data1),
       col="green")
 legend('topright',
       legend = c(paste("(7)", "MAPE:", round(as.numeric(fitted.val.7$bestMAPE),2),
                  "R²:", round(as.numeric(fitted.val.7$bestRcuad),3)),
                  paste("(9)", "MAPE:", round(as.numeric(fitted.val.9$bestMAPE),2),
                  "R²:", round(as.numeric(fitted.val.9$bestRcuad),3)),
                  paste("(11)", "MAPE:", round(as.numeric(fitted.val.11$bestMAPE),2),
                  "R²:", round(as.numeric(fitted.val.11$bestRcuad),3)),
                  paste("(13)", "MAPE:", round(as.numeric(fitted.val.13$bestMAPE),2),
                  "R²:", round(as.numeric(fitted.val.13$bestRcuad),3))),
                  col = c("red",'orange',"blue","darkgreen"), lty = c(2,6,3,5))
