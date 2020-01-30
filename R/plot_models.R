#' Attempt for a function to make the graphics
#' @param dataIn data for optimization
#' @export

#' @examples
#' \dontrun{
#' plot_RC(dataIn)
#' }
#'

plot_RC <- function(dataIn){

  x11()
  plot(dataIn, cex=0.1, xlab = 'Age',
       ylab = 'Standarized Migration Rate')
  lines(dataIn[,1],
        model.rc.7$value(fitted.val.7$bestParam,data1),
        col="blue")
  lines(dataIn[,1],
        model.rc.9$value(fitted.val.9$bestParam,data1),
        col="orange")
  lines(dataIn[,1],
        model.rc.11$value(fitted.val.11$bestParam,data1),
        col="blue", lty=3)
  lines(dataIn[,1],
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


}

