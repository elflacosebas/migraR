

library(devtools)
library(pkgdown)
library(goodpractice)
library(testthat)

document()
build()
check()
#gp()
#spell_check()
#?build_website()

library(migraR)
library(dplyr)
library(tidyverse)

data("es_asmr")
data1 <- es_asmr[-c(1,2),c(1,6)]
colnames(data1) <- c("x","y")

fitted.val.7 <- best_migramod(dataIn = data1, maxite = 10, profile = "seven")
fitted.val.9 <- best_migramod(dataIn = data1, maxite = 10, profile = "nine")
fitted.val.11 <- best_migramod(dataIn = data1, maxite = 10, profile = "eleven")
fitted.val.13 <- best_migramod(dataIn = data1, maxite = 10, profile = "thirteen")

x11()
plot(data1, cex=0.1, xlab = 'Age',
     ylab = 'Standarized Migration Rate')
lines(data1[,1],
      fitted.val.7$modelClass$value(fitted.val.7$bestParam,data1),
      col="blue")
lines(data1[,1],
      fitted.val.9$modelClass$value(fitted.val.9$bestParam,data1),
      col="orange")
lines(data1[,1],
      fitted.val.11$modelClass$value(fitted.val.11$bestParam,data1),
      col="blue", lty=3)
lines(data1[,1],
      fitted.val.13$modelClass$value(fitted.val.13$bestParam,data1),
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



# Example using ggplot

expat.male <- ksmooth(es_asmr$age, es_asmr$male.foreign,
                      "normal", bandwidth = 5, x.points = es_asmr$age)
expat.female <- ksmooth(es_asmr$age, es_asmr$female.foreign,
                        "normal", bandwidth = 5, x.points = es_asmr$age)
interegional.male <- ksmooth(es_asmr$age, es_asmr$male.inter_regional,
                             "normal", bandwidth = 5, x.points = es_asmr$age)
interegional.female <- ksmooth(es_asmr$age, es_asmr$female.inter_regional,
                               "normal", bandwidth = 5, x.points = es_asmr$age)
intraprovince.male <- ksmooth(es_asmr$age, es_asmr$male.intra_province,
                              "normal", bandwidth = 5, x.points = es_asmr$age)
intraprovince.female <- ksmooth(es_asmr$age, es_asmr$female.intra_province,
                                "normal", bandwidth = 5, x.points = es_asmr$age)

#you must create first the data.frame

SpanishMig <- data.frame(Age= es_asmr$age,
                         Expat_female = expat.female$y,
                         Interegional_female = interegional.female$y,
                         Intraprovince_female = intraprovince.female$y)
#then the pivot_longer
rates.long <- SpanishMig %>%
  pivot_longer(cols = 2:4, names_to = 'Type', values_to = "Rates_Smoothed")

#now ggplot
ggplot(data = rates.long, mapping= aes(x= Age, y= Rates_Smoothed, colour= Type)) +
  geom_line()

######### Compare the smoothed curves of man and women of intra-province migration

SpanishMig <- data.frame(Age= es_asmr$age,
                         Intraprovince_male = intraprovince.male$y,
                         Intraprovince_female = intraprovince.female$y)
#then the pivot_longer
rates.long <- SpanishMig %>%
  pivot_longer(cols = 2:3, names_to = 'Type', values_to = "Rates_Smoothed")

#now ggplot
ggplot(data = rates.long, mapping= aes(x= Age, y= Rates_Smoothed, colour= Type)) +
  geom_line()


rates <- data.frame(Age = es_asmr$age,
                    Original = es_asmr$male.inter_regional,
                    Seven = fitted.val.7$modelClass$value(fitted.val.7$bestParam,data1),
                    Nine = fitted.val.7$modelClass$value(fitted.val.7$bestParam,data1),
                    Eleven = fitted.val.7$modelClass$value(fitted.val.7$bestParam,data1),
                    Thirteen = fitted.val.7$modelClass$value(fitted.val.7$bestParam,data1))

rates.long <- rates %>%
  pivot_longer(cols = 2:6, names_to = 'Type', values_to = "Rates_OPTIM")

ggplot(data = rates.long, mapping= aes(x= Age, y= Rates_OPTIM, colour= Type)) +
  geom_line()


