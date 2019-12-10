
# migraR
R package for migration analysis focused now on the Rogers and Castro multi exponential model and the estimation of the parameters using a bayesian simulation based on uniform a priori distributions for each parameter between 0 and 1, and 0 to 100 in the case of the location parameters corresponding to ages in the migration pattern. 

This package was created on GitHub by J. Sebastian Ruiz-Santacruz and Jackson Garcés between February and June 2018.
Cite as: 

Ruiz-Santacruz and Garcés, 2018. migraR. Prototype package for adjusting Rogers and Castro models. Retrieved: dd/mm/aa

**Description**

The migraR package provides functions for estimate the parameters for Rogers and Castro Models: 
rc.7, rc.9, rc.11, rc.13 are created in the example using the Migramodel class. You may create your own multiexponetial model making 
profit of the fucntion migramodel.

**migraR functions**

The migraR functions were concived for estimate via linear optimization of the log(Migration Rate), the parameters of the Rogers and Castro models wit seven, eleven and thirteen parameters. Besides, the package gives the user the posibility to investigate wich curve fits better using the methodology provided in the IUSSP manual for the estimation of Rogers and Castro multi exponential model migration schedule with the same method for the optimization without constrains. 

http://demographicestimation.iussp.org/content/multi-exponential-model-migration-schedule

Package under contiuous changes! place your bugs on the github tab issues :) :rocket:

## Getting Started

The package should be downloaded using devtools::install_github("elflacosebas/migraR"). Migration rates from Spain were calculated using the census 2011. The fucntion best_migramod has the complete example. There are probably some bugs that we pledge you can report to us using issues in github. 

### Application of the method 

You can download the original working paper in Spanish please click here: 
https://ddd.uab.cat/record/212615

A traslation in English please go to: 
https://www.researchgate.net/publication/335928518_Estimation_of_migration_schedules_by_simulating_the_initial_values_in_the_parameter_optimizations_of_multi-exponential_migration_models_an_application_to_intra-Latin_American_international_migration


### Prerequisites

What things you need to install the software and how to install them

```
install.packages('devtools')
install.packages('dplyr')
```

### Installing

Call the packages with `library(dplyr)` and `library(devtools)` then type `install_github("elflacosebas/migraR")` 

###Example

The example was chosen selecting the migration data from Spainish Census 2011. 

```
# Calling packages and dataset.

require(migraR)
require(dplyr)
data("es_asmr")
data1 <- es_asmr[-c(1,2),c(1,5)]
colnames(data1) <- c("x","y")
attach(data1)

# Creating MigraModel Classes based on Rogers and Castro expressions. 

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

# Fitting and Plotting data
 fitted.val.7 <- best_migramod(dataIn = data1, model.rc =model.rc.7, maxite = 5E2, profile = "seven")
 fitted.val.9 <- best_migramod(dataIn = data1, model.rc =model.rc.9, maxite = 5E2, profile = "nine")
 fitted.val.11 <- best_migramod(dataIn = data1, model.rc =model.rc.11, maxite = 5E2, profile = "eleven")
 fitted.val.13 <- best_migramod(dataIn = data1, model.rc =model.rc.13, maxite = 5E2, profile = "thirteen")

 plot(data1, cex=0.1, xlab = 'Age', ylab = 'Standarized Migration Rate')
 lines(data1[,1], model.rc.7$value(fitted.val.7$bestParam,data1), col="blue")
 lines(data1[,1], model.rc.9$value(fitted.val.9$bestParam,data1), col="orange")
 lines(data1[,1], model.rc.11$value(fitted.val.11$bestParam,data1), col="blue", lty=3)
 lines(data1[,1], model.rc.13$value(fitted.val.13$bestParam,data1), col="green")
legend('topright',
        legend = c(paste("(7)", "MAPE:", round(as.numeric(fitted.val.7$bestMAPE),2), "R²:", round(as.numeric(fitted.val.7$bestRcuad),3)),
                paste("(9)", "MAPE:", round(as.numeric(fitted.val.9$bestMAPE),2), "R²:", round(as.numeric(fitted.val.9$bestRcuad),3)),
                paste("(11)", "MAPE:", round(as.numeric(fitted.val.11$bestMAPE),2), "R²:", round(as.numeric(fitted.val.11$bestRcuad),3)),
               paste("(13)", "MAPE:", round(as.numeric(fitted.val.13$bestMAPE),2), "R²:", round(as.numeric(fitted.val.13$bestRcuad),3))),
    col = c("red",'orange',"blue","darkgreen"),
   lty = c(2,6,3,5))


```

## Contributing

Please read [DESCRIPTION.md](https://github.com/elflacosebas/migrar) for more details.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

<<<<<<< HEAD
* **J. Sebastian Ruiz-Santacruz** - *Initial work (Ath and Ctb)* - [PurpleBooth](https://github.com/elflacosebas)

* **Jackson Garcés** - *Initial work (Ctb)* - [PurpleBooth](https://github.com/jackowacko)
=======
* **J. Sebastian Ruiz-Santacruz** - *Initial Demographic and statistical work* - [PurpleBooth](https://github.com/elflacosebas)

* **Jackson Garcés** - *Packaging work and statistical support* - [PurpleBooth](https://github.com/jackowacko)
>>>>>>> fa095bfa2163760623c547d5f67609e5b1b7fe14

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.


## Acknowledgments

* to Joaquín Recaño Valverde (http://ced.uab.es/es/directori/joaquim-recano-valverde/), who inspires this work. 
