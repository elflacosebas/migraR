<<<<<<< HEAD


# Project Title

migraR: package with functions to estimate Rogers and Castro models and other fuctions useful to estimate migration. 

## Getting Started

The package should be downloaded using devtools::install_github("elflacosebas/migraR"). Migration rates from Spain were calculated using the census 2011. The fucntion best_migramod has the complete example. 
There are probably some bugs that we pledge you can report to us using issues in github. 

### Prerequisites

What things you need to install the software and how to install them

```
install.packages('devtools')
install.packages('dplyr')
```

### Installing

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

The best_migramod function was designed using a tolerance that can be changed inside the functions. 

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
require(migraR)
require(dplyr)
data("es_asmr")
data1 <- es_asmr[-c(1,2),c(1,5)]
colnames(data1) <- c("x","y")
attach(data1)
model.rc.13 = MigraModel(
  name = 'castro_13',
  expr = rc_expression(profile = "thirteen")
)

model.rc.7 = MigraModel(
  name = 'castro_7',
  expr = rc_expression(profile = "seven")
)
model.rc.11 = MigraModel(
  name = 'castro_11',
  expr = rc_expression(profile = "eleven")
)

plot(data1, cex=0.1, xlab = 'Age', ylab = 'Standarized Migration Rate')
fitted.val.11 <- best_migramod(dataIn = data1, model.rc =model.rc.11, maxite = 5E2, profile = "eleven")
lines(data1[,1], model.rc.11$value(fitted.val.11$bestParam,data1), col="blue", lty=3)
fitted.val.7 <- best_migramod(dataIn = data1, model.rc =model.rc.7, maxite = 5E2, profile = "seven")
lines(data1[,1], model.rc.7$value(fitted.val.7$bestParam,data1), col="blue")
fitted.val.13 <- best_migramod(dataIn = data1, model.rc =model.rc.13, maxite = 5E2, profile = "thirteen")
lines(data1[,1], model.rc.13$value(fitted.val.13$bestParam,data1), col="green")
legend("topright",legend = c("seven","eleven","thirteen"),fill = c("red","blue","green"))

```

## Contributing

Please read [DESCRIPTION.md](https://github.com/elflacosebas/migrar) for more details.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **J. Sebastian Ruiz-Santacruz** - *Initial work* - [PurpleBooth](https://github.com/elflacosebas)

* **Jackson Garcés** - *Initial work* - [PurpleBooth](https://github.com/jackowacko)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.


## Acknowledgments

* to Joaquín Recaño Valverde (http://ced.uab.es/es/directori/joaquim-recano-valverde/), who inspires this work. 

=======
# migraR
R package for migration analysis focused now on the Rogers and Castro multi exponential model and the estimation of the parameters using a bayesian simulation based on uniform a priori distributions for each parameter between 0 and 1. 


This package was created by J. Sebastian Ruiz-Santacruz and Jackson Garcés in 2018.

To install the package in R, you must install first the package dplyr and devtools running the following statements:

`install.packages(dplyr)`

`install.packages(devtools)`

and call them with `library(dplyr)` and `library(devtools)`

after running previous lines, you should use the function install.github to dowload de package as follows:

`install_github("elflacosebas/migraR")`

**Description**

The migraR package provides functions for estimate the parameters for Rogers and Castro Models: 
rc.7, rc.11, rc.13 are created in the example using the Migramodel class. You may create your own multiexponetial model making 
profit of the fucntion migramodel.

**migraR functions**

The migraR functions were concived for estimate via linear optimization of the log(Migration Rate), the parameters of 
the Rogers and Castro models wit seven, eleven and thirteen parameters. Besides, the package 
gives the user the posibility to investigate wich curve fits better using the methodology provided 
in the IUSSP manual for the estimation of Rogers and Castro multi exponential model migration 
schedule with variation on the optimization method.

http://demographicestimation.iussp.org/content/multi-exponential-model-migration-schedule


Package under contiuous changes! place your bugs on the github tab issues :) :rocket:
>>>>>>> 682e5544ac4dd3cf0775a8de711031e54e88783a
