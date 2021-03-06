# Epidemiologic Compartmental Modeling Monad R package

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test coverage](https://codecov.io/gh/antononcube/ECMMon-R/branch/master/graph/badge.svg)](https://codecov.io/gh/antononcube/ECMMon-R?branch=master)
[![R-CMD-check](https://github.com/antononcube/ECMMon-R/workflows/R-CMD-check/badge.svg)](https://github.com/antononcube/ECMMon-R/actions)
<!-- badges: end -->
  
## Introduction

This package has implementations of [epidemiologic compartmental models](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology), [Wk1, HH1], 
modified to fit better simulations of propagation of [COVID-19](https://en.wikipedia.org/wiki/Coronavirus_disease_2019), [Wk2]. 

The models implemented in this package deal with one, two, or all three of the rectangles in this diagram:

![0y1f7ckbmwf5i](https://github.com/antononcube/SystemModeling/raw/master/Projects/Coronavirus-propagation-dynamics/Diagrams/Coronavirus-propagation-simple-dynamics.jpeg)

The content of the package corresponds to the Mathematica packages and notebooks referenced below. 
(But there are some significant differences.)

## Installation

Here is an installation command using ["devtools"](https://cran.r-project.org/web/packages/devtools/readme/README.html):

```r
devtools::install_github(repo = "antononcube/ECMMon-R")
```

**Remark:** The simulations done by the package functions rely on the R package 
["deSolve"](https://cran.r-project.org/web/packages/deSolve/index.html),
which in turn relies on FORTRAN code. For example, on Mac OS X the installation of `gfortran` is needed.

## Usage

Here is a notebook that demonstrates usage of the provided objects and functions:
["Basic experiments workflow for simple epidemiological models"](https://github.com/antononcube/ECMMon-R/blob/master/notebooks/Basic-experiments-workflow-for-simple-epidemiological-models.Rmd),
([HTML](https://htmlpreview.github.io/?https://github.com/antononcube/ECMMon-R/blob/master/notebooks/Basic-experiments-workflow-for-simple-epidemiological-models.nb.html)).
  
The package repository has [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/index.html) interfaces for the more important models. 

### Core models

Here are the core (single-site) models 

  - SIR   
    “Susceptible, Infected, Recovered” populations

  - SEIR   
    “Susceptible, Exposed, Infected, Recovered” populations

  - SEI2R   
    “Susceptible, Exposed, Infected two, Recovered” populations

  - SEI2HR   
    “Susceptible, Exposed, Infected two, Hospitalized, Recovered” populations

  - SEI2HREcon   
    “Susceptible, Exposed, Infected two, Hospitalized, Recovered” populations with Economic extensions



## References

### Articles

[Wk1] Wikipedia entry, ["Compartmental models in epidemiology"](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology).

[Wl2] Wikipedia entry, ["Coronavirus disease 2019"](https://en.wikipedia.org/wiki/Coronavirus_disease_2019).

[HH1] Herbert W. Hethcote (2000). ["The Mathematics of Infectious Diseases"](http://leonidzhukov.net/hse/2014/socialnetworks/papers/2000SiamRev.pdf). SIAM Review. 42 (4): 599–653. Bibcode:2000SIAMR..42..599H. doi:10.1137/s0036144500371907.

[AA1] Anton Antonov, ["Coronavirus propagation modeling considerations"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/Documents/Coronavirus-propagation-modeling-considerations.md), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AA2] Anton Antonov, ["Basic experiments workflow for simple epidemiological models"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/Documents/Basic-experiments-workflow-for-simple-epidemiological-models.md), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AA3] Anton Antonov, ["Scaling of Epidemiology Models with Multi-site Compartments"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/Documents/Scaling-of-epidemiology-models-with-multi-site-compartments.md), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

### Repositories, packages

[AAr1] Anton Antonov, [Coronavirus propagation dynamics project](https://github.com/antononcube/SystemModeling/tree/master/Projects/Coronavirus-propagation-dynamics), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp1] Anton Antonov, ["Epidemiology models Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModels.m), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp2] Anton Antonov, ["Epidemiology models modifications Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelModifications.m), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp3] Anton Antonov, ["Epidemiology modeling visualization functions Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/Projects/Coronavirus-propagation-dynamics/WL/EpidemiologyModelingVisualizationFunctions.m), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).

[AAp4] Anton Antonov, ["System dynamics interactive interfaces functions Mathematica package"](https://github.com/antononcube/SystemModeling/blob/master/WL/SystemDynamicsInteractiveInterfacesFunctions.m), (2020), [SystemModeling at GitHub](https://github.com/antononcube/SystemModeling).


