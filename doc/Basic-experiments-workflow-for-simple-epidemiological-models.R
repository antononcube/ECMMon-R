## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ECMMon)

## ---- message=FALSE-----------------------------------------------------------
devtools::install_github( "antononcube/ECMMon-R")
library(ECMMon)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

## -----------------------------------------------------------------------------
model <- SEI2RModel( initialConditionsQ = TRUE, rateRulesQ = TRUE, birthsTermQ = FALSE )

## -----------------------------------------------------------------------------
names(model)

## -----------------------------------------------------------------------------
as.data.frame(model$Stocks)

## -----------------------------------------------------------------------------
as.data.frame(model$Rates)

## -----------------------------------------------------------------------------
model$RHSFunction

## -----------------------------------------------------------------------------
as.data.frame(model$InitialConditions)

## -----------------------------------------------------------------------------
as.data.frame(model$RateRules)

## -----------------------------------------------------------------------------
model$InitialConditions["ISSPt"] <- 20

## -----------------------------------------------------------------------------
model$RateRules["contactRateISSP"] <- 0.3

## -----------------------------------------------------------------------------
times <- seq( 0, 365, 1)

## -----------------------------------------------------------------------------
sol <- deSolve::ode(y = model[["InitialConditions"]], times = times, func = model[["RHSFunction"]], parms = model[["RateRules"]], method = "rk4" )

## -----------------------------------------------------------------------------
dfSol <- as.data.frame(sol)
colnames(dfSol) <- gsub( "time", "Time", colnames(dfSol) )
head(dfSol)

## -----------------------------------------------------------------------------
dfSol %>% 
  tidyr::pivot_longer( cols = colnames(dfSol)[-1], names_to = "Stock", values_to = "Value" ) %>%
  dplyr::filter( Stock != "MLPt" ) %>% 
  ggplot2::ggplot( ) +
  ggplot2::geom_line( ggplot2::aes( x = Time, y = Value, color = Stock ) )

## -----------------------------------------------------------------------------
hist(sol)

## -----------------------------------------------------------------------------
ecmObj <- 
  ECMMonUnit( SEI2RModel() ) %>% 
  ECMMonAssignInitialConditions( initConds = c( "ISSPt" = 20 ) ) %>%
  ECMMonAssignRateValues(rateValues = c("contactRateISSP" = 0.3 ) ) %>% 
  ECMMonSimulate( 365 ) %>% 
  ECMMonPlotSolutions( stocksSpec = ".*Population" )

## -----------------------------------------------------------------------------
ecmObj <- 
  ecmObj %>%
  ECMMonPlotSolutionHistograms(bins=20)

## -----------------------------------------------------------------------------
head(ecmObj %>% ECMMonTakeSolution)

