##===========================================================
## Epidemiology model modifications in R
## Copyright (C) 2020  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================

#' @import magrittr
#' @import purrr
NULL


##===========================================================
## AddModelIdentifier
##===========================================================

#' A model object check.
#' @description Adds a specified identifier id to all stocks and rates on the model m.
#' @param model A model object.
#' @param id A number or string to used as ID.
#' @return A model.
#' @family Epidemiology Model Modifiers
#' @export
AddModelIdentifier <- function( model, id ) {

  aNewStocks <- setNames( gsub( "([[:upper:]]+)t", paste0( "\\1_", id, "_t" ), names(model$Stocks) ), names(model$Stocks) )

  aNewStocksD <- setNames( paste0( "d", aNewStocks ), paste0( "d", names(model$Stocks) ) )

  aNewStocks0 <- setNames( gsub( "([[:upper:]]+)t", paste0( "\\1_", id, "_0" ), names(model$Stocks) ),
                           gsub( "([[:upper:]]+)t", paste0( "\\10" ), names(model$Stocks) ) )

  aNewRates <- setNames( paste0( names(model$Rates), "_", id ), names(model$Rates) )

  lsRHS <- deparse(model$RHSFunction, control = "all" )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocks)[[i]], "\\b" ), aNewStocks[[i]], txt ) }, init = lsRHS, x = 1:length(aNewStocks) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocksD)[[i]], "\\b" ), aNewStocksD[[i]], txt ) }, init = lsRHS, x = 1:length(aNewStocksD) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocks0)[[i]], "\\b" ), aNewStocks0[[i]], txt ) }, init = lsRHS, x = 1:length(aNewStocks0) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewRates)[[i]], "\\b" ), aNewRates[[i]], txt ) }, init = lsRHS, x = 1:length(aNewRates) )

  lsRHS
}
