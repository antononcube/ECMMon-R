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
#' @param birthTermsQ A logical.
#' @return A model.
#' @details The original model object has \code{birthTermsQ} in the closure of \code{model$RHSFunction}.
#' (For now) it is easier to just have that parameter specified than imply it.
#' @family Epidemiology Model Modifier functions
#' @export
AddModelIdentifier <- function( model, id, birthTermsQ = FALSE ) {

  if( !EpidemiologyFullModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( !( is.character(id) && length(id) == 1 ) ) {
    stop( "The argument id is expected to be a string", call. = TRUE )
  }

  ## New model
  newModel <- model

  ## Mapping rules
  aNewStocks <- setNames( gsub( "([[:upper:]]+)t", paste0( "\\1_", id, "_t" ), names(model$Stocks) ), names(model$Stocks) )

  aNewStocksD <- setNames( paste0( "d", aNewStocks ), paste0( "d", names(model$Stocks) ) )

  aNewStocks0 <- setNames( gsub( "([[:upper:]]+)t", paste0( "\\1_", id, "_0" ), names(model$Stocks) ),
                           gsub( "([[:upper:]]+)t", paste0( "\\10" ), names(model$Stocks) ) )

  aNewRates <- setNames( paste0( names(model$Rates), "_", id ), names(model$Rates) )

  if( sum( names(aNewRates) %in% names(aNewStocks0) ) > 0 ) {

    common <- intersect( names(aNewRates), names(aNewStocks0) )

    aNewRates[ common ] <- aNewStocks0[ common ]

  }

  aBirthsTerm <- setNames( ifelse( birthTermsQ, "TRUE", "FALSE" ), c("birthsTermQ") )

  ## Apply rules to stocks, rates, initial conditions, and rate rules
  newModel$Stocks <- setNames( newModel$Stocks, aNewStocks[ names(newModel$Stocks) ] )

  newModel$Rates <- setNames( newModel$Rates, aNewRates[ names(newModel$Rates) ] )

  newModel$InitialConditions <- setNames( newModel$InitialConditions, aNewStocks[ names(newModel$InitialConditions) ] )

  newModel$RateRules <- setNames( newModel$RateRules, aNewRates[ names(newModel$RateRules) ] )


  ## Deparse RHS function and apply mapping rules
  lsRHS <- deparse(model$RHSFunction, control = "all" )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocks)[[i]], "\\b" ), aNewStocks[[i]], txt ) }, init = lsRHS, x = 1:length(aNewStocks) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocksD)[[i]], "\\b" ), aNewStocksD[[i]], txt ) }, init = lsRHS, x = 1:length(aNewStocksD) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocks0)[[i]], "\\b" ), aNewStocks0[[i]], txt ) }, init = lsRHS, x = 1:length(aNewStocks0) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewRates)[[i]], "\\b" ), aNewRates[[i]], txt ) }, init = lsRHS, x = 1:length(aNewRates) )

  lsRHS <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aBirthsTerm)[[i]], "\\b" ), aBirthsTerm[[i]], txt ) }, init = lsRHS, x = 1:length(aBirthsTerm) )

  newModel$RHSFunction <- eval( parse( text = lsRHS ) )

  ## Result
  newModel
}


##===========================================================
## GetStocks
##===========================================================

#' Get stock symbols.
#' @description Gets the stock symbols that have descriptions matching specified patterns.
#' @param model A model object.
#' @param patterns A a character vector of patterns to be applied to the descriptions of model's stocks.
#' @return A character vector
#' @family Epidemiology Model Query functions
#' @export
GetStocks <- function( model, patterns ) {

  if( !EpidemiologyModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( ! is.character(patterns) ) {
    stop( "The argument patterns is expected to be a character vector.", call. = TRUE )
  }

  lsAll <- model$Stocks

  lsFocus <- purrr::map( patterns, function(ss) { grep( pattern = ss, x = lsAll, value = TRUE ) } )
  lsFocus <- as.character( unlist( lsFocus ) )

  names( model$Stocks[ model$Stocks %in% lsFocus  ] )

}


##===========================================================
## GetRates
##===========================================================

#' Get rate symbols.
#' @description Gets the rate symbols that have descriptions matching specified patterns.
#' @param model A model object.
#' @param patterns A a character vector of patterns to be applied to the descriptions of model's rates.
#' @return A character vector
#' @family Epidemiology Model Query functions
#' @export
GetRates <- function( model, patterns ) {

  ## This code is very similar / same as GetStocks -- it has to be refactored.

  if( !EpidemiologyModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( ! is.character(patterns) ) {
    stop( "The argument patterns is expected to be a character vector.", call. = TRUE )
  }

  lsAll <- model$Rates

  lsFocus <- purrr::map( patterns, function(ss) { grep( pattern = ss, x = lsAll, value = TRUE ) } )
  lsFocus <- as.character( unlist( lsFocus ) )

  names( model$Rates[ model$Rates %in% lsFocus  ] )

}
