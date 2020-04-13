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

GetLeftHandSides <- function( definitionLines ) {

  if( ! is.character(definitionLines) ) {
    stop( "The argument definitionLines is expected to be a string", call. = TRUE )
  }

  pos <- grep( "\\s([[:alpha:]].*)\\s*<-(.*)", definitionLines )

  lsOtherLHS <- gsub( "(\\s)([[:alpha:]].*)\\s*<-(.*)", "\\2", definitionLines[pos] )

  trimws(lsOtherLHS)
}

#' A model object check.
#' @description Adds a specified identifier id to all stocks and rates on the model m.
#' @param model A model object.
#' @param id A number or string to used as ID.
#' @param addToOtherLeftHandSidesQ Should the id added to other left hand side symbols or not?
#' @param birthsTermQ Should the births term be included or not?
#' @return A model.
#' @details The original model object has \code{birthTermsQ} in the closure of \code{model$RHSFunction}.
#' (For now) it is easier to just have that parameter specified than imply it.
#' @family Epidemiology Model Modifier functions
#' @export
AddModelIdentifier <- function( model, id, addToOtherLeftHandSidesQ = TRUE, birthsTermQ = FALSE ) {

  if( !EpidemiologyFullModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( !( is.character(id) && length(id) == 1 ) ) {
    stop( "The argument id is expected to be a string", call. = TRUE )
  }

  ## New model
  newModel <- model

  ## Deparse RHS function
  lsRHSdefinition <- deparse(model$RHSFunction, control = "all" )

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

  aBirthsTerm <- setNames( ifelse( birthsTermQ, "TRUE", "FALSE" ), c("birthsTermQ") )

  if( addToOtherLeftHandSidesQ ) {

    # pos <- grep( "\\s([[:alpha:]].*)\\s*<-(.*)", lsRHSdefinition )
    #
    # lsOtherLHS <- gsub( "(\\s)([[:alpha:]].*)\\s*<-(.*)", "\\2", lsRHSdefinition[pos] )
    #
    # lsOtherLHS <- trimws(lsOtherLHS)
    lsOtherLHS <- GetLeftHandSides( lsRHSdefinition )

    lsOtherLHS <- setdiff( lsOtherLHS, names(model$Stocks) )

    aOtherLHS <- setNames( paste0(lsOtherLHS, "_", id), lsOtherLHS )
  }

  ## Apply rules to stocks, rates, initial conditions, and rate rules
  newModel$Stocks <- setNames( newModel$Stocks, aNewStocks[ names(newModel$Stocks) ] )

  newModel$Rates <- setNames( newModel$Rates, aNewRates[ names(newModel$Rates) ] )

  newModel$InitialConditions <- setNames( newModel$InitialConditions, aNewStocks[ names(newModel$InitialConditions) ] )

  newModel$RateRules <- setNames( newModel$RateRules, aNewRates[ names(newModel$RateRules) ] )


  ## Apply mapping rules to deparsed RHS definition
  lsRHSdefinition <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocks)[[i]], "\\b" ), aNewStocks[[i]], txt ) }, init = lsRHSdefinition, x = 1:length(aNewStocks) )

  lsRHSdefinition <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocksD)[[i]], "\\b" ), aNewStocksD[[i]], txt ) }, init = lsRHSdefinition, x = 1:length(aNewStocksD) )

  lsRHSdefinition <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewStocks0)[[i]], "\\b" ), aNewStocks0[[i]], txt ) }, init = lsRHSdefinition, x = 1:length(aNewStocks0) )

  lsRHSdefinition <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aNewRates)[[i]], "\\b" ), aNewRates[[i]], txt ) }, init = lsRHSdefinition, x = 1:length(aNewRates) )

  lsRHSdefinition <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aBirthsTerm)[[i]], "\\b" ), aBirthsTerm[[i]], txt ) }, init = lsRHSdefinition, x = 1:length(aBirthsTerm) )

  if( addToOtherLeftHandSidesQ ) {
    lsRHSdefinition <- Reduce( function( txt, i ) { gsub( paste0( "\\b", names(aOtherLHS)[[i]], "\\b" ), aOtherLHS[[i]], txt ) }, init = lsRHSdefinition, x = 1:length(aOtherLHS) )
  }

  newModel$RHSFunction <- eval( parse( text = lsRHSdefinition ) )

  ## Result
  newModel
}


##===========================================================
## TakeRHSFunctionBodyLines
##===========================================================

#' Take RHSFunction body lines.
#' @description Takes RHSFunction body lines of a model.
#' @param model A model object.
#' @return A character vector
#' @family Epidemiology Model Modifier functions
#' @export
TakeRHSFunctionBodyLines <- function( model ) {

  if( !EpidemiologyModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  ## Deparse RHS function
  lsRHSdefinition <- deparse(model$RHSFunction, control = "all" )

  ## Determine start and end positions
  posStart <- grep( "^function", lsRHSdefinition )
  posEnd <- grep( "\\s+return", lsRHSdefinition )

  assertthat::assert_that( posStart == 1 )
  assertthat::assert_that( grepl( "\\s+\\{", lsRHSdefinition[[posStart+2]] ) )

  lsRHSdefinition[ (posStart+3) : (posEnd-1) ]
}


##===========================================================
## JoinModels
##===========================================================

#' Join two models.
#' @description Makes a multi-site model with the stocks, rates, and equations
#' of arguments joined.
#' @param model1 A model object.
#' @param model2 A model object.
#' @return A model object.
#' @family Epidemiology Model Modifier functions
#' @export
JoinModels <- function( model1, model2 ) {

  if( !EpidemiologyModelQ(model1) ) {
    stop( "The argument model1 is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( !EpidemiologyModelQ(model2) ) {
    stop( "The argument model2 is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( length( intersect( names(model1$Stocks), names(model2$Stocks) ) ) > 0 ) {
    stop( "The stocks of the two models overlap.", call. = TRUE )
  }

  if( EpidemiologyFullModelQ(model1) || EpidemiologyFullModelQ(model2) ) {
    newModel <- SIRModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )
  } else {
    newModel <- SIRModel( initialConditionsQ = FALSE, rateRulesQ = FALSE)
  }

  ## Stocks & rates
  newModel$Stocks <- c( model1$Stocks, model2$Stocks )

  newModel$Rates <- c( model1$Rates, model2$Rates )

  ## RHS functions
  lsRHSFuncLines1 <- TakeRHSFunctionBodyLines(model1)

  lsRHSFuncLines2 <- TakeRHSFunctionBodyLines(model2)

  lsStartLines <-
    c("function( time, state, parameters ) {",
      "          with(as.list( c( state, parameters ) ),",
      "               {")

  lsEndLines <-
    c( "               }",
       "          )",
       "        }")

  returnStockNames1 <- names(model1$Stocks)
  returnStockNames1 <-paste0( "d", returnStockNames1 )
  returnStockNames1 <- returnStockNames1[ returnStockNames1 %in% GetLeftHandSides( lsRHSFuncLines1 ) ]

  returnStockNames2 <- names(model2$Stocks)
  returnStockNames2 <-paste0( "d", returnStockNames2 )
  returnStockNames2 <- returnStockNames2[ returnStockNames2 %in% GetLeftHandSides( lsRHSFuncLines2 ) ]


  lsReturnLine <- paste( c( returnStockNames1, returnStockNames2 ), collapse = ", " )
  lsReturnLine <- paste( "                 return( list( c( ", lsReturnLine, " ) ) )" )

  newModel$RHSFunction <- eval( parse( text = c( lsStartLines, lsRHSFuncLines1, lsRHSFuncLines2, lsReturnLine, lsEndLines ) ) )

  ## Initial conditions and rate rules
  if( EpidemiologyFullModelQ(newModel) ) {

    newModel$InitialConditions <- c( model1$InitialConditions, model2$InitialConditions )

    newModel$RateRules <- c( model1$RateRules, model2$RateRules )

  }

  #Result
  newModel
}


##===========================================================
## MakeCoreMultiSiteModel
##===========================================================

#' Make core multi-site model.
#' @description Makes core multi-site model.
#' @param model A model object.
#' @param cellIdentifiers A a character vector of cell identifiers.
#' @return A character vector
#' @family Epidemiology Model Query functions
#' @export
MakeCoreMultiSiteModel <- function( model, cellIdentifiers ) {

  if( !EpidemiologyModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( ! is.character(cellIdentifiers) ) {
    stop( "The argument cellIdentifiers is expected to be a character vector.", call. = TRUE )
  }


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
