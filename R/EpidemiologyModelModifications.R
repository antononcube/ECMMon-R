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
## SparseMatrixQ
##===========================================================

SparseMatrixQ <- function (x)
{
  sum(c("dgCMatrix", "dgRMatrix", "dgTMatrix") %in% class(x)) > 0
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


##===========================================================
## GrepDefinitions
##===========================================================

GrepDefinitions <- function( definitionLines, patterns, ... ) {

  if( ! is.character(definitionLines) ) {
    stop( "The argument definitionLines is expected to be a character vector.", call. = TRUE )
  }

  if( ! is.character(patterns) ) {
    stop( "The argument patterns is expected to be a character vector.", call. = TRUE )
  }

  res <- purrr::map( patterns, function(x) grep( x, definitionLines, ... ) )
  names(res) <- patterns

  res
}


##===========================================================
## GetLeftHandSides
##===========================================================

GetLeftHandSides <- function( definitionLines ) {

  if( ! is.character(definitionLines) ) {
    stop( "The argument definitionLines is expected to be a string", call. = TRUE )
  }

  pos <- grep( "\\s([[:alpha:]].*)\\s*<-(.*)", definitionLines )

  lsOtherLHS <- gsub( "(\\s)([[:alpha:]].*)\\s*<-(.*)", "\\2", definitionLines[pos] )

  trimws(lsOtherLHS)
}


##===========================================================
## AddModelIdentifier
##===========================================================

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

  ## Initial conditions and rate rules
  if( EpidemiologyFullModelQ(newModel) ) {

    newModel$InitialConditions <- c( model1$InitialConditions, model2$InitialConditions )

    newModel$RateRules <- c( model1$RateRules, model2$RateRules )

  }

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

  if( EpidemiologyFullModelQ(newModel) ) {

    lsReturnLine <- paste( paste0( "d", names(newModel$InitialConditions) ), collapse = ", " )

  } else {

    returnStockNames1 <- names(model1$Stocks)
    returnStockNames1 <- paste0( "d", returnStockNames1 )
    returnStockNames1 <- returnStockNames1[ returnStockNames1 %in% GetLeftHandSides( lsRHSFuncLines1 ) ]

    returnStockNames2 <- names(model2$Stocks)
    returnStockNames2 <- paste0( "d", returnStockNames2 )
    returnStockNames2 <- returnStockNames2[ returnStockNames2 %in% GetLeftHandSides( lsRHSFuncLines2 ) ]


    lsReturnLine <- paste( c( returnStockNames1, returnStockNames2 ), collapse = ", " )

  }

  lsReturnLine <- paste( "                 return( list( c( ", lsReturnLine, " ) ) )" )

  newModel$RHSFunction <- eval( parse( text = c( lsStartLines, lsRHSFuncLines1, lsRHSFuncLines2, lsReturnLine, lsEndLines ) ) )

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
#' @return A model object
#' @family Epidemiology Model Modification functions
#' @export
MakeCoreMultiSiteModel <- function( model, cellIdentifiers ) {

  if( !EpidemiologyModelQ(model) ) {
    stop( "The argument model is expected to be an epidemiology model object.", call. = TRUE )
  }

  if( ! is.character(cellIdentifiers) && length(cellIdentifiers) > 1 ) {
    stop( "The argument cellIdentifiers is expected to be a character vector with more than one element.", call. = TRUE )
  }

  lsModels <- purrr::map( cellIdentifiers, function(x) { AddModelIdentifier( model = model, id = x ) } )

  Reduce( function( a, x) JoinModels( model1 = a, model2 = x), init = lsModels[[1]], x = lsModels[-1] )
}


##===========================================================
## MakeMigrationEquationsEquations
##===========================================================

#' Make migration equations.
#' @description Makes the migration (supplemental) equations with the specified matrix and stocks.
#' @param data A matrix or a data frame with columns \code{c('i', 'j', 'x')}.
#' @param migratingStocks A a character vector with stocks.
#' @param totalStock A string
#' @param nLeadingWhitespace Number of leading whitespace characters for each equation.
#' @return A character vector
#' @family Epidemiology Model Modifier functions
#' @export
MakeMigrationEquations <- function( data, migratingStocks, totalStock = "TPt", nLeadingWhitespace = 8 ) {

  if( !( is.matrix(data) ||
         SparseMatrixQ(data) ||
         is.data.frame(data) && sum( c('i', 'j', 'x') %in% colnames(data) ) == 3 ) ) {
    stop( "The argument data is expected to be a matrix, a sparse matrix, or a (matrix long form) data frame with columns c('i', 'j', 'x') .", call. = TRUE )
  }

  if( ! is.character(totalStock) && length(totalStock) == 1 ) {
    stop( "The argument totalStock is expected to be a string.", call. = TRUE )
  }

  if( ! is.character(migratingStocks) ) {
    stop( "The argument migratingStocks is expected to be a character vector.", call. = TRUE )
  }

  if( !is.data.frame(data) ) {
    data <- Matrix::summary( as( data, "dgCMatrix") )
  }

  ## Leading white space
  leadingWhitespace <- paste0( rep_len( x = " ", length.out = nLeadingWhitespace ), collapse = "" )

  ## Remove the last character "t" if present
  totalStock <- gsub( "t$", "", totalStock)
  migratingStocks <- gsub( "t$", "", migratingStocks)

  ## Main loop
  lsRes <-
    purrr::map( migratingStocks, function(mStock) {

      ## Equations for stocks (people) getting out
      lsEqsOut <-
        purrr::map( split( data, data$i ), function(dfX) {

          lhs <- paste0( "d", mStock, "_", dfX$i[[1]], "_t" )

          terms <- purrr::map2_chr( dfX$j, dfX$x, function(j,x) paste0( x, " / ", totalStock, "_", dfX$i[[1]], "_0", " * ", mStock, "_", dfX$i[[1]], "_t" ) )

          paste( lhs, "<-", lhs, "-", paste( terms, collapse = " - " ) )
        })

      ## Equations for stocks (people) comming in
      lsEqsIn <-
        purrr::map( split( data, data$j ), function(dfX) {

          lhs <- paste0( "d", mStock, "_", dfX$j[[1]], "_t" )

          terms <- purrr::map2_chr( dfX$i, dfX$x, function(i,x) paste0( x, " / ", totalStock, "_", i, "_0", " * ", mStock, "_", i, "_t" ) )

          paste( lhs, "<-", lhs, "+", paste( terms, collapse = " + " ) )
        })

      ## Join equations
      lsEqsAll <- c( setNames(lsEqsOut, NULL), setNames(lsEqsIn, NULL) )

      ## Put leading whitespace
      purrr::map( lsEqsAll, function(x) paste0( leadingWhitespace, x ) )
    })

  names(lsRes) <- migratingStocks

  lsRes
}


##===========================================================
## To site compartmental model
##===========================================================

#' Crate site compartmental model
#' @description Makes the migration (supplemental) equations with the specified matrix and stocks.
#' @param model A (single-site) model object.
#' @param mat A matrix.
#' @param migratingStocks A a character vector with stocks (names.)
#' @param cellIdentifiers A character vector with cell identifiers.
#' If NULL then \code{unique(c(rownames(mat), colnames(mat)))} is used.
#' @param ... Additional arguments for \code{MakeMigrationEquations}.
#' @return A character vector
#' @family Epidemiology Model Modifier functions
#' @export
ToSiteCompartmentalModel <- function( model, mat, migratingStocks = NULL, cellIdentifiers = NULL, ... ) {

  if( !( is.matrix(mat) || SparseMatrixQ(mat) ) ) {
    # stop( "The argument data is expected to be a matrix, a sparse matrix, or a (matrix long form) data frame with columns c('i', 'j', 'x') .", call. = TRUE )
    stop( "The argument data is expected to be a matrix, a sparse matrix.", call. = TRUE )
  }

  ## Dealing with matrix names
  if( is.null(rownames(mat)) && is.null(colnames(mat)) ) {

    rownames(mat) <- seq(1, nrow(mat))
    colnames(mat) <- seq(1, ncol(mat))

  } else if ( is.null(rownames(mat))  ) {

    warning( "Assigning automatic rownames.", call. = TRUE )
    rownames(mat) <- seq(1, nrow(mat))

  } else if ( is.null(colnames(mat))  ) {

    warning( "Assigning automatic colnames", call. = TRUE )
    colnames(mat) <- seq(1, ncol(mat))

  }

  if( is.null(cellIdentifiers) ) {
    cellIdentifiers <- unique( c(rownames(mat), colnames(mat)) )
  }

  if( is.null(migratingStocks) ) {
    migratingStocks <- c( "SPt", "EPt", "RPt", "INSPt", "IPt" )
    migratingStocks <- intersect( migratingStocks, names(model$Stocks) )
  }

  ## Core mult-site model extension
  modelMultiSite <- MakeCoreMultiSiteModel( model = model, cellIdentifiers = cellIdentifiers )

  ## Derive migration terms equations
  lsMigrationEquations <- MakeMigrationEquations( data = mat, migratingStocks = migratingStocks, ... )
  lsMigrationEquations <- do.call(c, lsMigrationEquations)
  #lsMigrationEquations <- paste( lsMigrationEquations, ";" )

  ## Add migration terms equations
  lsRHSLines <- deparse(modelMultiSite$RHSFunction, control = "all" )

  pos <- GrepDefinitions( lsRHSLines, patterns = "^\\s*return" )

  assertthat::assert_that( length(pos) == 1 )
  pos <- pos[[1]]

  lsRHSLines <- c( lsRHSLines[1:(pos-1)], "", lsMigrationEquations, "", lsRHSLines[pos:length(lsRHSLines)] )

  modelMultiSite$RHSFunction <- eval( parse( text = lsRHSLines ) )

  ## Result
  modelMultiSite
}
