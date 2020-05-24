##===========================================================
## Epidemiology Compartmental Modeling Monad in R
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


#' @import deSolve
#' @import magrittr
#' @import purrr
#' @import dplyr
#' @improt tidyr
#' @import ggplot2
NULL

##===========================================================
## ECMMon failure symbol
##===========================================================

#' Failure symbol for ECMMon.
#' @description Failure symbol for the monad ECMMon.
#' @export
ECMMonFailureSymbol <- NA

#' Failure test for an ECMMon object.
#' @description Test is an ECMMon object a failure symbol.
#' @export
ECMMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## ECMMon Unit
##===========================================================

#' Make a ECMMon Unit
#' @description Creates a monad object.
#' @param model A single site model.
#' @return An S3 class "ECMMon". In other words, a list with the attribute "class" set to "ECMMon".
#' @export
ECMMonUnit <- function( model = NULL ) {

  res <- list( Value = NULL, SingleSiteModel = NULL, MultiSiteModel = NULL, Grid = NULL, deSolveSolution = NULL, Solution = NULL )
  attr(res, "class") <- "ECMMon"

  if( ! is.null(model) ) {

    if( EpidemiologyModelQ(model) ) {
      res$SingleSiteModel <- model
    } else {
      warning("The value of the argument model is expected to be an epidemiology model object or NULL.", call. = TRUE)
      return(ECMMonFailureSymbol)
    }

  }

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a ECMMon object.
#' @description Sets the value in a ECMMon monad object.
#' @param ecmObj An ECMMon object.
#' @param value The new value.
#' @return A ECMMon object.
#' @details Assigns \code{value} to \code{ecmObj$Value}.
#' @family Set/Take functions
#' @export
ECMMonSetValue <- function( ecmObj, value ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  ecmObj$Value <- value
  ecmObj
}

#' Take the value in a ECMMon object.
#' @description Takes the value from ECMMon monad object.
#' @param ecmObj An ECMMon object.
#' @return Just \code{ecmObj$Value}.
#' @family Set/Take functions
#' @export
ECMMonTakeValue <- function( ecmObj ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  ecmObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an ECMMon object.
#' @param ecmObj An ECMMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value or not?
#' @param warningQ Should warning message be issued or not?
#' @return A logical value or an ECMMon object.
#' @export
ECMMonMemberPresenceCheck <- function( ecmObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE, warningQ = TRUE ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(ecmObj[[memberName]]) ) {
    if( warningQ ) {
      warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    }
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { ECMMonFailureSymbol }
  else { ecmObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param ecmObj An ECMMon object.
#' @return A ECMMon object.
#' @details Prints \code{ecmObj$Value}.
#' @export
ECMMonEchoValue <- function( ecmObj ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  print( ecmObj$Value )

  ecmObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
#' @param ecmObj An ECMMon object.
#' @param f A function to be applied to \code{ecmObj$Value}.
#' @return A ECMMon object.
#' @details Prints \code{f(ecmObj$Value)}.
#' @export
ECMMonEchoFunctionValue <- function( ecmObj, f ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  print( f(ecmObj$Value) )

  ecmObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{f(ecmObj)}
#' then returns the original \code{ecmObj};
#' else returns the result of \code{f(ecmObj)}.
#' @param ecmObj An ECMMon object.
#' @param f A function to be applied to the monad object.
#' @return A ECMMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
ECMMonOption <- function( ecmObj, f ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  res <- ecmObj %>% f

  if( ECMMonFailureQ(res) ) { return(ecmObj) }

  res
}

##===========================================================
## SingleSiteModel setter
##===========================================================

#' Set SingleSiteModel.
#' @description Sets SingleSiteModel into the monad object.
#' @param ecmObj An ECMMon object.
#' @param SingleSiteModel An object member to be set.
#' @return An ECMMon object.
#' @family Set/Take functions
#' @export
ECMMonSetSingleSiteModel <- function( ecmObj, SingleSiteModel ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.null(SingleSiteModel) || is.list(SingleSiteModel)) ) {
    warning("The argument SingleSiteModel is expected to be NULL or a list.", call. = TRUE)
    return(ECMMonFailureSymbol)
  }

  ecmObj$SingleSiteModel <- SingleSiteModel

  ecmObj
}

##===========================================================
## MultiSiteModel setter
##===========================================================

#' Set MultiSiteModel.
#' @description Sets MultiSiteModel into the monad object.
#' @param ecmObj An ECMMon object.
#' @param MultiSiteModel An object member to be set.
#' @return An ECMMon object.
#' @family Set/Take functions
#' @export
ECMMonSetMultiSiteModel <- function( ecmObj, MultiSiteModel ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.null(MultiSiteModel) || is.list(MultiSiteModel)) ) {
    warning("The argument MultiSiteModel is expected to be NULL or a list.", call. = TRUE)
    return(ECMMonFailureSymbol)
  }

  ecmObj$MultiSiteModel <- MultiSiteModel

  ecmObj
}

##===========================================================
## Grid setter
##===========================================================

#' Set Grid.
#' @description Sets Grid into the monad object.
#' @param ecmObj An ECMMon object.
#' @param Grid An object member to be set.
#' @return An ECMMon object.
#' @family Set/Take functions
#' @export
ECMMonSetGrid <- function( ecmObj, Grid ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.null(Grid) || is.list(Grid)) ) {
    warning("The argument Grid is expected to be NULL or a list.", call. = TRUE)
    return(ECMMonFailureSymbol)
  }

  ecmObj$Grid <- Grid

  ecmObj
}

##===========================================================
## deSolveSolution setter
##===========================================================

#' Set deSolveSolution.
#' @description Sets deSolveSolution into the monad object.
#' @param ecmObj An ECMMon object.
#' @param deSolveSolution An object member to be set.
#' @return An ECMMon object.
#' @family Set/Take functions
#' @export
ECMMonSetDeSolveSolution <- function( ecmObj, deSolveSolution ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.null(deSolveSolution) || is.matrix(deSolveSolution)) ) {
    warning("The argument deSolveSolution is expected to be NULL or a matrix.", call. = TRUE)
    return(ECMMonFailureSymbol)
  }

  ecmObj$deSolveSolution <- deSolveSolution

  ecmObj
}

##===========================================================
## Solution setter
##===========================================================

#' Set Solution.
#' @description Sets Solution into the monad object.
#' @param ecmObj An ECMMon object.
#' @param Solution An object member to be set.
#' @return An ECMMon object.
#' @family Set/Take functions
#' @export
ECMMonSetSolution <- function( ecmObj, Solution ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.null(Solution) || is.data.frame(Solution)) ) {
    warning("The argument Solution is expected to be NULL or a data.frame.", call. = TRUE)
    return(ECMMonFailureSymbol)
  }

  ecmObj$Solution <- Solution

  ecmObj
}

##===========================================================
## SingleSiteModel Taker
##===========================================================

#' Take SingleSiteModel.
#' @description Takes SingleSiteModel from the monad object.
#' @param ecmObj An ECMMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{ECMMonFailureSymbol}.
#' @family Set/Take functions
#' @export
ECMMonTakeSingleSiteModel <- function( ecmObj, functionName = "ECMMonTakeSingleSiteModel" ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = functionName,  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  ecmObj$SingleSiteModel
}

##===========================================================
## MultiSiteModel Taker
##===========================================================

#' Take MultiSiteModel.
#' @description Takes MultiSiteModel from the monad object.
#' @param ecmObj An ECMMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{ECMMonFailureSymbol}.
#' @family Set/Take functions
#' @export
ECMMonTakeMultiSiteModel <- function( ecmObj, functionName = "ECMMonTakeMultiSiteModel" ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "MultiSiteModel", memberPrettyName = "MultiSiteModel", functionName = functionName,  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  ecmObj$MultiSiteModel
}

##===========================================================
## Grid Taker
##===========================================================

#' Take Grid.
#' @description Takes Grid from the monad object.
#' @param ecmObj An ECMMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{ECMMonFailureSymbol}.
#' @family Set/Take functions
#' @export
ECMMonTakeGrid <- function( ecmObj, functionName = "ECMMonTakeGrid" ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "Grid", memberPrettyName = "Grid", functionName = functionName,  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  ecmObj$Grid
}

##===========================================================
## deSolveSolution Taker
##===========================================================

#' Take deSolveSolution.
#' @description Takes deSolveSolution from the monad object.
#' @param ecmObj An ECMMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{ECMMonFailureSymbol}.
#' @family Set/Take functions
#' @export
ECMMonTakeDeSolveSolution <- function( ecmObj, functionName = "ECMMonTakeDeSolveSolution" ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "deSolveSolution", memberPrettyName = "deSolveSolution", functionName = functionName,  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  ecmObj$deSolveSolution
}

##===========================================================
## Solution Taker
##===========================================================

#' Take Solution.
#' @description Takes Solution from the monad object.
#' @param ecmObj An ECMMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{ECMMonFailureSymbol}.
#' @family Set/Take functions
#' @export
ECMMonTakeSolution <- function( ecmObj, functionName = "ECMMonTakeSolution" ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "Solution", memberPrettyName = "Solution", functionName = functionName,  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  ecmObj$Solution
}


##===========================================================
##***********************************************************
##===========================================================

##===========================================================
## GetDefaultModel
##===========================================================

#' Get default model object.
#' @description Gets the default model of the monad object.
#' @param ecmObj An ECMMon object.
#' @param functionName The name of the delegating function.
#' @return An ECMMon object.
#' @family Get functions
#' @export
ECMMonGetDefaultModel <- function( ecmObj, functionName = "ECMMonGetDefaultModel" ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( ECMMonMemberPresenceCheck( ecmObj, memberName = "MultiSiteModel", memberPrettyName = "MultiSiteModel", functionName = functionName,  logicalResult = TRUE, warningQ = FALSE) ) {

    ecmObj$Value <- ecmObj$MultiSiteModel

  } else if( ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = functionName,  logicalResult = TRUE, warningQ = FALSE) ) {

    ecmObj$Value <- ecmObj$SingleSiteModel

  } else {

    warning( "Cannot find a model. (Neither single-site nor multi-site.)", call. = TRUE )
    return(ECMMonFailureSymbol)

  }

  ecmObj
}


##===========================================================
## GetSolutionLongForm
##===========================================================

#' Get long form solution.
#' @description Gets the long form of the solution data frame \code{ecmObj$Solution}.
#' @param ecmObj An ECMMon object.
#' @param stockDescriptionsQ Should a column with the stock descriptions be added or not?
#' @param siteIdentifiersQ Should a column with site identifiers be added or not?
#' If \code{ecmObj} has only a single-site model, the that column will have all zeroes.
#' @return An ECMMon object.
#' @details The stocks types are derived from the stocks descriptions, \code{ecmObj$Stocks}.
#' The stocks types are "Population", "Money", "Other".
#' @family Get functions
#' @export
ECMMonGetSolutionLongForm <- function( ecmObj, stockDescriptionsQ = TRUE, siteIdentifiersQ = TRUE ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "Solution", memberPrettyName = "Solution", functionName = "GetSolutionLongForm",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  dfQuery <-
    ecmObj$Solution %>%
    tidyr::pivot_longer( cols = colnames(ecmObj$Solution)[-1], names_to = "Stock", values_to = "Value" )

  if( stockDescriptionsQ ) {

    model <- ecmObj %>% ECMMonGetDefaultModel %>% ECMMonTakeValue

    dfQuery <-
      dfQuery %>%
      dplyr::mutate( Description = model$Stocks[ Stock ] )

  }


  if( siteIdentifiersQ ) {

    if( ECMMonMemberPresenceCheck( ecmObj, memberName = "MultiSiteModel", memberPrettyName = "MultiSiteModel", functionName = "ECMMonGetSolutionLongForm",  logicalResult = TRUE, warningQ = FALSE ) ) {

      model <- ecmObj %>% ECMMonGetDefaultModel %>% ECMMonTakeValue

      dfQuery <-
        dfQuery %>%
        dplyr::mutate( SiteID = gsub( "(.*)_([[:digit:]]+)_t$", "\\2", names( model$Stocks[ Stock ] ) ) )

    } else {

      dfQuery <-
        dfQuery %>%
        dplyr::mutate( SiteID = "0" )

    }
  }

  ecmObj$Value <- dfQuery

  ecmObj
}


##===========================================================
## Echo model table form
##===========================================================

#' Echo model table form
#' @description If there is a multi-site models echoes (parts of) that model.
#' Otherwise echoes the single-site model.
#' @param ecmObj An ECMMon object.
#' @param part A string that which part of the model to echo.
#' If NULL all parts are echoed.
#' @param dataFrameResultQ Should the result be a data frame or not?
#' @return An ECMMon object
#' @details A list of data frames is assigned to \code{ecmObj$Value}.
#' @family Query functions
#' @export
ECMMonEchoModelTableForm <- function( ecmObj, part = NULL, echoQ = TRUE, dataFrameResultQ = FALSE ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  model <- ecmObj %>% ECMMonGetDefaultModel( functionName = "ECMMonEchoModelTableForm" ) %>% ECMMonTakeValue

  if( ECMMonFailureQ(model) ) { return(ECMMonFailureSymbol) }

  if( is.null(part) ) {
    part <- names(model)
  }

  if( is.character(part) && mean( names(model) %in% part ) ) {

    ## First pass
    res <-
      purrr::map( part, function(x) {

        if ( x %in% c( "Stocks", "Rates" ) ) {

          df <- data.frame( "Symbol" = names(model[[x]]), "Description" = model[[x]], stringsAsFactors = FALSE )

        } else if ( x %in% c( "InitialConditions" ) ) {

          df <- data.frame( "Symbol" = names(model[[x]]), "Value" = model[[x]], stringsAsFactors = FALSE)

        } else if ( x %in% c( "RateRules" ) && is.numeric( model[["RateRules"]] ) ) {

          df <- data.frame( "Symbol" = names(model[[x]]), "Value" = model[[x]], stringsAsFactors = FALSE)

        } else {

          df <- model[[x]]

        }

        if( echoQ && !dataFrameResultQ ) {
          print( df )
        }

        df
      })

    names(res) <- part

    ## Second pass if data.frame result is desired
    if( dataFrameResultQ ) {

      res <-
        purrr::map_df( names(res), function(nm) {

          if( is.data.frame(res[[nm]]) ) {

            cbind( ModelElement = nm, res[[nm]], stringsAsFactors = FALSE )

          } else {

            warning( paste0( "Not including the model element '", nm, "' in the result." ), call. = TRUE )

            NULL
          }

        })

      if( echoQ ) {
        print( res )
      }

    }

    ecmObj$Value <- res
  }

  ecmObj
}


##===========================================================
## AssignInitialConditions
##===========================================================

#' Assign initial conditions to model.
#' @description If there is a multi-site model assign initial conditions to it.
#' Otherwise the initial conditions are assigned to the single-site model.
#' @param ecmObj An ECMMon object.
#' @param initConds A numerical vector with named elements.
#' The names are expected to be known stocks in the model.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonAssignInitialConditions <- function( ecmObj, initConds ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.numeric(initConds) && is.character(names(initConds)) ) ) {
    warning( "The argument initConds is expected to be a numerical vector with named elements.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  model <- ecmObj %>% ECMMonGetDefaultModel( functionName = "ECMMonAssignInitialConditions" ) %>% ECMMonTakeValue

  if( ECMMonFailureQ(model) ) {return(ECMMonFailureSymbol) }

  if( mean( names(initConds) %in% names(model$InitialConditions) ) < 1 ) {
    warning( "Some of the names of initConds are not known stocks.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  model$InitialConditions[ names(initConds) ] <- initConds

  if( ECMMonMemberPresenceCheck( ecmObj, memberName = "MultiSiteModel", memberPrettyName = "MultiSiteModel", functionName = "ECMMonAssignInitialConditions",  logicalResult = TRUE, warningQ = FALSE) ) {
    ecmObj$MultiSiteModel <- model
  } else {
    ecmObj$SingleSiteModel <- model
  }

  ecmObj
}


##===========================================================
## AssignRateValues
##===========================================================

#' Assign rate rules to model.
#' @description If there is a multi-site model assign rate values to it.
#' Otherwise the rate values are assigned to the single-site model.
#' @param ecmObj An ECMMon object.
#' @param rateValues A numerical vector with named elements.
#' The names are expected to be known stocks in the model.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonAssignRateValues <- function( ecmObj, rateValues ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( ( is.numeric(rateValues) || is.list(rateValues) ) && is.character(names(rateValues)) ) ) {
    warning( "The argument rateValues is expected to be a numerical vector with named elements or a list of functions and numbers with named elements", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  model <- ecmObj %>% ECMMonGetDefaultModel( functionName = "ECMMonAssignRateValues" ) %>% ECMMonTakeValue

  if( ECMMonFailureQ(model) ) { return(ECMMonFailureSymbol) }

  frKnown = mean( names(rateValues) %in% names(model$RateRules) )

  if( 0 < frKnown && frKnown < 1 ) {

    warning( "Some of the names of rateValues are not known rates.", call. = TRUE )

  } else if( frKnown == 0 ) {

    warning( "None of the names of rateValues are known rates.", call. = TRUE )

  }

  model$RateRules[ names(rateValues) ] <- rateValues

  # Not needed.
  # if( is.numeric( rateValues ) ) {
  #   model$RateRules[ names(rateValues) ] <- rateValues
  # } else {
  #   model$RateRules[ names(rateValues) ] <- as.list( model$RateRules[ names(rateValues) ] )
  #   model$RateRules[ names(rateValues) ] <- rateValues
  # }

  if( ECMMonMemberPresenceCheck( ecmObj, memberName = "MultiSiteModel", memberPrettyName = "MultiSiteModel", functionName = "ECMMonAssignRateValues",  logicalResult = TRUE, warningQ = FALSE) ) {
    ecmObj$MultiSiteModel <- model
  } else {
    ecmObj$SingleSiteModel <- model
  }

  ecmObj
}


##===========================================================
## Simulate
##===========================================================

#' Extend by adjacency matrix
#' @description Extends monad's single site model into multi-site model using the numerical matrix mat.
#' If there is a multi-site models simulates with that model for the specified maximum time.
#' Otherwise the simulation is done with single-site model.
#' @param ecmObj An ECMMon object.
#' @param mat A matrix.
#' @param migratingStocks A a character vector with stocks (names.)
#' @param ... Additional arguments for \code{ToSiteCompartmentalModel}.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonExtendByAdjacencyMatrix <- function( ecmObj, mat, migratingStocks = NULL, ... ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = "ECMMonSimulate",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  if( !( is.matrix(mat) || SparseMatrixQ(mat) ) ) {
    # stop( "The argument data is expected to be a matrix, a sparse matrix, or a (matrix long form) data frame with columns c('i', 'j', 'x') .", call. = TRUE )
    stop( "The argument data is expected to be a matrix, a sparse matrix.", call. = TRUE )
  }

  modelMultiSite <- ToSiteCompartmentalModel( model = ecmObj$SingleSiteModel, mat = mat, migratingStocks = migratingStocks, ... )

  ecmObj <- ecmObj %>% ECMMonSetMultiSiteModel( modelMultiSite )

  ecmObj
}


##===========================================================
## Simulate
##===========================================================

#' Simulate.
#' @description If there is a multi-site models simulates with that model for the specified maximum time.
#' Otherwise the simulation is done with single-site model.
#' @param ecmObj An ECMMon object.
#' @param maxTime A numerical non-negative value for the maximum simulation time.
#' @param ... Additional parameters of \code{\link{deSolve::ode}}.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonSimulate <- function( ecmObj, maxTime, ... ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.numeric(maxTime) && maxTime >=0 ) ) {
    warning( "The argument maxTime is expected to be a non-negative number.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  model <- ecmObj %>% ECMMonGetDefaultModel( functionName = "ECMMonSimulate" ) %>% ECMMonTakeValue

  if( ECMMonFailureQ(model) ) { return(ECMMonFailureSymbol) }

  times <- seq( 0, maxTime, 1)

  sol <- deSolve::ode(y = model[["InitialConditions"]], times = times, func = model[["RHSFunction"]], parms = model[["RateRules"]], ... )

  dfSol <- as.data.frame(sol)
  colnames(dfSol) <- gsub( "time", "Time", colnames(dfSol) )

  ecmObj$deSolveSolution <- sol
  ecmObj$Solution <- dfSol

  ecmObj
}



##===========================================================
## BatchSimulate
##===========================================================

ListOfRateValueNumericVectorsQ <- function(model, obj) {

  is.list(obj) &&
    mean( names(obj) %in% names(model$Rates) ) == 1 &&
    mean( purrr::map_lgl( obj, function(x) is.numeric(x) ) ) == 1

}

ListOfParameterNumericVectorsQ <- function(model, obj) {

  is.list(obj) &&
    mean( ( names(obj) %in% names(model$Stocks) ) | ( names(obj) %in% names(model$Rates) ) ) == 1 &&
    mean( purrr::map_lgl( obj, function(x) is.numeric(x) ) ) == 1

}


#' Batch simulate.
#' @description Batch simulates over a data frame of parameters.
#' @param ecmObj An ECMMon object.
#' @param params A data frame of a list of numberic vectors.
#' If \code{params} is a list of numeric vectors then \code{params} is expected to have named elemets,
#' and \code{names(params)} to be known parameter names.
#' @param maxTime A numerical non-negative value for the maximum simulation time.
#' @param ... Additional parameters of \code{\link{deSolve::ode}}.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonBatchSimulate <- function( ecmObj, params, maxTime, ... ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  model <- ecmObj %>% ECMMonGetDefaultModel %>% ECMMonTakeValue

  if( !( is.data.frame(params) || ListOfRateValueNumericVectorsQ(model, params) ) ) {

    warning(
      paste0(
        "The argument params is expected to be a data frame or a list of numerical vectors with names that are model rates: '",
        paste0( names(model$Rates), collapse = "', '" ),
        "'."
      ),
      call. = TRUE )

    return(ECMMonFailureSymbol)
  }

  if( !( is.numeric(maxTime) && maxTime >=0 ) ) {
    warning( "The argument maxTime is expected to be a non-negative number.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  if( ListOfRateValueNumericVectorsQ(model, params) ){

    params <- do.call( expand.grid, params )

  }

  lsRes <-
    purrr::map( split(params, params, sep = "_"), function(par) {

      ecmObj %>%
        ECMMonAssignRateValues( rateValues = par ) %>%
        ECMMonSimulate( maxTime = maxTime ) %>%
        ECMMonGetSolutionLongForm() %>%
        ECMMonTakeValue

    } )

  ecmObj$Value <- lsRes

  ecmObj
}


##===========================================================
## PlotSolutions
##===========================================================

#' Plot solutions.
#' @description Plots solutions.
#' @param ecmObj An ECMMon object.
#' @param stocksSpec A character with stock names or string patterns for stock names to be plotted.
#' @param maxTime A numerical non-negative value for the maximum simulation time or NULL.
#' If NULL the maximum time is derived from \code{ecmObj$Solution}.
#' @param echoQ Should the plot be echoed or not?
#' @param stockDescriptionsQ Should the stock descriptions be added or not?
#' @param separatePlotsQ Should the stock plots be separated or not?
#' @param ... Additional parameters for \code{\link{ggplot2::facet_wrap}}
#' @return An ECMMon object.
#' @details The plot is assigned to \code{ecmObj$Value}.
#' @family Plot functions
#' @export
ECMMonPlotSolutions <- function( ecmObj, stocksSpec = NULL, maxTime = NULL, echoQ = TRUE, stockDescriptionsQ = TRUE, separatePlotsQ = FALSE, ... ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "Solution", memberPrettyName = "Solution", functionName = "ECMMonPlotSolutions",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  dfQuery <-
    ecmObj$Solution %>%
    tidyr::pivot_longer( cols = colnames(ecmObj$Solution)[-1], names_to = "Stock", values_to = "Value" )

  model <- ecmObj %>% ECMMonGetDefaultModel %>% ECMMonTakeValue

  if( is.character(stocksSpec) ) {

    dfQuery <-
      dfQuery %>%
      dplyr::filter( Stock %in% GetStocks( model, stocksSpec, applyToStockNamesQ = TRUE ) )
  }

  if( is.numeric(maxTime) ) {
    dfQuery <- dfQuery %>% dplyr::filter( Time <= maxTime )
  }

  if( stockDescriptionsQ ) {
    dfQuery <-
      dfQuery %>%
      dplyr::mutate( Stock = paste0( Stock, ", ",  model$Stocks[ Stock ]) )
  }

  if( separatePlotsQ ) {
    res <-
      ggplot2::ggplot(dfQuery) +
      ggplot2::geom_line( ggplot2::aes( x = Time, y = Value, color = Stock ) ) +
      ggplot2::facet_wrap( facets = ~Stock, ... )
  } else {
    res <-
      ggplot2::ggplot(dfQuery) +
      ggplot2::geom_line( ggplot2::aes( x = Time, y = Value, color = Stock ) )
  }

  if( echoQ ) {
    print(res)
  }

  ecmObj$Value <- res

  ecmObj
}


##===========================================================
## ECMMonPlotSolutionHistograms
##===========================================================

#' Plot solution histograms.
#' @description Plots solution histograms.
#' @param ecmObj An ECMMon object.
#' @param stocksSpec A character with stock names or string patterns for stock names to be plotted.
#' @param maxTime A numerical non-negative value for the maximum simulation time or NULL.
#' If NULL the maximum time is derived from \code{ecmObj$Solution}.
#' @param echoQ Should the plot be echoed or not?
#' @param stockDescriptionsQ Should the stock descriptions be added or not?
#' @param scales Scales argument \code{scales} for \code{\link{ggplot2::facet_wrap}}.
#' @param ncol Number of columns \code{ncol} argument for \code{\link{ggplot2::facet_wrap}}.
#' @param ... Additional parameters for \code{\link{ggplot2::geom_histogram}}.
#' @return An ECMMon object.
#' @details The plot is assigned to \code{ecmObj$Value}.
#' @family Plot functions
#' @export
ECMMonPlotSolutionHistograms <- function( ecmObj, stocksSpec = NULL, maxTime = NULL, echoQ = TRUE, stockDescriptionsQ = TRUE, scales = "free", ncol = 2, ... ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "Solution", memberPrettyName = "Solution", functionName = "ECMMonPlotSolutions",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  dfQuery <-
    ecmObj$Solution %>%
    tidyr::pivot_longer( cols = colnames(ecmObj$Solution)[-1], names_to = "Stock", values_to = "Value" )

  model <- ecmObj %>% ECMMonGetDefaultModel %>% ECMMonTakeValue

  if( is.character(stocksSpec) ) {

    dfQuery <-
      dfQuery %>%
      dplyr::filter( Stock %in% GetStocks( model, stocksSpec, applyToStockNamesQ = TRUE ) )
  }

  if( is.numeric(maxTime) ) {
    dfQuery <- dfQuery %>% dplyr::filter( Time <= maxTime )
  }

  if( stockDescriptionsQ ) {
    dfQuery <-
      dfQuery %>%
      dplyr::mutate( Stock = paste0( Stock, ", ",  model$Stocks[ Stock ]) )
  }

  res <-
    dfQuery %>%
    ggplot2::ggplot( ) +
    ggplot2::geom_histogram( ggplot2::aes( x = Value), ... ) +
    ggplot2::facet_wrap( facets = ~Stock, scales = scales, ncol = ncol )

  if( echoQ ) {
    print(res)
  }

  ecmObj$Value <- res

  ecmObj
}


##===========================================================
## ECMMonExport
##===========================================================

#' Export model and solution into CSV files.
#' @description Exports the model elements and solution data frame into CSV files
#' with a specified prefix and in a specified directory.
#' @param ecmObj An ECMMon object.
#' @param directoryName A directory name for the export. If \code{NULL} no files are written.
#' @param modelID A string.
#' @param fileNamePrefix A string.
#' @return An ECMMon object or \code{ECMMonFailureSymbol}.
#' @details The CSV files are written in the specified directory \code{directoryName}.
#' The file name prefix \code{fileNamePrefix} is concatenated to the generic file names:
#' \code{"longFormComputationSpecification.csv", "featureMatrix.csv", "timeCellsInterpretation.csv", "featureMatrixTimeSeries.csv"}.
#' The conversion into long form of the computation specification is considered to be
#' more convenient from a "model management" perspective.
#' The data to be exported is assigned to result's \code{$Value}.
#' @export
ECMMonExport <- function( ecmObj, directoryName, modelID, fileNamePrefix = paste0(modelID,"-") ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !( is.character(directoryName) && file.exists(directoryName) || is.null(directoryName) ) ) {
    warning( "The argument directoryName is expected to be a string that is a valid directory name or NULL.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  if( !is.character(modelID) ) {
    warning( "The argument modelID is expected to be a string.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  if( !(is.null(fileNamePrefix) || is.character(fileNamePrefix) ) ) {
    warning( "The argument fileNamePrefix is expected to be a string or NULL.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  if( is.null(fileNamePrefix) ) { fileNamePrefix <- "" }

  ## Export model
  ecmObj <- ecmObj %>% ECMMonEchoModelTableForm( dataFrameResultQ = TRUE, echoQ = FALSE )

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  dfModel <- ecmObj %>% ECMMonTakeValue

  if( !is.data.frame(dfModel) ) { return(ECMMonFailureSymbol) }

  if( !is.null(directoryName) ) {
    write.csv( x = dfModel, file = file.path( directoryName, paste0(fileNamePrefix, "model.csv")), row.names = FALSE )
  }

  ## Export solution
  if( !is.null(directoryName) && is.data.frame(ecmObj$Solution) ) {
    write.csv( x = ecmObj$Solution, file = file.path( directoryName, paste0(fileNamePrefix, "solution.csv")), row.names = FALSE )
  }

  ecmObj$Value <- list( Model = dfModel, Solution = ecmObj$Solution )

  ## Result
  ecmObj
}


##===========================================================
## Support functions
##===========================================================

#' Verify does a directory have ECMMon data files.
#' @description Verify does a directory have the CSV files \code{model.csv}
#' and \code{solution.csv}.
#' @param directoryName A directory name string.
#' @return A logical value.
#' @family Non-monadic functions.
#' @export
ECMMonVerifyDataDirectory <- function( directoryName ) {
  if( !is.character(directoryName) ) {
    stop("A string is expected for the argument directoryName.", call. = TRUE )
  }
  VerifyDataDirectory( directoryName )
}
