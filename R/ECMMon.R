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


#' @import magrittr
#' @import purrr
#' @import ggplot2
#' @import deSolve
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
#' @param logicalResultQ Should the result be a logical value?
#' @return A logical value or an ECMMon object.
#' @export
ECMMonMemberPresenceCheck <- function( ecmObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(ecmObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
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
ECMMonSetdeSolveSolution <- function( ecmObj, deSolveSolution ) {

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
ECMMonTakedeSolveSolution <- function( ecmObj, functionName = "ECMMonTakedeSolveSolution" ) {

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
#' @description Get the default model of the monad object.
#' @param ecmObj An ECMMon object.
#' @return An ECMMon object.
#' @family Get functions
#' @export
ECMMonGetDefaultModel <- function( ecmObj ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !is.null(ecmObj$MultiSiteModel) ) {

    ecmObj$Value <- ecmObj$MultiSiteModel

  } else if( !is.null(ecmObj$SingleSiteModel) ) {

    ecmObj$Value <- ecmObj$SingleSiteModel

  } else {

    warning( "Cannot find a model.", call. = TRUE )
    return(ECMMonFailureSymbol)

  }

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
#' @return An ECMMon object
#' @details A list of data frames is assigned to \code{ecmObj$Value}.
#' @family Query functions
#' @export
ECMMonEchoModelTableForm <- function( ecmObj, part = NULL, echoQ = TRUE ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = "ECMMonEchoModelTableForm",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  model <- ecmObj %>% ECMMonTakeSingleSiteModel

  if( is.null(part) ) {
    part <- names(model)
  }

  if( is.character(part) && mean( names(model) %in% part ) ) {

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

        if( echoQ ) {
          print( df )
        }

      })

    names(res) <- part
  }

  ecmObj$Value <- res

  ecmObj
}


##===========================================================
## AssignInitialConditions
##===========================================================

#' Assign initial conditions to model.
#' @description If there is a multi-site model assign intial conditions to it.
#' Otherwise the initial conditions are assigned to the single-site model.
#' @param ecmObj An ECMMon object.
#' @param initConds A numerical vector with named elements.
#' The names are expected to be known stocks in the model.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonAssignInitialConditions <- function( ecmObj, initConds ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = "ECMMonSimulate",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  if( !( is.numeric(initConds) && is.character(names(initConds)) ) ) {
    warning( "The argument initConds is expected to be a numerical vector with named elements.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  ## This should be changed when MultiSiteModel is introduced.
  model <- ecmObj$SingleSiteModel

  if( mean( names(initConds) %in% names(model$InitialConditions) ) < 1 ) {
    warning( "Some of the names of initConds are not known stocks in ecmObj$SingleSiteModel.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  model$InitialConditions[ names(initConds) ] <- initConds

  ecmObj$SingleSiteModel <- model

  ecmObj
}


##===========================================================
## AssignRateValues
##===========================================================

#' Assign rate rules to model.
#' @description If there is a multi-site model assign rate values to it.
#' Otherwise the rate values are assigned to the single-site model.
#' @param ecmObj An ECMMon object.
#' @param initConds A numerical vector with named elements.
#' The names are expected to be known stocks in the model.
#' @return An ECMMon object.
#' @family Simulation functions
#' @export
ECMMonAssignRateValues <- function( ecmObj, rateValues ) {

  if( ECMMonFailureQ(ecmObj) ) { return(ECMMonFailureSymbol) }

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = "ECMMonSimulate",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  if( !( ( is.numeric(rateValues) || is.list(rateValues) ) && is.character(names(rateValues)) ) ) {
    warning( "The argument rateValues is expected to be a numerical vector with named elements or a list of functions and numbers with named elements", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  ## This should be changed when MultiSiteModel is introduced.
  model <- ecmObj$SingleSiteModel

  frKnown = mean( names(rateValues) %in% names(model$RateRules) )

  if( 0 < frKnown && frKnown < 1 ) {

    warning( "Some of the names of rateValues are not known rates in ecmObj$SingleSiteModel.", call. = TRUE )

  } else if( frKnown == 0 ) {

    warning( "None of the names of rateValues are not known rates in ecmObj$SingleSiteModel.", call. = TRUE )

  }

  model$RateRules[ names(rateValues) ] <- rateValues

  # Not needed.
  # if( is.numeric( rateValues ) ) {
  #   model$RateRules[ names(rateValues) ] <- rateValues
  # } else {
  #   model$RateRules[ names(rateValues) ] <- as.list( model$RateRules[ names(rateValues) ] )
  #   model$RateRules[ names(rateValues) ] <- rateValues
  # }

  ecmObj$SingleSiteModel <- model

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

  if( !ECMMonMemberPresenceCheck( ecmObj, memberName = "SingleSiteModel", memberPrettyName = "SingleSiteModel", functionName = "ECMMonSimulate",  logicalResult = TRUE) ) {
    return(ECMMonFailureSymbol)
  }

  if( !( is.numeric(maxTime) && maxTime >=0 ) ) {
    warning( "The argument maxTime is expected to be a non-negative number.", call. = TRUE )
    return(ECMMonFailureSymbol)
  }

  times <- seq( 0, maxTime, 1)

  model <- ecmObj$SingleSiteModel

  sol <- deSolve::ode(y = model[["InitialConditions"]], times = times, func = model[["RHSFunction"]], parms = model[["RateRules"]], ... )

  dfSol <- as.data.frame(sol)
  colnames(dfSol) <- gsub( "time", "Time", colnames(dfSol) )

  ecmObj$deSolveSolution <- sol
  ecmObj$Solution <- dfSol

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
#' @return An ECMMon object.
#' @details The plot is assigned to \code{ecmObj$Value}.
#' @family Plot functions
#' @export
ECMMonPlotSolutions <- function( ecmObj, stocksSpec = NULL, maxTime = NULL, echoQ = TRUE ) {

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
      dplyr::filter( Stock %in% GetStocks( model, stocksSpec ) )
  }

  if( is.numeric(maxTime) ) {
    dfQuery <- dfQuery %>% dplyr::filter( Time <= maxTime )
  }

  dfQuery <-
    dfQuery %>%
    dplyr::mutate( Stock = paste0( Stock, ", ",  model$Stocks[ Stock ]) )

  res <-
    ggplot2::ggplot(dfQuery) +
    ggplot2::geom_line( ggplot2::aes( x = Time, y = Value, color = Stock ) )

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
#' @param scales Scales argument \code{scales} for \code{\link{ggplot2::facet_wrap}}.
#' @param ncol Number of columns \code{ncol} argument for \code{\link{ggplot2::facet_wrap}}.
#' @param ... Additional parameters for \code{\link{ggplot2::geom_histogram}}.
#' @return An ECMMon object.
#' @details The plot is assigned to \code{ecmObj$Value}.
#' @family Plot functions
#' @export
ECMMonPlotSolutionHistograms <- function( ecmObj, stocksSpec = NULL, maxTime = NULL, echoQ = TRUE, scales = "free", ncol = 2, ... ) {

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
      dplyr::filter( Stock %in% GetStocks( model, stocksSpec ) )
  }

  if( is.numeric(maxTime) ) {
    dfQuery <- dfQuery %>% dplyr::filter( Time <= maxTime )
  }

  dfQuery <-
    dfQuery %>%
    dplyr::mutate( Stock = paste0( Stock, ", ",  model$Stocks[ Stock ]) )

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

