##=======================================================================================
## General ECMMon Models Interface
## Copyright (C) 2020 Anton Antonov
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
## antononcube @ gmail . com,
## Windermere, Florida, USA.
##
##=======================================================================================
##
## This Shiny interface is made to be used with the models (S3 objects)
## in the file EpidemiologyModels.R .
##
##=======================================================================================




#' @import DT
#' @import dplyr
#' @import Matrix
#' @import purrr
#' @import stringr
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
NULL



##===========================================================
## UI function
##===========================================================

#' ECMMon dashboard UI
#' @description Creates the Shiny UI function for an ECMMon interface.
#' @param ecmObj A ECMMon object.
#' @param dashboardTheme A string specifying the dashboard theme.
#' See \code{\link{dashboardthemes::shinyDashboardThemes}}.
#' Ignored if the package \code{dashboardthemes} is not installed.
#' @return Shiny UI object.
#' @family ECMMon interface functions
#' @export
ECMMonDashboardUI <- function( ecmObj, dashboardTheme = NULL ) {

  if( !( is.null(dashboardTheme) ||
         is.character(dashboardTheme) && length(dashboardTheme) == 1 ) ) {
    warning( "Ignoring the argument dashboardTheme.", call. = TRUE )
    dashboardTheme <- NULL
  }

  shinyUI(
    dashboardPage(
      dashboardHeader( title = "ECMMon Dashboard" ),

      dashboardSidebar(
        sidebarMenu(
          menuItem( "ECMObject", tabName = "ECMMonObject", icon = icon("map") ),
          menuItem( "Calibration", tabName = "Calibration", icon = icon("object-group") ),
          hr(),
          menuItem( "Sliders", tabName = "Sliders", icon = icon("sliders-h"),
                    uiOutput( "multiSliders" )
          )
        )
      ),

      dashboardBody(

        ### Changing dashboard theme
        if( ! is.null(dashboardTheme) && exists("shinyDashboardThemes") ) {
          dashboardthemes::shinyDashboardThemes( theme = dashboardTheme )
        },

        tabItems(

          ## ECMMon object oboarding
          tabItem( tabName = "ECMMonObject",
                   fluidRow(

                     h2( "ECMMon object and interpretation data summaries" ),

                     box(
                       title = "Stocks",

                       div( DT::dataTableOutput("stocksTable"), style = "font-size: 75%; width: 75%"),

                       width = 6
                     ),

                     box(
                       title = "Rates",

                       div( DT::dataTableOutput("ratesTable"), style = "font-size: 75%; width: 75%"),

                       width = 6
                     )

                   )
          ),



          ## Facets
          tabItem( tabName = "Calibration",
                   fluidRow(

                     h2( "Calibration and experiments" ),

                     box(
                       title = "Simulation parameters",

                       sliderInput( inputId = "maxtime", label = "Max time:", value = 120 ),

                       width = 3
                     ),

                     uiOutput( "collageBoxes" )

                   )
          )



        )
      )
    )
  )
}


##===========================================================
## Server function
##===========================================================

#' ECMMon dashboard server function
#' @description Creates the Shiny server function for a sparse matrix recommender interface.
#' @param ecmObj A time series recommender.
#' @return Shiny server function.
#' @family ECMMon interface functions
#' @export
ECMMonDashboardServerFunction <- function( ecmObj ) {

  if( is.null(itemDataColNames) ) {
    itemDataColNames <- colnames(itemData)
  }

  if( mean( itemDataColNames %in% colnames(itemData) ) < 1 ) {
    stop( "Not all elements of the argument itemDataColNames are column names of itemData.")
  }

  if( is.null(itemDataIDColName) ) {
    itemDataIDColName <- colnames(itemData)[[1]]
  }

  if( !( itemDataIDColName %in% colnames(itemData) ) ) {
    stop( "The argument itemDataIDColName is not a column name in itemData.")
  }

  if( is.null("itemListIDsSplitPattern") ) {
    itemListIDsSplitPattern <- "\\W"
  }

  shinyServer( function(input, output, session) {

    ##------------------------------------------------------------
    ## ECMMon object onboarding
    ##------------------------------------------------------------

    # smrDataLongForm <- reactive(
    #   ecmObj %>% ECMMonGetLongFormData( tagTypesQ = TRUE ) %>% ECMMonTakeValue
    # )

    output$smrSummary <- renderPrint({
      smat <- ecmObj %>% ECMMonTakeM; smat@x[ smat@x > 0 ] <- 1;
      smat <- (ecmObj %>% ECMMonTakeM)[ , order( -colSums(smat) )[1:20] ]
      summary( setNames( as.data.frame( unclass( SMRSparseMatrixToTriplets( smat ) ) )[, 2:3], c( "Tag", "Weight" ) ) )
    })

    output$tagTypeRangesTable <-
      DT::renderDataTable({ datatable({
        ecmObj %>% ECMMonGetProperty( "TagTypeRanges" ) %>% ECMMonTakeValue
      }, rownames = TRUE, filter = 'none', options = list(pageLength = 12, autoWidth = TRUE) ) })

    output$itemDataSummary <- renderPrint({
      ##summary( as.data.frame( unclass( itemData ) ) )
      dplyr::glimpse( itemData )
    })


    ##------------------------------------------------------------
    ## Search/recommendations by profile
    ##------------------------------------------------------------

    output$multiSliders <- renderUI({

      tagTypes <- ecmObj %>% ECMMonTakeTagTypes

      purrr::map( tagTypes, function(tt) {
        sliderInput( inputId = paste0('slider.', tt ),  label = tt,  min = 0, max = 10, value = 1 )
      })

    })

    slidersValues <- reactive({

      sliderNames <- paste0( "slider.", ecmObj %>% ECMMonTakeTagTypes )
      purrr::map_dbl( setNames( sliderNames, ecmObj %>% ECMMonTakeTagTypes ), function(x) input[[x]] )

    })

    searchProfile <- reactive({

      sTags <- strsplit( x = input$searchProfileString, split = itemListIDsSplitPattern )[[1]]

      if( input$searchPatternFixed ) {
        ## Fixed search pattern.
        predInds <- sTags %in% colnames( ecmObj %>% ECMMonTakeM )

        if( length(predInds) > 0 ) {
          sTags <- sTags[ predInds ]
        } else {
          sTags <- c()
        }

      } else {
        ## General grep pattern.
        sTags <- Reduce( function(a,x) c( a, grep( x, colnames( ecmObj %>% ECMMonTakeM ), value = T, ignore.case = T )), init = c(), x = sTags )

      }

      sTags

    })


    recommendations <- reactive({

      localSearchProfile <- searchProfile()

      if( length(localSearchProfile) == 0 ) {

        NULL

      } else if( sum(grepl( "slider", names(input) )) >= length(ecmObj %>% ECMMonTakeTagTypes) ) {

        ecmObj %>%
          ECMMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
          ECMMonRecommendByProfile( profile = localSearchProfile, nrecs = input$nrecs ) %>%
          ECMMonTakeValue

      } else {

        ecmObj %>%
          ECMMonRecommendByProfile( profile = localSearchProfile, nrecs = input$nrecs ) %>%
          ECMMonTakeValue
      }

    })

    ## OUTPUT: recommendations table
    output$recommendationsTable <-
      DT::renderDataTable({ datatable({
        if( is.null(recommendations()) ) {
          NULL
        } else {
          recommendations() %>%
            dplyr::inner_join( itemData, by = ecmObj %>% ECMMonTakeItemColumnName )
        }
      }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE), selection = list( mode = 'multiple', selected = c(1)) ) })


    ## OUTPUT: history profile table
    output$recommendationsSelectionProfileTable <-
      output$uproof <- DT::renderDataTable({ datatable({

        sRow <- input$recommendationsTable_rows_selected

        if( is.null(recommendations()) || is.null(sRow) ) {

          NULL

        } else {

          itemID <- recommendations()[sRow, ecmObj %>% ECMMonTakeItemColumnName ]

          if( sum(grepl( "slider", names(input) )) >= length(ecmObj %>% ECMMonTakeTagTypes) ) {

            ecmObj %>%
              ECMMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
              ECMMonProfile( history = itemID ) %>%
              ECMMonTakeValue

          } else {

            ecmObj %>%
              ECMMonProfile( history = itemID ) %>%
              ECMMonTakeValue

          }

        }

      }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })


    ##------------------------------------------------------------
    ## Facets
    ##------------------------------------------------------------

    collageItemSummary <- reactive({

      ecmObj %>%
        ECMMonSummarizeItem( item = input$collageItem, tagTypesQ = TRUE, nTopTags = 12 ) %>%
        ECMMonTakeValue

    })

    collageItemNearestNeigbors <- reactive({

      if( sum(grepl( "slider", names(input) )) >= length(ecmObj %>% ECMMonTakeTagTypes) ) {

        ecmObj %>%
          ECMMonApplyTagTypeWeights( weights = slidersValues(), default = 0 ) %>%
          ECMMonRecommend( history = input$collageItem, nrecs = 12 ) %>%
          ECMMonJoinAcross( data = itemData ) %>%
          ECMMonTakeValue

      } else {

        ecmObj %>%
          ECMMonRecommend( history = input$collageItem, nrecs = 12 ) %>%
          ECMMonJoinAcross( data = itemData ) %>%
          ECMMonTakeValue

      }

    })


    output$collageBoxes <- renderUI({

      if( !is.list( collageItemSummary() ) ) {

        NULL

      } else {

        purrr::map( input$selectedFacets, function(sf) {

          if( sf == "profile" ) {

            box( title = "Profile",

                 ## renderTable( collageItemSummary()$Profile ),
                 DT::renderDataTable({ datatable({

                   collageItemSummary()$Profile

                 }, rownames = FALSE, filter = 'none', options = list(pageLength = 8, autoWidth = TRUE) ) }),

                 width = 4 )

          } else if ( sf == "tagsSummaryTable" ) {

            box( title = "Tags summary",

                 ##renderTable( collageItemSummary()$TagsSummary ),
                 DT::renderDataTable({ datatable({

                   collageItemSummary()$TagsSummary

                 }, rownames = FALSE, filter = 'none', options = list(pageLength = 8, autoWidth = TRUE) ) }),

                 width = 4 )

          } else if ( sf == "tagsSummaryPieChart" ) {

            box( title = "Tags summary pie chart",

                 ## renderPlot( pie( collageItemSummary()$TagsSummary$NumberOfTags ) ),

                 renderPlot(

                   ggplot( collageItemSummary()$TagsSummary, aes( x = "", y = NumberOfTags, fill = TagType) ) +
                     geom_bar(width = 1, stat = "identity") +
                     coord_polar( "y", start = 0 )

                 ),

                 width = 4 )

          } else if ( sf == "nearestNeighbors" ) {

            box( title  = "Nearest neighbors",

                 ##renderTable( collageItemNearestNeigbors() ),
                 DT::renderDataTable({ datatable({

                   collageItemNearestNeigbors()

                 }, rownames = FALSE, filter = 'none', options = list(pageLength = 8, autoWidth = TRUE) ) }),

                 width = 12 )

          } else {

            NULL
          }
        })
      }

    })

  })

}


##===========================================================
## Make shiny app
##===========================================================

#' Creation of an ECMMon dashboard.
#' @param ecmObj An ECMMon object.
#' @param dashboardTheme A string specifying the dashbaard theme.
#' See \code{\link{dashboardthemes::shinyDashboardThemes}}.
#' Ignored if the package \code{dashboardthemes} is not installed.
#' @return Shiny app
#' @family ECMMon interface functions
#' @export
ECMMonCreateDasboard <- function( ecmObj, dashboardTheme = NULL ) {

  shiny::shinyApp( ui = ECMMonDashboardUI( ecmObj = ecmObj, dashboardTheme = dashboardTheme ),
                   server = ECMMonDashboardServerFunction( ecmObj = ecmObj )
  )

}



