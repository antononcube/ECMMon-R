##===========================================================
## Epidemiology Models in R
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


##===========================================================
## EpidemiologyModelQ
##===========================================================

#' Epidemiology model object check.
#' @description Checks is an object an epidemiology model object.
#' @param x An object.
#' @return A logical.
#' @family Epidemiology Modeling functions
#' @export
EpidemiologyModelQ <- function(x) {
  is.list(x) && mean( c( "Stocks", "Rates", "RHSFunction" ) %in% names(x) ) == 1
}


##===========================================================
## EpidemiologyFullModelQ
##===========================================================

#' Epidemiology full model object check.
#' @description Checks is an object an epidemiology model object.
#' @param x An object
#' @return A logical.
#' @family Epidemiology Modeling functions
#' @export
EpidemiologyFullModelQ <- function(x) {
  is.list(x) && mean( c( "Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules" ) %in% names(x) ) == 1
}


##===========================================================
## EpidemiologyFullModelProperlyDefinedQ
##===========================================================

#' Properly defined epidemiology full model object check.
#' @description Checks is an object a full epidemiology model object and
#' are its rates and stocks are properly defined.
#' @param model An object
#' @return A logical.
#' @family Epidemiology Modeling functions
#' @export
EpidemiologyFullModelProperlyDefinedQ<- function( model ) {

  if( ! EpidemiologyModelQ(model) ) {
    return(FALSE)
  }

  if( ! EpidemiologyFullModelQ(model) ) {
    return(TRUE)
  }

  mean( names(model$InitialConditions) %in% names(model$Stocks) ) == 1 &&
    mean( names(model$RateRules) %in% names(model$Rates) ) == 1
}


##===========================================================
## SIRModel
##===========================================================

#' SIR model
#' @description SIRModel
#' @param initialConditionsQ Should initial conditions be included?
#' @param rateRulesQ Should rate rules be included?
#' @return A list.
#' @family Epidemiology Modeling creation functions
#' @export
SIRModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "IPt" = "Infected Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity"
        ),

      Rates =
        c(
          "TP0" = "Initial value of Total Population",
          "deathRateTP" = "Population death rate",
          "deathRateIP" = "Infected Population death rate",
          "contactRateIP" = "Contact rate for the infected population",
          "aip" = "Average infectious period",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ) ,
               {

                 dSPt <- - contactRateIP * SPt * IPt / TP0 - deathRateTP * SPt

                 dIPt <-   contactRateIP * SPt * IPt / TP0 - IPt / aip - deathRateIP * IPt

                 dRPt <-   IPt / aip - deathRateTP * RPt

                 return( list( c( dSPt, dIPt, dRPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c( SPt = 10^5 - 100, IPt = 100, RPt = 0 )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c( TP0 = 10^5, deathRateTP = 0.0000219178, deathRateIP = 0.035 / 28, aip = 28, contactRateIP = 0.56)))
  }

  res
}


##===========================================================
## SI2RModel
##===========================================================

#' SI2R model
#' @description SI2RModel
#' @param initialConditionsQ Should initial conditions be included?
#' @param rateRulesQ Should rate rules be included?
#' @return A list.
#' @family Epidemiology Modeling creation functions
#' @export
SI2RModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "INSPt" = "Infected Normally Symptomatic Population",
           "ISSPt" = "Infected Severely Symptomatic Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity"
        ),

      Rates =
        c(
          "TP0" = "Initial value of Total Population",
          "deathRateTP" = "Population death rate",
          "deathRateINSP" = "Infected Normally Symptomatic Population death rate",
          "deathRateISSP" = "Infected Severely Symptomatic Population death rate",
          "contactRateINSP" = "Contact rate for the infected normally syptomatic population",
          "contactRateISSP" = "Contact rate for the infected severely syptomatic population",
          "sspf" = "Severely symptomatic population fraction",
          "aip" = "Average infectious period",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ),
               {

                 newlyInfectedTerm <- contactRateINSP * SPt * INSPt / TP0 + contactRateISSP * SPt * ISSPt / TP0

                 dSPt <- - newlyInfectedTerm - deathRateTP * SPt

                 dINSPt <- (1 - sspf) * newlyInfectedTerm - INSPt / aip - deathRateINSP * INSPt

                 dISSPt <- sspf * newlyInfectedTerm - ISSPt / aip - deathRateISSP * ISSPt

                 dRPt <- (INSPt + ISSPt) / aip - deathRateTP * RPt

                 return( list( c( dSPt, dINSPt, dISSPt, dRPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c(
               SPt = 10^5 - 1,
               INSPt = 0,
               ISSPt = 1,
               RPt = 0
             )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c(
               TP0 = 10^5,
               deathRateTP = 0.0000219178,
               deathRateINSP = 0.015 / 28,
               deathRateISSP = 0.035 / 28,
               aip = 28,
               contactRateINSP = 0.56,
               contactRateISSP = 0.56,
               sspf = 0.2
             )))
  }

  res
}


##===========================================================
## SEI2RModel
##===========================================================

#' SEI2RModel model
#' @description SEI2RModel
#' @param initialConditionsQ Should initial conditions be included?
#' @param rateRulesQ Should rate rules be included?
#' @param birthsTermQ Should the births term be included or not?
#' @return A list.
#' @family Epidemiology Model creation functions
#' @export
SEI2RModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE, birthsTermQ = FALSE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "EPt" = "Exposed Population",
           "INSPt" = "Infected Normally Symptomatic Population",
           "ISSPt" = "Infected Severely Symptomatic Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity"
        ),

      Rates =
        c(
          "TP0" = "Initial value of Total Population",
          "deathRateTP" = "Population death rate",
          "deathRateINSP" = "Infected Normally Symptomatic Population death rate",
          "deathRateISSP" = "Infected Severely Symptomatic Population death rate",
          "contactRateINSP" = "Contact rate for the infected normally syptomatic population",
          "contactRateISSP" = "Contact rate for the infected severely syptomatic population",
          "sspf" = "Severely symptomatic population fraction",
          "aip" = "Average infectious period",
          "aincp" = "Average incubation period",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ),
               {

                 newlyInfectedTerm <- contactRateISSP / TP0 * SPt * ISSPt + contactRateINSP / TP0 * SPt * INSPt;

                 if( birthsTermQ ) {
                   dSPt <-  deathRateTP * TP0 - newlyInfectedTerm - deathRateTP * SPt
                 } else {
                   dSPt <-  - newlyInfectedTerm - deathRateTP * SPt
                 }

                 dEPt <- newlyInfectedTerm - (deathRateTP + (1 / aincp) ) * EPt

                 dINSPt <- (1 - sspf) * (1 / aincp) * EPt - (1 / aip) * INSPt - deathRateINSP * INSPt

                 dISSPt <- sspf * (1 / aincp) * EPt - (1 / aip) * ISSPt - deathRateISSP * ISSPt

                 dRPt <- (1 / aip) * (ISSPt + INSPt) - deathRateTP * RPt

                 dMLPt <- lpcr * ( INSPt + ISSPt + deathRateINSP * INSPt + deathRateISSP * ISSPt )

                 return( list( c( dSPt, dEPt, dINSPt, dISSPt, dRPt, dMLPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c(
               SPt = 10^5 - 1,
               EPt = 0,
               INSPt = 0,
               ISSPt = 1,
               RPt = 0,
               MLPt = 0
             )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c(
               TP0 = 10^5,
               deathRateTP = 0.0000219178,
               deathRateINSP = 0.015 / 26,
               deathRateISSP = 0.035 / 26,
               aip = 26,
               aincp = 6,
               contactRateINSP = 0.56,
               contactRateISSP = 0.56,
               sspf = 0.2,
               lpcr = 1
             )))
  }

  res
}



##===========================================================
## SEI2HRModel
##===========================================================

#' SEI2HRModel model
#' @description SEI2HRModel
#' @param initialConditionsQ Should initial conditions be included?
#' @param rateRulesQ Should rate rules be included?
#' @param birthsTermQ Should the births term be included or not?
#' @param A list
#' @export
SEI2HRModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE, birthsTermQ = FALSE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "EPt" = "Exposed Population",
           "INSPt" = "Infected Normally Symptomatic Population",
           "ISSPt" = "Infected Severely Symptomatic Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity",
           "HPt" = "Hospitalized Population",
           "DIPt" = "Deceased Infected Population",
           "HBt" = "Hospital Beds",
           "MHSt" = "Money for Hospital Services"
        ),

      Rates =
        c(
          "TP0" = "Initial value of Total Population",
          "deathRateTP" = "Population death rate",
          "deathRateINSP" = "Infected Normally Symptomatic Population death rate",
          "deathRateISSP" = "Infected Severely Symptomatic Population death rate",
          "deathRateHP" = "Hospitalized Population death rate",
          "contactRateINSP" = "Contact rate for the infected normally syptomatic population",
          "contactRateISSP" = "Contact rate for the infected severely syptomatic population",
          "contactRateHP" = "Contact rate for the hospitalized population",
          "sspf" = "Severely symptomatic population fraction",
          "aip" = "Average infectious period",
          "aincp" = "Average incubation period",
          "nhbrTP" = "Number of hospital beds rate",
          "hscr" = "Hospital services cost rate (per bed per day)",
          "nhbcr" = "Number of hospital beds change rate (per day)",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ) ,
               {

                 ##----------------------------------------

                 if( is.function(contactRateINSP) ) {
                   nContactRateINSP <- contactRateINSP ( time )
                 } else {
                   nContactRateINSP <- contactRateINSP
                 }

                 if( is.function(contactRateISSP) ) {
                   nContactRateISSP <- contactRateISSP ( time  )
                 } else {
                   nContactRateISSP <- contactRateISSP
                 }

                 ##----------------------------------------

                 newBySeverelyInfectedTerm <- nContactRateISSP / TP0 * SPt * max( ISSPt - HPt, 0) + contactRateHP / TP0 * SPt * HPt;
                 newByNormallyInfectedTerm <- nContactRateINSP / TP0 * SPt * INSPt;
                 newlyInfectedTerm <-  newBySeverelyInfectedTerm + newByNormallyInfectedTerm;

                 peopleDyingPerDay <- deathRateISSP * ( ISSPt - HPt ) + deathRateINSP * INSPt + deathRateHP * HPt;


                 if( birthsTermQ ) {
                   dSPt <-  deathRateTP * TP0 - newlyInfectedTerm - deathRateTP * SPt
                 } else {
                   dSPt <-  - newlyInfectedTerm - deathRateTP * SPt
                 }

                 dEPt <-  newlyInfectedTerm - (deathRateTP + (1 / aincp) ) * EPt

                 dINSPt <- (1 - sspf) * (1 / aincp) * EPt - (1 / aip) * INSPt - deathRateINSP * INSPt

                 dISSPt <-  sspf * (1 / aincp) * EPt - (1 / aip) * ISSPt - deathRateISSP * ( ISSPt - HPt ) - deathRateHP * HPt

                 dHPt <-  ifelse( HPt < HBt, min( HBt - HPt, sspf * (1 / aincp) * EPt ), 0 ) - (1 / aip) * HPt - deathRateHP * HPt

                 dRPt <-  (1 / aip) * (ISSPt + INSPt) - deathRateTP * RPt

                 dDIPt <- peopleDyingPerDay

                 dHBt <-  nhbcr * HBt

                 dMHSt <-  hscr * HPt

                 dMLPt <-  lpcr * (ISSPt + INSPt + peopleDyingPerDay)


                 return( list( c( dSPt, dEPt, dINSPt, dISSPt, dHPt, dRPt, dDIPt, dHBt, dMHSt, dMLPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c(
               SPt = 10^5 - 1, # TP0 - 1
               EPt = 0,
               INSPt = 0,
               ISSPt = 1,
               HPt = 0,
               RPt = 0,
               DIPt = 0,
               HBt = 2.9/1000 * 10^5, # nhbrTP * TP0
               MHSt = 0,
               MLPt = 0
             )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c(
               TP0 = 10^5,
               deathRateTP = 0.0000219178,
               deathRateINSP = 0.015 / 28, # 0.015 * aip
               deathRateISSP = 0.035 / 28, # 0.015 * aip
               aip = 26,
               aincp = 6,
               contactRateINSP = 0.56,
               contactRateISSP = 0.56,
               sspf = 0.2,
               lpcr = 1,
               deathRateHP = 0.25 * 0.035 / 26, # 0.25 * deathRateISSP
               contactRateHP = 0.1 * 0.56, # 0.1 * contactRateISSP
               nhbrTP = 2.9 / 1000, # 2.9 hospital beds per 1000 people in USA
               nhbcr = 0,
               hscr = 1
             )))
  }

  res
}



##===========================================================
## SEI2HREconModel
##===========================================================

#' SEI2HREconModel model
#' @description SEI2HREconModel
#' @param initialConditionsQ Should initial conditions be included?
#' @param rateRulesQ Should rate rules be included?
#' @param birthsTermQ Should the births term be included or not?
#' @param A list
#' @export
SEI2HREconModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE, birthsTermQ = FALSE ) {

  res <-
    list(

      Stocks =
        c(
          "TPt" = "Total Population",
          "SPt" = "Susceptible Population",
          "EPt" = "Exposed Population",
          "INSPt" = "Infected Normally Symptomatic Population",
          "ISSPt" = "Infected Severely Symptomatic Population",
          "RPt" = "Recovered Population",
          "MLPt" = "Money of Lost Productivity",
          "HPt" = "Hospitalized Population",
          "DIPt" = "Deceased Infected Population",
          "MSt" = "Medical Supplies",
          "MSDt" = "Medical Supplies Demand",
          "HBt" = "Hospital Beds",
          "MMSPt" = "Money for Medical Supplies Production",
          "MHSt" = "Money for Hospital Services",
          "HMSt" = "Hospital Medical Supplies"),

      Rates =
        c(
          "TP0" = "Initial value of Total Population",
          "deathRateTP" = "Population death rate",
          "deathRateINSP" = "Infected Normally Symptomatic Population death rate",
          "deathRateISSP" = "Infected Severely Symptomatic Population death rate",
          "sspfSP" = "Severely Symptomatic Population Fraction",
          "contactRateINSP" = "Contact rate for the normally symptomatic population",
          "contactRateISSP" = "Contact rate for the severely symptomatic population",
          "aip" = "Average infectious period",
          "aincp" = "Average incubation period",
          "lpcr" = "Lost productivity cost rate (per person per day)",
          "deathRateHP" = "Hospitalized Population death rate",
          "contactRateHP" = "Contact rate for the hospitalized population",
          "nhbrTP" = "Number of hospital beds rate (number of beds per person)",
          "hscr" = "Hospital services cost rate (per bed per day)",
          "nhbcr" = "Number of hospital beds change rate (per day)",
          "hpmscr" = "Hospitalized population medical supplies consumption rate (per day)",
          "upmscr" = "Un-hospitalized population medical supplies consumption rate (units per day)",
          "msprHB" = "Medical supplies production rate (units per pay)",
          "mspcrHB" = "Medical supplies production cost rate (per unit)",
          "msdpHB" = "Medical supplies delivery period (number of days)",
          "mscrTP" = "Medical supplies consumption rate (units per day per person)",
          "mscrINSP" = "Medical supplies consumption rate (units per day per person)",
          "mscrISSP" = "Medical supplies consumption rate (units per day per person)",
          "mscrHP" = "Medical supplies consumption rate (units per day per person)",
          "capacityHMS" = "Capacity to store Hospital Medical Supplies",
          "capacityMS" = "Capacity to store produced Medical Supplies",
          "capacityMSD" = "Capacity to transport produced Medical Supplies"),


      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ) ,
               {

                 ##----------------------------------------

                 if( is.function(contactRateINSP) ) {
                   nContactRateINSP <- contactRateINSP ( time )
                 } else {
                   nContactRateINSP <- contactRateINSP
                 }

                 if( is.function(contactRateISSP) ) {
                   nContactRateISSP <- contactRateISSP ( time  )
                 } else {
                   nContactRateISSP <- contactRateISSP
                 }

                 ##----------------------------------------

                 newBySeverelyInfectedTerm = nContactRateISSP / TP0 * SPt * max(ISSPt - HPt, 0) + contactRateHP / TP0 * SPt * HPt

                 newByNormallyInfectedTerm = nContactRateINSP / TP0 * SPt * INSPt

                 newlyInfectedTerm = newBySeverelyInfectedTerm + newByNormallyInfectedTerm

                 usableHospitalBeds = min(HBt - HPt, HMSt / mscrISSP)

                 peopleDyingPerDay = deathRateISSP * ( ISSPt - HPt ) + deathRateINSP * INSPt + deathRateHP * HPt;

                 orderedHospitalSupplies = min( capacityMSD, MSt, mscrHP * HBt, capacityHMS - HMSt ) / msdpHB;

                 if( birthsTermQ ) {
                   dSPt <-  deathRateTP * TP0 - newlyInfectedTerm - deathRateTP * SPt
                 } else {
                   dSPt <-  - newlyInfectedTerm - deathRateTP * SPt
                 }

                 dEPt <- newlyInfectedTerm - (1/aincp + deathRateTP) * EPt

                 dINSPt <-  (1 - sspfSP) * (1 / aincp) * EPt - (1 / aip) * INSPt - deathRateINSP * INSPt

                 dISSPt <-  sspfSP * (1 / aincp) * EPt - (1 / aip) * ISSPt - deathRateISSP * ( ISSPt - HPt ) - deathRateHP * HPt

                 dHPt <-  ifelse( HPt < HBt, min( usableHospitalBeds, sspfSP * (1 / aincp) * EPt), 0 ) - (1 / aip) * HPt - deathRateHP * HPt

                 dRPt <-  (1 / aip) * (ISSPt + INSPt) - deathRateTP * RPt

                 dDIPt <-  peopleDyingPerDay

                 dHBt <-  nhbcr * HBt

                 dHMSt <-  - min( HMSt, mscrISSP * HPt ) + orderedHospitalSupplies

                 dMSt <- min( msprHB, capacityMS - MSt ) - orderedHospitalSupplies - min( MSt - orderedHospitalSupplies, mscrISSP * (ISSPt - HPt) + mscrINSP * INSPt + mscrTP * (SPt + EPt + RPt) )

                 dMSDt <- HPt*mscrHP + INSPt*mscrINSP + ISSPt*mscrISSP + mscrTP*(EPt + RPt + SPt)

                 dMHSt <- HPt*hscr

                 dMMSPt <- MSDt*mspcrHB

                 dMLPt <- lpcr*(INSPt + ISSPt + peopleDyingPerDay)


                 return( list( c( dSPt, dEPt, dINSPt, dISSPt, dHPt, dRPt, dDIPt, dHBt, dHMSt, dMSt, dMSDt, dMMSPt, dMHSt, dMLPt ) ) )

               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c(
               SPt = 10^5 - 2, # TP0 - 1
               EPt = 0,
               INSPt = 1,
               ISSPt = 1,
               HPt = 0,
               RPt = 0,
               DIPt = 0,
               HBt = 2.9/1000 * 10^5, # nhbrTP * TP0
               HMSt = 1000.,
               MSt = 4000,
               MSDt = 0,
               MMSPt = 0,
               MHSt = 0,
               MLPt = 0
             )
         ))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c(
               TP0 = 10^5,
               deathRateTP = (800 / 10^5) / 365.,
               deathRateISSP = 0.035 / 26, # 0.035 / aip,
               deathRateINSP = 0.015 / 26, # 0.015 / aip
               contactRateISSP = 0.15,
               contactRateINSP = 0.15,
               aip = 26,
               aincp = 6,
               sspfSP = 0.2,
               lpcr = 1,
               deathRateHP =  0.25 * 0.035 / 26, #  0.25 *deathRateISSP
               contactRateHP = 0.015, # 0.1 * contactRateISSP
               nhbrTP = 0.0029,
               nhbcr = 0.00001,
               hscr = 1,
               hpmscr = 4,
               upmscr = 2,
               mspcrHB = 1,
               msdpHB = 1.5,
               msprHB = 29000.,
               mscrTP = 0.02,
               mscrINSP = 0.7,
               mscrISSP = 3,
               mscrHP = 4,
               capacityHMS = 6*290,
               capacityMS = 4000.,
               capacityMSD = 290/10.
             )))
  }

  res
}

