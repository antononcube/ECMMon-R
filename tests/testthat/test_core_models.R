context("Core models")

#library(ECMMon)
devtools::load_all()

## Definitions
ProprelyDefinedStocksQ <- function( model ) {

  if( ! EpidemiologyModelQ(model) ) {
    return(FALSE)
  }

  if( ! EpidemiologyFullModelQ(model) ) {
    return(TRUE)
  }

  mean( names(model$InitialConditions) %in% names(model$Stocks) ) == 1
}

ProprelyDefinedRatesQ <- function( model ) {

  if( ! EpidemiologyModelQ(model) ) {
    return(FALSE)
  }

  if( ! EpidemiologyFullModelQ(model) ) {
    return(TRUE)
  }

  mean( names(model$RateRules) %in% names(model$Rates) ) == 1
}



## Model objects
modelSIR <- SIRModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )
modelSEI2R <- SEI2RModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )
modelSEI2HR <- SEI2HRModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )

## Tests: expected objects
test_that("Expected SIR model object", {

  expect_is( modelSIR, "list" )

  expect_equal( sort(names(modelSIR)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

test_that("Expected SEI2R model object", {

  expect_is( modelSEI2R, "list" )

  expect_equal( sort(names(modelSEI2R)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

test_that("Expected SEI2HR model object", {

  expect_is( modelSEI2HR, "list" )

  expect_equal( sort(names(modelSEI2HR)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

## Tests: verification tests
test_that("Expected verification functions results", {

  expect_true( mean( sapply( list(modelSIR, modelSEI2R, modelSEI2HR), EpidemiologyModelQ ) ) == 1 )

  expect_true( mean( sapply( list(modelSIR, modelSEI2R, modelSEI2HR), EpidemiologyFullModelQ ) ) == 1 )

})

## Tests: proper definitions
test_that("Properly defined models stocks", {

  ## expect_true( mean( sapply( list(modelSIR, modelSEI2R, modelSEI2HR), ProprelyDefinedQ ) ) == 1 )
  expect_true( ProprelyDefinedStocksQ(modelSIR) )

  expect_true( ProprelyDefinedStocksQ(modelSEI2R) )

  expect_true( ProprelyDefinedStocksQ(modelSEI2HR) )


})


test_that("Properly defined models rates", {

  expect_true( ProprelyDefinedRatesQ(modelSIR) )

  expect_true( ProprelyDefinedRatesQ(modelSEI2R) )

  expect_true( ProprelyDefinedRatesQ(modelSEI2HR) )

})
