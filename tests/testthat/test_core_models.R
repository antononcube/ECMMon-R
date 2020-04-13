context("Core models")

library(ECMMon)

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
modelSI2R <- SIRModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )
modelSEI2R <- SEI2RModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )
modelSEI2HR <- SEI2HRModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )
modelSEI2HREcon <- SEI2HREconModel( initialConditionsQ = TRUE, rateRulesQ = TRUE )


## Tests: expected objects
test_that("Expected SIR model object", {

  expect_is( modelSIR, "list" )

  expect_equal( sort(names(modelSIR)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

test_that("Expected SI2R model object", {

  expect_is( modelSI2R, "list" )

  expect_equal( sort(names(modelSI2R)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

test_that("Expected SEI2R model object", {

  expect_is( modelSEI2R, "list" )

  expect_equal( sort(names(modelSEI2R)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

test_that("Expected SEI2HR model object", {

  expect_is( modelSEI2HR, "list" )

  expect_equal( sort(names(modelSEI2HR)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})

test_that("Expected SEI2HREcon model object", {

  expect_is( modelSEI2HREcon, "list" )

  expect_equal( sort(names(modelSEI2HREcon)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

})


## Tests: verification tests
test_that("Expected verification functions results", {

  expect_true( mean( sapply( list(modelSIR, modelSI2R, modelSEI2R, modelSEI2HR, modelSEI2HREcon), EpidemiologyModelQ ) ) == 1 )

  expect_true( mean( sapply( list(modelSIR, modelSI2R, modelSEI2R, modelSEI2HR, modelSEI2HREcon), EpidemiologyFullModelQ ) ) == 1 )

})

## Tests: proper definitions
test_that("Properly defined models stocks", {

  ## expect_true( mean( sapply( list(modelSIR, modelSEI2R, modelSEI2HR), ProprelyDefinedQ ) ) == 1 )
  expect_true( ProprelyDefinedStocksQ(modelSIR) )

  expect_true( ProprelyDefinedStocksQ(modelSI2R) )

  expect_true( ProprelyDefinedStocksQ(modelSEI2R) )

  expect_true( ProprelyDefinedStocksQ(modelSEI2HR) )

  expect_true( ProprelyDefinedStocksQ(modelSEI2HREcon) )

})


test_that("Properly defined models rates", {

  expect_true( ProprelyDefinedRatesQ(modelSIR) )

  expect_true( ProprelyDefinedRatesQ(modelSI2R) )

  expect_true( ProprelyDefinedRatesQ(modelSEI2R) )

  expect_true( ProprelyDefinedRatesQ(modelSEI2HR) )

  expect_true( ProprelyDefinedRatesQ(modelSEI2HREcon) )

})
