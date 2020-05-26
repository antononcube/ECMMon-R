context("Core model modificatons")

library(ECMMon)

modelSEI2HR <- SEI2HRModel( initialConditionsQ = TRUE, rateRulesQ = TRUE, birthsTermQ = TRUE )

## Tests: expected objects
test_that("Expected SEI2HR model object", {

  expect_is( modelSEI2HR, "list" )

  expect_equal( sort(names(modelSEI2HR)), sort(c("Stocks", "Rates", "RHSFunction", "InitialConditions", "RateRules")) )

  expect_true( EpidemiologyFullModelQ(modelSEI2HR) )

})


##-----------------------------------------------------------
## Tests model adding ID
##-----------------------------------------------------------

model_modelID <- AddModelIdentifier( model =  modelSEI2HR, id = "MY_ID", addToOtherLeftHandSidesQ = FALSE, birthsTermQ = TRUE )

## Tests
test_that("Adding model identifier", {

  expect_true( EpidemiologyFullModelQ(model_modelID) )

  expect_equal( grep( pattern = "MY_ID", x = names(model_modelID$Stocks), value = TRUE ), names(model_modelID$Stocks) )

  expect_equal( grep( pattern = "MY_ID", x = names(model_modelID$Rates), value = TRUE ), names(model_modelID$Rates) )

  expect_equal( grep( pattern = "MY_ID", x = names(model_modelID$InitialConditions), value = TRUE ), names(model_modelID$InitialConditions) )

  expect_equal( grep( pattern = "MY_ID", x = names(model_modelID$RateRules), value = TRUE ), names(model_modelID$RateRules) )

})


##-----------------------------------------------------------
## Tests model adding new rates
##-----------------------------------------------------------

model_newRates <-
  AddModelRates(
    model = modelSEI2HR,
    rateDescriptions = c( "qsd" = "Quarantine start day", "ql" = "Quarantine length" ),
    rateValues = c( "qsd" = 100, "ql" = 14 )
  )

## Tests
test_that("Adding new rates", {

  expect_true( EpidemiologyFullModelQ(model_newRates) )

  expect_true( mean( c( "qsd", "ql" ) %in% names(model_newRates$Rates) ) == 1 )

  expect_true( mean( c( "Quarantine start day", "Quarantine length" ) %in% model_newRates$Rates ) == 1 )

  expect_true( mean( c( "qsd", "ql" ) %in% names(model_newRates$RateRules) ) == 1 )

  expect_equal( model_newRates$RateRules[c("qsd", "ql")], c( "qsd" = 100, "ql" = 14) )

})
