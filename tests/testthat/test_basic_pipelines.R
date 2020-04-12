context("Basic pipelines")

library(ECMMon)


## Tests: expected objects
test_that("Expected ECMMon model object", {

  expect_is( ECMMonUnit(), "ECMMon" )

  expect_is( ECMMonUnit( SIRModel( initialConditionsQ = TRUE ) ), "ECMMon" )

})

##-----------------------------------------------------------
## Tests model creation and simulation
##-----------------------------------------------------------

ecmObj1 <-
  ECMMonUnit( SIRModel(initialConditionsQ = TRUE, rateRulesQ = TRUE) ) %>%
  ECMMonEchoModelTableForm( c("Stocks", "InitialConditions"), echoQ = FALSE ) %>%
  ECMMonSimulate( maxTime = 365 ) %>%
  ECMMonPlotSolutions( stocksSpec = ".*P.", echoQ = FALSE)

ecmObj2 <-
  ECMMonUnit( SEI2RModel(initialConditionsQ = TRUE, rateRulesQ = TRUE) ) %>%
  ECMMonEchoModelTableForm( c("Stocks", "InitialConditions"), echoQ = FALSE ) %>%
  ECMMonSimulate( maxTime = 365 ) %>%
  ECMMonPlotSolutions( stocksSpec = ".*P.", echoQ = FALSE )

ecmObj3 <-
  ECMMonUnit( SEI2HRModel(initialConditionsQ = TRUE, rateRulesQ = TRUE) ) %>%
  ECMMonEchoModelTableForm( c("Stocks", "InitialConditions"), echoQ = FALSE ) %>%
  ECMMonSimulate( maxTime = 365 ) %>%
  ECMMonPlotSolutions( stocksSpec = ".*P.", echoQ = FALSE )

## Tests
test_that("Core pipelines", {

  expect_true( "Solution" %in% names(ecmObj1) )

  expect_true( "deSolveSolution" %in% names(ecmObj1) )

  expect_is( ecmObj1$Solution, "data.frame" )

  expect_is( ecmObj1$deSolveSolution, "deSolve" )


  expect_true( "Solution" %in% names(ecmObj2) )

  expect_true( "deSolveSolution" %in% names(ecmObj2) )

  expect_is( ecmObj2$Solution, "data.frame" )

  expect_is( ecmObj2$deSolveSolution, "deSolve" )


  expect_true( "Solution" %in% names(ecmObj3) )

  expect_true( "deSolveSolution" %in% names(ecmObj3) )

  expect_is( ecmObj3$Solution, "data.frame" )

  expect_is( ecmObj3$deSolveSolution, "deSolve" )

})
