library(testthat)
library(readr)


source("app/app.R")  # Source your Shiny app script

context("Data Loading")

test_that("Data is loaded correctly", {
  # Load the data
  cdc_growth_data <- read_csv("statage.csv")
  
  # Test if the data has the expected number of rows and columns
  expect_equal(nrow(cdc_growth_data), expected_number_of_rows)
  expect_equal(ncol(cdc_growth_data), expected_number_of_columns)
  
  # Test if the "Sex" column is present and contains the expected values
  expect_true("Sex" %in% colnames(cdc_growth_data))
  expect_true(all(cdc_growth_data$Sex %in% c("Boys", "Girls")))
  
  # Test if the "Age" column is present and contains the expected values
  expect_true("Age" %in% colnames(cdc_growth_data))
  # Add more specific checks based on the nature of your data in the "Age" column
  
  # Test if the columns L, M, and S are removed
  expect_false("L" %in% colnames(cdc_growth_data))
  expect_false("M" %in% colnames(cdc_growth_data))
  expect_false("S" %in% colnames(cdc_growth_data))
})

