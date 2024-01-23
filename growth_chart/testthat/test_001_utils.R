library(testthat)

context("Package Loading")

test_that("All necessary packages are loaded", {
  # Source the script that loads packages
  source("path/to/load_packages.R")
  
  # Define a vector of expected package names
  expected_packages <- c("shiny", "dplyr", "tidyverse","ggplot2","scales","lubridate","DT")
  
  # Check if all expected packages are available
  for (pkg in expected_packages) {
    expect_true(requireNamespace(pkg, quietly = TRUE), 
                sprintf("Package '%s' should be loaded", pkg))
  }
})