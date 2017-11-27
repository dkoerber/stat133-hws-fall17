# test script
library(testthat)

# source functions to be tested
source('functions.R')

sink('../output/test-reporter.txt')
test_file('tests.R')
sink()