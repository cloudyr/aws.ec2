k1 <- Sys.getenv("AWS_ACCESS_KEY_ID")
k2 <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
if ((k1 != "") && (k2 != "")) {
  library(testthat)
  library(aws.ec2)
  test_check("aws.ec2")
} # else {
#   stop(
#     "You must configure environment variables to run these tests.",
#     "\nSee README.md for details."
#   )
# }
