context("Templates")

test_that("delete/describe stops if Id/Name isn't passed", {
    expect_error(delete_template(), "delete_template wasn't passed")   
    expect_error(describe_template(), "describe_template wasn't passed")
})
