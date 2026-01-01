test_that("example.function computes correct sum for numeric scalars", {
    expect_equal(example.function(1, 2), 3)
    expect_equal(example.function(0.5, -0.25), 0.25)
})
