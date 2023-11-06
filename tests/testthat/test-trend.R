test_that ('trend computation works on artificial data', {
    test_data <- rren_artificial (c (0, 1), 2, 2, 2)
    test_trends <- compute_trends (test_data, time)
    expect_tibble (test_trends, nrows = 8)
    expect_double (test_trends $ time_trend, 0.999, 1.001)
})
