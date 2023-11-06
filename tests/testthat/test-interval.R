test_that ('flat confidence interval computation works on artificial data', {
    test_data <- rren_artificial (c (0, 1), 2, 2, 2)
    test_ci <- compute_flat_percentile_ci (test_data, time, .R = 100)
    expect_tibble (test_ci, nrows = 4)
    expect_double (test_ci $ lo, 0, 1)
    expect_double (test_ci $ hi, 0, 1)
    expect_double (test_ci $ mid, 0.499, 0.501)
})

test_that ('hierarchical confidence interval computation works on artificial data', {
    test_data <- rren_artificial (c (0, 1), 2, 2, 2)
    test_ci <- compute_flat_percentile_ci (test_data, time, .R = 100)
    expect_tibble (test_ci, nrows = 4)
    expect_double (test_ci $ lo, 0, 1)
    expect_double (test_ci $ hi, 0, 1)
    expect_double (test_ci $ mid, 0.499, 0.501)
})
