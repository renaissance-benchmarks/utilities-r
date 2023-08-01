test_that ('confidence interval computation works on artificial data', {
    test_data <- rren_artificial (c (0, 1), 2, 2, 2)
    test_cis <- compute_flat_percentile_cis (test_data, time, .R = 100)
    expect_tibble (test_cis, nrow = 4)
    expect_double (test_cis $ time_lo, 0, 1)
    expect_double (test_cis $ time_hi, 0, 1)
})
