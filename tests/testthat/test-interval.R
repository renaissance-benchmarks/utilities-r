test_that ('flat confidence interval computation works on artificial data', {
    test_data <- rren_artificial (c (0, 1), 2L, 2L, 2L)
    test_ci <- compute_flat_percentile_ci (test_data, time, .R = 1000L, .reduction = 1)
    # With only 2 runs of 2 samples and reduction 1 there is (1/2)^3 = 12.5% chance of observing the minimum average.
    # The left side of the 99% confidence interval will not be equal to the minimum only if
    # more than 99.5% of replicates are not equal to the minimum.
    # For 1000 replicates the probability of this is (1-(1/2)^3)^995 ~ 2e-58.
    expect_tibble (test_ci, nrows = 4L)
    expect_double (test_ci $ lo, 0, 0.001)
    expect_double (test_ci $ hi, 0.999, 1)
    expect_double (test_ci $ mid, 0.499, 0.501)
})

test_that ('hierarchical confidence interval computation works on artificial data', {
    test_data <- rren_artificial (rep.int (0, 10L), 2L, 2L, 2L) |> mutate (time = time + c (rep.int (0, 10L), rep.int (1, 10L)))
    test_ci <- compute_hierarchical_percentile_ci (test_data, time, .R = 1000L, .reduction = 1)
    # With only 2 runs of equal samples and reduction 1 there is 50% chance of observing the minimum average.
    # The left side of the 99% confidence interval will not be equal to the minimum only if
    # more than 99.5% of replicates are not equal to the minimum.
    # For 1000 replicates the probability of this is (1-1/2)^995 ~ 3e-300.
    expect_tibble (test_ci, nrows = 4L)
    expect_double (test_ci $ lo, 0, 0.001)
    expect_double (test_ci $ hi, 0.999, 1)
    expect_double (test_ci $ mid, 0.499, 0.501)
})

test_that ('flat relative confidence interval computation works on artificial data', {
    test_data <- rren_artificial (c (0, 1), 2L, 2L, 2L)
    test_base <- test_data |> distinct (.data $ vm) |> pull (.data $ vm) |> first ()
    test_ci <- compute_flat_relative_percentile_ci (test_data, test_base, time, .R = 1000L, .reduction = 1)
    # With only 2 runs of 2 samples and reduction 1 there is (1/2)^3 = 12.5% chance of observing the minimum average
    # in each data set and ((1/2)^3)^2 ~ 2% chance of observing the minimum combinations of the averages.
    # The left side of the 99% confidence interval will not be equal to the minimum only if
    # more than 99.5% of replicates are not equal to the minimum.
    # For 1000 replicates the probability of this is (1-((1/2)^3)^2)^995 ~ 2e-7.
    expect_tibble (test_ci, nrows = 4L)
    expect_double (test_ci $ lo, -1, -0.999)
    expect_double (test_ci $ hi, 0.999, 1)
    expect_double (test_ci $ mid, -0.001, 0.001)
})

test_that ('hierarchical relative confidence interval computation works on artificial data', {
    test_data <- rren_artificial (rep.int (0, 10L), 2L, 2L, 2L) |> mutate (time = time + c (rep.int (0, 10L), rep.int (1, 10L)))
    test_base <- test_data |> distinct (.data $ vm) |> pull (.data $ vm) |> first ()
    test_ci <- compute_hierarchical_relative_percentile_ci (test_data, test_base, time, .R = 1000L, .reduction = 1)
    # With only 2 runs of equal samples and reduction 1 there is 50% chance of observing the minimum average
    # in each data set and 25% chance of observing the minimum combination of the averages.
    # The left side of the 99% confidence interval will not be equal to the minimum only if
    # more than 99.5% of replicates are not equal to the minimum.
    # For 1000 replicates the probability of this is (1-(1/2)^2)^995 ~ 5e-125.
    expect_tibble (test_ci, nrows = 4L)
    expect_double (test_ci $ lo, -1, -0.999)
    expect_double (test_ci $ hi, 0.999, 1)
    expect_double (test_ci $ mid, -0.001, 0.001)
})
