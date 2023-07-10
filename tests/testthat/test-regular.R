test_that ('regular artifact listing works on artificial data', {
    test_data <- rren_artificial (c (abs (rnorm (1000, mean = 0)), abs (rnorm (1000, mean = 100))), 2, 22, 2)

    test_dimensions <- list_dimensions (test_data)
    expect_tibble (test_dimensions, nrow = 88)
    expect_integer (test_dimensions $ index_last, 2000)
    expect_numeric (test_dimensions $ total_last, max (test_data $ total))

    test_segments <- list_segment_boundaries (test_data, time, 'barrett')
    expect_tibble (test_segments, nrow = 88)
    expect_integer (test_segments $ index, 1001 - 8, 1001 + 8)

    test_artifacts <- list_regular_artifacts (test_segments, test_dimensions)
    expect_tibble (test_artifacts, nrow = 4)
    expect_set_equal (test_artifacts $ index, test_segments $ index)
    expect_integer (test_artifacts $ artifacts, 22L)
    expect_integer (test_artifacts $ runs, 22L)
    expect_numeric (test_artifacts $ share, 1)
})
