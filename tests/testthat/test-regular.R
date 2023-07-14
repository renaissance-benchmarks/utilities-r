test_that ('regular artifact listing by position works on artificial data', {
    test_data <- rren_artificial (c (abs (rnorm (1000, mean = 0)), abs (rnorm (1000, mean = 100))), 2, 22, 2)

    test_dimensions <- list_dimensions (test_data)
    expect_tibble (test_dimensions, nrow = 88)
    expect_integer (test_dimensions $ index_last, 2000)
    expect_numeric (test_dimensions $ total_last, max (test_data $ total))

    test_segments <- list_segment_boundaries (test_data, time, 'barrett')
    expect_tibble (test_segments, nrow = 88)
    expect_integer (test_segments $ index, 1001 - 8, 1001 + 8)

    test_artifacts <- list_regular_artifacts_by_position (test_segments, test_dimensions)
    expect_tibble (test_artifacts, nrow = 4)
    expect_set_equal (test_artifacts $ index, test_segments $ index)
    expect_integer (test_artifacts $ artifacts, 22L)
    expect_integer (test_artifacts $ runs, 22L)
    expect_numeric (test_artifacts $ share, 1)
})

test_that ('regular artifact listing by interval works on artificial data', {
    test_data <- rren_artificial (c (abs (rnorm (1000, mean = 0)), abs (rnorm (1000, mean = 100))), 2, 22, 2)

    test_dimensions <- list_dimensions (test_data)
    expect_tibble (test_dimensions, nrow = 88)
    expect_integer (test_dimensions $ index_last, 2000)
    expect_numeric (test_dimensions $ total_last, max (test_data $ total))

    test_segments <- list_segment_boundaries (test_data, time, 'barrett')
    expect_tibble (test_segments, nrow = 88)
    expect_integer (test_segments $ index, 1001 - 8, 1001 + 8)

    test_artifacts <- list_regular_artifacts_by_interval (test_segments, test_dimensions)
    expect_tibble (test_artifacts, nrow = 4)
    expect_set_equal (test_artifacts $ total_before, test_segments $ total_before)
    expect_set_equal (test_artifacts $ total_after, test_segments $ total_after)
})

test_that ('regular artifact listing by interval works on multiple segments', {
    test_data <- rren_artificial (c (abs (rnorm (1000, mean = 0)), abs (rnorm (1000, mean = 100)), abs (rnorm (1000, mean = 0))), 1, 22, 1)

    test_dimensions <- list_dimensions (test_data)
    expect_tibble (test_dimensions, nrow = 22)
    expect_integer (test_dimensions $ index_last, 3000)
    expect_numeric (test_dimensions $ total_last, max (test_data $ total))

    test_segments <- list_segment_boundaries (test_data, time, 'barrett')
    expect_tibble (test_segments, nrow = 44)
    expect_integer (test_segments $ index [seq (1L, 43L, 2L)], 1001 - 8, 1001 + 8)
    expect_integer (test_segments $ index [seq (2L, 44L, 2L)], 2001 - 8, 2001 + 8)

    test_artifacts <- list_regular_artifacts_by_interval (test_segments, test_dimensions)
    expect_tibble (test_artifacts, nrow = 2)
    expect_set_equal (test_artifacts $ total_before, test_segments $ total_before)
    expect_set_equal (test_artifacts $ total_after, test_segments $ total_after)
})
