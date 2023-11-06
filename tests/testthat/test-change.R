test_that ('segment location works', {
    expect_equal (locate_vector_segments (1:10, 100), c (1))
    expect_equal (locate_vector_segments (c (1:10, 100:110), 100), c (1, 11))
})

test_that ('segment listing works on actual data', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (list_segment_boundaries (test_data, time, 100), nrows = 0)
})

test_that ('segment listing works on artificial data', {
    test_data <- rren_artificial (c (abs (rnorm (1000, mean = 0)), abs (rnorm (1000, mean = 100))), 2, 2, 2)
    test_segments <- list_segment_boundaries (test_data, time, 'barrett')
    expect_tibble (test_segments, nrows = 8)
    expect_integer (test_segments $ index, 1001 - 8, 1001 + 8)
})
