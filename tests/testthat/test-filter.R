test_that ('global outlier identification works', {
    expect_equal (identify_vector_outliers_global (1:10), rep_len (FALSE, 10))
    expect_equal (identify_vector_outliers_global (c (1:10, 100)), c (rep_len (FALSE, 10), TRUE))
})

test_that ('window outlier identification works', {
    expect_equal (identify_vector_outliers_window (c (100, 15:25, 95:105, 100), .window = 12), c (TRUE, rep_len (FALSE, 23)))
    expect_equal (identify_vector_outliers_window (c (100, 15:25, 95:105, 100), .window = 13), rep_len (FALSE, 24))
})

test_that ('global outlier removal works', {
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (remove_outliers_global (test_data, time), nrow = 4)
})

test_that ('window outlier removal works', {
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (remove_outliers_window (test_data, time, .window = 2), nrow = 6)
    expect_tibble (remove_outliers_window (test_data, time, .window = 3), nrow = 4)
})
