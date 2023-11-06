test_that ('global outlier identification works', {
    expect_equal (identify_vector_outliers_global (1:10), rep_len (FALSE, 10))
    expect_equal (identify_vector_outliers_global (c (1:10, 100)), c (rep_len (FALSE, 10), TRUE))
})

test_that ('window outlier identification works', {
    expect_equal (identify_vector_outliers_window (c (100, 15:25, 95:105, 100), .window = 12), c (TRUE, rep_len (FALSE, 23)))
    expect_equal (identify_vector_outliers_window (c (100, 15:25, 95:105, 100), .window = 13), rep_len (FALSE, 24))
    expect_equal (identify_vector_outliers_window (c (100, 15:25, 95:105, 100), .window = 24), rep_len (FALSE, 24))
    expect_equal (identify_vector_outliers_window (c (100, 15:25, 95:105, 100), .window = 25), rep_len (FALSE, 24))
})

test_that ('global outlier removal works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (remove_outliers_global (test_data, time), nrows = 4)
})

test_that ('window outlier removal works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (remove_outliers_window (test_data, .data $ time, .window = 2), nrows = 6)
    expect_tibble (remove_outliers_window (test_data, .data $ time, .window = 3), nrows = 4)
})

test_that ('global outlier listing works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (list_outliers_global (test_data, .data $ time), nrows = 2)
})

test_that ('window outlier listing works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_tibble (list_outliers_window (test_data, .data $ time, .window = 2), nrows = 0)
    expect_tibble (list_outliers_window (test_data, .data $ time, .window = 3), nrows = 2)
})

test_that ('global outlier flagging works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    test_flagged <- flag_outliers_global (test_data, .data $ time, .data $ outlier)
    expect_tibble (test_flagged, nrows = 6)
    expect_names (names (test_flagged), must.include = 'outlier')
    expect_true (is.logical (test_flagged $ outlier))
    expect_equal (sum (test_flagged $ outlier), 2)
})

test_that ('window outlier flagging works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    test_flagged <- flag_outliers_window (test_data, .data $ time, .data $ outlier, .window = 2)
    expect_tibble (test_flagged, nrows = 6)
    expect_names (names (test_flagged), must.include = 'outlier')
    expect_true (is.logical (test_flagged $ outlier))
    expect_equal (sum (test_flagged $ outlier), 0)
    test_flagged <- flag_outliers_window (test_data, .data $ time, .data $ outlier, .window = 3)
    expect_tibble (test_flagged, nrows = 6)
    expect_names (names (test_flagged), must.include = 'outlier')
    expect_true (is.logical (test_flagged $ outlier))
    expect_equal (sum (test_flagged $ outlier), 2)
})
