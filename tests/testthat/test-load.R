test_that ('loading JSON version 1 works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-1.json'))
    expect_renaissance (test_data)
    expect_tibble (test_data, nrows = 6)
    expect_setequal (test_data |> distinct (benchmark) |> pull (benchmark), c ('page-rank', 'naive-bayes'))
    expect_equal (test_data |> filter (benchmark == 'page-rank', index == 1) |> pull (time), 19.159228357)
})

test_that ('loading JSON version 2 works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-2.json'))
    expect_renaissance (test_data)
    expect_tibble (test_data, nrows = 6)
    expect_setequal (test_data |> distinct (benchmark) |> pull (benchmark), c ('page-rank', 'naive-bayes'))
    expect_equal (test_data |> filter (benchmark == 'page-rank', index == 1) |> pull (time), 21.724393426)
})

test_that ('loading JSON version 5 works', {
    log_threshold (WARN)
    test_data <- load_file_json (rren_example ('results-small-version-5.json'))
    expect_renaissance (test_data)
    expect_tibble (test_data, nrows = 6)
    expect_setequal (test_data |> distinct (benchmark) |> pull (benchmark), c ('page-rank', 'naive-bayes'))
    expect_equal (test_data |> filter (benchmark == 'page-rank', index == 1) |> pull (time), 5.411979039)
})

test_that ('loading multiple JSON files works', {
    log_threshold (WARN)
    test_data <- load_path_json (rren_example (), '^results-small-version-[0-9]+\\.json$')
    expect_renaissance (test_data)
    expect_tibble (test_data, nrows = 18)
    expect_setequal (test_data |> distinct (benchmark) |> pull (benchmark), c ('page-rank', 'naive-bayes'))
    expect_equal (test_data |> filter (str_ends (run, 'results-small-version-1\\.json'), benchmark == 'page-rank', index == 1) |> pull (time), 19.159228357)
    expect_equal (test_data |> filter (str_ends (run, 'results-small-version-2\\.json'), benchmark == 'page-rank', index == 1) |> pull (time), 21.724393426)
    expect_equal (test_data |> filter (str_ends (run, 'results-small-version-5\\.json'), benchmark == 'page-rank', index == 1) |> pull (time), 5.411979039)
})

test_that ('generating artificial data files works', {
    test_data <- rren_artificial (seq (1000))
    expect_renaissance (test_data)
    expect_tibble (test_data, nrows = 1000)
    expect_equal (test_data $ time, seq (1000))
})
