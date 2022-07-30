test_that ('loading JSON version 1 works', {
    test_data <- load_file_json (test_path ('results-small-version-1.json'))
    expect_tibble (test_data, nrow = 6)
    test_benchmarks <- test_data %>% distinct (benchmark) %>% pull (benchmark)
    expect_setequal (test_benchmarks, c ('page-rank', 'naive-bayes'))
    test_page_rank_first <- test_data %>% filter (benchmark == 'page-rank', index == 1) %>% pull (time)
    expect_equal (test_page_rank_first, 19.159228357)
})

test_that ('loading JSON version 2 works', {
    test_data <- load_file_json (test_path ('results-small-version-2.json'))
    expect_tibble (test_data, nrow = 6)
    test_benchmarks <- test_data %>% distinct (benchmark) %>% pull (benchmark)
    expect_setequal (test_benchmarks, c ('page-rank', 'naive-bayes'))
    test_page_rank_first <- test_data %>% filter (benchmark == 'page-rank', index == 1) %>% pull (time)
    expect_equal (test_page_rank_first, 21.724393426)
})

test_that ('loading JSON version 5 works', {
    test_data <- load_file_json (test_path ('results-small-version-5.json'))
    expect_tibble (test_data, nrow = 6)
    test_benchmarks <- test_data %>% distinct (benchmark) %>% pull (benchmark)
    expect_setequal (test_benchmarks, c ('page-rank', 'naive-bayes'))
    test_page_rank_first <- test_data %>% filter (benchmark == 'page-rank', index == 1) %>% pull (time)
    expect_equal (test_page_rank_first, 5.411979039)
})
