test_that ('global outlier identification works', {
    expect_equal (identify_outliers_global (1:10), rep_len (FALSE, 10))
    expect_equal (identify_outliers_global (c (1:10, 100)), c (rep_len (FALSE, 10), TRUE))
})

test_that ('window outlier identification works', {
    expect_equal (identify_outliers_window (c (100, 15:25, 95:105, 100), window = 12), c (TRUE, rep_len (FALSE, 23)))
    expect_equal (identify_outliers_window (c (100, 15:25, 95:105, 100), window = 13), rep_len (FALSE, 24))
})
