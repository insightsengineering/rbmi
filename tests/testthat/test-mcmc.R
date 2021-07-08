test_that("transformation from long to wide format works", {
    vec_long = c(c(NA, 1, 2), c(1, NA, 2), c(3, NA, 4))
    vec_wide_true = rbind(c(NA, 1, 2), c(1, NA, 2), c(3, NA, 4))
    
    vec_wide = long2wide(vec_long = vec_long,
                         J = 3)
    
    vec_wide_oneElem = long2wide(vec_long = vec_long[1:3],
                                 J = 3)
    
    expect_true(is.matrix(vec_wide))
    expect_equal(vec_wide, vec_wide_true)
    
    expect_true(is.matrix(vec_wide_oneElem))
    expect_equal(vec_wide_oneElem, vec_wide_true[1,, drop = FALSE])
})

test_that("Proper missingness patterns are detected", {
    outcome = rbind(c(NA, 1, 2), c(1, NA, 2), c(3, NA, 4))
    
    res = get_obs_missingness_patterns(outcome)
    missingness_patterns = res$missingness_patterns
    M = res$M
    
    number_missingness_patterns = nrow(missingness_patterns)
    
    expect_equal(number_missingness_patterns, 2)
    expect_equal(missingness_patterns, rbind(c(0,1,1),c(1,0,1)))
    expect_equal(M, c(1,2,2))
})

test_that("Proper missingness pattern is detected when only one patient", {
    outcome = t(matrix(c(NA, 1, 2)))
    
    res = get_obs_missingness_patterns(outcome)
    missingness_patterns = res$missingness_patterns
    M = res$M
    
    number_missingness_patterns = nrow(missingness_patterns)
    
    expect_equal(number_missingness_patterns, 1)
    expect_equal(missingness_patterns, t(matrix(c(0,1,1))))
    expect_equal(M, 1)
})