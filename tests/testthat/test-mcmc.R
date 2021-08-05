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

test_that("Covariance matrices are sort correctly", {

    sigma_reml <- list(
        diag(rep(1,2)),
        diag(rep(1,2)) + 0.5,
        diag(rep(1,2)) + 0.8
    )

    group_sigma <- c("A", "C", "B")

    expected_output <- list(
        diag(rep(1,2)),
        diag(rep(1,2)) + 0.8,
        diag(rep(1,2)) + 0.5
    )

    ordered_sigma <- match_groups_sigmas(sigma_reml, group_sigma)

    expect_equal(ordered_sigma,
                 expected_output)

})

test_that("QR decomposition is performed correctly", {

    N <- 6
    J = 2
    designmat <- cbind(rep(1,N*J), rnorm(N*J))
    QR <- QR_decomp(designmat, N, J)

    expect_equal(QR$Q %*% QR$R, designmat)
})

test_that("List of matrices is correctly transformed into array", {

    mat <- rbind(c(1, 0.2), c(0.2, 1))
    input <- list(mat, mat+1, mat+2)

    actual_output <- listmat_to_array(input)

    expected_output <- array(NA, dim = c(3,2,2))
    for(i in 1:3) {
        expected_output[i,,] <- mat + i-1
    }
    expect_equal(actual_output, expected_output)

})

