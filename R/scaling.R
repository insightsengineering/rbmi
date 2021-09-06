
#' R6 Class for scaling (and un-scaling) design matrices
#'
#' @description
#' Scales a design matrix so that all non-categorical columns have a mean
#' of 0 and an standard deviation of 1.
#'
#' @details
#' The object initialisation
#' is used to determine the relevant mean and SD's to scale by and then
#' the scaling (and un-scaling) itself is performed by the relevant object
#' methods.
#'
#' Un-scaling is done on linear model Beta and Sigma coefficients. For this purpose
#' the first column on the dataset to be scaled is assumed to be the outcome variable
#' with all other variables assumed to be post-transformation predictor variables (i.e.
#' all dummy variables have already been expanded)
scalerConstructor <- R6::R6Class(
    classname = "scaler",
    public = list(

        #' @field centre Vector of column means. The first value is the outcome
        #' variable, all other variables are the predictors.
        centre = NULL,

        #' @field scales Vector of column standard deviations. The first value is the outcome
        #' variable, all other variables are the predictors.
        scales = NULL,


        #' @description
        #' Uses `dat` to determine the relevant column means and standard deviations to use
        #' when scaling and un-scaling future datasets. Implicitly assumes that new datasets
        #' have the same column order as `dat`
        #' @param dat A dataframe or matrix. All columns must be numeric (i.e dummy variables,
        #' must have already been expanded out).
        #' @details
        #' Categorical columns (as determined by those who's values are entirely `1` or `0`)
        #' will not be scaled. This is achieved by setting the corresponding values of centre
        #' to `0` and scale to `1`.
        initialize = function(dat) {

            assert_that(
                is.data.frame(dat) | is.matrix(dat),
                all(vapply(dat, is.numeric, logical(1))),
                msg = "Input must be a numeric dataframe or matrix"
            )

            cat_flag <- vapply(
                X = dat,
                FUN = function(x) all(x %in% c(0, 1)),
                FUN.VALUE = logical(1),
                USE.NAMES = FALSE
            )

            centre <- vapply(
                X = dat,
                FUN = function(x) mean(x, na.rm = TRUE),
                FUN.VALUE = numeric(1),
                USE.NAMES = FALSE
            )

            scales <- vapply(
                X = dat,
                FUN = function(x) sd(x, na.rm = TRUE),
                FUN.VALUE = numeric(1),
                USE.NAMES = FALSE
            )

            centre[cat_flag] <- 0
            scales[cat_flag] <- 1
            self$centre <- centre
            self$scales <- scales
        },


        #' @description
        #' Scales a dataset so that all continuous variables have a mean of 0 and a
        #' standard deviation of 1
        #' @param dat A dataframe or matrix whose columns are all numeric (i.e. dummy
        #' variables have all been expanded out) and whose columns are in the same
        #' order as the dataset used in the initialisation function
        scale = function(dat) {

            assert_that(
                is.data.frame(dat) | is.matrix(dat),
                all(vapply(dat, is.numeric, logical(1))),
                msg = "Input must be a numeric dataframe or matrix"
            )

            assert_that(
                ncol(dat) == length(self$centre),
                msg = sprintf("Input must have %s columns", length(self$centre))
            )

            dat2 <- dat

            dat2 <- sweep(
                dat2,
                MARGIN = 2,
                STATS = self$centre,
                FUN = `-`
            )

            dat2 <- sweep(
                dat2,
                MARGIN = 2,
                STATS = self$scales,
                FUN = `/`
            )

            class(dat2) <- class(dat)

            return(dat2)
        },


        #' @description
        #' Unscales a sigma value (or matrix) as estimated by a linear model
        #' using a design matrix scaled by this object. This function only
        #' works if the first column of the initialisation dataframe was the outcome
        #' variable.
        #' @param sigma A numeric value or matrix
        #' @return A numeric value or matrix
        unscale_sigma = function(sigma) {
            assert_that(
                is.matrix(sigma) | (is.numeric(sigma) & length(sigma) == 1),
                msg = "Input must be a matrix or a length 1 numeric vector"
            )
            return(sigma * self$scales[[1]]^2)
        },


        #' @description
        #' Unscales a beta value (or vector) as estimated by a linear model
        #' using a design matrix scaled by this object. This function only
        #' works if the first column of the initialisation dataframe was the outcome
        #' variable.
        #' @param beta A numeric vector of beta coefficients as estiamted from a linear model
        #' @return A numeric vector
        unscale_beta = function(beta) {

            len <- length(self$centre) - 1

            assert_that(
                is.numeric(beta),
                length(beta) == len,
                msg = sprintf("`beta` must be a numeric vector of length %s", len)
            )

            b_0 <- beta[1]
            b_i <- beta[-1]

            mu_y <- self$centre[1]
            mu_i <- self$centre[-c(1, 2)]

            sig_y <- self$scales[1]
            sig_i <- self$scales[-c(1, 2)]

            unscaled_beta <- c(
                mu_y + sig_y * b_0 - sum(sig_y * b_i * mu_i / sig_i),
                b_i * sig_y / sig_i
            )
            return(unscaled_beta)
        }
    )
)
