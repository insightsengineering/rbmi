
#' Title
#'
#' Descriptions
#' @return TODO
scalerConstructor <- R6::R6Class(
    classname = "scaler",
    public = list(

        #' @field center TODO
        center = NULL,

        #' @field scales TODO
        scales = NULL,


        #' @description
        #' TODO
        #' @param dat TODO
        #' @return TODO
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

            center <- vapply(
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

            center[cat_flag] <- 0
            scales[cat_flag] <- 1
            self$center <- center
            self$scales <- scales
        },


        #' @description
        #' TODO
        #' @param dat TODO
        #' @return TODO
        scale = function(dat) {

            assert_that(
                is.data.frame(dat) | is.matrix(dat),
                all(vapply(dat, is.numeric, logical(1))),
                msg = "Input must be a numeric dataframe or matrix"
            )

            assert_that(
                ncol(dat) == length(self$center),
                msg = sprintf("Input must have %s columns", length(self$center))
            )

            dat2 <- dat

            dat2 <- sweep(
                dat2,
                MARGIN = 2,
                STATS = self$center,
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
        #' TODO
        #' @param sigma TODO
        #' @return TODO
        unscale_sigma = function(sigma) {
            assert_that(
                is.matrix(sigma) | (is.numeric(sigma) & length(sigma) == 1),
                msg = "Input must be a matrix or a length 1 numeric vector"
            )
            return(sigma * self$scales[[1]]^2)
        },


        #' @description
        #' TODO
        #' @param beta TODO
        #' @return TODO
        unscale_beta = function(beta) {

            len <- length(self$center) - 1

            assert_that(
                is.numeric(beta),
                length(beta) == len,
                msg = sprintf("`beta` must be a numeric vector of length %s", len)
            )

            b_0 <- beta[1]
            b_i <- beta[-1]

            mu_y <- self$center[1]
            mu_i <- self$center[-c(1, 2)]

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
