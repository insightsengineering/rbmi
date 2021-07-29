
#' Ancova
#'
#' @description
#' TODO
#'
#' @param data TODO
#' @param vars TODO
#' @param visit_level TODO
#'
#' @importFrom stats lm coef vcov df.residual
#'
#' @export
ancova <- function(data, vars, visit_level = NULL) {

    outcome <- vars[["outcome"]]
    group <- vars[["group"]]
    covariates <- vars[["covariates"]]
    visit <- vars[["visit"]]


    assert_that(
        is.character(outcome),
        length(outcome) == 1,
        msg = "`outcome` must be a length 1 character"
    )

    assert_that(
        is.character(group),
        length(group) == 1,
        msg = "`outcome` must be a length 1 character"
    )

    assert_that(
        is.character(covariates) | is.null(covariates),
        msg = "`covariates` must be a character vector"
    )

    expected_vars <- c(extract_covariates(covariates), outcome, group)

    if (!is.null(visit_level) | !is.null(visit)) {

        assert_that(
            is.character(visit),
            length(visit) == 1,
            msg = "`visit` must be a length 1 character"
        )

        assert_that(
            is.character(visit_level),
            length(visit_level) == 1,
            msg = "`visit_level` must be a length 1 character"
        )

       expected_vars <- c(expected_vars, visit)
    }

    for (var in expected_vars) {
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` doesn't exist in data", var)
        )
    }

    assert_that(
        is.factor(data[[group]]),
        length(levels(data[[group]])) == 2,
        msg = "Group variable `%s` must be a factor variable with 2 levels"
    )

    # Manually convert to dummary variables to make extraction easier
    data[[group]] <- as.numeric(data[[group]]) - 1

    if (!is.null(visit)) {
        data2 <- data[data[[visit]] == visit_level, ]
    } else {
        data2 <- data
    }

    data2 <- data2[, c(extract_covariates(covariates), outcome, group)]

    frm <- as_simple_formula(list(group = group, outcome = outcome, covariates = covariates))

    mod <- lm(formula = frm, data = data2)

    lsm <- as.data.frame(emmeans::emmeans(mod, group))

    x <- list(
        "trt" = list(
            "est" = coef(mod)[[group]],
            "se" = sqrt(vcov(mod)[group, group]),
            "df" = df.residual(mod)
        ),
        "lsm_0" = list(
            est = lsm$emmean[[1]],
            se = lsm$SE[[1]],
            df = df.residual(mod)
        ),
        "lsm_1" = list(
            est = lsm$emmean[[2]],
            se = lsm$SE[[2]],
            df = df.residual(mod)
        )
    )
    return(x)
}


# #' Title - TODO
# #' 
# #' @param data TODO
# #' @param frm TODO
# #' @param coef TODO
# #' @param sigma TODO
# #' @param df TODO
# #' @importFrom stats model.matrix
# least_square_means <- function(data, frm, coef, sigma, df = NA) {
#     assert_that(
#         length(coef) == nrow(sigma),
#         nrow(sigma) == ncol(sigma),
#         is.data.frame(data) 
#     )
#     coef <- matrix(coef, ncol = 1)
#     design <- model.matrix(frm, data = data)
#     design_mean <- matrix(apply(design, 2, mean, simplify = TRUE), nrow = 1)
#     point <- design_mean %*% coef
#     sigma <- design_mean %*% sigma %*% t(design_mean)
#     assert_that(
#         length(point) == 1,
#         length(sigma) == 1
#     )
#     ret <- list(
#         est = as.vector(point),
#         se = sqrt(as.vector(sigma)), 
#         df = df
#     )
#     return(ret)
# }



# library(dplyr)
# x <- tibble(
#     w = rnorm(1),
#     x = factor("b", levels = c("a", "b")),
#     y = factor("x", levels = c("x", "y"))
# )
# model.matrix(~ w * x * y, x)


# library(tidyr)
# library(purrr)
# library(emmeans)

# ##### Manually

# lsm <- function(design, coef) {
#     coef <- matrix(coef, ncol = 1)
#     design_mean <- matrix(apply(design, 2, mean, simplify = TRUE), nrow = 1)
#     point <- design_mean %*% coef
#     return(point)
# }

# # write.csv(i2, "~/i2.csv", row.names = TRUE)

# i2 <- iris
# i2[["group"]] <- rep(c("A", "B"), each = 75)
# i2[["group2"]] <- sample(c("A", "B", "C", "D"), size = 150, replace = TRUE)
# frm <- Sepal.Length ~ Sepal.Width + Species * group2 + group

# mod <- lm(data = i2, frm)

# i3 <- expand.grid(
#     Species = unique(i2$Species),
#     group = unique(i2$group),
#     group2 = unique(i2$group2)
# ) %>%
#     mutate(Sepal.Length = mean(iris$Sepal.Length)) %>%
#         mutate(Sepal.Width = mean(iris$Sepal.Width))
    
# i3_frame <- model.matrix(frm, i3) %>%
#     as_tibble() %>%
#     mutate(index = row_number()) %>%
#     nest(data = c(-index)) %>%
#     mutate(point = map_dbl(data, lsm, coef = coef(mod)))

# i3 %>%
#     mutate(point = i3_frame$point) %>%
#     group_by(group) %>%
#     summarise(est = mean(point))

# ##### emmeans

# emmeans(mod, "group")
# ref_grid(mod)@grid



