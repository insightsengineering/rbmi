
#' Set Class
#'
#' Utility function to set an objects class.
#'
#' @param x Object to set the class of
#' @param cls The class to be set
#' @export
as_class <- function(x, cls) {
    class(x) <- cls
    return(x)
}


#' Add a class
#'
#' Utility function to add a class to an object. Adds the new class
#' after any existing classes.
#'
#' @param x Object to add a class to
#' @param cls the class to be added
#' @export
add_class <- function(x, cls) {
    class(x) <- c(class(x), cls)
    return(x)
}


#' Does object have a class ?
#'
#' Utility function to see if an object has a particular class.
#' Useful when we don't know how many other classes the object may
#' have
#'
#' @return
#' True if the object has the class
#' False if the object does not have the class
#'
#' @param x The object we want to check the class of
#' @param cls The class we want to know if it has or not
#' @export
has_class <- function(x, cls) {
    cls %in% class(x)
}


#' Convert an `ivars` object into a formula
#'
#' Takes an `ivars` object (as created by [set_vars()]) and converts
#' it into a formula roughly of the form:
#' ```
#' outcome ~ group + visit + covariate1 + covariate2 + ...
#' ```
#'
#' @param vars an `ivars` object as created by [set_vars()]
#' @return
#' A formula
as_simple_formula <- function(vars) {
    variables <- c(
        vars$group,
        vars$visit,
        vars$covariates
    )
    frm <- stats::as.formula(
        paste0(
            vars$outcome,
            "~ 1 + ",
            paste0(variables, collapse = " + ")
        )
    )
    return(frm)
}



#' Expand `data.frame` into a design matrix
#'
#' Expands out a `data.frame` using a formula to create a design matrix.
#' Key details are that it will always place the outcome variable into
#' the first column of the return object.
#'
#' The outcome column may contain NA's but none of the other variables
#' listed in the formula should contain missing values
#'
#' @param dat a data.frame
#' @param frm a formula
as_model_df <- function(dat, frm) {

    outcome <- as.character(attr(stats::terms(frm), "variables")[[2]])
    is_missing <- is.na(dat[[outcome]])
    dat[[outcome]][is_missing] <- 999
    design_mat <- stats::model.matrix(frm, dat)
    dat[[outcome]][is_missing] <- NA

    assert_that(
        nrow(design_mat) == nrow(dat),
        msg = "Model matrix has less rows than input dataset. You may have missing values."
    )

    full_mat <- cbind(dat[[outcome]], design_mat)
    colnames(full_mat) <- c("outcome", paste0("V", seq_len(ncol(full_mat) - 1)))
    design <- as.data.frame(full_mat)
    class(design) <- class(dat)
    return(design)
}



#' Character 2 Factor
#'
#' Converts all character variables within a `data.frame` to factor.
#' Determines character variables by `is.character()`
#'
#' @param data A `data.frame`
char2fct <- function(data) {
    for (v in colnames(data)) {
        if (is.character(data[[v]])) {
            data[[v]] <- factor(data[[v]])
        }
    }
    return(data)
}



#' if else
#'
#' A wrapper around `if() else()` to prevent unexpected
#' interactions between `ifelse()` and factor variables
#'
#' @details
#' By default `ifelse()` will convert factor variables to their
#' numeric values which is often undesirable. This connivance
#' function avoids that problem
#'
#' @param x True / False
#' @param a value to return if True
#' @param b value to return if False
ife <- function(x, a, b) {
    if (x) {
        return(a)
    } else {
        return(b)
    }
}



#' Sample random values from the multivariate normal distribution
#'
#' @param mu mean vector
#' @param sigma covariance matrix
#'
#' Samples multivariate normal variables by multiplying
#' univariate random normal variables by the cholesky
#' decomposition of the covariance matrix.
#'
#' If mu is length 1 then just uses rnorm instead.
sample_mvnorm <- function(mu, sigma) {
    if (length(sigma) == 1 & length(mu) == 1) {
        return(rnorm(1, mu, sqrt(sigma)))
    }
    assert_that(
        is.matrix(sigma),
        nrow(sigma) == ncol(sigma),
        nrow(sigma) == length(mu),
        msg = "`mu` and `sigma` are not of compatible sizes"
    )
    x <- rnorm(nrow(sigma), mean = 0, sd = 1)
    (x %*% chol(sigma)) + as.vector(mu)
}



#' Capture all Output
#'
#' This function silences all warnings, errors & messages and instead returns a list
#' containing the results (if it didn't error) + the warning and error messages as
#' character vectors.
#'
#' @param expr An expression to be executed
#'
#' @return
#' A list containing
#'
#' - **results** - The object returned by `expr` or `list()` if an error was thrown
#' - **warnings** - NULL or a character vector if warnings were thrown
#' - **errors** - NULL or a string if an error was thrown
#' - **messages** - NULL or a character vector if messages were produced
#'
#' @examples
#' \dontrun{
#' record({
#'   x <- 1
#'   y <- 2
#'   warning("something went wrong")
#'   message("O nearly done")
#'   x + y
#' })
#' }
record <- function(expr) {
    env <- new.env()
    result <- withCallingHandlers(
        withRestarts(
            expr,
            muffleStop = function() list()
        ),
        message = function(m) {
            env$message <- c(env$message, m$message)
            invokeRestart("muffleMessage")
        },
        warning = function(w) {
            env$warning <- c(env$warning, w$message)
            invokeRestart("muffleWarning")
        },
        error = function(e) {
            env$error <- c(env$error, e$message)
            invokeRestart("muffleStop")
        }
    )
    list(
        results = result,
        warnings = env$warning,
        errors = env$error,
        messages = env$message
    )
}


#' Is value absent
#'
#' Returns true if a value is either NULL, NA or "".
#' In the case of a vector all values must be NULL/NA/""
#' for x to be regarded as absent.
#'
#' @param x a value to check if it is absent or not
#' @param na do NAs count as absent
#' @param blank do blanks i.e. "" count as absent
is_absent <- function(x, na = TRUE, blank = TRUE) {
    if (is.null(x)) {
        return(TRUE)
    }

    if (na) {
        if (all(is.na(x))) {
            return(TRUE)
        }
    }

    if (blank & is.character(x)) {
        if (all(x == "")) {
            return(TRUE)
        }
    }

    return(FALSE)
}



#' Extract Variables from string vector
#'
#' Takes a string including potentially model terms like `*` and `:` and
#' extracts out the individual variables
#'
#' i.e.  `c("v1", "v2", "v2*v3", "v1:v2")` becomes `c("v1", "v2", "v3")`
#'
#' @param x string of variable names potentially including interaction terms
extract_covariates <- function(x) {
    if (is.null(x)) return(x)
    x_split <- strsplit(x, ":|\\*")
    x_vec <- unlist(x_split, use.names = FALSE)
    x_nws <- trimws(x_vec)
    x_uni <- unique(x_nws)
    return(x_uni)
}



#' Does a string contain a substring
#'
#' @param x character vector
#' @param subs a character vector of substrings to look for
#'
#' @description
#' Returns a vector of `TRUE`/`FALSE` for each element of x
#' if it contains any element in `subs`
#'
#' i.e.
#' ```
#' str_contains( c("ben", "tom", "harry"), c("e", "y"))
#' [1] TRUE FALSE TRUE
#' ```
str_contains <- function(x, subs) {
    strings <- x
    res_list <- lapply(subs, function(x) grepl(x, strings, fixed = T))
    res_matrix <- matrix(unlist(res_list), nrow = length(res_list), byrow = TRUE)
    res <- unlist(apply(res_matrix, MARGIN = 2, any, simplify = FALSE), use.names = TRUE)
    assert_that(length(res) == length(strings))
    return(res)
}






#' Sort `data.frame`
#'
#' Sorts a `data.frame` (ascending by default) based upon variables within the dataset
#' @param df data.frame
#' @param vars character vector of variables
#' @param decreasing logical whether sort order should be in descending or ascending (default) order.
#' Can be either a single logical value (in which case it is applied to
#' all variables) or a vector which is the same length as `vars`
#' @examples
#' \dontrun{
#' sort_by(iris, c("Sepal.Length", "Sepal.Width"), decreasing = c(TRUE, FALSE))
#' }
sort_by <- function(df, vars = NULL, decreasing = FALSE) {
    if (is.null(vars)) {
        return(df)
    }
    assert_that(
        is.data.frame(df),
        all(vars %in% names(df)),
        is.logical(decreasing),
        length(decreasing) == 1 | length(decreasing) == length(vars)
    )
    args <- as.list(df[, vars, drop = FALSE])
    args$decreasing <- decreasing
    ord <- do.call(order, args)
    df2 <- df[ord, ]
    assert_that(nrow(df) == nrow(df2), ncol(df) == ncol(df2))
    return(df2)
}





#' Set Key Variables
#'
#' @description
#' This function is used to define the names of key variables within the `data.frame`'s
#' that are provided as input arguments to [draws()] and [ancova()].
#'
#' @param subjid The name of the "subject ID" variable. A length 1 character vector.
#'
#' @param visit The name of the "Visit" variable. A length 1 character vector.
#'
#' @param outcome The name of the "Outcome" variable. A length 1 character vector.
#'
#' @param group The name of the "group" variable. A length 1 character vector.
#'
#' @param covariates The name of any covariates to be used in the context of modelling.
#' See details.
#'
#' @param strata The name of the any stratification models to be used in the context of bootstrap
#' sampling. See details.
#'
#' @param strategy The name of the "strategy" variable. A length 1 character vector.
#'
#' @details
#'
#' In both [draws()] and [ancova()] the `covariates` argument can be specified to indicate
#' which variables should be included in the imputation and analysis models respectively. If you wish
#' to include interaction terms these need to be manually specified i.e.
#' `covariates = c("group*visit", "age*sex")`. Please note that the use of the [I()] function to
#' inhibit the interpretation/conversion of objects is not supported.
#'
#' Currently `strata` is only used by [draws()] in combination with `method_condmean(type = "bootstrap")`
#' and `method_approxbayes()` in order to allow for the specification of stratified bootstrap sampling.
#' By default `strata` is set equal to the value of `group` as it is assumed most users will want to
#' preserver the group size between samples. See [draws()] for more details.
#'
#' Likewise, currently the `strategy` argument is only used by [draws()] to specify the name of the
#' strategy variable within the `data_ice` data.frame. See [draws()] for more details.
#'
#' @seealso [draws()]
#' @seealso [ancova()]
#'
#' @examples
#' \dontrun{
#'
#' # Using CDISC variable names as an example
#' set_vars(
#'     subjid = "usubjid",
#'     visit = "avisit",
#'     outcome = "aval",
#'     group = "arm",
#'     covariates = c("bwt", "bht", "arm * avisit"),
#'     strategy = "strat"
#' )
#'
#' }
#'
#' @export
set_vars <- function(
    subjid = "subjid",
    visit = "visit",
    outcome = "outcome",
    group = "group",
    covariates = character(0),
    strata = group,
    strategy = "strategy"
) {
    x <- list(
        subjid = subjid,
        visit = visit,
        outcome = outcome,
        group = group,
        covariates = covariates,
        strata = strata,
        strategy = strategy
    )
    class(x) <- c("ivars", "list")
    validate(x)
    return(x)
}



#' Validate inputs for `vars`
#'
#' Checks that the required variable names are defined within `vars` and
#' are of appropriate datatypes
#'
#' @param x named list indicating the names of key variables in the source dataset
#' @param ... not used
#' @export
validate.ivars <- function(x, ...) {

    assert_that(
        is_char_one(x$outcome),
        msg = "`vars$outcome` should be a length 1 character"
    )

    assert_that(
        is_char_one(x$group),
        msg = "`vars$group` should be a length 1 character"
    )

    assert_that(
        is_char_one(x$visit),
        msg = "`vars$visit` should be a length 1 character"
    )

    assert_that(
        is_char_one(x$subjid),
        msg = "`vars$subjid` should be a length 1 character"
    )

    assert_that(
        is_char_one(x$strategy),
        msg = "`vars$strategy` should be a length 1 character"
    )

    covars <- extract_covariates(x$covariates)
    assert_that(
        is.character(covars) | is.null(covars),
        msg = "`vars$covars` should be a character vector or NULL"
    )

    assert_that(
        is.character(x$strata) | is.null(x$strata),
        msg = "`vars$strata` should be a character vector or NULL"
    )
    return(invisible(TRUE))
}


#' Is single character
#'
#' returns true if x is a length 1 character vector
#'
#' @param x a character vector
is_char_one <- function(x) {
    is.character(x) & (length(x) == 1)
}


#' Is character or factor
#'
#' returns true if x is character or factor vector
#'
#' @param x a character or factor vector
is_char_fact <- function(x) {
    is.character(x) | is.factor(x)
}

#' Is character, factor or numeric
#'
#' returns true if x is a character, numeric or factor vector
#'
#' @param x a character, numeric or factor vector
is_num_char_fact <- function(x) {
    is.numeric(x) | is.character(x) | is.factor(x)
}



#' Convert object to dataframe
#'
#' @param x a data.frame like object
#'
#' Utility function to convert a "data.frame-like" object to an actual `data.frame`
#' to avoid issues with inconsitencies on methods (such as  `[`() and dplyr's grouped dataframes)
as_dataframe <- function(x) {
    x2 <- as.data.frame(x)
    row.names(x2) <- NULL
    return(x2)
}
