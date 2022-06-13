
#' Set Class
#'
#' Utility function to set an objects class.
#'
#' @param x object to set the class of.
#' @param cls the class to be set.
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
#' @param x object to add a class to.
#' @param cls the class to be added.
#' @export
add_class <- function(x, cls) {
    class(x) <- c(class(x), cls)
    return(x)
}


#' Does object have a class ?
#'
#' Utility function to see if an object has a particular class.
#' Useful when we don't know how many other classes the object may
#' have.
#'
#' @return
#' `TRUE` if the object has the class.
#' `FALSE` if the object does not have the class.
#'
#' @param x the object we want to check the class of.
#' @param cls the class we want to know if it has or not.
#' @export
has_class <- function(x, cls) {
    cls %in% class(x)
}


#' Creates a simple formula object from a string
#'
#' Converts a string list of variables into a formula object
#'
#' @param outcome character (length 1 vector). Name of the outcome variable
#' @param covars character (vector). Name of covariates
#' @return
#' A formula
as_simple_formula <- function(outcome, covars) {
    frm <- stats::as.formula(
        paste0(
            outcome,
            "~ 1 + ",
            paste0(covars, collapse = " + ")
        )
    )
    environment(frm) <- globalenv()
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



#' Convert character variables to factor
#'
#' Provided a vector of variable names this function converts any
#' character variables into factors. Has no affect on numeric or existing
#' factor variables
#' @param data A data.frame
#' @param vars a character vector of variables in `data`
char2fct <- function(data, vars = NULL) {
    if (is.null(vars)) {
        vars <- colnames(data)
    }
    for (var in vars) {
        assert_that(
            var %in% colnames(data),
            msg = sprintf("Variable %s is not in data", var)
        )
        if (is.character(data[[var]])) {
            data[[var]] <- factor(data[[var]])
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





#' Set key variables
#'
#' @description
#' This function is used to define the names of key variables within the `data.frame`'s
#' that are provided as input arguments to [draws()] and [ancova()].
#'
#' @param subjid The name of the "Subject ID" variable. A length 1 character vector.
#'
#' @param visit The name of the "Visit" variable. A length 1 character vector.
#'
#' @param outcome The name of the "Outcome" variable. A length 1 character vector.
#'
#' @param group The name of the "Group" variable. A length 1 character vector.
#'
#' @param covariates The name of any covariates to be used in the context of modeling.
#' See details.
#'
#' @param strata The name of the any stratification variable to be used in the context of bootstrap
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
#' preserve the group size between samples. See [draws()] for more details.
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
#' to avoid issues with inconsistency on methods (such as  `[`() and dplyr's grouped dataframes)
as_dataframe <- function(x) {
    x2 <- as.data.frame(x)
    row.names(x2) <- NULL
    return(x2)
}

#' Add meta information to customerize analysis function
#'
#' This function is used only internally for ancova
#'
#' @param var_name A character variable of the names of the elements to be added to meta
#' @param ... The values of the element to be added to meta. The number of items should be equal  to the length of the name parameter
add_meta <- function (var_names, ...) {
    var_values <- list(...)
    prettier <- function(x) paste(x, collapse = ' ')
    assert_that(
        all(!is.null(var_names),
            !is.null(var_values),
            all(Vectorize(isTRUE)(!is.na(var_names))),
            length(var_names) == length(var_values)),
        msg = sprintf("Invalid parameters: `%s`, `%s`", prettier(var_names), prettier(var_values))
    )

    names(var_values) <- var_names
    var_values
}

#' Assert variable's type
#'
#' @param what Variable to be asserted
#' @param how Type asserting functions: is.character, is.numeric, is.list, is.logic
#' @examples
#' \dontrun{
#' assert_type(est, is.numeric)
#' }
assert_type <- function(what,
                        how,
                        whatname = deparse(substitute(what)),
                        howname = deparse(substitute(how))) {

    prettier <- function(...) gsub("_", " ", as.character(...))

    type <- (function(s) sub(".*\\.", "", s))(howname)
    assert_that(how(what),
                msg = sprintf("%s of analysis_result is not %s", whatname, prettier(type))
    )
}

#' Create assert function comparing value
#'
#' @param how A function to generate value from the object to be asserted
#' @param where A character variable indicating the origin of the object to be asserted. Default: `NULL`
#' @param howname A character variable indicating the name of the how function. Default: `deparse(substitute(how))`
assert_value <- function(how, where = NULL, howname = deparse(substitute(how))) {
    inwhere <- ''
    if (!is.null(where)) inwhere <- paste(' in', where)
    function(what,
             should,
             whatname = deparse(substitute(what))) {

        prettier <- function(x) paste(x, collapse = " ")

        assert_that(all(how(what) == should),
                    msg = sprintf("%s of %s%s `%s` is not %s", howname, whatname, inwhere, prettier(how(what)), prettier(should))
                    )
    }
}

#' Assert length of the element in analysis_result
#'
#' @param what The element to be asserted
#' @param should The length expected
#' @param whatname The name of the element. Default: `deparse(substitute(what))`
assert_anares_length <- assert_value(length, where = 'analysis_result')

#' Make a chain of function calls with certain relation function
#' @param relation A relation function: `any` or `all`
#' @param ... Functions to be chained
#' @return A function taking arguments that are feed into chained functions
#' @examples
#' \dontrun{
#' is.numeric_or_na <- make_chain(any, is.numeric, is.na)
#' is.numeric_or_na(NA) # returns TRUE
#' is.numeric_or_na(15) # returns TRUE
#' is.numeric_or_na('a') # returns FALSE
#' }
make_chain <- function(relation, ...) {
    fs <- c(...)
    function(...) relation(sapply(fs, function(f) isTRUE(f(...))))
}

#' Order a named list by its names according to given character vector
#' @param L A list to be ordered
#' @param v A character contains the names in order
#' @return A list with names in order
#' @examples
#' \dontrun{
#' L_ordered <- order_list_by_name(list(a=1,b='x',c=TRUE), c("c", "a", "d", "x", "b", "t"))
#' # returns a list `list(c=TRUE, a=1, b='x)`
#'
#' }
order_list_by_name <- function(L, v) {
    ordered_pos <- match(v, names(L))
    ordered_pos <- ordered_pos[!is.na(ordered_pos)]
    L[ordered_pos]
}

#' Convert nested list to data.frame
#'
#' @param nestlist A nested list to be converted to data.frame
#' @return A data.frame binding each sublist as row in the data.frame and with NA filled for missing values
base_bind_rows <- function(nestlist) {
    nms     <- unique(unlist(lapply(nestlist, names)))
    frmls   <- as.list(setNames(rep(NA, length(nms)), nms))
    dflst   <- setNames(lapply(nms, function(x) call("unlist", as.symbol(x))), nms)
    make_df <- as.function(c(frmls, call("do.call", "data.frame", dflst)))

    do.call(rbind, lapply(nestlist, function(x) do.call(make_df, x)))
}

#' Create name checkers for object with message passing dispatch
#'
#' @param ... Character vectors for the reference to check against
#' @param optional Character vector of optional name. Default: NULL
#' @return A constructor to create checker functions with message passing dispatch
namechecker <- function(..., optional = NULL) {

    # compile the musthave list at the top level so that easier to maintain and update
    musthave <- c(...)

    # message passing as a dispatch
    function(msg) {

        # function to check if elements in list X exist in Y
        XsInYs <- function(x, y) vapply(x, function(.x) .x %in% y, logical(1))

        # wrapper to swap order of formal parameter of binary function
        swap <- function(f) {
            function(x, y) f(y, x)
        }

        # higher-order function to create template for validators
        chker_template <- function(musthave, wrapper=identity, f = XsInYs, .optional = optional) {
            function(...) {
                wrapper(f)(append(musthave, .optional), names(...))
            }
        }

        # Validator to check if elements in musthave present in the object's name
        # checker does not check against optional names. Only names in musthave have to be presented in the object
        musthave_in_objnames <- chker_template(musthave, .optional = NULL)

        # Validator to check if object's name belongs to musthave + optional names (simply swap the order of arguments in musthave_in_objnames: B_in_A = swap(A_in_B))
        objnames_in_musthave <- chker_template(musthave, swap)

        dispatch <- list(
            musthave_in_objnames = musthave_in_objnames,
            objnames_in_musthave  = objnames_in_musthave,
            musthave = musthave,
            optional = optional,
            all = append(musthave, optional)
            )

        dispatch[[msg]]
    }
}

#' Higher-order function to compose function n-times
#'
#' Taking a function as f argument, this function convert it to another function apply this function n-times:
#' n = 1: f(x) ==> f(x)
#' n = 2: f(x) ==> f(f(x))
#' n = 4: f(x) ==> f(f(f(f(x)))
#' @param f function to be converted to composed version
#' @param n times to be composed
#' @examples
#' \dontrun{
#' add_one <- function (x) x + 1
#' add_two <- compose_n(add_one, 2)
#' add_two(5) # This equivalents to add_one(add_one(5)) and returns 7
#'
#' }
compose_n <- function(f, n) {
    function(x) {
        if (n <= 0) x
        else f(compose_n(f, n-1)(x))
    }
}

#' Apply function at last N levels of a nested list
#'
#' Recursively traverse a nested list and apply a function at the Nth level backward counting from deepest level
#' @param lst a list to be applied by the function
#' @param f a function to apply on `lst`
#' @param n a numeric value indicating the nth level to be applied backward counting from deepest level
#' @examples
#' \dontrun{
#' dt <- list(a1=list(
#'                b11=list(c111=1, c112=2,c113=3),
#'                b12=list(c121=4, c122=5,c123=6)),
#'            a2=list(
#'                b21=list(c211=7, c212=8,c213=9),
#'                b22=list(c221=10, c222=11,c223=12))
#'                )
#'       )
#'
#' back_apply_at(dt, function(x) x+1, 1) # This will apply `function(x) x+1` to the deepest level of `dt`, i.e. 1st level counting backward from deepest level
#'
#'}
back_apply_at <- function(lst, f, n) {
    lapply(lst,
           function(sublst) {
               nextNlevel <- compose_n(function(x) x[[1]], n-1)
               if (!is.list(nextNlevel(sublst))) f(sublst)
               else back_apply_at(sublst, f, n)
           }
    )
}

#' Convert vector to formula
#'
#' Convert character vector c('a1', 'a2') to formula ~ a1 + a2
#'
#' @param chr character vector to be converted
#' @param bothside A logical variable indicating whether to generate fomula with both right and left side. Default: FALSE - only right side formula will be generated
#' @return an object of formula class representing formula ~ chr[[1]] + chr[[2]] + ...
vec2form <- function(chr, bothside = FALSE) {
    prefix = '~'
    if (bothside) prefix = paste('.', prefix)
    eval(parse(text = paste(prefix, paste(chr, collapse = ' + '))))
}


#' Reduce a dataframe
#'
#' Reduce a data.frame row-wisely by concatenating values within group to list or multiple columns
#'
#' @param df A data frame to be reduced
#' @param keys A character vectors of the group keys when reducing
#' @param split A logical variable indicating whether to concatenate row-wise information to a list in single column or create multiple columns for each individual rows within group
#' @return A data frame with reduced information
reduce_df <- function(df, keys, split = FALSE) {
    make_concat <- function(f) function(x) f(unique(x))
    concat <- ife(split, make_concat(c), make_concat(list))
    pos_process <- ife(split, function(x) do.call(data.frame, x), identity)
    pos_process(
        aggregate(vec2form(keys, bothside = TRUE), data = df, FUN =  concat)
    )
}


