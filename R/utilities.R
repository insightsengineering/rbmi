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
    if (length(sigma) == 1 && length(mu) == 1) {
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

    if (blank && is.character(x)) {
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
    if (is.null(x)) {
        return(x)
    }
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
    res_list <- lapply(subs, function(x) grepl(x, strings, fixed = TRUE))
    res_matrix <- matrix(
        unlist(res_list),
        nrow = length(res_list),
        byrow = TRUE
    )
    res <- unlist(
        apply(res_matrix, MARGIN = 2, any, simplify = FALSE),
        use.names = TRUE
    )
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


#' Format method descriptions
#'
#' This function formats method descriptions by combining method names and their descriptions.
#'
#' @param method A named list of methods and their descriptions.
#' @return A character vector of formatted method descriptions.
#' @details If any non-atomic elements are present in the method list, they are converted to
#' a string representation using `dput()`.
format_method_descriptions <- function(method) {
    assertthat::assert_that(is.list(method))

    is_atomic <- vapply(method, is.atomic, logical(1))
    if (any(!is_atomic)) {
        method[!is_atomic] <- lapply(
            method[!is_atomic],
            function(x) {
                paste(
                    capture.output(dput(x)),
                    collapse = "\n    "
                )
            }
        )
    }
    vapply(
        mapply(
            function(x, y) sprintf("    %s: %s", y, x),
            method,
            names(method),
            USE.NAMES = FALSE,
            SIMPLIFY = FALSE
        ),
        identity,
        character(1)
    )
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


#' Ensure `rstan` exists
#'
#' Checks to see if rstan exists and if not throws a helpful error message
#' @keywords internal
ensure_rstan <- function() {
    if (!requireNamespace("rstan", quietly = TRUE)) {
        stop(
            "In order to use `method_bayes()` the `rstan` package must be installed.",
            " This can be installed from CRAN by running:\n\n",
            "      install.packages('rstan')\n\n",
            "Please note that for `rstan` to work you need to ensure you have a valid C++ toolchain;",
            " for details please see:\n",
            "https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#configuring-c-toolchain\n\n"
        )
    }
}

#' Get session hash
#'
#' Gets a unique string based on the current R version and relevant packages.
#' @importFrom utils sessionInfo
#' @keywords internal
get_session_hash <- function() {
    pkg_versions <- vapply(
        sessionInfo(c("rbmi", "rstan", "Rcpp", "RcppEigen"))[["otherPkgs"]],
        function(x) x[["Version"]],
        character(1L)
    )
    version_string <- paste0(
        R.version.string,
        paste0(names(pkg_versions), pkg_versions, collapse = ":")
    )
    temp_file <- tempfile()
    writeLines(version_string, temp_file)
    hash <- tools::md5sum(temp_file)
    unlist(temp_file)
    return(hash)
}

#' Clear Model Cache
#'
#' Clears the compiled Stan model cache, keeping only the models that match the `keep` argument.
#'
#' @param keep A character string that specifies which models to keep in the cache.
#' @param cache_dir The directory where the compiled Stan models are cached. Defaults to the option `rbmi.cache_dir`.
#' @keywords internal
clear_model_cache <- function(keep, cache_dir = getOption("rbmi.cache_dir")) {
    assert_that(assertthat::is.string(keep))
    all_model_files <- list.files(
        cache_dir,
        pattern = "(rbmi_MMRM_).*(\\.stan|\\.rds)",
        full.names = TRUE
    )
    should_keep <- grepl(pattern = keep, x = all_model_files, fixed = TRUE)
    old_model_files <- all_model_files[!should_keep]
    unlink(old_model_files)
}

#' List of Stan Blocks
#'
#' @description
#' A list with 1 element per standard Stan program blocks.
#' This object is mostly used internally as a reference for
#' what blocks are parsed from a covariance / prior Stan definition file.
#'
#' @export
STAN_BLOCKS <- list(
    functions = "functions",
    data = "data",
    parameters = "parameters",
    transformed_parameters = "transformed parameters",
    model = "model"
)

#' Conversion of Character Vector into Stan Code Block List
#'
#' @param x the single Stan code vector.
#' @param stan_blocks reference list of stan blocks.
#'
#' @return A list with the Stan code blocks.
#'
#' @author Craig Gower-Page (from `jmpost` R package)
#' @details
#' Function only works if code is in format
#' ```
#' data {
#'     <code>
#' }
#' model {
#'     <code>
#' }
#' ```
#' That is to say we do not support code in inline format i.e.
#' ```
#' data { <code> }
#' model { <code> }
#' ```
#'
#' @keywords internal
as_stan_fragments <- function(x, stan_blocks = STAN_BLOCKS) {
    code <- unlist(stringr::str_split(x, "\n"))

    errmsg <- paste(
        "There were problems parsing the `%s` block.",
        "Please consult the `Formatting Stan Files` section of the",
        "`Extending jmpost` vignette"
    )

    # Check to see if any block openings exist that have code on the same line
    # e.g.  `data { int i;}`. This is unsupported so we throw an error
    for (block in stan_blocks) {
        regex <- sprintf("^\\s*%s\\s*\\{\\s*[^\\s-]+", block)
        if (any(grepl(regex, code, perl = TRUE))) {
            stop(sprintf(errmsg, block))
        }
    }

    # We first look to identify the opening of a block e.g.  `data {`
    # We then regard all lines that follow as belonging to that block
    # until we see another block being opened e.g. `model{`
    results <- list()
    target <- NULL
    for (line in code) {
        for (block in names(stan_blocks)) {
            regex <- sprintf("^\\s*%s\\s*\\{\\s*$", stan_blocks[[block]])
            if (stringr::str_detect(line, regex)) {
                target <- block
                line <- NULL
                break
            }
        }
        if (!is.null(target)) {
            # This is memory inefficient but given the relatively small size of
            # stan files its regarded as a acceptable simplification to ease the
            # code burden
            results[[target]] <- c(results[[target]], line)
        }
    }

    # Loop over each block to remove trailing "}".
    for (block in names(results)) {
        block_length <- length(results[[block]])
        # The following processing is only required if the block actually has content
        if (block_length == 1 && results[[block]] == "") {
            next
        }
        has_removed_char <- FALSE
        # Walk backwards to find the closing `}` that corresponds to the `<block> {`
        for (index in rev(seq_len(block_length))) {
            line <- results[[block]][[index]]
            # This code will exit the for loop as soon as it hits the closing `}`
            # thus if we ever see a line that ends in text/numbers it means
            # somethings gone wrong
            if (stringr::str_detect(line, "[\\w\\d]+\\s*$")) {
                stop(sprintf(errmsg, block))
            }
            if (stringr::str_detect(line, "\\}\\s*$")) {
                new_line <- stringr::str_replace(line, "\\s*\\}\\s*$", "")
                # If the line is now blank after removing the closing `}` then drop the line
                keep_offset <- if (nchar(new_line) == 0) -1 else 0
                # Only keep lines from the start of the block to the closing `}`
                # this is to ensure we drop blank lines that were between the end
                # of the block and the start of the next
                keep_range <- seq_len(index + keep_offset)
                results[[block]][[index]] <- new_line
                results[[block]] <- results[[block]][keep_range]
                has_removed_char <- TRUE
                break
            }
        }
        # If we haven't actually removed a closing `}` then something has gone wrong...
        if (!has_removed_char) {
            stop(sprintf(errmsg, block))
        }
    }

    # Add any missing blocks back in
    for (block in names(stan_blocks)) {
        if (is.null(results[[block]])) {
            results[[block]] <- ""
        }
    }
    results
}

#' Find Stan File
#'
#' Finds a Stan file either in the local `inst/stan` directory or in the system package directory.
#'
#' @param file The name of the Stan file to find.
#' @param subdir Optional subdirectory within `inst/stan` where the file might be located.
#' @return The full path to the Stan file if found, otherwise an error is raised.
#'
#' @keywords internal
find_stan_file <- function(file, subdir = "") {
    assert_that(assertthat::is.string(file))
    assert_that(assertthat::is.string(subdir))

    local_file <- file.path("inst", "stan", subdir, file)
    system_file <- system.file(
        file.path("stan", subdir, file),
        package = "rbmi"
    )
    if (file.exists(local_file)) {
        local_file
    } else if (file.exists(system_file)) {
        system_file
    } else {
        stop(paste0("Unable to find ", file, "; Please report this as a bug"))
    }
}

#' Get Compiled Stan Object
#'
#' Gets a compiled Stan object that can be used with `rstan::sampling()`,
#' based on the choice of the covariance structure and the prior on the parameters.
#'
#' @keywords internal
get_stan_model <- function(covariance, prior_cov) {
    # Compiling Stan models updates the current seed state. This can lead to
    # non-reproducibility as compiling is conditional on wether there is a cached
    # model available or not. Thus we save the current seed state and restore it
    # at the end of this function so that it is in the same state regardless of
    # whether the model was compiled or not.
    # See https://github.com/insightsengineering/rbmi/issues/469
    # Note that .Random.seed is only set if the seed has been set or if a random number
    # has been generated.
    current_seed_state <- globalenv()$.Random.seed
    on.exit({
        if (
            is.null(current_seed_state) &&
                exists(".Random.seed", envir = globalenv())
        ) {
            rm(".Random.seed", envir = globalenv(), inherits = FALSE)
        } else {
            assign(
                ".Random.seed",
                value = current_seed_state,
                envir = globalenv(),
                inherits = FALSE
            )
        }
    })

    ensure_rstan()

    # Find the correct MMRM and covariance prior model Stan files.
    file_loc_mmrm <- find_stan_file("MMRM.stan")
    cov_prior_file <- paste0(covariance, "_", prior_cov, ".stan")
    file_loc_cov_prior <- find_stan_file(
        cov_prior_file,
        subdir = "covariance_priors"
    )

    # Replace constants in the covariance prior file and parse it
    # into a list of Stan code blocks.
    cov_prior_template <- jinjar::parse_template(
        fs::path(file_loc_cov_prior)
    )
    cov_prior_string <- jinjar::render(
        .x = cov_prior_template,
        machine_double_eps = .Machine$double.eps
    )
    cov_prior_blocks <- as_stan_fragments(cov_prior_string)
    cov_prior_blocks <- lapply(cov_prior_blocks, paste, collapse = "\n")

    # Decide file location for the final Stan model file.
    cache_dir <- getOption("rbmi.cache_dir")
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    session_hash <- get_session_hash()
    model_file <- file.path(
        cache_dir,
        paste0(
            "rbmi_MMRM_",
            session_hash,
            "_",
            cov_prior_file
        )
    )

    # If it does not exist yet, create the model file from the template
    # and save it to the cache directory.
    if (!file.exists(model_file)) {
        model_template <- jinjar::parse_template(
            fs::path(file_loc_mmrm),
            .config = jinjar::jinjar_config(
                trim_blocks = TRUE,
                lstrip_blocks = TRUE
            )
        )
        model_data <- c(
            cov_prior_blocks,
            machine_double_eps = .Machine$double.eps
        )
        model_string <- jinjar::render(
            .x = model_template,
            !!!model_data
        )
        clear_model_cache(keep = session_hash)
        writeLines(model_string, model_file)
    }

    rstan::stan_model(
        file = model_file,
        auto_write = getOption("rbmi.enable_cache"),
        model_name = paste0("rbmi_mmrm_", covariance, "_", prior_cov)
    )
}


#' rbmi settings
#'
#' @description
#' Define settings that modify the behaviour of the `rbmi` package
#'
#' Each of the following are the name of options that can be set via:
#' ```
#' options(<option_name> = <value>)
#' ```
#'
#' ## `rbmi.cache_dir`
#'
#' Default = `tools::R_user_dir("rbmi", which = "cache")`
#'
#' Directory to store compiled Stan models in to avoid having to re-compile.
#' If the environment variable `RBMI_CACHE_DIR` has been set this will be used
#' as the default value.
#' Note that if you are running rbmi in multiple R processes at the same time
#' (that is say multiple calls to `Rscript` at once) then there is a theoretical
#' risk of the processes breaking each other as they attempt to read/write to the
#' same cache folder at the same time. To avoid this potential issue it is recommended
#' to set the cache directory to a unique folder for each R session e.g.
#'
#' ```
#' options("rbmi.cache_dir" = tempdir(check = TRUE))
#' ```
#'
#' ## `rbmi.enable_cache`
#'
#' Default = `TRUE`
#'
#' If `TRUE` then the package will attempt to cache compiled Stan models to the
#' `rbmi.cache_dir` directory. If `FALSE` then the package will re-compile the
#' Stan model each time it is required. If the environment variable `RBMI_ENABLE_CACHE`
#' has been set this will be used as the default value.
#'
#' @examples
#' \dontrun{
#' options(rbmi.cache_dir = "some/directory/path")
#' }
#' @name rbmi-settings
set_options <- function() {
    cache_dir <- Sys.getenv(
        "RBMI_CACHE_DIR",
        unset = tools::R_user_dir("rbmi", which = "cache")
    )
    enable_cache <- isTRUE(as.logical(Sys.getenv(
        "RBMI_ENABLE_CACHE",
        unset = "TRUE"
    )))

    current_opts <- names(options())
    rbmi_opts <- list(
        rbmi.cache_dir = cache_dir,
        rbmi.enable_cache = enable_cache
    )
    for (opt in names(rbmi_opts)) {
        if (!opt %in% current_opts) {
            options(rbmi_opts[opt])
        }
    }
}
