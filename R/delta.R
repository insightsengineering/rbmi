

#' Create a delta data.frame template
#'
#' @description
#' Creates a data.frame in the format required by [analyse()] for the use
#' of applying a delta adjustment.
#'
#' @details
#' To apply a delta adjustment the [analyse()] function expects
#' a delta data.frame with 3 variables; `vars$subjid`, `vars$visit` and `delta`
#' (where `vars` is the object supplied in the original call to [draws()]
#' as created by the [set_vars()] function).
#'
#' These functions will return a data.frame with the aforementioned variables with 1
#' row per subject per visit. If the `delta` argument to this function is `NULL`
#' then the `delta` column in the returned data.frame will be 0 for all observations.
#' If the `delta` argument is not `NULL` then `delta` will be calculated separately
#' for each subject as the accumulative sum of `delta` multiplied by the scaling
#' coefficient `dlag` based upon how many visits after the subject's intercurrent
#' event (ICE) the visit in question is.
#' This is best illustrated with an example:
#'
#' Let `delta = c(5,6,7,8)` and `dlag=c(1,2,3,4)` (i.e. assuming there are 4 visits)
#' and lets say that the subject had an ICE on visit 2. The calculation would then be
#' as follows:
#'
#' ```
#' v1  v2  v3  v4
#' --------------
#'  5   6   7   8  # delta assigned to each visit
#'  0   1   2   3  # scaling starting from the first visit after the subjects ICE
#' --------------
#'  0   6  14  24  # delta * scaling
#' --------------
#'  0   6  20  44  # accumulative sum / delta to be applied to each visit
#' ```
#'
#' That is to say the subject would have a delta offset of 0 applied for visit-1, 6
#' for visit-2, 20 for visit-3 and 44 for visit-4. As a comparison, lets say that the
#' subject instead had their ICE on visit 3, the calculation would then be as follows:
#'
#' ```
#' v1  v2  v3  v4
#' --------------
#'  5   6   7   8  # delta assigned to each visit
#'  0   0   1   2  # scaling starting from the first visit after the subjects ICE
#' --------------
#'  0   0   7  16  # delta * scaling
#' --------------
#'  0   0   7  23  # accumulative sum / delta to be applied to each visit
#' ```
#'
#' In terms of practical usage, lets say that you wanted a delta of 5 to be used for all post ICE visits
#' regardless of their proximity to the ICE visit. This can be achieved by setting
#' `delta = c(5,5,5,5)` and `dlag = c(1,0,0,0)`. For example lets say a subject had their
#' ICE on visit-1, then the calculation would be as follows:
#'
#' ```
#' v1  v2  v3  v4
#' --------------
#'  5   5   5   5  # delta assigned to each visit
#'  1   0   0   0  # scaling starting from the first visit after the subjects ICE
#' --------------
#'  5   0   0  0  # delta * scaling
#' --------------
#'  5   5   5  5  # accumulative sum / delta to be applied to each visit
#' ```
#'
#' Another way of using these arguments
#' is to set `delta` to be the difference in time between visits and `dlag` to be the
#' amount of delta per unit of time. For example lets say that we have a visit on weeks
#' 1, 5, 6 & 9 and that we want a delta of 3 to be applied for each week after an ICE. This
#' can be achieved by setting `delta = c(0,4,1,3)` (the difference in weeks between each visit)
#' and `dlag = c(3, 3, 3, 3)`. For example lets say we have a subject who had their ICE on week-5
#' (i.e. visit-2) then the calculation would be:
#'
#' ```
#' v1  v2  v3  v4
#' --------------
#'  0   4   1   3  # delta assigned to each visit
#'  0   0   3   3  # scaling starting from the first visit after the subjects ICE
#' --------------
#'  0   0   3   9  # delta * scaling
#' --------------
#'  0   0   3  12  # accumulative sum / delta to be applied to each visit
#' ```
#'
#' i.e. on week-6 (1 week after the ICE) they have a delta of 3 and on week-9 (4 weeks after the ICE)
#' they have a delta of 12.
#'
#'
#' Please note that this function also returns several utility variables so that
#' the user can create their own custom logic for defining what `delta`
#' should be set to. These additional variables include:
#'
#' - `is_mar` - If the observation was missing would it be regarded as MAR ? This variable
#' is set to `FALSE` if it occurred after a non-MAR ICE, otherwise it is set to `TRUE`.
#' - `is_missing` - Is the outcome variable for this observation missing.
#' - `is_post_ice` - Does the observation occur after the patients ICE as defined by the
#' `data_ice` dataset supplied to [draws()].
#' - `strategy` - What imputation strategy was assigned to for this subject.
#'
#'
#' The design and implementation of this function is largely based upon the same functionality
#' as implemented in the so called "five marcos" by James Roger. See Roger (2021).
#'
#' @references
#' Roger, James. Reference-based mi via multivariate normal rm (the “five macros” and miwithd), 2021. URL
#' https://www.lshtm.ac.uk/research/centres-projects-groups/missing-data#dia-missing-data.
#'
#'
#' @param imputations an `imputation` object as created by [impute()].
#'
#' @param delta `NULL` or a numeric vector. Determines the baseline amount of delta
#' to be applied to each visit. See details. If a numeric vector it must have
#' the same length as the number of unique visits in the original dataset.
#'
#' @param dlag `NULL` or a numeric vector. Determines the scaling to be applied
#' to `delta` based upon with visit the ICE occurred on. See details. If a
#' numeric vector it must have the same length as the number of unique visits in
#' the original dataset.
#'
#' @param missing_only Logical, if `TRUE` then non-missing post-ICE data will have a delta value
#' of 0 assigned. Note that the calculation (as described in the details section) is performed
#' first and then overwritten with 0's at the end (i.e. the delta values for missing
#' post-ICE visits will stay the same regardless of this option).
#'
#' @examples
#' \dontrun{
#' delta_template(imputeObj)
#' delta_template(imputeObj, delta = c(5,6,7,8), dlag=c(1,2,3,4))
#' }
#' @seealso [analyse()]
#' @export
delta_template <- function(
    imputations,
    delta = NULL,
    dlag =  NULL,
    missing_only = TRUE
) {
    dat <- get_delta_template(imputations)

    if(is.null(delta)) return(dat)

    ld <- imputations$data

    assert_that(
        is.numeric(delta),
        is.numeric(dlag),
        length(delta) == length(dlag),
        length(delta) == length(ld$visits),
        msg = sprintf(
            "`delta` and `dlag` must both be a length %s numeric vector",
            length(ld$visits)
        )
    )

    assert_that(
        is.logical(missing_only),
        length(missing_only) == 1,
        msg = "`missing_only` must be TRUE or FALSE"
    )

    delta_list <- lapply(
        ld$ids,
        function(id) {
            d_lagscale(
                delta = delta,
                dlag = dlag,
                is_post_ice = ld$is_post_ice[[id]]
            )
        }
    )
    delta <- unlist(delta_list)
    assert_that(length(delta) == length(dat[["delta"]]))
    dat["delta"] <- delta

    if (missing_only) {
        ## Correction to remove any delta assigned to none missing values
        dat$delta[!dat[["is_missing"]] & dat[["is_post_ice"]]] <- 0
    }

    return(dat)
}



#' Get delta utility variables
#'
#' This function creates the default delta template (1 row per subject per visit)
#' and extracts all the utility information that users need to define their own logic
#' for defining delta. See [delta_template()] for full details.
#'
#' @param imputations an imputations object created by [impute()].
get_delta_template <- function(imputations){
    ld <- imputations$data
    x <- lapply(
        ld$ids,
        function(id) {
            strat <- ifelse(ld$is_mar[[id]], "MAR", ld$strategies[[id]])
            strat[!ld$is_missing[[id]]] <- NA_character_

            df <- data.frame(
                "is_mar" = ld$is_mar[[id]],
                "is_missing" = ld$is_missing[[id]],
                "is_post_ice" = ld$is_post_ice[[id]],
                "strategy" = strat,
                "delta" = 0,
                stringsAsFactors = FALSE
            )
            df[[ld$vars$subjid]] <- factor(id, levels = ld$ids)
            df[[ld$vars$visit]] <- factor(ld$visits, labels = ld$visits)
            df[[ld$vars$group]] <- ld$group[[id]]

            vars <- c(
                ld$vars$subjid,
                ld$vars$visit,
                ld$vars$group,
                "is_mar",
                "is_missing",
                "is_post_ice",
                "strategy",
                "delta"
            )

            return(df[, vars])
        }
    )
    Reduce(rbind, x)
}


#' Calculate delta from a lagged Sscale coefficient
#'
#' @description
#' Calculates a delta value based upon a baseline delta value and a
#' post ICE scaling coefficient.
#'
#' @param delta a numeric vector. Determines the baseline amount of delta
#' to be applied to each visit.
#'
#' @param dlag a numeric vector. Determines the scaling to be applied
#' to `delta` based upon with visit the ICE occurred on. Must be the same
#' length as delta.
#'
#' @param is_post_ice logical vector. Indicates whether a visit is "post-ICE" or
#' not.
#'
#'
#' @details
#' See [delta_template()] for full details on how this calculation is performed.
d_lagscale <- function(delta, dlag, is_post_ice) {

    assert_that(
        is.numeric(dlag),
        is.numeric(delta),
        is.logical(is_post_ice),
        msg = "`dlag` and `delta` must be numeric, `is_post_ice` must be logical"
    )

    assert_that(
        length(delta) == length(dlag),
        length(delta) == length(is_post_ice),
        msg = "`delta`, `dlag` and `is_post_ice` must all be the same length"
    )

    result <- rep(0, length(dlag))
    if (all(!is_post_ice)) {
        return(result)
    }
    is_post_ice_scale <- delta[is_post_ice]
    is_post_ice_lag <- dlag[seq_len(length(is_post_ice_scale))]
    vals <- cumsum(is_post_ice_lag * is_post_ice_scale)
    result[is_post_ice] <- vals
    return(result)
}



#' Applies delta adjustment
#'
#' @description
#' Takes a delta dataset and adjusts the outcome variable by adding the
#' corresponding delta.
#'
#' @param data data.frame which will have its `outcome` column adjusted.
#' @param delta data.frame (must contain a column called `delta`).
#' @param group character vector of variables in both `data` and `delta` that will be used
#' to merge the 2 data.frames together by.
#' @param outcome character, name of the outcome variable in `data`.
apply_delta <- function(data, delta = NULL, group = NULL, outcome = NULL) {

    assert_that(
        is.character(group),
        length(group) >= 1,
        is.character(outcome),
        length(outcome) == 1,
        msg = "`group` and `outcome` must be character vectors"
    )

    assert_that(
        is.data.frame(data),
        is.data.frame(delta) | is.null(delta),
        msg = "`dat` and `delta` must be data.frames"
    )

    assert_that(
        !"delta" %in% names(data),
        msg = " `delta` is a reserved variable name should not be already defined in `data`"
    )

    if (is.null(delta)) {
        return(data)
    }
    if (nrow(delta) == 0) {
        return(data)
    }

    for (var in c(group, outcome)) {
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` is not in `data`", var)
        )
    }

    for (var in c(group, "delta")) {
        assert_that(
            var %in% names(delta),
            msg = sprintf("Variable `%s` is not in `delta`", var)
        )
    }

    delta_min <- delta[, c(group, "delta")]

    # We insert a variable in order to recover the original sort order of our data.frame
    # We use a obfuscated name in order to prevent variable overwriting
    sort_var <- "sort_variable_awdji394thgmngrq2rqb2"
    data[[sort_var]] <- seq_len(nrow(data))

    data2 <- merge(
        data,
        delta_min,
        all.x = TRUE,
        by = group
    )

    # Restore sort order and remove variable
    data2 <- data2[order(data2[[sort_var]]), ]
    data[[sort_var]] <- NULL  # As we rely on the names of data later to reduce data2

    data2[is.na(data2[["delta"]]), "delta"] <- 0
    data2[[outcome]] <- data2[[outcome]] + data2[["delta"]]

    data3 <- data2[, names(data)]

    assert_that(
        ncol(data3) == ncol(data),
        nrow(data3) == nrow(data),
        all(names(data3) == names(data)),
        msg = paste0(
            "Data structure has been altered whilst applying delta. ",
            "This is most likely caused by having duplicate rows per id within the delta dataset"
        )
    )
    class(data3) <- class(data)
    return(data3)
}
